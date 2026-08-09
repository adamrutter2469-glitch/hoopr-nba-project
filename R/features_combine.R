# ============================================================
# Script: R/features_combine.R
# Purpose: Join box scores + rest/travel + rolling averages +
#          advanced rebounding into the two "comprehensive"
#          analysis-ready tables that are the actual point of a
#          pipeline run:
#            data_processed/team_game_features.parquet   (1 row/team-game)
#            data_processed/player_game_features.parquet (1 row/player-game)
#          Also updates state/manifest.json. These two are single
#          files, fully rewritten each run - cheap at this data's
#          size, unlike the raw season-partitioned datasets.
# ============================================================

build_team_game_features <- function(cfg, logger) {
  team_logs <- read_full_dataset(cfg$path_team_logs_dataset)

  if (is.null(team_logs)) {
    logger$log("team_game_features: SKIPPED, no team game logs yet.")
    return(invisible(NULL))
  }

  team_logs <- team_logs %>%
    dplyr::mutate(team_id = as.character(team_id), game_id_nba = as.character(game_id_nba))

  # 1. Own box-score rolling (now includes oreb/dreb, not just total reb -
  #    see ROLLING_STAT_CANDIDATES in R/features_rolling.R).
  out <- compute_team_rolling_features(team_logs, windows = cfg$rolling_windows)

  # 2. Rest/travel - brings in opponent_id, which every matchup feature
  #    below keys on. opponent_id comes from hoopR::nba_schedule() as
  #    int32, so it MUST be cast to character like every other join key
  #    here, or the self-joins in stages 4-5 fail on a type mismatch.
  rest_travel <- read_parquet_or_null(cfg$path_schedule_team_level)
  if (!is.null(rest_travel)) {
    rest_travel <- rest_travel %>%
      dplyr::mutate(
        team_id     = as.character(team_id),
        game_id_nba = as.character(game_id_nba),
        opponent_id = as.character(opponent_id)
      ) %>%
      dplyr::select(
        team_id, game_id_nba, opponent_id, home_away,
        rest_days, opp_rest_days, is_b2b, opp_is_b2b, is_3in4, opp_is_3in4,
        est_flight_time, opp_est_flight_time, time_zone_shift, opp_time_zone_shift
      )
    out <- out %>% dplyr::left_join(rest_travel, by = c("team_id", "game_id_nba"))
  } else {
    logger$log("team_game_features: rest/travel table not found yet, joining without it.")
  }

  # 3. Advanced rebounding: raw this-game actuals (adv_*, kept for
  #    target/QA purposes only - never a predictor, see models/), plus
  #    their own rolling averages (previously only the raw actuals
  #    existed - nothing about a team's trailing advanced-rebounding
  #    form was available before this).
  reb <- read_parquet_or_null(cfg$path_rebounding_features)
  if (!is.null(reb)) {
    reb <- reb %>%
      dplyr::mutate(team_id = as.character(team_id), game_id_nba = as.character(game_id_nba)) %>%
      dplyr::rename_with(~ paste0("adv_", .x), -c(team_id, game_id_nba))
    out <- out %>% dplyr::left_join(reb, by = c("team_id", "game_id_nba"))

    adv_rolled <- out %>%
      dplyr::select(team_id, game_id_nba, season, game_date, dplyr::any_of(ADV_REBOUNDING_STAT_COLS)) %>%
      compute_adv_rebounding_rolling_features(windows = cfg$rolling_windows) %>%
      dplyr::select(team_id, game_id_nba, dplyr::matches("^adv_.*_roll\\d+$"))
    out <- out %>% dplyr::left_join(adv_rolled, by = c("team_id", "game_id_nba"))
  } else {
    logger$log("team_game_features: advanced rebounding table not found yet, joining without it.")
  }

  # 4-6: opponent/matchup features. All key on opponent_id, so skip
  # gracefully (rather than error) on a cold-start run where stage 2
  # didn't run yet.
  if ("opponent_id" %in% names(out)) {
    # 4. "Allowed": this team's own trailing history of what opponents
    #    have actually put up against them - "how many boards does this
    #    team typically give up." Scoped to rebounding stats only (not
    #    the full pts/ast/etc. list) to keep the table width reasonable.
    allowed_stat_cols <- c("reb", "oreb", "dreb", ADV_REBOUNDING_STAT_COLS)
    raw_full <- out %>%
      dplyr::select(team_id, opponent_id, game_id_nba, season, game_date,
                     dplyr::any_of(allowed_stat_cols))
    allowed_rolled <- compute_allowed_rolling_features(raw_full, allowed_stat_cols, cfg$rolling_windows)
    out <- out %>% dplyr::left_join(allowed_rolled, by = c("team_id", "game_id_nba"))

    # 5. Mirror the opponent's own profile onto this team's row - both
    #    their offensive rolling rebounding stats AND their own
    #    allowed-rolling stats - so "the opponent's rolling form" and
    #    "what the opponent typically allows" both sit side by side
    #    with this team's own numbers. Must run after stages 1-4, since
    #    it mirrors columns those stages produce.
    own_reb_roll_cols <- grep("^(reb|oreb|dreb|adv_[a-z0-9_]+)_roll[0-9]+$", names(out), value = TRUE)
    allowed_roll_cols <- grep("_allowed_roll[0-9]+$", names(out), value = TRUE)
    out <- attach_opponent_rolling_profile(out, roll_cols = c(own_reb_roll_cols, allowed_roll_cols))

    # 6. Matchup differentials (roll10 only, explicit and small): is
    #    this team's typical rebound output higher or lower than what
    #    this opponent typically allows. Built defensively (only added
    #    if both sides exist) so a partially-populated pipeline run
    #    (e.g. rebounding stage still catching up) degrades gracefully.
    matchup_specs <- list(
      reb_matchup_edge_roll10         = c("reb_roll10",        "opp_reb_allowed_roll10"),
      oreb_matchup_edge_roll10        = c("oreb_roll10",       "opp_oreb_allowed_roll10"),
      dreb_matchup_edge_roll10        = c("dreb_roll10",       "opp_dreb_allowed_roll10"),
      adv_c_reb_matchup_edge_roll10   = c("adv_c_reb_roll10",  "opp_adv_c_reb_allowed_roll10"),
      adv_reb_0_3_matchup_edge_roll10 = c("adv_reb_0_3_roll10", "opp_adv_reb_0_3_allowed_roll10")
    )
    for (new_col in names(matchup_specs)) {
      needed <- matchup_specs[[new_col]]
      if (all(needed %in% names(out))) {
        out[[new_col]] <- out[[needed[1]]] - out[[needed[2]]]
      }
    }
  } else {
    logger$log("team_game_features: no opponent_id yet, skipping allowed/matchup features.")
  }

  write_parquet(out, cfg$path_team_game_features)
  logger$log("  ", cfg$path_team_game_features, " written (", nrow(out), " rows, ", ncol(out), " cols)")
  out
}

build_player_game_features <- function(cfg, logger) {
  player_logs <- read_full_dataset(cfg$path_player_logs_dataset)

  if (is.null(player_logs)) {
    logger$log("player_game_features: SKIPPED, no player game logs yet.")
    return(invisible(NULL))
  }

  player_logs <- player_logs %>%
    dplyr::mutate(
      team_id     = as.character(team_id),
      game_id_nba = as.character(game_id_nba),
      player_id   = as.character(player_id)
    )

  out <- compute_player_rolling_features(player_logs, windows = cfg$rolling_windows)

  team_context <- read_parquet_or_null(cfg$path_team_game_features)
  if (!is.null(team_context)) {
    team_context <- team_context %>%
      dplyr::mutate(team_id = as.character(team_id), game_id_nba = as.character(game_id_nba)) %>%
      dplyr::select(
        team_id, game_id_nba, home_away,
        rest_days, opp_rest_days, is_b2b, opp_is_b2b, is_3in4, opp_is_3in4,
        est_flight_time, opp_est_flight_time, time_zone_shift, opp_time_zone_shift
      )
    out <- out %>% dplyr::left_join(team_context, by = c("team_id", "game_id_nba"))
  } else {
    logger$log("player_game_features: team rest/travel context not found yet, joining without it.")
  }

  write_parquet(out, cfg$path_player_game_features)
  logger$log("  ", cfg$path_player_game_features, " written (", nrow(out), " rows, ", ncol(out), " cols)")
  out
}

# ------------------------------------------------------------
# Orchestration entry point: builds both final tables and
# records this run in state/manifest.json.
# ------------------------------------------------------------
combine_all_features <- function(cfg, logger) {
  team_feat   <- build_team_game_features(cfg, logger)
  player_feat <- build_player_game_features(cfg, logger)

  manifest <- read_manifest(cfg$path_manifest)
  manifest$last_run_at <- as.character(Sys.time())

  if (!is.null(team_feat) && nrow(team_feat) > 0) {
    manifest$last_game_date_pulled <- as.character(max(team_feat$game_date, na.rm = TRUE))
  }

  loaded_seasons <- dataset_seasons_present(cfg$path_schedule_dataset)
  if (length(loaded_seasons) > 0) {
    # Everything except the current (always-refreshed) season counts as
    # durably "fully loaded" for reporting purposes.
    manifest$seasons_fully_loaded <- setdiff(loaded_seasons, current_season())
  }

  write_manifest(manifest, cfg$path_manifest)
  logger$log("Manifest updated: last_run_at=", manifest$last_run_at,
             ", last_game_date_pulled=", manifest$last_game_date_pulled)

  invisible(list(team_game_features = team_feat, player_game_features = player_feat))
}
