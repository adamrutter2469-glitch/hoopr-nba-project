# ============================================================
# Script: R/features_combine.R
# Purpose: Join box scores + rest/travel + rolling averages +
#          advanced rebounding into the two "comprehensive"
#          analysis-ready tables that are the actual point of a
#          pipeline run:
#            data_processed/team_game_features.rds   (1 row/team-game)
#            data_processed/player_game_features.rds (1 row/player-game)
#          Also updates state/manifest.json.
# ============================================================

build_team_game_features <- function(cfg, logger) {
  team_logs_path    <- file.path(cfg$path_data_raw, "team_game_logs.rds")
  rest_travel_path  <- file.path(cfg$path_data_processed, "schedule_team_level_final.rds")
  reb_path          <- file.path(cfg$path_data_processed, "team_rebounding_features.rds")

  if (!file.exists(team_logs_path)) {
    logger$log("team_game_features: SKIPPED, no team_game_logs.rds yet.")
    return(invisible(NULL))
  }

  team_logs <- readRDS(team_logs_path) %>%
    dplyr::mutate(team_id = as.character(team_id), game_id_nba = as.character(game_id_nba))

  out <- compute_team_rolling_features(team_logs, windows = cfg$rolling_windows)

  if (file.exists(rest_travel_path)) {
    rest_travel <- readRDS(rest_travel_path) %>%
      dplyr::mutate(team_id = as.character(team_id), game_id_nba = as.character(game_id_nba)) %>%
      dplyr::select(
        team_id, game_id_nba, opponent_id, home_away,
        rest_days, opp_rest_days, is_b2b, opp_is_b2b, is_3in4, opp_is_3in4,
        est_flight_time, opp_est_flight_time, time_zone_shift, opp_time_zone_shift
      )
    out <- out %>% dplyr::left_join(rest_travel, by = c("team_id", "game_id_nba"))
  } else {
    logger$log("team_game_features: rest/travel table not found yet, joining without it.")
  }

  if (file.exists(reb_path)) {
    reb <- readRDS(reb_path) %>%
      dplyr::mutate(team_id = as.character(team_id), game_id_nba = as.character(game_id_nba)) %>%
      dplyr::rename_with(~ paste0("adv_", .x), -c(team_id, game_id_nba))
    out <- out %>% dplyr::left_join(reb, by = c("team_id", "game_id_nba"))
  } else {
    logger$log("team_game_features: advanced rebounding table not found yet, joining without it.")
  }

  saveRDS(out, file.path(cfg$path_data_processed, "team_game_features.rds"))
  logger$log("  team_game_features.rds written (", nrow(out), " rows, ", ncol(out), " cols)")
  out
}

build_player_game_features <- function(cfg, logger) {
  player_logs_path   <- file.path(cfg$path_data_raw, "player_game_logs.rds")
  team_features_path <- file.path(cfg$path_data_processed, "team_game_features.rds")

  if (!file.exists(player_logs_path)) {
    logger$log("player_game_features: SKIPPED, no player_game_logs.rds yet.")
    return(invisible(NULL))
  }

  player_logs <- readRDS(player_logs_path) %>%
    dplyr::mutate(
      team_id     = as.character(team_id),
      game_id_nba = as.character(game_id_nba),
      player_id   = as.character(player_id)
    )

  out <- compute_player_rolling_features(player_logs, windows = cfg$rolling_windows)

  if (file.exists(team_features_path)) {
    team_context <- readRDS(team_features_path) %>%
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

  saveRDS(out, file.path(cfg$path_data_processed, "player_game_features.rds"))
  logger$log("  player_game_features.rds written (", nrow(out), " rows, ", ncol(out), " cols)")
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

  schedule_path <- file.path(cfg$path_data_raw, "schedule.rds")
  if (file.exists(schedule_path)) {
    loaded_seasons <- unique(readRDS(schedule_path)$season)
    # Everything except the current (always-refreshed) season counts as
    # durably "fully loaded" for reporting purposes.
    manifest$seasons_fully_loaded <- setdiff(loaded_seasons, current_season())
  }

  write_manifest(manifest, cfg$path_manifest)
  logger$log("Manifest updated: last_run_at=", manifest$last_run_at,
             ", last_game_date_pulled=", manifest$last_game_date_pulled)

  invisible(list(team_game_features = team_feat, player_game_features = player_feat))
}
