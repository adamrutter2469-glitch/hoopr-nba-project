# ============================================================
# Script: R/pull_player_rebounding.R
# Purpose: Advanced (shot-distance / contested) rebounding splits per
#          PLAYER per game, via hoopR::nba_playerdashptreb() - the
#          player-grain analog of R/pull_rebounding.R's team-level
#          nba_teamdashptreb() pull. Same incremental/checkpointed/
#          toggleable design as that file; see its header comment for
#          the full reasoning. `safe_pull()` is defined there and
#          reused here as-is (all R/ files share one sourced
#          environment - see scripts/run_pipeline.R).
#
# One real API difference from the team version: this endpoint takes
# player_id + a date range, NOT a game_id - so each pull is scoped via
# date_from = date_to = <that game's date> (verified live: this
# reliably returns exactly that one game's totals, matching the
# existing box score exactly) and game_id_nba is attached afterward
# from our own player_game_logs, not from the API response.
#
# It's also richer than the team version: an extra ShotDistanceRebounding
# split (distance of the missed SHOT, not the rebound) that the team
# endpoint doesn't expose - included here as bonus columns.
#
# Scale note: this is player-game grain (~11x more rows than the team
# version), so it's scoped down by default via
# cfg$player_rebounding_seasons and cfg$player_rebounding_min_minutes
# (see config/config.R) rather than backfilling every player-game ever.
# ============================================================

# ------------------------------------------------------------
# Fetch one player-game's rebounding dashboard from the API.
# ------------------------------------------------------------
fetch_player_reb_dash <- function(player_id, season, game_date) {
  date_str <- format(as.Date(game_date), "%m/%d/%Y")
  try(
    hoopR::nba_playerdashptreb(
      player_id = player_id,
      season    = season,
      date_from = date_str,
      date_to   = date_str,
      per_mode  = "Totals"
    ),
    silent = TRUE
  )
}

# ------------------------------------------------------------
# Flatten one player-game's rebounding dashboard into a single row.
# Pass `dash` directly to parse an already-pulled result (used when
# rebuilding the parsed table from the raw cache).
# ------------------------------------------------------------
get_player_reb_metrics <- function(player_id, game_id_nba, dash) {
  if (is.null(dash) || inherits(dash, "try-error")) {
    return(tibble::tibble(
      player_id, game_id_nba,
      oreb = NA_real_, dreb = NA_real_, reb = NA_real_,
      c_oreb = NA_real_, c_dreb = NA_real_, c_reb = NA_real_,
      reb_2pt_miss = NA_real_, reb_3pt_miss = NA_real_,
      reb_0_3 = NA_real_, reb_3_6 = NA_real_, reb_6_10 = NA_real_, reb_10_plus = NA_real_,
      reb_shot_0_6 = NA_real_, reb_shot_7_13 = NA_real_, reb_shot_13_19 = NA_real_, reb_shot_19_plus = NA_real_
    ))
  }

  overall_tbl   <- dash$OverallRebounding
  shot_type_tbl <- dash$ShotTypeRebounding
  reb_dist_tbl  <- dash$RebDistanceRebounding
  shot_dist_tbl <- dash$ShotDistanceRebounding

  pull_overall <- function(col) {
    if (!is.null(overall_tbl) && nrow(overall_tbl) > 0) as.numeric(overall_tbl[[col]]) else NA_real_
  }

  tibble::tibble(
    player_id, game_id_nba,
    oreb   = pull_overall("OREB"),
    dreb   = pull_overall("DREB"),
    reb    = pull_overall("REB"),
    c_oreb = pull_overall("C_OREB"),
    c_dreb = pull_overall("C_DREB"),
    c_reb  = pull_overall("C_REB"),
    reb_2pt_miss = safe_pull(shot_type_tbl, "SHOT_TYPE_RANGE", "Miss 2FG", "REB"),
    reb_3pt_miss = safe_pull(shot_type_tbl, "SHOT_TYPE_RANGE", "Miss 3FG", "REB"),
    reb_0_3      = safe_pull(reb_dist_tbl, "REB_DIST_RANGE", "0-3 Feet", "REB"),
    reb_3_6      = safe_pull(reb_dist_tbl, "REB_DIST_RANGE", "3-6 Feet", "REB"),
    reb_6_10     = safe_pull(reb_dist_tbl, "REB_DIST_RANGE", "6-10 Feet", "REB"),
    reb_10_plus  = safe_pull(reb_dist_tbl, "REB_DIST_RANGE", "10+ Feet", "REB"),
    # Bonus: distance of the missed SHOT the rebound came off of -
    # not available at the team level.
    reb_shot_0_6     = safe_pull(shot_dist_tbl, "SHOT_DIST_RANGE", "0-6 Feet", "REB"),
    reb_shot_7_13    = safe_pull(shot_dist_tbl, "SHOT_DIST_RANGE", "7-13 Feet", "REB"),
    reb_shot_13_19   = safe_pull(shot_dist_tbl, "SHOT_DIST_RANGE", "13-19 Feet", "REB"),
    reb_shot_19_plus = safe_pull(shot_dist_tbl, "SHOT_DIST_RANGE", "19+ Feet", "REB")
  )
}

# ------------------------------------------------------------
# Orchestration entry point.
# ------------------------------------------------------------
refresh_player_rebounding_features <- function(cfg, logger) {
  if (!isTRUE(cfg$player_rebounding_enabled)) {
    logger$log("Player rebounding: SKIPPED (cfg$player_rebounding_enabled = FALSE).")
    return(invisible(NULL))
  }

  player_logs <- read_full_dataset(cfg$path_player_logs_dataset)
  if (is.null(player_logs)) {
    logger$log("Player rebounding: SKIPPED, no player game logs yet - run that stage first.")
    return(invisible(NULL))
  }

  needed <- player_logs %>%
    dplyr::filter(season %in% cfg$player_rebounding_seasons, min >= cfg$player_rebounding_min_minutes) %>%
    dplyr::distinct(player_id, game_id_nba, game_date, season) %>%
    dplyr::mutate(player_id = as.character(player_id), game_id_nba = as.character(game_id_nba))

  raw_path <- cfg$path_player_rebounding_raw_cache
  existing_raw <- read_existing_rds(raw_path, required_cols = c("player_id", "game_id_nba"))

  already_have <- if (is.null(existing_raw)) {
    dplyr::tibble(player_id = character(), game_id_nba = character())
  } else {
    dplyr::distinct(existing_raw, player_id, game_id_nba)
  }

  pending <- dplyr::anti_join(needed, already_have, by = c("player_id", "game_id_nba"))

  # Declared here (not just inside the else-branch below) so they're
  # always defined for this stage's return value, whether or not
  # there was anything pending this run - the run summary reports
  # these regardless.
  success_count <- 0L
  fail_count <- 0L

  if (nrow(pending) == 0) {
    logger$log("Player rebounding: nothing new to pull (", nrow(already_have), " player-games already cached).")
  } else {
    est_min <- round(nrow(pending) * cfg$throttle_player_rebounding_sec / 60, 1)
    logger$log("Player rebounding: ", nrow(pending), " player-game pairs to pull ",
               "(>= ", est_min, " min at current throttle, likely more with real API latency; ",
               "scope: season in {", paste(cfg$player_rebounding_seasons, collapse = ", "),
               "}, min >= ", cfg$player_rebounding_min_minutes, ")...")

    buffer <- vector("list", nrow(pending))

    for (i in seq_len(nrow(pending))) {
      pid   <- pending$player_id[i]
      gid   <- pending$game_id_nba[i]
      gdate <- pending$game_date[i]
      season <- pending$season[i]

      dash <- fetch_player_reb_dash(pid, season, gdate)
      ok <- !inherits(dash, "try-error")
      if (ok) success_count <- success_count + 1L else fail_count <- fail_count + 1L

      buffer[[i]] <- tibble::tibble(
        player_id = pid,
        game_id_nba = gid,
        dash_raw = list(if (ok) dash else NULL)
      )

      Sys.sleep(cfg$throttle_player_rebounding_sec)

      is_checkpoint <- (i %% cfg$player_rebounding_checkpoint_every == 0) || (i == nrow(pending))
      if (is_checkpoint) {
        new_raw <- dplyr::bind_rows(buffer[seq_len(i)])
        existing_raw <- combine_and_dedupe(existing_raw, new_raw, dedupe_cols = c("player_id", "game_id_nba"))
        saveRDS(existing_raw, raw_path)
        logger$log("  checkpoint: ", i, "/", nrow(pending), " pulled (",
                   success_count, " ok, ", fail_count, " failed) - saved to disk")
      }
    }

    logger$log("Player rebounding: pull complete (", success_count, " ok, ", fail_count, " failed).")
  }

  if (is.null(existing_raw) || nrow(existing_raw) == 0) {
    logger$log("Player rebounding: no cached dashboards to parse yet.")
    return(list(pulled = success_count, failed = fail_count, data = NULL))
  }

  parsed <- purrr::pmap_dfr(
    list(existing_raw$player_id, existing_raw$game_id_nba, existing_raw$dash_raw),
    function(pid, gid, dash) get_player_reb_metrics(pid, gid, dash = dash)
  )

  write_parquet(parsed, cfg$path_player_rebounding_features)
  logger$log("  ", cfg$path_player_rebounding_features, " written (", nrow(parsed), " rows)")
  list(pulled = success_count, failed = fail_count, data = parsed)
}
