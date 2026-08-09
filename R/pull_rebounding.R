# ============================================================
# Script: R/pull_rebounding.R
# Purpose: Advanced (shot-distance / contested) rebounding splits
#          per team per game, via hoopR::nba_teamdashptreb().
#
#          There is no bulk endpoint for this tracking data - it
#          is genuinely one API call per team per game. This stage
#          is therefore built to be:
#            - incremental: only pulls team-game pairs not already
#              in data_raw/team_rebounding_dashboards.rds
#            - checkpointed: saves to disk every
#              cfg$rebounding_checkpoint_every pulls, not after
#              every single one (the old script wrote the whole
#              accumulated file after every row)
#            - resumable: an interrupted run picks up where it
#              left off next time, no special handling needed
#            - toggleable: cfg$advanced_rebounding_enabled can
#              switch this off with no code changes if a historical
#              backfill proves too slow to run routinely
#
#          A first-ever run backfills 2022-23-to-date, which at
#          ~7,000+ team-game pairs and a polite throttle is a
#          multi-hour one-time job - every run after that is fast,
#          since only newly-completed games are pending.
# ============================================================

# ------------------------------------------------------------
# Safely extract a value from a dashboard sub-table
# ------------------------------------------------------------
safe_pull <- function(tbl, filter_col, filter_val, pull_col) {
  if (is.null(tbl) || nrow(tbl) == 0) return(NA_real_)
  row <- tbl %>% dplyr::filter(.data[[filter_col]] == filter_val)
  if (nrow(row) == 0) return(NA_real_)
  as.numeric(row[[pull_col]])
}

# ------------------------------------------------------------
# Flatten one team-game's rebounding dashboard into a single row.
# Pass `dash` directly to parse an already-pulled result (used
# when rebuilding the parsed table from the raw cache); omit it
# to pull fresh from the API.
# ------------------------------------------------------------
get_team_reb_metrics <- function(team_id, game_id_nba, dash = NULL) {
  if (is.null(dash)) {
    dash <- try(hoopR::nba_teamdashptreb(team_id = team_id, game_id = game_id_nba), silent = TRUE)
  }

  if (is.null(dash) || inherits(dash, "try-error")) {
    return(tibble::tibble(
      team_id, game_id_nba,
      oreb = NA_real_, dreb = NA_real_, reb = NA_real_,
      c_oreb = NA_real_, c_dreb = NA_real_, c_reb = NA_real_,
      reb_2pt_miss = NA_real_, reb_3pt_miss = NA_real_,
      reb_0_3 = NA_real_, reb_3_6 = NA_real_, reb_6_10 = NA_real_, reb_10_plus = NA_real_
    ))
  }

  overall_tbl      <- dash$OverallRebounding
  shot_type_tbl    <- dash$ShotTypeRebounding
  reb_dist_tbl     <- dash$RebDistanceRebounding

  pull_overall <- function(col) {
    if (!is.null(overall_tbl) && nrow(overall_tbl) > 0) as.numeric(overall_tbl[[col]]) else NA_real_
  }

  tibble::tibble(
    team_id, game_id_nba,
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
    reb_10_plus  = safe_pull(reb_dist_tbl, "REB_DIST_RANGE", "10+ Feet", "REB")
  )
}

# ------------------------------------------------------------
# Orchestration entry point.
# ------------------------------------------------------------
refresh_rebounding_features <- function(cfg, logger) {
  if (!isTRUE(cfg$advanced_rebounding_enabled)) {
    logger$log("Advanced rebounding: SKIPPED (cfg$advanced_rebounding_enabled = FALSE).")
    return(invisible(NULL))
  }

  team_logs <- read_full_dataset(cfg$path_team_logs_dataset)
  if (is.null(team_logs)) {
    logger$log("Advanced rebounding: SKIPPED, no team game logs yet - run that stage first.")
    return(invisible(NULL))
  }

  needed <- team_logs %>%
    dplyr::distinct(team_id, game_id_nba) %>%
    dplyr::mutate(team_id = as.character(team_id), game_id_nba = as.character(game_id_nba))

  # Nested API responses (multiple sub-tables per team-game) don't fit
  # parquet's flat/columnar model, so this one cache stays RDS.
  raw_path <- cfg$path_rebounding_raw_cache
  existing_raw <- read_existing_rds(raw_path, required_cols = c("team_id", "game_id_nba"))

  already_have <- if (is.null(existing_raw)) {
    dplyr::tibble(team_id = character(), game_id_nba = character())
  } else {
    dplyr::distinct(existing_raw, team_id, game_id_nba)
  }

  pending <- dplyr::anti_join(needed, already_have, by = c("team_id", "game_id_nba"))

  # Declared here (not just inside the else-branch below) so they're
  # always defined for this stage's return value, whether or not
  # there was anything pending this run - the run summary reports
  # these regardless.
  success_count <- 0L
  fail_count <- 0L

  if (nrow(pending) == 0) {
    logger$log("Advanced rebounding: nothing new to pull (", nrow(already_have), " team-games already cached).")
  } else {
    est_min <- round(nrow(pending) * cfg$throttle_rebounding_sec / 60, 1)
    logger$log("Advanced rebounding: ", nrow(pending), " team-game pairs to pull ",
               "(>= ", est_min, " min at current throttle, likely more with real API latency)...")

    buffer <- vector("list", nrow(pending))

    for (i in seq_len(nrow(pending))) {
      tid <- pending$team_id[i]
      gid <- pending$game_id_nba[i]

      dash <- try(hoopR::nba_teamdashptreb(team_id = tid, game_id = gid), silent = TRUE)
      ok <- !inherits(dash, "try-error")
      if (ok) success_count <- success_count + 1L else fail_count <- fail_count + 1L

      buffer[[i]] <- tibble::tibble(
        team_id = tid,
        game_id_nba = gid,
        dash_raw = list(if (ok) dash else NULL)
      )

      Sys.sleep(cfg$throttle_rebounding_sec)

      is_checkpoint <- (i %% cfg$rebounding_checkpoint_every == 0) || (i == nrow(pending))
      if (is_checkpoint) {
        new_raw <- dplyr::bind_rows(buffer[seq_len(i)])
        existing_raw <- combine_and_dedupe(existing_raw, new_raw, dedupe_cols = c("team_id", "game_id_nba"))
        saveRDS(existing_raw, raw_path)
        logger$log("  checkpoint: ", i, "/", nrow(pending), " pulled (",
                   success_count, " ok, ", fail_count, " failed) - saved to disk")
      }
    }

    logger$log("Advanced rebounding: pull complete (", success_count, " ok, ", fail_count, " failed).")
  }

  if (is.null(existing_raw) || nrow(existing_raw) == 0) {
    logger$log("Advanced rebounding: no cached dashboards to parse yet.")
    return(list(pulled = success_count, failed = fail_count, data = NULL))
  }

  parsed <- purrr::pmap_dfr(
    list(existing_raw$team_id, existing_raw$game_id_nba, existing_raw$dash_raw),
    function(tid, gid, dash) get_team_reb_metrics(tid, gid, dash = dash)
  )

  write_parquet(parsed, cfg$path_rebounding_features)
  logger$log("  ", cfg$path_rebounding_features, " written (", nrow(parsed), " rows)")
  list(pulled = success_count, failed = fail_count, data = parsed)
}
