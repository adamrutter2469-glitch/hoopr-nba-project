# ============================================================
# Script: R/pull_game_logs.R
# Purpose: Pull team and player box-score game logs.
#
# Replaces the old per-team loop (30 API calls per season) and
# per-game loop (1 API call PER GAME, plus a full-file rewrite
# after every single game) with hoopR::nba_leaguegamelog(), which
# returns every team's or every player's box score for an entire
# season in ONE API call. Verified live: columns include
# TEAM_ID/PLAYER_ID, GAME_ID, GAME_DATE, MATCHUP, WL, and the
# full box score stat line.
#
# Incremental logic is season-level (same proven pattern as the
# rest of the pipeline): historical seasons, once fully pulled,
# are never re-pulled; the current season is always re-pulled
# since new games complete continuously and hoopR::nba_leaguegamelog
# naturally only returns games that have already happened.
# ============================================================

# Box score stat columns that should be numeric. The API returns them as
# strings; janitor::clean_names() only touches names, not types, so without
# this every downstream numeric op (rolling averages, comparisons) would
# silently coerce or fail. any_of() so this works for both the team log
# (no fantasy_pts) and player log (has it) variants below.
GAME_LOG_NUMERIC_COLS <- c(
  "min", "fgm", "fga", "fg_pct", "fg3m", "fg3a", "fg3_pct", "ftm", "fta", "ft_pct",
  "oreb", "dreb", "reb", "ast", "stl", "blk", "tov", "pf", "pts", "plus_minus",
  "fantasy_pts", "video_available"
)

pull_league_game_log <- function(season, player_or_team) {
  raw <- hoopR::nba_leaguegamelog(season = season, player_or_team = player_or_team)
  raw[[1]] %>%
    janitor::clean_names() %>%
    dplyr::rename(game_id_nba = game_id) %>%
    dplyr::mutate(
      dplyr::across(dplyr::any_of(GAME_LOG_NUMERIC_COLS), as.numeric),
      season    = season,
      game_date = as.Date(game_date)
    )
}

# ------------------------------------------------------------
# Shared driver for both stages below - only the output path,
# the API's player_or_team flag, and the dedupe key differ.
# ------------------------------------------------------------
refresh_game_log <- function(cfg, logger, label, filename, player_or_team, dedupe_cols) {
  path <- file.path(cfg$path_data_raw, filename)
  existing <- read_existing_rds(path, required_cols = c("season", "game_id_nba"))

  all_seasons      <- season_sequence(cfg$first_season)
  existing_seasons <- if (is.null(existing)) character(0) else unique(existing$season)
  seasons_to_pull  <- compute_seasons_to_pull(all_seasons, existing_seasons, refresh_current = TRUE)

  if (length(seasons_to_pull) == 0) {
    logger$log(label, ": nothing to pull, all seasons up to date.")
    return(existing)
  }

  logger$log(label, ": pulling ", length(seasons_to_pull), " season(s) via bulk league game log: ",
             paste(seasons_to_pull, collapse = ", "))

  new_logs <- purrr::map_dfr(seasons_to_pull, function(s) {
    logger$log("  pulling ", s, "...")
    out <- try(pull_league_game_log(s, player_or_team), silent = TRUE)
    Sys.sleep(cfg$throttle_team_logs_sec)
    if (inherits(out, "try-error")) {
      logger$log("    FAILED: ", conditionMessage(attr(out, "condition")))
      return(NULL)
    }
    out
  })

  n_before <- if (is.null(existing)) 0L else nrow(existing)
  combined <- combine_and_dedupe(existing, new_logs, dedupe_cols = dedupe_cols)
  n_added  <- nrow(combined) - n_before

  saveRDS(combined, path)
  logger$log("  ", filename, " written (", nrow(combined), " total rows, ", n_added, " new)")
  combined
}

refresh_team_game_logs <- function(cfg, logger) {
  refresh_game_log(
    cfg, logger,
    label           = "Team game logs",
    filename        = "team_game_logs.rds",
    player_or_team  = "T",
    dedupe_cols     = c("team_id", "game_id_nba")
  )
}

refresh_player_game_logs <- function(cfg, logger) {
  refresh_game_log(
    cfg, logger,
    label           = "Player game logs",
    filename        = "player_game_logs.rds",
    player_or_team  = "P",
    dedupe_cols     = c("player_id", "game_id_nba")
  )
}
