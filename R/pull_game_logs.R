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
# since new games complete continuously.
#
# Finalized-games-only guarantee: nba_leaguegamelog is understood to
# be a historical league log (not a live feed), but that's not
# something this pipeline can fully verify without a game actually in
# progress to test against. Rather than rely on that assumption,
# every pulled row is cross-checked against that season's schedule
# (R/pull_schedule.R, which carries the NBA Stats API's own
# game_status field) and dropped if the schedule doesn't say Final -
# see filter_to_final_games() below. This makes "only ingest
# finalized games" true by construction, independent of whatever the
# log endpoint itself does or doesn't include.
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
# Drop any pulled row whose game isn't marked Final in that season's
# schedule - the actual "only finalized games" guarantee. Missing
# schedule data (cold start, or an older cached season partition from
# before game_status was captured) fails open rather than dropping
# everything: better to keep a row we can't verify than to silently
# discard a whole season's pull because of a schema mismatch.
# ------------------------------------------------------------
filter_to_final_games <- function(rows, schedule_season, logger, label) {
  if (is.null(schedule_season) || !("is_final" %in% names(schedule_season))) {
    logger$log("    (schedule status unavailable for this season - keeping all pulled ", label, " rows unfiltered)")
    return(rows)
  }

  final_game_ids <- schedule_season %>%
    dplyr::filter(is_final) %>%
    dplyr::pull(game_id_nba) %>%
    unique()

  before <- nrow(rows)
  out <- rows %>% dplyr::filter(game_id_nba %in% final_game_ids)
  dropped <- before - nrow(out)
  if (dropped > 0) {
    logger$log("    dropped ", dropped, " ", label, " row(s) for games not marked Final (in progress, scheduled, or postponed)")
  }
  out
}

# ------------------------------------------------------------
# Shared driver for both stages below - only the dataset path,
# the API's player_or_team flag, and the dedupe key differ.
#
# Season-partitioned, same pattern as R/pull_schedule.R: each
# season to pull is deduped against just its own existing
# partition and written back to just that partition.
# ------------------------------------------------------------
refresh_game_log <- function(cfg, logger, label, path, player_or_team, dedupe_cols) {
  all_seasons      <- season_sequence(cfg$first_season)
  existing_seasons <- dataset_seasons_present(path)
  seasons_to_pull  <- compute_seasons_to_pull(all_seasons, existing_seasons, refresh_current = TRUE)

  if (length(seasons_to_pull) == 0) {
    logger$log(label, ": nothing to pull, all seasons up to date.")
    return(invisible(NULL))
  }

  logger$log(label, ": pulling ", length(seasons_to_pull), " season(s) via bulk league game log: ",
             paste(seasons_to_pull, collapse = ", "))

  for (s in seasons_to_pull) {
    logger$log("  pulling ", s, "...")
    new_rows <- try(pull_league_game_log(s, player_or_team), silent = TRUE)
    Sys.sleep(cfg$throttle_team_logs_sec)
    if (inherits(new_rows, "try-error")) {
      logger$log("    FAILED: ", conditionMessage(attr(new_rows, "condition")))
      next
    }

    schedule_season <- read_season_partition(cfg$path_schedule_dataset, s, required_cols = c("game_id_nba"))
    new_rows <- filter_to_final_games(new_rows, schedule_season, logger, label)

    existing <- read_season_partition(path, s, required_cols = c("season", "game_id_nba"))
    combined <- combine_and_dedupe(existing, new_rows, dedupe_cols = dedupe_cols)
    write_season_partition(combined, path)

    n_new <- nrow(combined) - (if (is.null(existing)) 0L else nrow(existing))
    logger$log("    season ", s, ": ", nrow(combined), " rows (", n_new, " new)")
  }

  invisible(NULL)
}

refresh_team_game_logs <- function(cfg, logger) {
  refresh_game_log(
    cfg, logger,
    label           = "Team game logs",
    path            = cfg$path_team_logs_dataset,
    player_or_team  = "T",
    dedupe_cols     = c("team_id", "game_id_nba")
  )
}

refresh_player_game_logs <- function(cfg, logger) {
  refresh_game_log(
    cfg, logger,
    label           = "Player game logs",
    path            = cfg$path_player_logs_dataset,
    player_or_team  = "P",
    dedupe_cols     = c("player_id", "game_id_nba")
  )
}
