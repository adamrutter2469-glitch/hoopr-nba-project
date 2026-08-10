# ============================================================
# Script: R/pull_playbyplay.R
# Purpose: Incrementally pull event-level play-by-play data (every
#          shot/foul/turnover/rebound/etc., with shot coordinates,
#          the players involved, and game-clock context) for every
#          season in scope.
#
#          Source: hoopR::load_nba_pbp() - a bulk per-season pull
#          from sportsdataverse's own hosted release files (NOT the
#          live NBA Stats API), confirmed still working. The two
#          NBA-Stats-native alternatives (nba_pbp(), nba_data_pbp())
#          are both currently broken/blocked on NBA's side (403 /
#          silent failures, same symptom seen on the shot-location
#          dashboard endpoints - see R/pull_rebounding.R era notes)
#          - not something fixable from our end, so ESPN-sourced data
#          is the practical option right now despite needing an ID
#          bridge (see below).
#
#          ID MISMATCH: this data uses ESPN's own game_id and team
#          abbreviations, not NBA Stats' game_id_nba that the rest of
#          the pipeline joins on. Bridged deterministically (NOT the
#          fuzzy name-matching approach used for bigballsdata) by
#          joining on (game_date, home abbrev, away abbrev) - a team
#          plays at most one game against a given opponent on a given
#          date, so this key is unambiguous. Verified empirically on
#          the 2022-23 season: 1320/1321 games matched (99.9%) after
#          fixing 6 known ESPN-vs-NBA abbreviation mismatches; the
#          one miss was the All-Star exhibition game (Team LeBron vs
#          Team Giannis - "LEB"/"GIA" - which correctly has no NBA
#          Stats regular-season/playoff counterpart to match).
#
#          Rows that don't find a game_id_nba match are dropped (not
#          joinable to any other table in the pipeline anyway) and
#          counted in the log for visibility.
# ============================================================

# ------------------------------------------------------------
# ESPN uses different short codes than NBA Stats' tricode for 6
# franchises. Anything not in this list is assumed to already match
# (verified true for the other 24 teams during testing).
# ------------------------------------------------------------
ESPN_ABBREV_FIX <- c(NY = "NYK", GS = "GSW", SA = "SAS", UTAH = "UTA", WSH = "WAS", NO = "NOP")

fix_espn_abbrev <- function(x) dplyr::coalesce(ESPN_ABBREV_FIX[x], x)

# Our "2022-23" season string -> the numeric END year load_nba_pbp()
# expects (2023). NBA_pbp season param convention differs from every
# other pull in this project, which is why this conversion lives here
# rather than in R/utils_season.R.
season_to_pbp_year <- function(season_string) {
  season_start_year(normalize_season(season_string)) + 1L
}

# ------------------------------------------------------------
# Pull one season's raw PBP and attach game_id_nba via the
# deterministic (date, home, away) bridge. Returns NULL (with a log
# line) rather than erroring if the schedule for this season isn't
# available yet - PBP depends on the schedule already being pulled.
# ------------------------------------------------------------
pull_playbyplay_season <- function(season, cfg, logger) {
  schedule <- read_season_partition(cfg$path_schedule_dataset, season,
                                     required_cols = c("season", "game_id_nba", "game_date",
                                                        "home_team_tricode", "away_team_tricode"))
  if (is.null(schedule) || nrow(schedule) == 0) {
    logger$log("    Play-by-play SKIPPED for ", season, ": no schedule data yet for this season.")
    return(NULL)
  }

  pbp_year <- season_to_pbp_year(season)
  raw <- hoopR::load_nba_pbp(seasons = pbp_year)
  if (is.null(raw) || nrow(raw) == 0) {
    logger$log("    Play-by-play: no data returned for ", season, " (pbp year ", pbp_year, ").")
    return(NULL)
  }

  bridge <- raw %>%
    dplyr::distinct(game_id, game_date, home_team_abbrev, away_team_abbrev) %>%
    dplyr::mutate(
      home_fixed = fix_espn_abbrev(home_team_abbrev),
      away_fixed = fix_espn_abbrev(away_team_abbrev)
    ) %>%
    dplyr::inner_join(
      schedule %>% dplyr::select(game_id_nba, game_date, home_team_tricode, away_team_tricode),
      by = c("game_date" = "game_date", "home_fixed" = "home_team_tricode", "away_fixed" = "away_team_tricode")
    ) %>%
    dplyr::select(game_id, game_id_nba)

  n_pbp_games <- dplyr::n_distinct(raw$game_id)
  n_matched   <- nrow(bridge)
  if (n_matched < n_pbp_games) {
    logger$log("    game-ID bridge: ", n_matched, "/", n_pbp_games, " games matched (",
               n_pbp_games - n_matched, " unmatched - typically All-Star/exhibition games with no NBA Stats counterpart).")
  }

  raw %>%
    dplyr::rename(game_id_espn = game_id) %>%
    dplyr::inner_join(bridge %>% dplyr::rename(game_id_espn = game_id), by = "game_id_espn") %>%
    dplyr::mutate(
      # !!season (not `season = season`) - load_nba_pbp()'s own output
      # already has a "season" column (a bare numeric year, ESPN's
      # convention), which would silently win over the function
      # argument in dplyr's data-masking evaluation (this bit
      # R/pull_schedule.R once already - same root cause). !! forces
      # the function argument's actual string value ("2022-23") in
      # before mutate's masking evaluation ever sees the column.
      season = !!season,
      game_date = as.Date(game_date)
    )
}

# ------------------------------------------------------------
# Orchestration entry point. Same season-partitioned incremental
# pattern as every other pull stage (R/pull_schedule.R,
# R/pull_game_logs.R) - historical seasons pulled once and kept,
# current season always re-pulled since it keeps growing.
# ------------------------------------------------------------
refresh_playbyplay <- function(cfg, logger) {
  if (!isTRUE(cfg$playbyplay_enabled)) {
    logger$log("Play-by-play: SKIPPED (cfg$playbyplay_enabled = FALSE).")
    return(invisible(NULL))
  }

  path <- cfg$path_playbyplay_dataset
  all_seasons      <- season_sequence(cfg$first_season)
  existing_seasons <- dataset_seasons_present(path)
  seasons_to_pull  <- compute_seasons_to_pull(all_seasons, existing_seasons, refresh_current = TRUE)

  if (length(seasons_to_pull) == 0) {
    logger$log("Play-by-play: nothing to pull, all seasons up to date.")
    return(invisible(NULL))
  }

  logger$log("Play-by-play: pulling ", length(seasons_to_pull), " season(s): ",
             paste(seasons_to_pull, collapse = ", "))

  for (s in seasons_to_pull) {
    logger$log("  pulling play-by-play for ", s, "...")
    new_rows <- try(pull_playbyplay_season(s, cfg, logger), silent = TRUE)
    if (inherits(new_rows, "try-error")) {
      logger$log("    FAILED: ", conditionMessage(attr(new_rows, "condition")))
      next
    }
    if (is.null(new_rows) || nrow(new_rows) == 0) next

    write_season_partition(new_rows, path)
    logger$log("    season ", s, ": ", nrow(new_rows), " rows written (",
               dplyr::n_distinct(new_rows$game_id_nba), " games)")
  }

  invisible(NULL)
}
