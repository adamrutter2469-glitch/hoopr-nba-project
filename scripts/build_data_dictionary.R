# ============================================================
# Script: scripts/build_data_dictionary.R
# Purpose: Auto-generate docs/data_dictionary/*.csv (one file per
#          table) from the LIVE parquet schemas plus this pipeline's
#          own naming-pattern knowledge, so the dictionary can never
#          silently drift from the real data the way a hand-typed
#          document would. Run manually whenever a table's schema
#          changes: Rscript scripts/build_data_dictionary.R
#
# Each output CSV has one row for the table itself (row_num=1) and
# one row per column (row_num=2..n+1), with columns:
#   name          table name, or column name
#   description   table: high-level purpose. column: what it holds.
#   logic         NA for the table row. For columns: how it's
#                 derived, or NA if pulled directly with no
#                 transformation.
#   sources       table: core source tables. column: ';'-separated
#                 source column(s) it's derived from, or the raw
#                 API call if pulled directly.
#   dtype         column's actual type, read from the live data.
#   feature_family  a tag for filtering (own_rolling, opponent_rolling,
#                 allowed, opponent_allowed, matchup, context,
#                 identifier, metadata, raw_actual, reference,
#                 unclassified).
#   leakage_risk  whether this column is safe to use as a same-game
#                 predictor, or is itself part of this game's actual
#                 result. "unclassified" columns get NA here too -
#                 review them before trusting either way.
#
# Anything the pattern rules + manual overrides below don't recognize
# is written as feature_family="unclassified" with a "NEEDS REVIEW"
# description, rather than guessed at - the script prints a count of
# these per table so they're easy to find and fill in by hand.
# ============================================================

suppressMessages({
  library(dplyr)
  library(arrow)
  library(purrr)
  library(readr)
  library(tibble)
})

source("config/config.R")
invisible(purrr::walk(list.files("R", full.names = TRUE, pattern = "\\.R$"), source))

OUT_DIR <- "docs/data_dictionary"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Base stat glossary: plain-language meaning of every raw stat this
# pipeline tracks, keyed by its column-name stem. Reused to build
# descriptions for every column DERIVED from a stat (_rollN, opp_*,
# _allowed, _matchup_edge) as well as the raw stat column itself.
# ------------------------------------------------------------
STAT_GLOSSARY <- c(
  pts = "points scored", reb = "total rebounds", oreb = "offensive rebounds",
  dreb = "defensive rebounds", ast = "assists", stl = "steals", blk = "blocks",
  tov = "turnovers", pf = "personal fouls", fgm = "field goals made",
  fga = "field goals attempted", fg_pct = "field goal percentage",
  fg3m = "3-pointers made", fg3a = "3-pointers attempted", fg3_pct = "3-point percentage",
  ftm = "free throws made", fta = "free throws attempted", ft_pct = "free throw percentage",
  min = "minutes played", plus_minus = "plus/minus",
  adv_oreb = "offensive rebounds (tracking data)", adv_dreb = "defensive rebounds (tracking data)",
  adv_reb = "total rebounds (tracking data)", adv_c_oreb = "contested offensive rebounds",
  adv_c_dreb = "contested defensive rebounds", adv_c_reb = "contested total rebounds",
  adv_reb_2pt_miss = "rebounds off a missed 2-point shot",
  adv_reb_3pt_miss = "rebounds off a missed 3-point shot",
  adv_reb_0_3 = "rebounds grabbed 0-3 feet from the basket",
  adv_reb_3_6 = "rebounds grabbed 3-6 feet from the basket",
  adv_reb_6_10 = "rebounds grabbed 6-10 feet from the basket",
  adv_reb_10_plus = "rebounds grabbed 10+ feet from the basket",
  # Player-level only (hoopR::nba_playerdashptreb() exposes this split,
  # nba_teamdashptreb() doesn't): distance of the missed SHOT the
  # rebound came off of, not the rebound's own distance from the hoop.
  reb_shot_0_6 = "rebounds off a shot missed 0-6 feet out",
  reb_shot_7_13 = "rebounds off a shot missed 7-13 feet out",
  reb_shot_13_19 = "rebounds off a shot missed 13-19 feet out",
  reb_shot_19_plus = "rebounds off a shot missed 19+ feet out"
)

# Raw per-game stat columns that reflect THIS game's actual outcome -
# never safe to use predicting that same game.
UNSAFE_RAW_STATS <- c(names(STAT_GLOSSARY), "fantasy_pts")

# ------------------------------------------------------------
# Identifiers/context/metadata that mean the same thing everywhere
# they appear (team-game grain). Checked before the pattern rules.
# ------------------------------------------------------------
GLOBAL_OVERRIDES <- list(
  team_id = list(description = "Team's NBA Stats API identifier.", logic = NA,
    sources = "hoopR API", family = "identifier", leakage_risk = "safe"),
  opponent_id = list(description = "This game's opposing team's identifier.", logic = NA,
    sources = "schedule.home_team_id / away_team_id", family = "identifier", leakage_risk = "safe"),
  player_id = list(description = "Player's NBA Stats API identifier.", logic = NA,
    sources = "hoopR API", family = "identifier", leakage_risk = "safe"),
  person_id = list(description = "Player's NBA Stats API identifier (same identifier as player_id; this name comes from hoopR's raw player-reference endpoints).", logic = NA,
    sources = "hoopR::nba_commonallplayers(); hoopR::nba_playerindex()", family = "identifier", leakage_risk = "safe"),
  game_id_nba = list(description = "NBA Stats API's canonical game identifier.", logic = NA,
    sources = "hoopR API", family = "identifier", leakage_risk = "safe"),
  game_id = list(description = "Alias of game_id_nba, kept for join compatibility with hoopR's raw schedule fields.", logic = "Set equal to game_id_nba.",
    sources = "game_id_nba", family = "identifier", leakage_risk = "safe"),
  season = list(description = "Season string, e.g. '2022-23'.", logic = "Set explicitly to the season being pulled, not trusted from the API's own inconsistently-typed season field - see R/pull_schedule.R.",
    sources = NA, family = "identifier", leakage_risk = "safe"),
  season_id = list(description = "hoopR's raw season identifier (e.g. '22022'), distinct from the human-readable 'season' column.", logic = NA,
    sources = "hoopR::nba_leaguegamelog()", family = "identifier", leakage_risk = "safe"),
  game_date = list(description = "Calendar date the game was/will be played.", logic = NA,
    sources = "hoopR API", family = "identifier", leakage_risk = "safe"),
  home_away = list(description = "Whether this team was the home or away team for this game.", logic = NA,
    sources = "schedule.home_team_id / away_team_id", family = "context", leakage_risk = "safe"),
  is_home = list(description = "1 if this team was the home team, 0 if away.", logic = "as.integer(home_away == \"home\")",
    sources = "home_away", family = "context", leakage_risk = "safe"),
  rest_days = list(description = "Days of rest this team had entering this game (0 = a back-to-back). NA for a team's first game of the season.",
    logic = "Days since this team's previous game in the same season, minus 1.",
    sources = "schedule.game_date (lagged per team/season)", family = "context", leakage_risk = "safe"),
  opp_rest_days = list(description = "The opponent's rest_days for this same game.", logic = NA,
    sources = "rest_days (opponent's row)", family = "context", leakage_risk = "safe"),
  is_b2b = list(description = "TRUE if this team is playing on zero days of rest (a back-to-back).", logic = "rest_days == 0",
    sources = "rest_days", family = "context", leakage_risk = "safe"),
  opp_is_b2b = list(description = "The opponent's is_b2b for this same game.", logic = NA,
    sources = "opp_rest_days", family = "context", leakage_risk = "safe"),
  is_3in4 = list(description = "TRUE if this game is the second of two games within 2 days with <=1 rest day between them (a proxy for 3-games-in-4-nights fatigue).",
    logic = "rest_days <= 1 AND the previous game's rest_days was also <= 1.", sources = "rest_days", family = "context", leakage_risk = "safe"),
  opp_is_3in4 = list(description = "The opponent's is_3in4 for this same game.", logic = NA,
    sources = "opp_rest_days", family = "context", leakage_risk = "safe"),
  est_flight_time = list(description = "Estimated flight time (hours) from this team's previous game city to this game's city.",
    logic = "Looked up from the travel-time reference table by previous-city -> current-city.",
    sources = "data_raw/external/nba_airport_flight_matrix.csv", family = "context", leakage_risk = "safe"),
  opp_est_flight_time = list(description = "The opponent's est_flight_time for this same game.", logic = NA,
    sources = "est_flight_time (opponent's row)", family = "context", leakage_risk = "safe"),
  time_zone_shift = list(description = "Number of timezones crossed from this team's previous game city to this game's city.",
    logic = "Looked up from the travel-time reference table.", sources = "data_raw/external/nba_airport_flight_matrix.csv",
    family = "context", leakage_risk = "safe"),
  opp_time_zone_shift = list(description = "The opponent's time_zone_shift for this same game.", logic = NA,
    sources = "time_zone_shift (opponent's row)", family = "context", leakage_risk = "safe"),
  matchup = list(description = "Human-readable matchup string, e.g. 'LAL vs. BOS' or 'LAL @ BOS'.", logic = NA,
    sources = "hoopR::nba_leaguegamelog()", family = "metadata", leakage_risk = "safe (metadata only)"),
  wl = list(description = "Win ('W') or loss ('L') for this team in this game.", logic = NA,
    sources = "hoopR::nba_leaguegamelog()", family = "raw_actual", leakage_risk = "UNSAFE - this game's actual outcome"),
  team_abbreviation = list(description = "Team's 3-letter abbreviation (e.g. 'LAL').", logic = NA,
    sources = "hoopR API", family = "metadata", leakage_risk = "safe (metadata only)"),
  team_name = list(description = "Team's full name.", logic = NA,
    sources = "hoopR API", family = "metadata", leakage_risk = "safe (metadata only)"),
  player_name = list(description = "Player's full name.", logic = NA,
    sources = "hoopR::nba_leaguegamelog()", family = "metadata", leakage_risk = "safe (metadata only)"),
  video_available = list(description = "Whether NBA.com has video available for this game (metadata flag, not a performance stat).", logic = NA,
    sources = "hoopR::nba_leaguegamelog()", family = "metadata", leakage_risk = "safe (metadata only)"),
  fantasy_pts = list(description = "Actual NBA Stats fantasy points scored in this exact game.", logic = NA,
    sources = "hoopR::nba_leaguegamelog()", family = "raw_actual", leakage_risk = "UNSAFE - this game's actual result"),
  game_status = list(description = "NBA Stats API game status code: 1 = scheduled (not yet started), 2 = live/in progress, 3 = final.", logic = NA,
    sources = "hoopR::nba_schedule()", family = "metadata", leakage_risk = "safe (metadata only)"),
  game_status_text = list(description = "Human-readable status - 'Final'/'Final/OT' once done, or the scheduled tip-off time (e.g. '7:00 pm ET') beforehand.", logic = NA,
    sources = "hoopR::nba_schedule()", family = "metadata", leakage_risk = "safe (metadata only)"),
  is_final = list(description = "TRUE once this game is officially final. R/pull_game_logs.R cross-checks every pulled box-score row against this before keeping it - the pipeline's guarantee that only finalized games' stats are ingested (in-progress and future scheduled games are excluded from box scores, though scheduled games ARE kept in the schedule table itself for reference).",
    logic = "game_status == 3", sources = "game_status", family = "context", leakage_risk = "safe"),
  # game-grain (schedule_with_travel_detail) home_*/away_* variants -
  # same concepts as above, one game per row instead of one team-game.
  home_team_id = list(description = "Home team's identifier.", logic = NA, sources = "hoopR::nba_schedule()", family = "identifier", leakage_risk = "safe"),
  away_team_id = list(description = "Away team's identifier.", logic = NA, sources = "hoopR::nba_schedule()", family = "identifier", leakage_risk = "safe"),
  home_team_tricode = list(description = "Home team's 3-letter code.", logic = NA, sources = "hoopR::nba_schedule()", family = "metadata", leakage_risk = "safe (metadata only)"),
  away_team_tricode = list(description = "Away team's 3-letter code.", logic = NA, sources = "hoopR::nba_schedule()", family = "metadata", leakage_risk = "safe (metadata only)"),
  home_rest_days = list(description = "Home team's rest_days for this game.", logic = NA, sources = "rest_days", family = "context", leakage_risk = "safe"),
  away_rest_days = list(description = "Away team's rest_days for this game.", logic = NA, sources = "rest_days", family = "context", leakage_risk = "safe"),
  home_is_b2b = list(description = "Home team's is_b2b for this game.", logic = NA, sources = "is_b2b", family = "context", leakage_risk = "safe"),
  away_is_b2b = list(description = "Away team's is_b2b for this game.", logic = NA, sources = "is_b2b", family = "context", leakage_risk = "safe"),
  home_is_3in4 = list(description = "Home team's is_3in4 for this game.", logic = NA, sources = "is_3in4", family = "context", leakage_risk = "safe"),
  away_is_3in4 = list(description = "Away team's is_3in4 for this game.", logic = NA, sources = "is_3in4", family = "context", leakage_risk = "safe"),
  home_est_flight_time = list(description = "Home team's est_flight_time for this game.", logic = NA, sources = "est_flight_time", family = "context", leakage_risk = "safe"),
  away_est_flight_time = list(description = "Away team's est_flight_time for this game.", logic = NA, sources = "est_flight_time", family = "context", leakage_risk = "safe"),
  home_timezone_shift = list(description = "Home team's time_zone_shift for this game.", logic = NA, sources = "time_zone_shift", family = "context", leakage_risk = "safe"),
  away_timezone_shift = list(description = "Away team's time_zone_shift for this game.", logic = NA, sources = "time_zone_shift", family = "context", leakage_risk = "safe"),
  # players_raw / teams_raw fields confident enough to describe directly
  display_last_comma_first = list(description = "Player name formatted 'Last, First'.", logic = NA, sources = "hoopR::nba_commonallplayers()", family = "metadata", leakage_risk = "safe"),
  display_first_last = list(description = "Player name formatted 'First Last'.", logic = NA, sources = "hoopR::nba_commonallplayers()", family = "metadata", leakage_risk = "safe"),
  height = list(description = "Player height.", logic = NA, sources = "hoopR::nba_playerindex()", family = "reference", leakage_risk = "safe"),
  weight = list(description = "Player weight.", logic = NA, sources = "hoopR::nba_playerindex()", family = "reference", leakage_risk = "safe"),
  college = list(description = "Player's college, if any.", logic = NA, sources = "hoopR::nba_playerindex()", family = "reference", leakage_risk = "safe"),
  country = list(description = "Player's home country.", logic = NA, sources = "hoopR::nba_playerindex()", family = "reference", leakage_risk = "safe"),
  draft_year = list(description = "Year the player was drafted.", logic = NA, sources = "hoopR::nba_playerindex()", family = "reference", leakage_risk = "safe"),
  draft_round = list(description = "Draft round.", logic = NA, sources = "hoopR::nba_playerindex()", family = "reference", leakage_risk = "safe"),
  draft_number = list(description = "Overall draft pick number.", logic = NA, sources = "hoopR::nba_playerindex()", family = "reference", leakage_risk = "safe"),
  jersey_number = list(description = "Player's current jersey number.", logic = NA, sources = "hoopR::nba_playerindex()", family = "reference", leakage_risk = "safe"),
  position = list(description = "Player's listed position.", logic = NA, sources = "hoopR::nba_playerindex()", family = "reference", leakage_risk = "safe"),
  player_first_name = list(description = "Player's first name.", logic = NA, sources = "hoopR::nba_playerindex()", family = "metadata", leakage_risk = "safe"),
  player_last_name = list(description = "Player's last name.", logic = NA, sources = "hoopR::nba_playerindex()", family = "metadata", leakage_risk = "safe")
)

# Columns whose meaning genuinely differs by table (would be wrong if
# applied globally) - checked before GLOBAL_OVERRIDES and before the
# raw-stat fallback.
TABLE_SPECIFIC_OVERRIDES <- list(
  players_raw = list(
    pts = list(description = "NEEDS REVIEW - a reference-context points figure from hoopR::nba_playerindex() (not tied to a specific game); confirm the exact time window (career/season-to-date) before relying on it.",
      logic = NA, sources = "hoopR::nba_playerindex()", family = "reference", leakage_risk = NA),
    reb = list(description = "NEEDS REVIEW - a reference-context rebounds figure from hoopR::nba_playerindex() (not tied to a specific game); confirm the exact time window before relying on it.",
      logic = NA, sources = "hoopR::nba_playerindex()", family = "reference", leakage_risk = NA),
    ast = list(description = "NEEDS REVIEW - a reference-context assists figure from hoopR::nba_playerindex() (not tied to a specific game); confirm the exact time window before relying on it.",
      logic = NA, sources = "hoopR::nba_playerindex()", family = "reference", leakage_risk = NA)
  ),
  # team_rebounding_features.parquet stores these WITHOUT the "adv_"
  # prefix - that gets added later, when features_combine.R joins this
  # table into team_game_features. Documented here directly as the
  # advanced-tracking stat it actually is (bare "reb" here would
  # otherwise incorrectly match the box-score glossary entry and get
  # attributed to the wrong API call).
  team_rebounding_features = setNames(
    lapply(names(STAT_GLOSSARY)[grepl("^adv_", names(STAT_GLOSSARY))], function(adv_name) {
      list(description = sprintf("Actual %s recorded in this exact game.", STAT_GLOSSARY[[adv_name]]),
           logic = NA_character_, sources = "hoopR::nba_teamdashptreb()", family = "raw_actual",
           leakage_risk = "UNSAFE - this game's actual result, never use to predict this same game")
    }),
    sub("^adv_", "", names(STAT_GLOSSARY)[grepl("^adv_", names(STAT_GLOSSARY))])
  ),
  # Same idea, player grain: the 12 rebounding stats shared with the
  # team-level table (bare names, same reasoning as above) plus the 4
  # player-only shot-distance columns (already bare in STAT_GLOSSARY,
  # no "adv_" stripping needed).
  player_rebounding_features = setNames(
    lapply(c(names(STAT_GLOSSARY)[grepl("^adv_", names(STAT_GLOSSARY))],
             "reb_shot_0_6", "reb_shot_7_13", "reb_shot_13_19", "reb_shot_19_plus"),
           function(glossary_key) {
             list(description = sprintf("Actual %s recorded in this exact game.", STAT_GLOSSARY[[glossary_key]]),
                  logic = NA_character_, sources = "hoopR::nba_playerdashptreb()", family = "raw_actual",
                  leakage_risk = "UNSAFE - this game's actual result, never use to predict this same game")
           }),
    c(sub("^adv_", "", names(STAT_GLOSSARY)[grepl("^adv_", names(STAT_GLOSSARY))]),
      "reb_shot_0_6", "reb_shot_7_13", "reb_shot_13_19", "reb_shot_19_plus")
  )
)

# ------------------------------------------------------------
# Classify one column name into description/logic/sources/family/
# leakage_risk. Order: table-specific override -> global override ->
# derived-feature naming patterns (checked most-specific first, since
# e.g. "opp_reb_allowed_roll10" would also match a looser "_roll"
# pattern) -> raw-stat glossary -> unclassified fallback.
# ------------------------------------------------------------
classify_column <- function(col, table_key) {
  specific <- TABLE_SPECIFIC_OVERRIDES[[table_key]]
  if (!is.null(specific) && col %in% names(specific)) return(specific[[col]])
  if (col %in% names(GLOBAL_OVERRIDES)) return(GLOBAL_OVERRIDES[[col]])

  # opp_<stat>_allowed_rollN - what the opponent typically allows
  m <- regmatches(col, regexec("^opp_(.+)_allowed_roll([0-9]+)$", col))[[1]]
  if (length(m) == 3 && m[2] %in% names(STAT_GLOSSARY)) {
    stat <- m[2]; n <- m[3]
    return(list(
      description = sprintf("What the opponent typically allows: their trailing %s-game average of %s conceded to opponents.", n, STAT_GLOSSARY[[stat]]),
      logic = sprintf("Mirrored from the opponent's own %s_allowed_roll%s (same game_id_nba, opponent's row) via attach_opponent_rolling_profile() in R/features_rolling.R.", stat, n),
      sources = sprintf("%s_allowed_roll%s", stat, n), family = "opponent_allowed", leakage_risk = "safe (known pre-game)"
    ))
  }
  # <stat>_allowed_rollN - this team's history of what opponents do to them
  m <- regmatches(col, regexec("^(.+)_allowed_roll([0-9]+)$", col))[[1]]
  if (length(m) == 3 && m[2] %in% names(STAT_GLOSSARY)) {
    stat <- m[2]; n <- m[3]
    return(list(
      description = sprintf("How many %s this team typically gives up: trailing %s-game average of %s the OPPONENT actually recorded against this team.", STAT_GLOSSARY[[stat]], n, STAT_GLOSSARY[[stat]]),
      logic = sprintf("Self-join to the opponent's same-game actual %s, then trailing mean over this team's prior %s games (current game excluded) via compute_allowed_rolling_features() in R/features_rolling.R.", stat, n),
      sources = sprintf("team_game_logs.%s (opponent's actual that game, via game_id_nba self-join)", stat),
      family = "allowed", leakage_risk = "safe (known pre-game)"
    ))
  }
  # opp_<stat>_rollN - opponent's own rolling form mirrored
  m <- regmatches(col, regexec("^opp_(.+)_roll([0-9]+)$", col))[[1]]
  if (length(m) == 3 && m[2] %in% names(STAT_GLOSSARY)) {
    stat <- m[2]; n <- m[3]
    return(list(
      description = sprintf("The opponent's own trailing %s-game average of %s, regardless of who they played.", n, STAT_GLOSSARY[[stat]]),
      logic = sprintf("Mirrored from the opponent's own %s_roll%s (same game_id_nba, opponent's row) via attach_opponent_rolling_profile().", stat, n),
      sources = sprintf("%s_roll%s", stat, n), family = "opponent_rolling", leakage_risk = "safe (known pre-game)"
    ))
  }
  # <stat>_matchup_edge_rollN
  m <- regmatches(col, regexec("^(.+)_matchup_edge_roll([0-9]+)$", col))[[1]]
  if (length(m) == 3 && m[2] %in% names(STAT_GLOSSARY)) {
    stat <- m[2]; n <- m[3]
    return(list(
      description = sprintf("Matchup differential: is this team's typical %s output higher or lower than what this opponent typically allows.", STAT_GLOSSARY[[stat]]),
      logic = sprintf("%s_roll%s minus opp_%s_allowed_roll%s.", stat, n, stat, n),
      sources = sprintf("%s_roll%s; opp_%s_allowed_roll%s", stat, n, stat, n),
      family = "matchup", leakage_risk = "safe (known pre-game)"
    ))
  }
  # <stat>_rollN - this team's own rolling form
  m <- regmatches(col, regexec("^(.+)_roll([0-9]+)$", col))[[1]]
  if (length(m) == 3 && m[2] %in% names(STAT_GLOSSARY)) {
    stat <- m[2]; n <- m[3]
    is_adv <- grepl("^adv_", stat)
    return(list(
      description = sprintf("This team's own trailing %s-game average of %s.", n, STAT_GLOSSARY[[stat]]),
      logic = sprintf("Trailing mean of %s over the prior %s games (current game excluded), via add_rolling_stats() in R/features_rolling.R.", stat, n),
      sources = if (is_adv) sprintf("team_rebounding_features.%s", sub("^adv_", "", stat)) else sprintf("team_game_logs.%s", stat),
      family = "own_rolling", leakage_risk = "safe (known pre-game)"
    ))
  }
  # raw actual stat (this exact game's result)
  if (col %in% names(STAT_GLOSSARY)) {
    is_adv <- grepl("^adv_", col)
    return(list(
      description = sprintf("Actual %s recorded in this exact game.", STAT_GLOSSARY[[col]]),
      logic = NA_character_,
      sources = if (is_adv) "hoopR::nba_teamdashptreb()" else "hoopR::nba_leaguegamelog()",
      family = "raw_actual",
      leakage_risk = if (col %in% UNSAFE_RAW_STATS) "UNSAFE - this game's actual result, never use to predict this same game" else "safe"
    ))
  }

  # teams_raw's "hr_" prefix (added by R/pull_reference.R's
  # rename_with(~paste0("hr_", .x))) marks a field pulled straight from
  # hoopR::nba_teams(). Most are self-explanatory from the stripped
  # name (hr_team_city, hr_logo, hr_mascot, ...) - low ambiguity, so a
  # generic description is more useful here than a blanket NEEDS
  # REVIEW, unlike players_raw's genuinely ambiguous .x/.y duplicates.
  if (grepl("^hr_", col)) {
    bare <- sub("^hr_", "", col)
    return(list(
      description = sprintf("Team reference field ('%s') pulled directly from hoopR::nba_teams() - name is self-explanatory; review if not.", bare),
      logic = NA_character_, sources = "hoopR::nba_teams()", family = "reference", leakage_risk = "safe"
    ))
  }

  list(description = "NEEDS REVIEW - not recognized by any auto-fill pattern or override; fill in by hand.",
       logic = NA_character_, sources = NA_character_, family = "unclassified", leakage_risk = NA_character_)
}

# ------------------------------------------------------------
# Table registry: how to load each table, its top-level description,
# and its core source tables (the table-row's own "sources" value).
# ------------------------------------------------------------
TABLES <- list(
  team_game_features = list(path = cfg$path_team_game_features, kind = "parquet",
    description = "Comprehensive team-game feature table: one row per team per game. Box score actuals, rest/travel context, this team's and the opponent's trailing rolling form, 'allowed' history, and explicit matchup-comparison features. The main table for rebounding-prediction modeling.",
    sources = "team_game_logs; schedule_team_level_final; team_rebounding_features"),
  player_game_features = list(path = cfg$path_player_game_features, kind = "parquet",
    description = "Comprehensive player-game feature table: one row per player per game. Box score actuals, trailing rolling averages, and the player's team's rest/travel context that game.",
    sources = "player_game_logs; team_game_features"),
  team_rebounding_features = list(path = cfg$path_rebounding_features, kind = "parquet",
    description = "Parsed advanced (shot-distance / contested) rebounding splits, one row per team per game. This-game actuals only - the rolling averages built from these live in team_game_features with an adv_ prefix.",
    sources = "hoopR::nba_teamdashptreb() via data_raw/team_rebounding_dashboards.rds"),
  player_rebounding_features = list(path = cfg$path_player_rebounding_features, kind = "parquet",
    description = "Parsed advanced (shot-distance / contested) rebounding splits, one row per player per game. Player-grain analog of team_rebounding_features, plus a bonus shot-distance-of-miss split not available at the team level. Scoped by cfg$player_rebounding_seasons and cfg$player_rebounding_min_minutes (see config/config.R) - not necessarily every player-game like the other tables.",
    sources = "hoopR::nba_playerdashptreb() via data_raw/player_rebounding_dashboards.rds"),
  schedule_with_travel_detail = list(path = cfg$path_schedule_with_travel, kind = "parquet",
    description = "One row per game, with home_*/away_* rest and travel columns side by side.",
    sources = "schedule; data_raw/external/nba_airport_flight_matrix.csv"),
  schedule_team_level_final = list(path = cfg$path_schedule_team_level, kind = "parquet",
    description = "schedule_with_travel_detail expanded to one row per team per game (2 rows per game), with opp_* columns for the opposing team's rest/travel that same game.",
    sources = "schedule_with_travel_detail"),
  schedule = list(path = cfg$path_schedule_dataset, kind = "dataset",
    description = "Regular-season game schedule (preseason excluded), one row per game.",
    sources = "hoopR::nba_schedule()"),
  team_game_logs = list(path = cfg$path_team_logs_dataset, kind = "dataset",
    description = "Team box scores, one row per team per game.",
    sources = "hoopR::nba_leaguegamelog(player_or_team='T')"),
  player_game_logs = list(path = cfg$path_player_logs_dataset, kind = "dataset",
    description = "Player box scores, one row per player per game.",
    sources = "hoopR::nba_leaguegamelog(player_or_team='P')"),
  players_raw = list(path = cfg$path_players_raw, kind = "parquet",
    description = "NBA player reference/master data (current player universe), fully refreshed every pipeline run.",
    sources = "hoopR::nba_commonallplayers(); hoopR::nba_playerindex()"),
  teams_raw = list(path = cfg$path_teams_raw, kind = "parquet",
    description = "NBA team reference/master data (all 30 teams), fully refreshed every pipeline run.",
    sources = "hoopR::nba_teams()")
)

# ------------------------------------------------------------
# Build and write one table's dictionary CSV.
# ------------------------------------------------------------
build_table_dictionary <- function(spec, table_key) {
  df <- if (spec$kind == "dataset") read_full_dataset(spec$path) else read_parquet_or_null(spec$path)
  if (is.null(df)) {
    message(table_key, ": SKIPPED, no data on disk yet.")
    return(invisible(NULL))
  }

  col_types <- vapply(df, function(x) class(x)[1], character(1))

  table_row <- tibble(row_num = 1L, name = table_key, description = spec$description,
                       logic = NA_character_, sources = spec$sources, dtype = NA_character_,
                       feature_family = NA_character_, leakage_risk = NA_character_)

  col_rows <- purrr::imap_dfr(names(df), function(col, i) {
    info <- classify_column(col, table_key)
    tibble(row_num = i + 1L, name = col, description = info$description, logic = info$logic,
           sources = info$sources, dtype = col_types[[col]], feature_family = info$family,
           leakage_risk = info$leakage_risk)
  })

  out <- bind_rows(table_row, col_rows)
  out_path <- file.path(OUT_DIR, paste0(table_key, ".csv"))
  write_csv(out, out_path, na = "NA")

  n_review <- sum(col_rows$feature_family == "unclassified", na.rm = TRUE)
  message(sprintf("%-28s %4d columns -> %s%s", table_key, nrow(col_rows), out_path,
                   if (n_review > 0) sprintf("  (%d NEEDS REVIEW)", n_review) else ""))
  invisible(out)
}

message("=== Building data dictionary ===")
results <- purrr::imap(TABLES, build_table_dictionary)
message("=== Done - see ", OUT_DIR, "/ ===")
