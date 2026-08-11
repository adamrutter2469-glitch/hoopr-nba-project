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
  player_last_name = list(description = "Player's last name.", logic = NA, sources = "hoopR::nba_playerindex()", family = "metadata", leakage_risk = "safe"),
  # Big Balls Sports Data (bigballsdata.com) id-mapping bridge tables
  team_name_full = list(description = "Team's full name (e.g. 'Golden State Warriors') - the join key used to build this mapping, since abbreviations differ between the two sources for 5 franchises.", logic = NA, sources = "teams_raw.hr_team_name_full", family = "identifier", leakage_risk = "safe"),
  bbs_team_id = list(description = "This team's id in Big Balls Sports Data's system (UUID) - unrelated to our own numeric team_id.", logic = "Matched by normalized full team name.", sources = "bigballsdata.com /v1/teams", family = "identifier", leakage_risk = "safe"),
  bbs_short_name = list(description = "Team abbreviation as Big Balls Sports Data spells it - differs from our team_abbreviation for 5 teams (GS/NO/SA/UTAH/WSH vs our GSW/NOP/SAS/UTA/WAS).", logic = NA, sources = "bigballsdata.com /v1/teams", family = "metadata", leakage_risk = "safe (metadata only)"),
  bbs_player_id = list(description = "This player's id in Big Balls Sports Data's system (UUID) - unrelated to our own numeric player_id.", logic = "Matched by normalized name (diacritics/periods/whitespace stripped); name collisions on either side excluded rather than guessed at.", sources = "bigballsdata.com /v1/players", family = "identifier", leakage_risk = "safe"),
  bbs_player_name = list(description = "Player's name as Big Balls Sports Data spells it - the join key used to build this mapping.", logic = NA, sources = "bigballsdata.com /v1/players", family = "metadata", leakage_risk = "safe (metadata only)"),
  # Player archetype tables (models/build_player_archetypes.R,
  # models/build_offensive_archetypes.R) - shared columns; the
  # cluster COUNT/definitions differ per table, but what these
  # columns themselves mean doesn't.
  games_played = list(description = "Games played this player-season (season-total grain, not rolling).", logic = "count(*) grouped by player_id + season", sources = "player_game_logs", family = "context", leakage_risk = "safe"),
  min_per_game = list(description = "Average minutes played per game this player-season.", logic = "sum(min) / games_played", sources = "player_game_logs.min", family = "context", leakage_risk = "safe"),
  total_fga = list(description = "Total field goal attempts this player-season, summed across the 8 shot zones - the denominator for every *_share column in this table.", logic = "Sum of fga_<zone> across all 8 zones.", sources = "player_shot_zone_features", family = "context", leakage_risk = "safe"),
  archetype_cluster = list(description = "Which k-means cluster this player-season was assigned to (an arbitrary integer, not itself meaningful - see archetype_label for the human-readable name). Fit on standardized shape features via models/build_player_archetypes.R or models/build_offensive_archetypes.R depending on the table.", logic = "kmeans() cluster assignment.", sources = NA, family = "identifier", leakage_risk = "safe"),
  archetype_label = list(description = "Human-assigned name for this player-season's archetype_cluster, chosen after reviewing real centroid profiles and sample rosters - never pre-decided before seeing the actual clustering output.", logic = "Manual lookup from CLUSTER_LABELS in the build script.", sources = "archetype_cluster", family = "metadata", leakage_risk = "safe (metadata only)"),
  # ESPN player-id bridge (R/pull_playbyplay.R) - lets any future
  # play-by-play mining join back to our own player_id.
  athlete_id_espn = list(description = "This player's id in ESPN's system (as used in play_by_play's athlete_id_1/2/3) - unrelated to our own numeric player_id.", logic = "Matched by normalized name; name collisions on either side excluded rather than guessed at (empirically verified zero collisions within this project's season scope before building, but the exclusion logic is kept as a safety net regardless).", sources = "play_by_play.athlete_id_1/2/3", family = "identifier", leakage_risk = "safe"),
  athlete_name_espn = list(description = "Player's name as ESPN spells it in play_by_play - the join key used to build this mapping.", logic = NA, sources = "play_by_play.athlete_name_1/2/3", family = "metadata", leakage_risk = "safe (metadata only)")
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
  # Combined (offense+defense) archetype system - k=9, box-score-only
  # shape features. See models/build_player_archetypes.R for the full
  # design (archetype = shape via clustering, tier = caliber within
  # archetype via a role-specific weighted composite).
  player_archetypes = list(
    fg3a_rate  = list(description = "Share of this player-season's field goal attempts that were 3-pointers.", logic = "fg3a / fga (season totals)", sources = "player_game_logs.fg3a; player_game_logs.fga", family = "context", leakage_risk = "safe"),
    fta_rate   = list(description = "Free throw attempts relative to field goal attempts this player-season - a proxy for how often this player draws fouls/gets to the line.", logic = "fta / fga (season totals)", sources = "player_game_logs.fta; player_game_logs.fga", family = "context", leakage_risk = "safe"),
    oreb_share = list(description = "Share of this player's own total rebounds that were offensive (vs defensive) this player-season.", logic = "oreb / reb (season totals)", sources = "player_game_logs.oreb; player_game_logs.reb", family = "context", leakage_risk = "safe"),
    ast_to_tov = list(description = "Assist-to-turnover ratio this player-season (capped at 10 before clustering, to keep one very-low-turnover season from dominating the standardized distance metric - the capped value is what's stored here).", logic = "pmin(ast / tov, 10)", sources = "player_game_logs.ast; player_game_logs.tov", family = "context", leakage_risk = "safe"),
    reb_per36  = list(description = "Total rebounds per 36 minutes this player-season (season-totals basis, not an average of per-game rates).", logic = "reb / min_total * 36", sources = "player_game_logs.reb; player_game_logs.min", family = "context", leakage_risk = "safe"),
    ast_per36  = list(description = "Assists per 36 minutes this player-season.", logic = "ast / min_total * 36", sources = "player_game_logs.ast; player_game_logs.min", family = "context", leakage_risk = "safe"),
    stl_per36  = list(description = "Steals per 36 minutes this player-season.", logic = "stl / min_total * 36", sources = "player_game_logs.stl; player_game_logs.min", family = "context", leakage_risk = "safe"),
    blk_per36  = list(description = "Blocks per 36 minutes this player-season.", logic = "blk / min_total * 36", sources = "player_game_logs.blk; player_game_logs.min", family = "context", leakage_risk = "safe"),
    tov_per36  = list(description = "Turnovers per 36 minutes this player-season.", logic = "tov / min_total * 36", sources = "player_game_logs.tov; player_game_logs.min", family = "context", leakage_risk = "safe"),
    pts_per36  = list(description = "Points per 36 minutes this player-season.", logic = "pts / min_total * 36", sources = "player_game_logs.pts; player_game_logs.min", family = "context", leakage_risk = "safe"),
    efg_pct    = list(description = "Effective field goal percentage this player-season (weights 3-pointers appropriately vs 2-pointers). Used both as a clustering shape input and as a tier-score component.", logic = "(fgm + 0.5*fg3m) / fga (season totals)", sources = "player_game_logs.fgm; player_game_logs.fg3m; player_game_logs.fga", family = "context", leakage_risk = "safe"),
    ft_pct     = list(description = "Free throw percentage this player-season.", logic = "ftm / fta (season totals)", sources = "player_game_logs.ftm; player_game_logs.fta", family = "context", leakage_risk = "safe"),
    tier       = list(description = "Caliber within this player's archetype ('Tier 1' best - 'Tier 4' lowest), NOT an absolute/league-wide caliber ranking. What counts toward tier differs by archetype - see TIER_WEIGHTS in models/build_player_archetypes.R (e.g. scoring/efficiency dominate for Elite Offensive Hub, rebounding/blocks dominate for Traditional Post Big).", logic = "Role-specific weighted sum of within-cluster z-scored production/efficiency stats, percentile-bucketed (top 10%/next 30%/next 30%/bottom 30%).", sources = "pts_per36; efg_pct; ast_per36; reb_per36; stl_per36; blk_per36; tov_per36", family = "metadata", leakage_risk = "safe (metadata only)"),
    tier_score = list(description = "The continuous (non-bucketed) score tier is derived from - higher means better within this player's own archetype.", logic = NA, sources = "tier", family = "metadata", leakage_risk = "safe (metadata only)")
  ),
  # Offense-only archetype system - k=9, enriched with real shot-
  # location data (R/features_shot_zones.R) and alley-oop rate mined
  # directly from play_by_play. No defensive stats (stl/blk/reb) at
  # all, by design. See models/build_offensive_archetypes.R.
  player_offensive_archetypes = list(
    restricted_area_share   = list(description = "Share of this player-season's total FGA taken from the restricted area (within 4ft of the hoop).", logic = "fga_restricted_area / total_fga", sources = "player_shot_zone_features", family = "context", leakage_risk = "safe"),
    paint_share             = list(description = "Share of total FGA taken from the paint, excluding the restricted area.", logic = "fga_paint_non_ra / total_fga", sources = "player_shot_zone_features", family = "context", leakage_risk = "safe"),
    mid_range_share         = list(description = "Share of total FGA that were mid-range shots (left + center + right mid-range zones combined).", logic = "(fga_left_mid_range + fga_center_mid_range + fga_right_mid_range) / total_fga", sources = "player_shot_zone_features", family = "context", leakage_risk = "safe"),
    corner_3_share          = list(description = "Share of total FGA that were corner 3s (left + right corner combined).", logic = "(fga_left_corner_3 + fga_right_corner_3) / total_fga", sources = "player_shot_zone_features", family = "context", leakage_risk = "safe"),
    above_the_break_3_share = list(description = "Share of total FGA that were above-the-break 3s.", logic = "fga_above_the_break_3 / total_fga", sources = "player_shot_zone_features", family = "context", leakage_risk = "safe"),
    restricted_area_fg_pct  = list(description = "Field goal percentage on restricted-area attempts this player-season - a rim-finishing skill marker, included as a clustering input the same way efg_pct is in the combined archetype system.", logic = "fgm_restricted_area / fga_restricted_area", sources = "player_shot_zone_features", family = "context", leakage_risk = "safe"),
    three_pt_fg_pct         = list(description = "Field goal percentage on all 3-point attempts (corner + above-the-break combined) this player-season.", logic = "(fgm_left_corner_3+fgm_right_corner_3+fgm_above_the_break_3) / (fga_left_corner_3+fga_right_corner_3+fga_above_the_break_3)", sources = "player_shot_zone_features", family = "context", leakage_risk = "safe"),
    alley_oop_rate           = list(description = "Share of total FGA that were alley-oops (dunk or layup) - the 'lob threat' signal. Not tracked anywhere else in this pipeline; mined directly from play_by_play type_text.", logic = "alley_oop_fga / total_fga", sources = "play_by_play.type_text (Alley Oop Dunk/Layup Shot, Running variants)", family = "context", leakage_risk = "safe"),
    alley_oop_fg_pct         = list(description = "Conversion rate on alley-oop attempts this player-season.", logic = "alley_oop_fgm / alley_oop_fga", sources = "play_by_play.type_text; play_by_play.scoring_play", family = "context", leakage_risk = "safe"),
    fta_rate   = list(description = "Free throw attempts relative to field goal attempts this player-season.", logic = "fta / fga (season totals, from player_game_logs)", sources = "player_game_logs.fta; player_game_logs.fga", family = "context", leakage_risk = "safe"),
    oreb_share = list(description = "Share of this player's own total rebounds that were offensive this player-season.", logic = "oreb / reb (season totals)", sources = "player_game_logs.oreb; player_game_logs.reb", family = "context", leakage_risk = "safe"),
    ast_to_tov = list(description = "Assist-to-turnover ratio this player-season (capped at 10 before clustering).", logic = "pmin(ast / tov, 10)", sources = "player_game_logs.ast; player_game_logs.tov", family = "context", leakage_risk = "safe"),
    ast_per36  = list(description = "Assists per 36 minutes this player-season.", logic = "ast / min_total * 36", sources = "player_game_logs.ast; player_game_logs.min", family = "context", leakage_risk = "safe"),
    pts_per36  = list(description = "Points per 36 minutes this player-season.", logic = "pts / min_total * 36", sources = "player_game_logs.pts; player_game_logs.min", family = "context", leakage_risk = "safe"),
    tov_per36  = list(description = "Turnovers per 36 minutes this player-season.", logic = "tov / min_total * 36", sources = "player_game_logs.tov; player_game_logs.min", family = "context", leakage_risk = "safe")
  ),
  # Defense/hustle-only archetype system - k=9, SEASON-SCOPED TO
  # 2025-26 ONLY (not all 4 seasons like the other two archetype
  # systems - depends on player_rebounding_features, which is only
  # pulled for cfg$player_rebounding_seasons). No offensive stats at
  # all, by design. See models/build_defensive_archetypes.R.
  player_defensive_archetypes = list(
    reb_per36 = list(description = "Total rebounds (offensive + defensive combined) per 36 minutes this player-season - deliberately folds offensive rebounding into the defense/hustle system rather than treating it as an offensive stat.", logic = "reb / min_total * 36", sources = "player_game_logs.reb; player_game_logs.min", family = "context", leakage_risk = "safe"),
    oreb_share = list(description = "Share of this player's own total rebounds that were offensive this player-season.", logic = "oreb / reb (season totals)", sources = "player_game_logs.oreb; player_game_logs.reb", family = "context", leakage_risk = "safe"),
    contested_reb_share = list(description = "Share of this player-season's rebounds (tracking-data total, not box-score reb) that were contested by an opposing player nearby, per NBA tracking data.", logic = "sum(c_reb) / sum(reb) from player_rebounding_features", sources = "player_rebounding_features.c_reb; player_rebounding_features.reb", family = "context", leakage_risk = "safe"),
    reb_0_3_share = list(description = "Share of this player-season's tracked rebounds grabbed 0-3 feet from the basket.", logic = "sum(reb_0_3) / sum(reb) from player_rebounding_features", sources = "player_rebounding_features.reb_0_3", family = "context", leakage_risk = "safe"),
    reb_3_6_share = list(description = "Share of this player-season's tracked rebounds grabbed 3-6 feet from the basket.", logic = "sum(reb_3_6) / sum(reb) from player_rebounding_features", sources = "player_rebounding_features.reb_3_6", family = "context", leakage_risk = "safe"),
    reb_6_10_share = list(description = "Share of this player-season's tracked rebounds grabbed 6-10 feet from the basket.", logic = "sum(reb_6_10) / sum(reb) from player_rebounding_features", sources = "player_rebounding_features.reb_6_10", family = "context", leakage_risk = "safe"),
    reb_10_plus_share = list(description = "Share of this player-season's tracked rebounds grabbed 10+ feet from the basket - typically long rebounds/loose-ball recoveries rather than fighting for position under the rim.", logic = "sum(reb_10_plus) / sum(reb) from player_rebounding_features", sources = "player_rebounding_features.reb_10_plus", family = "context", leakage_risk = "safe"),
    stl_per36 = list(description = "Steals per 36 minutes this player-season.", logic = "stl_total / min_total * 36", sources = "player_turnover_features (stl_lost_ball + stl_bad_pass); player_game_logs.min", family = "context", leakage_risk = "safe"),
    lost_ball_steal_share = list(description = "Share of this player-season's steals that were lost-ball recoveries (on-ball/loose-ball defense) rather than bad-pass interceptions.", logic = "stl_lost_ball / (stl_lost_ball + stl_bad_pass), summed over the season", sources = "player_turnover_features.stl_lost_ball; player_turnover_features.stl_bad_pass", family = "context", leakage_risk = "safe"),
    blk_per36 = list(description = "Blocks per 36 minutes this player-season.", logic = "blk / min_total * 36", sources = "player_game_logs.blk; player_game_logs.min", family = "context", leakage_risk = "safe"),
    paint_block_share = list(description = "Share of this player-season's blocks that came against a rim-area shot type (layup/dunk/hook/tip) rather than a jump shot - rim protection vs. perimeter contest.", logic = "(blk_layup+blk_dunk+blk_hook+blk_tip) / blk, summed over the season", sources = "player_block_features.blk_layup; player_block_features.blk_dunk; player_block_features.blk_hook; player_block_features.blk_tip; player_block_features.blk_jump_shot", family = "context", leakage_risk = "safe"),
    pf_per36 = list(description = "Personal fouls per 36 minutes this player-season.", logic = "pf_total / min_total * 36", sources = "player_foul_features.pf; player_game_logs.min", family = "context", leakage_risk = "safe"),
    early_foul_rate = list(description = "Share of this player's eligible games (games where they played at least foul_trouble_window_minutes) where their 2nd personal foul came within their own first foul_trouble_window_minutes ON THE FLOOR - anchored to when THEY checked in, not the raw game clock, so bench players who simply hadn't entered yet aren't misread as 'never foul early'. Replaced an earlier game-clock-relative design (fouls in Q1 / total fouls) that was biased against bench players - see conversation history.", logic = "count(eligible games where onct_elapsed_at_pf_2 <= foul_trouble_window_minutes*60) / count(eligible games), per player-season", sources = "player_foul_features.onct_elapsed_at_pf_2; player_game_logs.min", family = "context", leakage_risk = "safe")
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
  ),
  # play_by_play: ESPN-sourced (hoopR::load_nba_pbp()), so several
  # names collide with GLOBAL_OVERRIDES but mean something different
  # here - team_id/home_team_id/away_team_id/game_id are ESPN's OWN
  # identifiers, not our NBA Stats ones (game_id_nba is the bridged
  # column that IS our identifier - see R/pull_playbyplay.R). Table-
  # specific overrides take precedence over GLOBAL_OVERRIDES, so these
  # correctly override rather than inherit the wrong meaning.
  play_by_play = list(
    game_id_espn = list(description = "ESPN's own game identifier (this data's native key before bridging to game_id_nba).", logic = NA,
      sources = "hoopR::load_nba_pbp()", family = "identifier", leakage_risk = "safe"),
    game_id_nba = list(description = "NBA Stats API's canonical game identifier, bridged in from the schedule (not ESPN's native key).",
      logic = "Joined from schedule on (game_date, home team abbrev, away team abbrev) - a deterministic key, since a team plays a given opponent at most once per date. ~99.9% match rate; the rare miss is typically an All-Star/exhibition game with no NBA Stats regular-season/playoff counterpart.",
      sources = "schedule.game_id_nba (via date + team abbreviation match)", family = "identifier", leakage_risk = "safe"),
    id = list(description = "ESPN's unique identifier for this individual play event.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "identifier", leakage_risk = "safe"),
    sequence_number = list(description = "ESPN's sequence number for this play within the game feed.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "identifier", leakage_risk = "safe"),
    game_play_number = list(description = "This play's ordinal position within the game (1st play, 2nd play, ...).", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    type_id = list(description = "ESPN's numeric code for this play's event type.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "metadata", leakage_risk = "safe (metadata only)"),
    type_text = list(description = "Event type in plain text, e.g. 'Jump Shot', 'Defensive Rebound', 'Clear Path Foul', '8-Second Turnover'. Very granular - dozens of distinct foul/turnover/shot subtypes.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    type_abbreviation = list(description = "Short code for type_text.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "metadata", leakage_risk = "safe (metadata only)"),
    text = list(description = "Full human-readable play description, e.g. 'Chet Holmgren makes 1-foot layup (Luguentz Dort assists)'.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "metadata", leakage_risk = "safe (metadata only)"),
    home_score = list(description = "Home team's running score after this play.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe (this play's own outcome, not a prediction target)"),
    away_score = list(description = "Away team's running score after this play.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe (this play's own outcome, not a prediction target)"),
    scoring_play = list(description = "TRUE if this play scored points.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    shooting_play = list(description = "TRUE if this play was a shot attempt (made or missed).", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    score_value = list(description = "Points this play was worth (1/2/3), NA if not a scoring play.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    team_id = list(description = "ESPN's own team identifier for the team involved in this play - NOT our NBA Stats team_id used elsewhere in this pipeline.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "identifier", leakage_risk = "safe"),
    athlete_id_1 = list(description = "ESPN's player identifier for the primary player involved in this play (e.g. the shooter) - NOT our NBA Stats player_id.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "identifier", leakage_risk = "safe"),
    athlete_id_2 = list(description = "ESPN's player identifier for a secondary player involved (e.g. the assister), if any.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "identifier", leakage_risk = "safe"),
    athlete_id_3 = list(description = "ESPN's player identifier for a third player involved, if any.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "identifier", leakage_risk = "safe"),
    athlete_name_1 = list(description = "Name of the primary player involved in this play.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "metadata", leakage_risk = "safe (metadata only)"),
    athlete_name_2 = list(description = "Name of a secondary player involved, if any.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "metadata", leakage_risk = "safe (metadata only)"),
    athlete_name_3 = list(description = "Name of a third player involved, if any.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "metadata", leakage_risk = "safe (metadata only)"),
    coordinate_x_raw = list(description = "Shot/event x-coordinate in ESPN's raw pixel-space units.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    coordinate_y_raw = list(description = "Shot/event y-coordinate in ESPN's raw pixel-space units.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    coordinate_x = list(description = "Shot/event x-coordinate, normalized to real court units - use this (not the _raw version) for zone/distance calculations.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    coordinate_y = list(description = "Shot/event y-coordinate, normalized to real court units - use this (not the _raw version) for zone/distance calculations.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    wallclock = list(description = "Real-world UTC timestamp the play was recorded.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "metadata", leakage_risk = "safe (metadata only)"),
    season_type = list(description = "'Regular Season' / 'Playoffs' / etc. for this game.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    home_team_id = list(description = "ESPN's own identifier for the home team - NOT our NBA Stats team_id.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "identifier", leakage_risk = "safe"),
    away_team_id = list(description = "ESPN's own identifier for the away team - NOT our NBA Stats team_id.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "identifier", leakage_risk = "safe"),
    home_team_name = list(description = "Home team's name as ESPN spells it.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "metadata", leakage_risk = "safe (metadata only)"),
    away_team_name = list(description = "Away team's name as ESPN spells it.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "metadata", leakage_risk = "safe (metadata only)"),
    home_team_mascot = list(description = "Home team's mascot name.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "metadata", leakage_risk = "safe (metadata only)"),
    away_team_mascot = list(description = "Away team's mascot name.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "metadata", leakage_risk = "safe (metadata only)"),
    home_team_abbrev = list(description = "Home team's abbreviation as ESPN spells it - differs from our home_team_tricode for 6 franchises (NY/GS/SA/UTAH/WSH/NO vs our NYK/GSW/SAS/UTA/WAS/NOP); used to build the game_id_nba bridge (see R/pull_playbyplay.R).", logic = NA, sources = "hoopR::load_nba_pbp()", family = "metadata", leakage_risk = "safe (metadata only)"),
    away_team_abbrev = list(description = "Away team's abbreviation as ESPN spells it - same mismatch caveat as home_team_abbrev.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "metadata", leakage_risk = "safe (metadata only)"),
    home_team_name_alt = list(description = "Alternate/short form of the home team's name.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "metadata", leakage_risk = "safe (metadata only)"),
    away_team_name_alt = list(description = "Alternate/short form of the away team's name.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "metadata", leakage_risk = "safe (metadata only)"),
    game_spread = list(description = "Betting point spread for this game, as carried by ESPN's feed - not something this pipeline pulled deliberately; unrelated to the bigballsdata odds work (R/pull_bigballsdata_odds.R). Review before relying on it (coverage/definition not verified).", logic = NA, sources = "hoopR::load_nba_pbp()", family = "unclassified", leakage_risk = NA),
    home_favorite = list(description = "Whether the home team was favored, per ESPN's feed. Same caveat as game_spread - not independently verified.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "unclassified", leakage_risk = NA),
    game_spread_available = list(description = "Whether a spread was available for this game in ESPN's feed.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "unclassified", leakage_risk = NA),
    home_team_spread = list(description = "Home team's point spread, per ESPN's feed. Same caveat as game_spread.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "unclassified", leakage_risk = NA),
    qtr = list(description = "Quarter number (5+ = overtime period).", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    period = list(description = "Alias of qtr/period_number.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    period_number = list(description = "Quarter/period number.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    period_display_value = list(description = "Human-readable period, e.g. '1st Quarter', 'OT'.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "metadata", leakage_risk = "safe (metadata only)"),
    time = list(description = "Game clock display at this play, e.g. '11:36'.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    clock_display_value = list(description = "Alias of time.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    clock_minutes = list(description = "Minutes remaining on the game clock at this play.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    clock_seconds = list(description = "Seconds remaining on the game clock at this play.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    half = list(description = "Half number (1 or 2).", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    game_half = list(description = "Alias/label of half.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    lag_qtr = list(description = "Quarter number of the previous play - a lookback helper for detecting quarter transitions.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    lead_qtr = list(description = "Quarter number of the next play - a lookahead helper for detecting quarter transitions.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    lag_half = list(description = "Half number of the previous play.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    lead_half = list(description = "Half number of the next play.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    home_timeout_called = list(description = "TRUE if the home team called a timeout on this play.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    away_timeout_called = list(description = "TRUE if the away team called a timeout on this play.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    start_quarter_seconds_remaining = list(description = "Seconds remaining in the quarter at the start of this play.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    start_half_seconds_remaining = list(description = "Seconds remaining in the half at the start of this play.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    start_game_seconds_remaining = list(description = "Seconds remaining in the game at the start of this play.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    end_quarter_seconds_remaining = list(description = "Seconds remaining in the quarter after this play.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    end_half_seconds_remaining = list(description = "Seconds remaining in the half after this play.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    end_game_seconds_remaining = list(description = "Seconds remaining in the game after this play.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "context", leakage_risk = "safe"),
    game_date_time = list(description = "Full game start timestamp.", logic = NA, sources = "hoopR::load_nba_pbp()", family = "metadata", leakage_risk = "safe (metadata only)")
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
  # play-by-play-mined event-detail columns (R/features_playbyplay_events.R) -
  # checked before the raw-stat fallback below since none of these
  # collide with a bare STAT_GLOSSARY key (they're all prefixed/suffixed).
  m <- regmatches(col, regexec("^tov_(.+)$", col))[[1]]
  if (length(m) == 2) {
    label <- gsub("_", " ", m[2])
    return(list(
      description = sprintf("Turnovers of type '%s' this player committed in this exact game, mined from play_by_play event text.", label),
      logic = "Counted from play_by_play rows where type_text matches this turnover subtype and this player is athlete_1 (who committed it). NOTE: 5 turnover types (shot clock, 5/8-second, excess timeout, too many players) never name a player in ESPN's feed at all - those turnovers count toward player_game_logs.tov but can't be broken out here (~7% of all turnovers, verified empirically - see player_turnover_features table description).",
      sources = "play_by_play.type_text; play_by_play.athlete_id_1 (via espn_player_id_mapping)",
      family = "raw_actual", leakage_risk = "UNSAFE - this game's actual result, never use to predict this same game"
    ))
  }
  m <- regmatches(col, regexec("^stl_(.+)$", col))[[1]]
  if (length(m) == 2) {
    label <- gsub("_", " ", m[2])
    return(list(
      description = sprintf("Steals this player got by forcing a '%s' turnover in this exact game, mined from play_by_play event text.", label),
      logic = "Counted from play_by_play rows where type_text matches this turnover subtype and this player is athlete_2 (who forced/stole it) - only Bad Pass and Lost Ball turnovers ever populate a second player (verified empirically: every other turnover type is 0%).",
      sources = "play_by_play.type_text; play_by_play.athlete_id_2 (via espn_player_id_mapping)",
      family = "raw_actual", leakage_risk = "UNSAFE - this game's actual result, never use to predict this same game"
    ))
  }
  m <- regmatches(col, regexec("^blk_(.+)$", col))[[1]]
  if (length(m) == 2) {
    label <- gsub("_", " ", m[2])
    return(list(
      description = sprintf("Blocks this player recorded against a %s shot in this exact game, mined from play_by_play.", label),
      logic = "Counted from play_by_play rows where shooting_play is TRUE, the shot missed, and athlete_id_2 is populated (a missed shot with a 2nd player attached can only be a block - verified empirically), and this player is athlete_2 (the blocker). Shot type bucketed into dunk/layup/hook/tip/jump_shot via clean_shot_category().",
      sources = "play_by_play.type_text (bucketed); play_by_play.athlete_id_2 (via espn_player_id_mapping)",
      family = "raw_actual", leakage_risk = "UNSAFE - this game's actual result, never use to predict this same game"
    ))
  }
  m <- regmatches(col, regexec("^fg_blocked_(.+)$", col))[[1]]
  if (length(m) == 2) {
    label <- gsub("_", " ", m[2])
    return(list(
      description = sprintf("Times this player's own %s attempt was blocked in this exact game - a stat the official NBA box score doesn't track at all (no player_game_logs equivalent to validate against), mined from play_by_play.", label),
      logic = "Counted from play_by_play rows where shooting_play is TRUE, the shot missed, and athlete_id_2 is populated, and this player is athlete_1 (the shooter whose shot was blocked).",
      sources = "play_by_play.type_text (bucketed); play_by_play.athlete_id_1 (via espn_player_id_mapping)",
      family = "raw_actual", leakage_risk = "UNSAFE - this game's actual result, never use to predict this same game"
    ))
  }

  m <- regmatches(col, regexec("^pf_(q[0-9]|ot)$", col))[[1]]
  if (length(m) == 2) {
    label <- if (m[2] == "ot") "overtime (all OT periods combined)" else paste0("quarter ", substr(m[2], 2, 2))
    return(list(
      description = sprintf("Personal fouls this player committed during %s of this exact game.", label),
      logic = "Counted from play_by_play PF-eligible foul events (see PF_ELIGIBLE_TYPES in R/features_playbyplay_events.R), bucketed by period_number. Answers 'when' fouls happened - the box score only has the end-of-game total.",
      sources = "play_by_play.period_number; play_by_play.type_text (PF-eligible types only)",
      family = "raw_actual", leakage_risk = "UNSAFE - this game's actual result, never use to predict this same game"
    ))
  }
  m <- regmatches(col, regexec("^pf_(.+)$", col))[[1]]
  if (length(m) == 2) {
    label <- gsub("_", " ", m[2])
    return(list(
      description = sprintf("Personal fouls of type '%s' this player committed in this exact game, mined from play_by_play. Counts toward the real 6-foul disqualification limit - the eligible type set (base foul types + Flagrant Fouls, excluding Technical Fouls) was chosen empirically by testing which combination best reconciles against player_game_logs.pf (96.31%% exact match), not assumed from rules alone.", label),
      logic = "Counted from play_by_play rows where type_text is this foul subtype and this player is athlete_1 (or athlete_2 too, for Double Personal Foul - both named players are credited).",
      sources = "play_by_play.type_text; play_by_play.athlete_id_1/2 (via espn_player_id_mapping)",
      family = "raw_actual", leakage_risk = "UNSAFE - this game's actual result, never use to predict this same game"
    ))
  }
  m <- regmatches(col, regexec("^elapsed_seconds_at_pf_([0-9])$", col))[[1]]
  if (length(m) == 2) {
    return(list(
      description = sprintf("Seconds elapsed since tip-off (continuous across regulation and any OT periods) when this player picked up their %s%s personal foul this game. NA if they never reached that many fouls that game (not missing data - a real 'didn't happen').",
                             m[2], if (m[2] == "2") "nd" else if (m[2] == "3") "rd" else "th"),
      logic = "PF-eligible foul events ranked chronologically per player-game via compute_elapsed_seconds() (whole-game-relative in regulation, period-relative in OT - verified empirically, not assumed), then the Nth one's timestamp taken.",
      sources = "play_by_play.period_number; play_by_play.start_game_seconds_remaining",
      family = "raw_actual", leakage_risk = "UNSAFE - this game's actual result, never use to predict this same game"
    ))
  }
  if (col == "fouled_out") {
    return(list(description = "TRUE if this player committed 6+ PF-eligible personal fouls this game (the real disqualification threshold).",
      logic = "pf >= 6", sources = "pf", family = "raw_actual",
      leakage_risk = "UNSAFE - this game's actual result, never use to predict this same game"))
  }
  if (col == "checkin_elapsed_seconds") {
    return(list(description = "Seconds elapsed since tip-off when this player FIRST checked into the game (0 if they started on the floor - either as a starter or by never appearing in a substitution event at all, i.e. an iron-man). The anchor used to turn game-clock-relative foul timing into playing-time-relative timing (see onct_elapsed_at_pf_2/3/6).",
      logic = "Earliest play_by_play 'Substitution' event involving this player that game (as either the entering or leaving athlete) - if their earliest appearance is an exit, they started on the floor (0); if an entry, that event's timestamp; if no substitution event at all, they played the whole game with zero subs (0). See build_player_game_checkin_time() in R/features_playbyplay_events.R.",
      sources = "play_by_play.type_text ('Substitution'); play_by_play.athlete_id_1/2 (via espn_player_id_mapping)",
      family = "raw_actual", leakage_risk = "UNSAFE - this game's actual result, never use to predict this same game"))
  }
  m <- regmatches(col, regexec("^onct_elapsed_at_pf_([0-9])$", col))[[1]]
  if (length(m) == 2) {
    return(list(description = sprintf("Seconds of ON-COURT playing time (not raw game clock) this player had accumulated when they picked up their %s%s personal foul this game - elapsed_seconds_at_pf_%s minus their own checkin_elapsed_seconds. NA if they never reached that many fouls that game.",
                             m[2], if (m[2] == "2") "nd" else if (m[2] == "3") "rd" else "th", m[2]),
      logic = sprintf("pmax(elapsed_seconds_at_pf_%s - checkin_elapsed_seconds, 0) - corrects the raw game-clock milestone for when the player actually checked into the game, so a bench player isn't misread as having 'never fouled early' just because they hadn't entered yet.", m[2]),
      sources = sprintf("elapsed_seconds_at_pf_%s; checkin_elapsed_seconds", m[2]),
      family = "raw_actual", leakage_risk = "UNSAFE - this game's actual result, never use to predict this same game"))
  }

  m <- regmatches(col, regexec("^fg[am]_(.+)$", col))[[1]]
  if (length(m) == 2 && m[2] %in% c("above_the_break_3", "left_mid_range", "right_mid_range",
                                     "center_mid_range", "left_corner_3", "right_corner_3",
                                     "restricted_area", "paint_non_ra")) {
    zone_label <- gsub("_", " ", m[2])
    is_makes <- grepl("^fgm_", col)
    return(list(
      description = sprintf("Field goal %s from the %s zone in this exact game, mined from play_by_play shot coordinates.",
                             if (is_makes) "makes" else "attempts", zone_label),
      logic = "Every shooting_play row (free throws excluded) classified into one of 8 court zones via classify_shot_zone() in R/features_shot_zones.R, from coordinate_x/coordinate_y (feet, verified empirically against real court geometry - see file header). 2PT/3PT boundary verified against real recorded score_value: 99.966% match. Zone totals validated against player_game_logs.fga/fgm: 98.33%/99.73% exact match.",
      sources = "play_by_play.coordinate_x; play_by_play.coordinate_y; play_by_play.athlete_id_1 (via espn_player_id_mapping)",
      family = "raw_actual", leakage_risk = "UNSAFE - this game's actual result, never use to predict this same game"
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
  team_id_mapping = list(path = cfg$path_team_id_mapping, kind = "parquet",
    description = "Bridge table: our team_id <-> Big Balls Sports Data's bbs_team_id, one row per team. Matched by normalized full team name, not abbreviation - their canonical short_name differs from our tricode for 5 franchises (GS/NO/SA/UTAH/WSH vs our GSW/NOP/SAS/UTA/WAS). All 30 teams matched.",
    sources = "teams_raw; bigballsdata.com /v1/teams"),
  player_id_mapping = list(path = cfg$path_player_id_mapping, kind = "parquet",
    description = "Bridge table: our player_id <-> Big Balls Sports Data's bbs_player_id, one row per player. Matched by normalized name; same-name collisions on either side (e.g. two different players sharing a name across NBA history) are excluded rather than guessed at. Not every player matches - name variations across the two sources account for the gap.",
    sources = "players_raw; bigballsdata.com /v1/players"),
  espn_player_id_mapping = list(path = cfg$path_espn_player_id_mapping, kind = "parquet",
    description = "Bridge table: our player_id <-> ESPN's athlete_id (as used in play_by_play), one row per player. Matched in two passes (match_names_two_tier() in R/pull_bigballsdata_mapping.R, shared with the bigballsdata bridge below): exact normalized-name match first, then a second pass - restricted to whatever's left unmatched on both sides - with a trailing generational suffix (Jr./Sr./II/III/IV) stripped. Same-name collisions excluded rather than guessed at, independently at each pass. Suffix-stripping every name up front was tried and rejected - it collided some current suffixed players against a different, older same-named player in the full historical roster, reducing total matches; restricting it to the leftover-unmatched pool avoids that. Most of players_raw doesn't match, which is expected: it's the full historical player reference table, while this bridge only covers players who actually appeared in the seasons play_by_play has been pulled for.",
    sources = "players_raw; play_by_play"),
  player_turnover_features = list(path = cfg$path_player_turnover_features, kind = "parquet",
    description = "Player-game grain turnover detail, mined from play_by_play's type_text + athlete roles (R/features_playbyplay_events.R) - one row per player per game, every game player_game_logs has, with 0s for event types that didn't occur (not missing rows). 23 event columns (tov_bad_pass, tov_lost_ball, tov_offensive_foul, tov_traveling, ... plus stl_bad_pass/stl_lost_ball for steals forced by causing those two specific turnover types - the only ones ESPN ever attaches a second player to). Validated against player_game_logs.tov: 93.00% of player-games sum exactly - the gap is a real, structural ESPN data limitation (5 turnover types - shot clock, 5/8-second, excess timeout, too many players - never name a player at all in ESPN's feed, even though the NBA box score does credit someone), not a bug in the derivation.",
    sources = "play_by_play; espn_player_id_mapping; player_game_logs"),
  player_block_features = list(path = cfg$path_player_block_features, kind = "parquet",
    description = "Player-game grain block detail, mined from play_by_play (R/features_playbyplay_events.R) - one row per player per game player_game_logs has, 0s for types that didn't occur. Blocks have no dedicated type_text (a blocked shot just carries its normal shot type) - detected via a structural rule instead: a missed shot with a 2nd player attached can only be a block. Subdivided by shot category (dunk/layup/hook/tip/jump_shot). blk_* columns validated against player_game_logs.blk: 99.68% exact match. fg_blocked_* (how often a player's OWN shot gets blocked) has no official box-score equivalent to validate against - a genuinely new stat this data enables, not something the NBA box score tracks.",
    sources = "play_by_play; espn_player_id_mapping; player_game_logs"),
  player_shot_zone_features = list(path = cfg$path_player_shot_zone_features, kind = "parquet",
    description = "Player-game grain shot location detail, mined from play_by_play shot coordinates (R/features_shot_zones.R) - one row per player per game player_game_logs has, 0s for zones with no attempts. Every shot classified into 8 court zones (restricted area, paint non-RA, left/center/right mid-range, left/right corner 3, above-the-break 3) via geometric rules verified against real court dimensions, since NBA's own shot-location dashboard endpoints are currently broken (see play_by_play table description). The 2PT/3PT boundary was checked against actual recorded shot values before trusting it for anything (99.966% match); zone totals validated against player_game_logs.fga/fgm (98.33%/99.73% exact match). Free throws are correctly excluded (they're not field goal attempts and don't reflect real shot-selection location).",
    sources = "play_by_play; espn_player_id_mapping; player_game_logs"),
  player_archetypes = list(path = cfg$path_player_archetypes, kind = "parquet",
    description = "Combined offense+defense player play-style archetype, one row per player-season (min 20 games, min 10 min/game that season). K-means (k=9) on standardized box-score shape features (proportions/rates, not volume) - archetype describes STYLE; tier (percentile-ranked WITHIN each archetype via a role-specific weighted composite) describes CALIBER within that style. Position words deliberately excluded from archetype names. See models/build_player_archetypes.R for the full design and naming rationale.",
    sources = "player_game_logs"),
  player_offensive_archetypes = list(path = cfg$path_player_offensive_archetypes, kind = "parquet",
    description = "Offense-ONLY player play-style archetype (the counterpart to player_archetypes above) - one row per player-season (same games/minutes floor, plus total_fga >= 50), enriched with real shot-location data (player_shot_zone_features) and alley-oop rate/efficiency mined directly from play_by_play. No defensive stats (stl/blk/reb) at all, by design. K-means (k=9) on standardized shape features. No tier layer yet (built after this table, if wanted). See models/build_offensive_archetypes.R.",
    sources = "player_game_logs; player_shot_zone_features; play_by_play; espn_player_id_mapping"),
  player_foul_features = list(path = cfg$path_player_foul_features, kind = "parquet",
    description = "Player-game grain foul detail, mined from play_by_play (R/features_playbyplay_events.R) - one row per player per game player_game_logs has, 0s for types that didn't occur. Four views built from the same underlying events: pf_<type> (foul subtype - shooting/personal/loose ball/offensive/flagrant/etc.), pf_q1..pf_q4/pf_ot (WHEN in the game fouls happened - the box score's end-of-game total can't answer this), elapsed_seconds_at_pf_2/3/6 (the exact game-clock moment - continuous across regulation and OT - a player reached that many fouls, NA if they never did), and checkin_elapsed_seconds/onct_elapsed_at_pf_2/3/6 (the same milestones re-anchored to the player's own on-court playing time rather than the raw game clock, so bench players who simply hadn't checked in yet aren't misread as never getting in early foul trouble - see conversation history). The PF-eligible foul type set (which type_texts count toward the real 6-foul disqualification) was chosen empirically by testing candidate sets against player_game_logs.pf directly, not assumed from rulebook memory: base foul types + Flagrant Fouls won at 96.31% exact match; Technical Fouls (even 'double' ones) do not count and are excluded. 'Offensive Foul' and 'Offensive Foul Turnover' were verified to be the SAME real event logged as two separate play_by_play rows (100% overlap) - only 'Offensive Foul' is counted here to avoid double-counting; the turnover side already lives in player_turnover_features.",
    sources = "play_by_play; espn_player_id_mapping; player_game_logs"),
  player_defensive_archetypes = list(path = cfg$path_player_defensive_archetypes, kind = "parquet",
    description = "Defense/hustle-ONLY player play-style archetype (the third leg alongside player_archetypes and player_offensive_archetypes) - one row per player-season, SEASON SCOPE 2025-26 ONLY (unlike the other two archetype systems, which cover all 4 seasons - see below). Steal type (on-ball/loose-ball recovery vs interception), block type (rim/paint vs perimeter), rebounding (offensive + defensive combined, contested share, and distance-from-basket zone), and fouls (rate per-36 and on-court-time-relative early-foul-trouble rate). No offensive/scoring stats at all, by design. K-means (k=9), k chosen via a k=3..15 average-silhouette sweep reviewed against basketball legibility (the silhouette-optimal k=3 collapsed to a size/position proxy rather than the style distinctions this feature set targets - see models/build_defensive_archetypes.R header). No tier layer yet. SCOPE CAVEAT: relies on player_rebounding_features, which is only pulled for cfg$player_rebounding_seasons (currently just 2025-26, since that tracking data has no bulk API endpoint) - a future run may drop advanced rebounding to compare against all 4 seasons of box-score-only defensive data instead.",
    sources = "player_game_logs; player_turnover_features; player_block_features; player_foul_features; player_rebounding_features"),
  schedule_with_travel_detail = list(path = cfg$path_schedule_with_travel, kind = "parquet",
    description = "One row per game, with home_*/away_* rest and travel columns side by side.",
    sources = "schedule; data_raw/external/nba_airport_flight_matrix.csv"),
  schedule_team_level_final = list(path = cfg$path_schedule_team_level, kind = "parquet",
    description = "schedule_with_travel_detail expanded to one row per team per game (2 rows per game), with opp_* columns for the opposing team's rest/travel that same game.",
    sources = "schedule_with_travel_detail"),
  schedule = list(path = cfg$path_schedule_dataset, kind = "dataset",
    description = "Regular-season game schedule (preseason excluded), one row per game.",
    sources = "hoopR::nba_schedule()"),
  play_by_play = list(path = cfg$path_playbyplay_dataset, kind = "dataset",
    description = "Event-level play-by-play: one row per play (shot/foul/turnover/rebound/etc.) for every game, with shot coordinates, the players involved, and game-clock context. ESPN-sourced (hoopR::load_nba_pbp()) since the NBA-Stats-native PBP/shot-location endpoints are currently blocked on NBA's side. game_id_nba is bridged in deterministically by matching (game_date, home team abbrev, away team abbrev) against the schedule - not ESPN's native game_id (kept as game_id_espn). ~99.9% of games match; the rare miss is an All-Star/exhibition game with no NBA Stats counterpart.",
    sources = "hoopR::load_nba_pbp(); schedule (for the game_id_nba bridge)"),
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
