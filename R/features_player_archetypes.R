# ============================================================
# Script: R/features_player_archetypes.R
# Purpose: Build a per-player-season statistical "shape" profile from
#          raw box scores - the input to the clustering step in
#          models/build_player_archetypes.R. Lives in R/ (not models/)
#          because it's a reusable feature-building step, same
#          convention as R/features_rolling.R - the clustering/labeling
#          logic that CONSUMES this output is what stays a one-off,
#          user-run script.
#
#          Deliberately season-level totals, not rolling windows: an
#          archetype is meant to describe a player's identity for that
#          season as a whole, not a fluctuating trailing snapshot.
# ============================================================

# ------------------------------------------------------------
# One row per player_id + season: games played, minutes, and
# per-36-minute rate stats computed from SEASON TOTALS (sum stat /
# sum min * 36), not an average of each game's per-36 rate - the
# totals-based version is what's standard ("per 36" the way
# basketball-reference computes it) and avoids a handful of extreme
# single-game rates (garbage-time cameo, 2 min / 1 reb) distorting
# the season figure the way a simple per-game average would.
#
# Also computes the SHAPE features (rates/proportions, not raw
# volume) that clustering will actually run on - see the header comment
# in models/build_player_archetypes.R for why archetype is built from
# shape while tier is built from volume/efficiency.
# ------------------------------------------------------------
build_player_season_profiles <- function(cfg, logger) {
  logs <- read_full_dataset(cfg$path_player_logs_dataset)
  if (is.null(logs)) {
    logger$log("Player archetype profiles: SKIPPED, no player_game_logs data yet.")
    return(tibble::tibble())
  }

  totals <- logs %>%
    dplyr::group_by(player_id, player_name, season) %>%
    dplyr::summarise(
      games_played = dplyr::n(),
      min_total    = sum(min, na.rm = TRUE),
      min_per_game = min_total / games_played,
      pts   = sum(pts, na.rm = TRUE),
      reb   = sum(reb, na.rm = TRUE),
      oreb  = sum(oreb, na.rm = TRUE),
      dreb  = sum(dreb, na.rm = TRUE),
      ast   = sum(ast, na.rm = TRUE),
      stl   = sum(stl, na.rm = TRUE),
      blk   = sum(blk, na.rm = TRUE),
      tov   = sum(tov, na.rm = TRUE),
      fgm   = sum(fgm, na.rm = TRUE),
      fga   = sum(fga, na.rm = TRUE),
      fg3m  = sum(fg3m, na.rm = TRUE),
      fg3a  = sum(fg3a, na.rm = TRUE),
      ftm   = sum(ftm, na.rm = TRUE),
      fta   = sum(fta, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(
      games_played >= cfg$player_archetype_min_games,
      min_per_game >= cfg$player_archetype_min_minutes
    )

  totals %>%
    dplyr::mutate(
      # --- shape features (proportions/rates) - these drive clustering ---
      fg3a_rate  = dplyr::if_else(fga > 0, fg3a / fga, 0),          # shot profile: 3PT-heavy vs paint-heavy
      fta_rate   = dplyr::if_else(fga > 0, fta / fga, 0),           # how often they get to the line
      oreb_share = dplyr::if_else(reb > 0, oreb / reb, 0),          # crashes the offensive glass vs mostly D-reb
      ast_to_tov = dplyr::if_else(tov > 0, ast / tov, ast),         # playmaking/ballhandling role
      reb_per36  = dplyr::if_else(min_total > 0, reb / min_total * 36, 0),
      ast_per36  = dplyr::if_else(min_total > 0, ast / min_total * 36, 0),
      stl_per36  = dplyr::if_else(min_total > 0, stl / min_total * 36, 0),
      blk_per36  = dplyr::if_else(min_total > 0, blk / min_total * 36, 0),
      tov_per36  = dplyr::if_else(min_total > 0, tov / min_total * 36, 0),
      pts_per36  = dplyr::if_else(min_total > 0, pts / min_total * 36, 0),
      # --- efficiency / volume features - held OUT of clustering, used for tier instead ---
      efg_pct    = dplyr::if_else(fga > 0, (fgm + 0.5 * fg3m) / fga, 0),
      ft_pct     = dplyr::if_else(fta > 0, ftm / fta, NA_real_)
    ) %>%
    dplyr::select(
      player_id, player_name, season, games_played, min_per_game,
      fg3a_rate, fta_rate, oreb_share, ast_to_tov,
      reb_per36, ast_per36, stl_per36, blk_per36, tov_per36, pts_per36,
      efg_pct, ft_pct
    )
}

# ------------------------------------------------------------
# Season-level shot-SELECTION profile, built from the already-
# validated player_shot_zone_features.parquet (player-game grain,
# see R/features_shot_zones.R). Converted to SHARES (proportion of a
# player's own total FGA from each zone) rather than raw counts -
# shape, not volume, same philosophy as build_player_season_profiles()
# above. Mid-range and corner-3 are each collapsed left+right into
# one combined share (side-of-court handedness isn't the identity
# question here); restricted-area and 3PT efficiency are kept as two
# zone-specific skill markers, mirroring how efg_pct was already used
# as a shape input even though it's technically an efficiency stat.
# ------------------------------------------------------------
build_player_season_shot_profile <- function(cfg, logger) {
  zone <- read_parquet_or_null(cfg$path_player_shot_zone_features)
  if (is.null(zone)) {
    logger$log("Season shot-zone profile: SKIPPED, no player_shot_zone_features.parquet yet.")
    return(tibble::tibble())
  }

  zone_cols <- c("fga_restricted_area", "fga_paint_non_ra", "fga_left_mid_range", "fga_center_mid_range",
                 "fga_right_mid_range", "fga_left_corner_3", "fga_right_corner_3", "fga_above_the_break_3",
                 "fgm_restricted_area", "fgm_left_corner_3", "fgm_right_corner_3", "fgm_above_the_break_3")

  totals <- zone %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(zone_cols), sum), .groups = "drop")

  totals %>%
    dplyr::mutate(
      total_fga = fga_restricted_area + fga_paint_non_ra + fga_left_mid_range + fga_center_mid_range +
        fga_right_mid_range + fga_left_corner_3 + fga_right_corner_3 + fga_above_the_break_3,
      restricted_area_share   = dplyr::if_else(total_fga > 0, fga_restricted_area / total_fga, 0),
      paint_share             = dplyr::if_else(total_fga > 0, fga_paint_non_ra / total_fga, 0),
      mid_range_share         = dplyr::if_else(total_fga > 0, (fga_left_mid_range + fga_center_mid_range + fga_right_mid_range) / total_fga, 0),
      corner_3_share          = dplyr::if_else(total_fga > 0, (fga_left_corner_3 + fga_right_corner_3) / total_fga, 0),
      above_the_break_3_share = dplyr::if_else(total_fga > 0, fga_above_the_break_3 / total_fga, 0),
      restricted_area_fg_pct  = dplyr::if_else(fga_restricted_area > 0, fgm_restricted_area / fga_restricted_area, 0),
      three_pt_fga = fga_left_corner_3 + fga_right_corner_3 + fga_above_the_break_3,
      three_pt_fgm = fgm_left_corner_3 + fgm_right_corner_3 + fgm_above_the_break_3,
      three_pt_fg_pct = dplyr::if_else(three_pt_fga > 0, three_pt_fgm / three_pt_fga, 0)
    ) %>%
    dplyr::select(player_id, season, total_fga, restricted_area_share, paint_share, mid_range_share,
                  corner_3_share, above_the_break_3_share, restricted_area_fg_pct, three_pt_fg_pct)
}

# ------------------------------------------------------------
# Alley-oop rate/efficiency per player-season, mined directly from
# play_by_play type_text - not currently broken out anywhere else
# (they're folded into the general dunk/layup zone buckets in
# player_shot_zone_features). athlete_1 = shooter, the same
# convention verified for every shooting_play row.
# ------------------------------------------------------------
ALLEY_OOP_TYPES <- c("Alley Oop Dunk Shot", "Alley Oop Layup Shot",
                      "Running Alley Oop Dunk Shot", "Running Alley Oop Layup Shot")

build_player_season_alley_oop_profile <- function(cfg, logger) {
  pbp <- read_full_dataset(cfg$path_playbyplay_dataset)
  espn_map <- read_parquet_or_null(cfg$path_espn_player_id_mapping)
  if (is.null(pbp) || is.null(espn_map)) {
    logger$log("Season alley-oop profile: SKIPPED, missing play_by_play or espn_player_id_mapping.")
    return(tibble::tibble())
  }

  oops <- pbp %>%
    dplyr::filter(gsub("\n", " ", type_text) %in% ALLEY_OOP_TYPES, !is.na(athlete_id_1)) %>%
    dplyr::transmute(athlete_id_espn = as.character(athlete_id_1), season, made = scoring_play) %>%
    dplyr::inner_join(espn_map %>% dplyr::select(athlete_id_espn, player_id), by = "athlete_id_espn")

  oops %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(alley_oop_fga = dplyr::n(), alley_oop_fgm = sum(made), .groups = "drop") %>%
    dplyr::mutate(alley_oop_fg_pct = dplyr::if_else(alley_oop_fga > 0, alley_oop_fgm / alley_oop_fga, 0))
}

# ------------------------------------------------------------
# Season-level defensive EVENT profile - steal type (lost ball =
# on-ball/loose-ball recovery vs bad pass = interception, already
# mined in player_turnover_features), block type (rim/paint vs
# perimeter jump shot, already mined in player_block_features), and
# foul rate/timing (already mined in player_foul_features). All
# three source tables are player-game grain covering all 4 seasons -
# no new PBP mining needed here, purely aggregation.
# ------------------------------------------------------------
build_player_season_defense_event_profile <- function(cfg, logger) {
  tov <- read_parquet_or_null(cfg$path_player_turnover_features)
  blk <- read_parquet_or_null(cfg$path_player_block_features)
  pf  <- read_parquet_or_null(cfg$path_player_foul_features)
  if (is.null(tov) || is.null(blk) || is.null(pf)) {
    logger$log("Season defensive event profile: SKIPPED, missing turnover/block/foul features.")
    return(tibble::tibble())
  }

  stl_season <- tov %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(stl_lost_ball = sum(stl_lost_ball), stl_bad_pass = sum(stl_bad_pass), .groups = "drop") %>%
    dplyr::mutate(
      stl_total = stl_lost_ball + stl_bad_pass,
      lost_ball_steal_share = dplyr::if_else(stl_total > 0, stl_lost_ball / stl_total, 0)
    )

  blk_type_cols <- c("blk_layup", "blk_dunk", "blk_hook", "blk_tip", "blk_jump_shot")
  blk_season <- blk %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(blk_type_cols), sum), blk_total_derived = sum(blk), .groups = "drop") %>%
    dplyr::mutate(
      blk_rim_total = blk_layup + blk_dunk + blk_hook + blk_tip,
      paint_block_share = dplyr::if_else(blk_total_derived > 0, blk_rim_total / blk_total_derived, 0)
    ) %>%
    dplyr::select(player_id, season, paint_block_share)

  # Foul-timing shape feature: early_foul_rate = share of a player's
  # games (among games they actually played enough to be at risk of
  # foul trouble) where their 2nd personal foul came within their own
  # first foul_trouble_window_minutes ON THE FLOOR - NOT within the
  # game's first N minutes on the clock. Replaces an earlier pf_q1_share
  # (fouls-in-Q1 / total-fouls) design that penalized bench players who
  # simply hadn't checked in yet during Q1 - see conversation history.
  #
  # Eligibility gate: a player-game only counts in the denominator if
  # they played >= the window (their box-score `min` that game) - a
  # player who logs 4 minutes can't be assessed for "foul trouble
  # within their first 10 minutes" one way or the other, and including
  # them would understate the rate for players who frequently get
  # short/spot minutes for reasons unrelated to fouling.
  window_seconds <- cfg$foul_trouble_window_minutes * 60
  logs_min <- read_full_dataset(cfg$path_player_logs_dataset)
  if (is.null(logs_min)) {
    logger$log("Season defensive event profile: SKIPPED, no player_game_logs for foul-timing eligibility.")
    return(tibble::tibble())
  }
  logs_min <- logs_min %>% dplyr::select(player_id, game_id_nba, season, min)

  pf_timing <- pf %>%
    dplyr::select(player_id, game_id_nba, season, onct_elapsed_at_pf_2) %>%
    dplyr::inner_join(logs_min, by = c("player_id", "game_id_nba", "season")) %>%
    dplyr::filter(min >= cfg$foul_trouble_window_minutes) %>%
    dplyr::mutate(early_foul_game = !is.na(onct_elapsed_at_pf_2) & onct_elapsed_at_pf_2 <= window_seconds)

  pf_season <- pf %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(pf_total = sum(pf), .groups = "drop") %>%
    dplyr::left_join(
      pf_timing %>%
        dplyr::group_by(player_id, season) %>%
        dplyr::summarise(eligible_games = dplyr::n(), early_foul_games = sum(early_foul_game), .groups = "drop"),
      by = c("player_id", "season")
    ) %>%
    dplyr::mutate(
      eligible_games = tidyr::replace_na(eligible_games, 0),
      early_foul_games = tidyr::replace_na(early_foul_games, 0),
      early_foul_rate = dplyr::if_else(eligible_games > 0, early_foul_games / eligible_games, 0)
    )

  stl_season %>%
    dplyr::select(player_id, season, stl_total, lost_ball_steal_share) %>%
    dplyr::full_join(blk_season, by = c("player_id", "season")) %>%
    dplyr::full_join(pf_season %>% dplyr::select(player_id, season, pf_total, early_foul_rate), by = c("player_id", "season"))
}

# ------------------------------------------------------------
# Season-level ADVANCED rebounding profile - contested share and
# distance-from-basket zone shares, from player_rebounding_features
# (hoopR::nba_playerdashptreb() via R/pull_player_rebounding.R).
#
# IMPORTANT SCOPE LIMIT: unlike every other season-profile builder in
# this file, this one is NOT available across all 4 seasons -
# player_rebounding_features is scoped to cfg$player_rebounding_seasons
# (currently "2025-26" only - no bulk API endpoint exists for this
# tracking data, one call per player-game, so a full historical
# backfill is a real, multi-hour decision the user hasn't made yet).
# Whatever season(s) are actually in this table is what comes out
# here - the caller is responsible for knowing that's currently just
# one season, not silently assuming full historical coverage.
# ------------------------------------------------------------
build_player_season_advanced_rebounding_profile <- function(cfg, logger) {
  adv <- read_parquet_or_null(cfg$path_player_rebounding_features)
  sched <- read_full_dataset(cfg$path_schedule_dataset)
  if (is.null(adv) || is.null(sched)) {
    logger$log("Season advanced rebounding profile: SKIPPED, missing player_rebounding_features or schedule.")
    return(tibble::tibble())
  }

  adv <- adv %>% dplyr::inner_join(sched %>% dplyr::select(game_id_nba, season), by = "game_id_nba")

  dist_cols <- c("reb_0_3", "reb_3_6", "reb_6_10", "reb_10_plus")
  adv %>%
    dplyr::group_by(player_id, season) %>%
    dplyr::summarise(
      reb_adv_total = sum(reb, na.rm = TRUE),
      c_reb_total   = sum(c_reb, na.rm = TRUE),
      dplyr::across(dplyr::all_of(dist_cols), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      contested_reb_share = dplyr::if_else(reb_adv_total > 0, c_reb_total / reb_adv_total, 0),
      reb_0_3_share    = dplyr::if_else(reb_adv_total > 0, reb_0_3 / reb_adv_total, 0),
      reb_3_6_share    = dplyr::if_else(reb_adv_total > 0, reb_3_6 / reb_adv_total, 0),
      reb_6_10_share   = dplyr::if_else(reb_adv_total > 0, reb_6_10 / reb_adv_total, 0),
      reb_10_plus_share = dplyr::if_else(reb_adv_total > 0, reb_10_plus / reb_adv_total, 0)
    ) %>%
    dplyr::select(player_id, season, contested_reb_share, reb_0_3_share, reb_3_6_share, reb_6_10_share, reb_10_plus_share)
}
