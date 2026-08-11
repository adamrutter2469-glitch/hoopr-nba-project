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
