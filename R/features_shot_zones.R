# ============================================================
# Script: R/features_shot_zones.R
# Purpose: Classify every shot attempt in play_by_play into one of 8
#          court zones (Restricted Area, Paint Non-RA, Left/Center/
#          Right Mid-Range, Left/Right Corner 3, Above the Break 3)
#          from raw coordinate_x/coordinate_y, then build a player-
#          game grain shot-zone profile (FGM/FGA per zone) - the
#          basis for classifying shot selection tendency (corner-3
#          specialist, mid-range game, rim finisher, etc).
#
#          NBA's own shot-location dashboard endpoints are broken
#          (see R/pull_playbyplay.R header) - this replicates that
#          zone taxonomy ourselves from the coordinates we already
#          have, verified against real geometry rather than guessed:
#          coordinates are in FEET, y = lateral distance from court
#          center (+/-25, court is 50ft wide), x = distance from
#          center court along the court's length (+/-47, half-court
#          is 47ft) - confirmed by dunks clustering at |x| ~ 40-42ft,
#          matching the real hoop position (5.25ft from baseline,
#          47-5.25=41.75). ESPN does NOT normalize every shot to one
#          shooting direction - shots appear at BOTH x=+41.75 and
#          x=-41.75 hoops - so every calculation below first
#          normalizes to "as if shooting at the positive-x hoop" by
#          checking which of the two hoops is actually closer.
#
#          2PT/3PT boundary verified against real recorded outcomes
#          (score_value on made shots) before trusting this for
#          anything: 99.966% match, with the ~0.03% mismatch
#          clustering tightly at 23.6-24.0ft (the arc boundary
#          itself) - measurement-precision noise on shots taken
#          right on the line, not a formula error.
# ============================================================

HOOP_X <- 41.75                 # feet from center court - real hoop position (47 - 5.25)
RESTRICTED_AREA_RADIUS <- 4     # feet
PAINT_HALF_WIDTH <- 8           # feet either side of center (16ft lane)
PAINT_X_CUTOFF <- 28            # feet from center court = free-throw line (47 - 19)
CORNER_Y_THRESHOLD <- 22        # feet from center - corner 3 straight-line region starts here
CORNER_X_THRESHOLD <- 33        # feet from center court = within 14ft of baseline (47 - 14)
THREE_PT_ARC_RADIUS <- 23.75    # feet - above-the-break arc

# Re-express every shot as if it were taken at the +HOOP_X basket, by
# flipping x when the -HOOP_X basket is actually closer. y (lateral
# position) doesn't need flipping - it's already symmetric.
normalize_shot_x <- function(coordinate_x, coordinate_y) {
  d_pos <- (coordinate_x - HOOP_X)^2 + coordinate_y^2
  d_neg <- (coordinate_x + HOOP_X)^2 + coordinate_y^2
  ifelse(d_neg < d_pos, -coordinate_x, coordinate_x)
}

# ------------------------------------------------------------
# One zone per shot. "Left"/"right" mid-range/corner are relative to
# the coordinate_y sign as ESPN records it, not verified against a
# true broadcast-camera left/right convention - treat as a consistent
# label, not a verified stage-direction.
# ------------------------------------------------------------
classify_shot_zone <- function(coordinate_x, coordinate_y) {
  nx <- normalize_shot_x(coordinate_x, coordinate_y)
  ny <- coordinate_y
  dist <- sqrt((nx - HOOP_X)^2 + ny^2)
  is_corner_region <- abs(ny) >= CORNER_Y_THRESHOLD & nx >= CORNER_X_THRESHOLD
  is_paint <- abs(ny) <= PAINT_HALF_WIDTH & nx >= PAINT_X_CUTOFF

  dplyr::case_when(
    dist <= RESTRICTED_AREA_RADIUS ~ "restricted_area",
    is_corner_region & ny < 0 ~ "left_corner_3",
    is_corner_region & ny >= 0 ~ "right_corner_3",
    dist >= THREE_PT_ARC_RADIUS ~ "above_the_break_3",
    is_paint ~ "paint_non_ra",
    ny < -PAINT_HALF_WIDTH ~ "left_mid_range",
    ny > PAINT_HALF_WIDTH ~ "right_mid_range",
    TRUE ~ "center_mid_range"
  )
}

# ------------------------------------------------------------
# Long-format base: one row per shot attempt (make or miss), zone-
# classified, mapped to our player_id. athlete_1 is reliably "the
# shooter" across every shooting_play row (verified earlier - even
# for blocked shots, where athlete_2 is the blocker, not athlete_1).
# ------------------------------------------------------------
build_shot_zone_long_base <- function(cfg, logger) {
  pbp <- read_full_dataset(cfg$path_playbyplay_dataset)
  if (is.null(pbp)) {
    logger$log("Shot zone features: SKIPPED, no play_by_play data yet.")
    return(invisible(NULL))
  }
  espn_map <- read_parquet_or_null(cfg$path_espn_player_id_mapping)
  if (is.null(espn_map)) {
    logger$log("Shot zone features: SKIPPED, no espn_player_id_mapping.parquet yet.")
    return(invisible(NULL))
  }

  shots <- pbp %>%
    # shooting_play == TRUE also includes free throws (verified
    # empirically - ~238K rows across "Free Throw - 1 of 2" etc.) -
    # those aren't field goal attempts and are always taken from the
    # same fixed spot, so classifying them by court location would be
    # both wrong and would double-count against player_game_logs.fga.
    dplyr::filter(shooting_play, !grepl("Free Throw", type_text), !is.na(athlete_id_1)) %>%
    dplyr::mutate(
      zone = classify_shot_zone(coordinate_x, coordinate_y),
      made = scoring_play
    ) %>%
    dplyr::transmute(athlete_id_espn = as.character(athlete_id_1), game_id_nba, season, zone, made)

  long <- shots %>%
    dplyr::inner_join(espn_map %>% dplyr::select(athlete_id_espn, player_id), by = "athlete_id_espn")

  n_dropped <- nrow(shots) - nrow(long)
  if (n_dropped > 0) {
    logger$log("  ", n_dropped, " shot row(s) dropped (athlete not in espn_player_id_mapping).")
  }
  long
}

# ------------------------------------------------------------
# fga_<zone> (all attempts) and fgm_<zone> (makes only) per player-
# game, joined onto the full player_game_logs grain. Validated
# against player_game_logs.fga/fgm - the true test of whether the
# zone geometry is right, not just the 2PT/3PT split checked earlier.
# ------------------------------------------------------------
refresh_shot_zone_features <- function(cfg, logger) {
  long <- build_shot_zone_long_base(cfg, logger)
  if (is.null(long)) return(invisible(NULL))

  fga_wide <- long %>%
    dplyr::count(player_id, game_id_nba, season, zone, name = "n") %>%
    dplyr::mutate(col = paste0("fga_", zone)) %>%
    dplyr::select(-zone) %>%
    tidyr::pivot_wider(names_from = col, values_from = n, values_fill = 0)

  fgm_wide <- long %>%
    dplyr::filter(made) %>%
    dplyr::count(player_id, game_id_nba, season, zone, name = "n") %>%
    dplyr::mutate(col = paste0("fgm_", zone)) %>%
    dplyr::select(-zone) %>%
    tidyr::pivot_wider(names_from = col, values_from = n, values_fill = 0)

  logs <- read_full_dataset(cfg$path_player_logs_dataset) %>%
    dplyr::select(player_id, game_id_nba, season, fga, fgm)

  fga_cols <- setdiff(names(fga_wide), c("player_id", "game_id_nba", "season"))
  fgm_cols <- setdiff(names(fgm_wide), c("player_id", "game_id_nba", "season"))

  full <- logs %>%
    dplyr::left_join(fga_wide, by = c("player_id", "game_id_nba", "season")) %>%
    dplyr::left_join(fgm_wide, by = c("player_id", "game_id_nba", "season")) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(c(fga_cols, fgm_cols)), ~ tidyr::replace_na(.x, 0)))

  n_fga_mismatch <- sum(rowSums(dplyr::select(full, dplyr::all_of(fga_cols))) != full$fga)
  n_fgm_mismatch <- sum(rowSums(dplyr::select(full, dplyr::all_of(fgm_cols))) != full$fgm)
  fga_match_rate <- round(100 * (nrow(full) - n_fga_mismatch) / nrow(full), 2)
  fgm_match_rate <- round(100 * (nrow(full) - n_fgm_mismatch) / nrow(full), 2)
  logger$log("  validation: ", fga_match_rate, "% of player-games have fga_<zone> summing exactly to player_game_logs$fga (",
             n_fga_mismatch, "/", nrow(full), " mismatched).")
  logger$log("  validation: ", fgm_match_rate, "% of player-games have fgm_<zone> summing exactly to player_game_logs$fgm (",
             n_fgm_mismatch, "/", nrow(full), " mismatched).")

  write_parquet(full, cfg$path_player_shot_zone_features)
  logger$log("  ", cfg$path_player_shot_zone_features, " written (", nrow(full), " player-games, ",
             length(fga_cols) + length(fgm_cols), " zone columns).")
  full
}
