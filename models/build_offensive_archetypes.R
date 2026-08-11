# ============================================================
# Script: models/build_offensive_archetypes.R
# Purpose: Offense-ONLY player archetype clustering - the counterpart
#          to models/build_player_archetypes.R (the original combined
#          system), now enriched with real shot-location data
#          (R/features_shot_zones.R) and alley-oop rate/efficiency
#          mined directly from play_by_play. No defensive stats
#          (stl/blk/reb) included at all, per explicit scope.
#
#          Deliberately NOT pre-naming clusters around hypotheses
#          like "corner-3 specialist" or "lob threat" going in - the
#          point of clustering is to see what the data actually
#          groups together, then name clusters from the real centroid
#          profiles and sample rosters after review. Same workflow as
#          the original archetype build.
#
#          A separate, user-run script - NOT wired into run_pipeline.bat.
#          Run: Rscript models/build_offensive_archetypes.R
# ============================================================

suppressMessages({
  library(dplyr)
  library(arrow)
  library(tibble)
})

source("config/config.R")
source("R/utils_io.R")
source("R/features_player_archetypes.R")

logger <- init_logger(cfg$path_logs)
on.exit(logger$close(), add = TRUE)

# ------------------------------------------------------------
# 1. Build and merge the three season-level profile sources. Base
#    profile (box score) already applies the games/minutes floor -
#    left-joining the newer shot-zone/alley-oop profiles onto it
#    (not inner-joining) so a qualifying player with zero alley-oops
#    that season gets a real 0, not a dropped row.
# ------------------------------------------------------------
base_profile  <- build_player_season_profiles(cfg, logger)
shot_profile  <- build_player_season_shot_profile(cfg, logger)
oop_profile   <- build_player_season_alley_oop_profile(cfg, logger)

profiles <- base_profile %>%
  dplyr::left_join(shot_profile, by = c("player_id", "season")) %>%
  dplyr::left_join(oop_profile, by = c("player_id", "season")) %>%
  dplyr::mutate(
    dplyr::across(c(total_fga, restricted_area_share, paint_share, mid_range_share, corner_3_share,
                     above_the_break_3_share, restricted_area_fg_pct, three_pt_fg_pct,
                     alley_oop_fga, alley_oop_fgm, alley_oop_fg_pct), ~ tidyr::replace_na(.x, 0)),
    alley_oop_rate = dplyr::if_else(total_fga > 0, alley_oop_fga / total_fga, 0),
    ast_to_tov = pmin(ast_to_tov, 10)   # same outlier cap as the original build
  )

# Defensive floor: a handful of shots isn't enough for a stable zone-
# share estimate, and any remaining zero/near-zero total_fga rows
# (residual PBP/matching gaps despite the espn_player_id_mapping
# suffix fix) would otherwise cluster together as a fake "no shots"
# archetype rather than being genuinely excluded - happened once
# already during development (12 zero-fga rows formed their own
# degenerate cluster before this floor and the mapping fix existed).
n_before_fga_floor <- nrow(profiles)
profiles <- profiles %>% dplyr::filter(total_fga >= 50)
logger$log("Offensive archetypes: ", nrow(profiles), " player-seasons (",
           n_before_fga_floor - nrow(profiles), " dropped for total_fga < 50).")

# ------------------------------------------------------------
# 2. Offense-only shape feature set. Old aggregate fg3a_rate is
#    dropped in favor of the new corner_3_share + above_the_break_3_
#    share, which is a strict refinement of the same information
#    (which zone the 3 comes from, not just that it's a 3).
# ------------------------------------------------------------
shape_cols <- c(
  "restricted_area_share", "paint_share", "mid_range_share", "corner_3_share", "above_the_break_3_share",
  "restricted_area_fg_pct", "three_pt_fg_pct",
  "alley_oop_rate", "alley_oop_fg_pct",
  "fta_rate", "oreb_share", "ast_to_tov", "ast_per36", "pts_per36", "tov_per36"
)

X <- profiles %>% dplyr::select(dplyr::all_of(shape_cols)) %>% as.matrix() %>% scale()

set.seed(cfg$player_archetype_seed)
elbow <- sapply(4:12, function(k) kmeans(X, centers = k, nstart = 10, iter.max = 50)$tot.withinss)
logger$log("Elbow diagnostic (total within-cluster SS by k):")
print(setNames(round(elbow, 1), paste0("k=", 4:12)))

set.seed(cfg$player_archetype_seed)
fit <- kmeans(X, centers = cfg$player_archetype_offense_k, nstart = 25, iter.max = 50)
profiles$archetype_cluster <- fit$cluster

# ------------------------------------------------------------
# 3. Centroid profiles in original units, for review.
# ------------------------------------------------------------
centroid_profile <- profiles %>%
  dplyr::group_by(archetype_cluster) %>%
  dplyr::summarise(
    n_player_seasons = dplyr::n(),
    dplyr::across(dplyr::all_of(shape_cols), ~ round(mean(.x), 3)),
    .groups = "drop"
  ) %>%
  dplyr::arrange(archetype_cluster)

cat("\n=== Offense-only cluster centroid profiles (original units) ===\n")
print(as.data.frame(centroid_profile))

# ------------------------------------------------------------
# Cluster labels - assigned after reviewing real centroids, sample
# rosters, and k=7/k=8 comparisons (see conversation history):
# reducing k always merged the two rim-based clusters (6+7) before
# anything else, confirming the "lob threat" split (7) is a real,
# separable signal worth keeping distinct at k=9, not an artifact of
# over-fitting. Position words deliberately excluded, same convention
# as the combined archetype system (models/build_player_archetypes.R).
# Cluster NUMBERING isn't guaranteed stable across a re-cluster on
# different data (same caveat as the combined system) - sanity-check
# the printed centroids/sample rosters against these labels before
# trusting them if this is ever re-run after a large data change.
# ------------------------------------------------------------
CLUSTER_LABELS <- c(
  `1` = "Movement Shooter",          # Nickeil Alexander-Walker, Terry Rozier, Coby White - combo-guard shot creation + high 3PA rate
  `2` = "Primary Offensive Engine",  # SGA, Luka, Brunson, Anthony Edwards - elite scoring + elite playmaking together
  `3` = "Primary Facilitator",       # Fred VanVleet, Derrick White, Payton Pritchard - highest ast:tov, pass-first
  `4` = "Inside-Out Scorer",         # Anthony Davis, Siakam, KAT - real volume both at the rim and out to 3
  `5` = "Slasher-Spacer",            # OG Anunoby, Luguentz Dort, Jaden McDaniels - rim pressure + corner-3, low usage
  `6` = "Inside Scorer",             # Bam Adebayo, Evan Mobley, Zubac, Jarrett Allen - lives at the rim, minimal jumper
  `7` = "Lob Threat",                # Rudy Gobert, Nic Claxton, Jalen Duren - 23%+ alley-oop rate, near-zero jumper
  `8` = "Spot-Up Shooter",           # Mikal Bridges, Klay Thompson - efficient low-usage catch-and-shoot wing
  `9` = "Sharpshooter"               # Duncan Robinson, Max Strus, Malik Beasley - 69% of shots are 3s, purest floor-spacer
)
profiles$archetype_label <- dplyr::coalesce(
  CLUSTER_LABELS[as.character(profiles$archetype_cluster)],
  paste0("Archetype ", profiles$archetype_cluster)
)

# ------------------------------------------------------------
# 4. Sample rosters: 5 highest-volume (by total_fga, a neutral
#    "prominent this season" signal, not yet a tier judgment) players
#    per cluster, most recent season, for a basketball sanity check.
# ------------------------------------------------------------
cat("\n=== 5 highest-FGA-volume player-seasons per cluster ===\n")
sample_rows <- profiles %>%
  dplyr::group_by(archetype_cluster) %>%
  dplyr::slice_max(total_fga, n = 5, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(archetype_cluster, dplyr::desc(total_fga)) %>%
  dplyr::select(archetype_cluster, player_name, season, restricted_area_share, corner_3_share,
                above_the_break_3_share, mid_range_share, alley_oop_rate, pts_per36)
print(as.data.frame(sample_rows))

# ------------------------------------------------------------
# 5. Write output.
# ------------------------------------------------------------
out <- profiles %>%
  dplyr::select(player_id, player_name, season, games_played, min_per_game, total_fga,
                 archetype_cluster, archetype_label, dplyr::all_of(shape_cols))

write_parquet(out, cfg$path_player_offensive_archetypes)
logger$log("Offensive archetypes: ", nrow(out), " player-seasons written to ", cfg$path_player_offensive_archetypes)
