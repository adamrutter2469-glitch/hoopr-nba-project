# ============================================================
# Script: models/build_defensive_archetypes.R
# Purpose: Defense/hustle-ONLY player archetype clustering - the third
#          leg alongside models/build_player_archetypes.R (combined)
#          and models/build_offensive_archetypes.R (offense-only).
#          No scoring stats included at all, per explicit scope -
#          steals (on-ball/loose-ball recovery vs interception),
#          blocks (rim/paint vs perimeter), rebounding (all boards,
#          offensive + defensive together, contested share, and
#          distance-from-basket zone), and fouls (rate per-36 AND how
#          quickly they're accrued in a game).
#
#          SEASON SCOPE: 2025-26 ONLY, not all 4 seasons like the
#          other two archetype systems. player_rebounding_features
#          (the advanced/contested/distance rebounding tracking data)
#          is currently only pulled for cfg$player_rebounding_seasons
#          = "2025-26" - no bulk API endpoint exists for it, so a full
#          historical backfill is a real, separate decision the user
#          hasn't made yet. This is an explicit first pass WITH
#          advanced rebounding, scoped to the one season it's
#          available for - the user may follow up with a second run
#          that drops advanced rebounding and compares against all 4
#          seasons of box-score-only defensive data instead.
#
#          Deliberately NOT pre-naming clusters - print centroid
#          profiles and sample rosters for review first, same
#          build -> show -> discuss -> name rhythm as the other two
#          systems. CLUSTER_LABELS is intentionally left unassigned
#          below; archetype_label falls back to "Archetype N" until
#          real labels are added after reviewing this run's output.
#
#          A separate, user-run script - NOT wired into run_pipeline.bat.
#          Run: Rscript models/build_defensive_archetypes.R
# ============================================================

suppressMessages({
  library(dplyr)
  library(arrow)
  library(tibble)
  library(cluster)
})

source("config/config.R")
source("R/utils_io.R")
source("R/features_player_archetypes.R")

logger <- init_logger(cfg$path_logs)
on.exit(logger$close(), add = TRUE)

TARGET_SEASON <- "2025-26"

# ------------------------------------------------------------
# 1. Build and merge the three season-level profile sources, all
#    filtered/joined down to TARGET_SEASON only.
#
#    base_profile already applies the games/minutes floor and gives us
#    reb_per36 (all boards, off+def combined - folding offensive
#    rebounding in here per explicit request rather than treating it
#    as an offensive stat), oreb_share, stl_per36, blk_per36,
#    min_per_game, games_played.
#
#    defense_event_profile is left-joined (every qualifying player has
#    SOME steal/block/foul history, but a handful of true zeros - e.g.
#    a player who genuinely never blocked a shot all season - should
#    stay real zeros, not silently drop the row).
#
#    advanced_rebounding_profile is INNER-joined - this run is
#    explicitly "with advanced rebounding," so a player missing from
#    that (narrower-scoped, min-minutes-gated) table is out of scope
#    for this particular pass, not a bug to paper over with a 0.
# ------------------------------------------------------------
base_profile <- build_player_season_profiles(cfg, logger) %>%
  dplyr::filter(season == TARGET_SEASON)
defense_event_profile <- build_player_season_defense_event_profile(cfg, logger)
adv_rebounding_profile <- build_player_season_advanced_rebounding_profile(cfg, logger)

n_before_adv_join <- nrow(base_profile)

profiles <- base_profile %>%
  dplyr::left_join(defense_event_profile, by = c("player_id", "season")) %>%
  dplyr::inner_join(adv_rebounding_profile, by = c("player_id", "season")) %>%
  dplyr::mutate(
    dplyr::across(c(stl_total, lost_ball_steal_share, paint_block_share, pf_total, early_foul_rate),
                  ~ tidyr::replace_na(.x, 0)),
    min_total = min_per_game * games_played,
    pf_per36  = dplyr::if_else(min_total > 0, pf_total / min_total * 36, 0)
  )

logger$log("Defensive archetypes: ", nrow(profiles), " player-seasons for ", TARGET_SEASON, " (",
           n_before_adv_join - nrow(profiles), " dropped for missing advanced rebounding data).")

# ------------------------------------------------------------
# 2. Defense-only shape feature set.
# ------------------------------------------------------------
shape_cols <- c(
  "reb_per36", "oreb_share",
  "contested_reb_share", "reb_0_3_share", "reb_3_6_share", "reb_6_10_share", "reb_10_plus_share",
  "stl_per36", "lost_ball_steal_share",
  "blk_per36", "paint_block_share",
  "pf_per36", "early_foul_rate"
)

X <- profiles %>% dplyr::select(dplyr::all_of(shape_cols)) %>% as.matrix() %>% scale()
X_dist <- dist(X)

# ------------------------------------------------------------
# k selection: wide open, no assumed target - sweep a broad range
# (k=3..15, capped below n/8 or so to avoid degenerate tiny-n fits)
# and pick k by average silhouette width (cluster::silhouette),
# which - unlike the elbow/total-withinss diagnostic used for the
# other two systems - actually penalizes k values that only look
# good because they've carved off a small artifact cluster (like the
# n=11 cluster flagged in the k=9 review): silhouette rewards k where
# points are both tight within their own cluster AND well-separated
# from the nearest other cluster, both good and bad splits show up in
# the score. Elbow/withinss is still printed alongside for reference,
# since it monotonically decreases and can't itself pick a k.
# ------------------------------------------------------------
k_range <- 3:15
set.seed(cfg$player_archetype_seed)
sweep_results <- lapply(k_range, function(k) {
  fit_k <- kmeans(X, centers = k, nstart = 25, iter.max = 50)
  sil <- cluster::silhouette(fit_k$cluster, X_dist)
  list(k = k, tot_withinss = fit_k$tot.withinss, avg_sil = mean(sil[, "sil_width"]), fit = fit_k)
})

sweep_summary <- tibble::tibble(
  k = sapply(sweep_results, `[[`, "k"),
  tot_withinss = sapply(sweep_results, `[[`, "tot_withinss"),
  avg_silhouette = sapply(sweep_results, `[[`, "avg_sil")
)
cat("\n=== k sweep: total within-cluster SS and average silhouette width, k=3..15 ===\n")
print(as.data.frame(sweep_summary))

best_idx <- which.max(sweep_summary$avg_silhouette)
best_k <- sweep_summary$k[best_idx]
logger$log("Defensive archetypes: k selected by max avg silhouette width = ", best_k,
           " (avg_sil = ", round(sweep_summary$avg_silhouette[best_idx], 4), ")")

# FORCE_K overrides the silhouette-optimal pick with a specific k from
# the same sweep (no re-fit needed, same seed/nstart already run above)
# - set to NULL to defer to best_k. k=3 (the silhouette-optimal value)
# collapsed almost entirely to a size/position proxy (bigs vs wings vs
# guards, sorted by rebound/block volume) rather than the steal-type/
# block-type/foul-timing distinctions this feature set was built to
# capture. k=9 was reviewed against k=3 and confirmed basketball-
# legible with no merge candidates or small-n artifacts (see
# conversation history) - locked in as the final value.
FORCE_K <- 9
use_idx <- if (is.null(FORCE_K)) best_idx else which(sweep_summary$k == FORCE_K)
used_k <- sweep_summary$k[use_idx]
logger$log("Defensive archetypes: using k = ", used_k,
           if (!is.null(FORCE_K)) " (forced override)" else " (silhouette-optimal)",
           " (avg_sil = ", round(sweep_summary$avg_silhouette[use_idx], 4), ")")

fit <- sweep_results[[use_idx]]$fit
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

cat("\n=== Defense-only cluster centroid profiles (original units), ", TARGET_SEASON, " ===\n")
print(as.data.frame(centroid_profile))

# ------------------------------------------------------------
# Cluster labels - assigned after reviewing real centroids and sample
# rosters at k=9 (see conversation history). k was chosen over the
# silhouette-optimal k=3 because k=3 collapsed almost entirely to a
# size/position proxy (bigs vs wings vs guards, sorted by rebound/
# block volume) rather than the steal-type/block-type/foul-timing
# distinctions this feature set was specifically built to capture; at
# k=9 every cluster reads as a distinct, legible defensive identity
# with no merge candidates or small-n artifacts. Position words
# deliberately excluded, same convention as the other two systems.
# Cluster NUMBERING isn't guaranteed stable across a re-cluster on
# different data - sanity-check centroids/rosters against these
# labels before trusting them if this is ever re-run after a large
# data change (new season added, min-minutes floor changed, etc).
# ------------------------------------------------------------
CLUSTER_LABELS <- c(
  `1` = "Point-of-Attack Disruptor",  # Thybulle, Caruso, Herbert Jones, Kris Dunn - by far the highest steal rate of any cluster
  `2` = "Physical Glass-Crasher",     # Duren, Poeltl, Isaiah Stewart - elite offensive-rebound share, but the most fouls/36 and 2nd-highest early-foul-rate of any cluster
  `3` = "Disciplined Rim Anchor",     # Jokić, Sengun, Mobley, Embiid, Gobert - highest rebounding + real rim protection, LOWEST early-foul-rate among bigs (7.5%)
  `4` = "Foul-Trouble Forward",       # Dillon Brooks, Filipowski, Risacher - highest early-foul-rate of any cluster (22.6%)
  `5` = "Peripheral Guard",           # Maxey, Garland, DeRozan, Pritchard - lowest block rate, boards grabbed almost entirely 10+ feet from the rim
  `6` = "Minimal-Glass Ball-Handler", # Brunson, Reaves, Booker, Mitchell, SGA - primary shot-creators, lowest offensive-rebound share of any cluster
  `7` = "Do-It-All Forward",          # Dončić, Jalen Johnson, Banchero, Jaylen Brown - real volume across rebounding/steals/blocks without extreme specialization
  `8` = "Versatile Wing Defender",    # Amen Thompson, Edgecombe, Cunningham, Camara - high steal rate AND high offensive-rebound share, does a bit of everything
  `9` = "Low-Risk Wing Defender"      # Durant, Trey Murphy III, Harden, Edwards - lowest fouls/36 and lowest early-foul-rate of all, plays conservative defense
)
profiles$archetype_label <- dplyr::coalesce(
  CLUSTER_LABELS[as.character(profiles$archetype_cluster)],
  paste0("Archetype ", profiles$archetype_cluster)
)

# ------------------------------------------------------------
# 4. Sample rosters: 5 highest-minutes players per cluster (min_per_game
#    is a neutral "prominent this season" signal here, not itself a
#    clustering input), for a basketball sanity check.
# ------------------------------------------------------------
cat("\n=== 5 highest-minutes player-seasons per cluster ===\n")
sample_rows <- profiles %>%
  dplyr::group_by(archetype_cluster) %>%
  dplyr::slice_max(min_per_game, n = 5, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(archetype_cluster, dplyr::desc(min_per_game)) %>%
  dplyr::select(archetype_cluster, player_name, min_per_game, reb_per36, oreb_share, contested_reb_share,
                reb_0_3_share, stl_per36, lost_ball_steal_share, blk_per36, paint_block_share,
                pf_per36, early_foul_rate)
print(as.data.frame(sample_rows))

# ------------------------------------------------------------
# 5. Write output.
# ------------------------------------------------------------
out <- profiles %>%
  dplyr::select(player_id, player_name, season, games_played, min_per_game,
                 archetype_cluster, archetype_label, dplyr::all_of(shape_cols))

write_parquet(out, cfg$path_player_defensive_archetypes)
logger$log("Defensive archetypes: ", nrow(out), " player-seasons written to ", cfg$path_player_defensive_archetypes)
