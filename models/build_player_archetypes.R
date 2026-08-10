# ============================================================
# Script: models/build_player_archetypes.R
# Purpose: Classify each player-season into a text "archetype" (a
#          broad play-style category, e.g. "Stretch Rim Protector")
#          plus a "tier" (caliber within that archetype, "Tier 1"
#          best - "Tier 4" lowest), from box score / shooting-
#          efficiency stats.
#
#          Two-layer design, matching the actual product goal
#          ("generalize how a team performs against similar players"):
#            - ARCHETYPE = play-STYLE. Built by clustering (k-means) on
#              SHAPE features only - proportions/rates (shot profile,
#              rebound split, playmaking ratio, per-36 activity), never
#              raw production volume. Two players with very different
#              talent levels but the same role (e.g. a bench 3&D wing
#              and a starting 3&D wing) should land in the same cluster.
#            - TIER = caliber WITHIN that style. A separate composite
#              production/efficiency score, percentile-ranked ONLY
#              against other players already in the same archetype -
#              this is what keeps tier from just re-deriving "who
#              scores the most" league-wide.
#
#          Box-score/efficiency stats only for now (per the user's
#          explicit scoping) - advanced rebounding/shooting tracking
#          data can be folded in as additional clustering dimensions
#          later once the basic version is validated.
#
#          A separate, user-run analysis script - NOT wired into
#          run_pipeline.bat, same reasoning as train_rebounds_model.R:
#          re-clustering isn't an ETL step that belongs on every
#          incremental data pull. Re-run manually as the season
#          progresses (cfg$player_archetype_seed keeps cluster
#          numbering/composition reproducible run-to-run on unchanged
#          data - only genuinely new/changed player-seasons should
#          shift anything).
#
#          Run: Rscript models/build_player_archetypes.R
#          (from the project root, after scripts/00_install_packages.R)
#
#          IMPORTANT: archetype_label below is a PLACEHOLDER
#          ("Archetype 1", "Archetype 2", ...) until a human reviews
#          the printed centroid profiles and sample rosters and
#          assigns real basketball names - see CLUSTER_LABELS below.
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
# 1. Build the season-level shape profile (see R/features_player_archetypes.R)
# ------------------------------------------------------------
profiles <- build_player_season_profiles(cfg, logger)
logger$log("Player archetypes: ", nrow(profiles), " player-seasons cleared the ",
           cfg$player_archetype_min_games, "-game / ", cfg$player_archetype_min_minutes,
           "-min/game floor.")

shape_cols <- c("fg3a_rate", "fta_rate", "oreb_share", "ast_to_tov",
                "reb_per36", "ast_per36", "stl_per36", "blk_per36", "tov_per36", "pts_per36")

# ast_to_tov can be unbounded for a very low-TOV player - cap it so one
# outlier season doesn't dominate the standardized distance metric.
profiles <- profiles %>% dplyr::mutate(ast_to_tov = pmin(ast_to_tov, 10))

# ------------------------------------------------------------
# 2. Standardize shape features and fit k-means
# ------------------------------------------------------------
X <- profiles %>% dplyr::select(dplyr::all_of(shape_cols)) %>% as.matrix() %>% scale()

# Quick elbow diagnostic (k = 4..12) - printed for reference when
# deciding whether cfg$player_archetype_k is a reasonable choice, not
# used to auto-select k. The point is a small set of human-nameable
# categories, not a statistically "optimal" cluster count.
set.seed(cfg$player_archetype_seed)
elbow <- sapply(4:12, function(k) kmeans(X, centers = k, nstart = 10, iter.max = 50)$tot.withinss)
logger$log("Elbow diagnostic (total within-cluster SS by k):")
print(setNames(round(elbow, 1), paste0("k=", 4:12)))

set.seed(cfg$player_archetype_seed)
fit <- kmeans(X, centers = cfg$player_archetype_k, nstart = 25, iter.max = 50)
profiles$archetype_cluster <- fit$cluster

# ------------------------------------------------------------
# 3. Cluster centroid profiles in ORIGINAL units (for human labeling) -
#    NOT the standardized kmeans centers, which aren't directly
#    interpretable as basketball stats.
# ------------------------------------------------------------
centroid_profile <- profiles %>%
  dplyr::group_by(archetype_cluster) %>%
  dplyr::summarise(
    n_player_seasons = dplyr::n(),
    dplyr::across(dplyr::all_of(shape_cols), ~ round(mean(.x), 3)),
    .groups = "drop"
  ) %>%
  dplyr::arrange(archetype_cluster)

cat("\n=== Cluster centroid profiles (original units, mean per cluster) ===\n")
print(as.data.frame(centroid_profile))

# ------------------------------------------------------------
# 4. Archetype labels and per-archetype tier weights, reviewed against
#    real k=9 output (centroid profiles + sample rosters) with the
#    user. Both keyed by archetype_cluster - kmeans cluster NUMBERING
#    is not guaranteed stable across a re-cluster on different data
#    (same seed, but a changed input population can still converge to
#    a different arrangement), so if you re-run this after a big data
#    change, sanity-check the printed centroid profiles/sample rosters
#    against these labels/weights before trusting the output - a
#    silent renumbering would otherwise misapply one archetype's name
#    and tier weights to a different cluster.
# ------------------------------------------------------------
CLUSTER_LABELS <- c(
  `1` = "Paint Anchor",              # was "Traditional Post Big"
  `2` = "Scoring Initiator",         # was "Scoring Point Guard"
  `3` = "Primary Facilitator",       # was "Pass-First Guard"
  `4` = "Two-Way Defender",          # was "Two-Way Forward"
  `5` = "Stretch Rim Protector",
  `6` = "3&D Specialist",            # was "3&D / Floor-Spacing Wing"
  `7` = "Primary Offensive Engine",  # was "Elite Offensive Hub"
  `8` = "High-Usage Scorer",         # was "High-Usage Wing Scorer"
  `9` = "Balanced Role Player"       # was "Role Forward / Combo Wing-Big"
)
# Position words (Guard/Forward/Wing/Big/Center) deliberately removed from
# every label above - archetype is built from STYLE, not position, and a
# name like "Guard" reads as wrong the moment a forward (Dean Wade, Mikal
# Bridges) lands in that cluster on shape alone. "Rim Protector" is kept
# on #5 since it names a skill/role, not a position.
profiles$archetype_label <- dplyr::coalesce(
  CLUSTER_LABELS[as.character(profiles$archetype_cluster)],
  paste0("Archetype ", profiles$archetype_cluster)   # fallback if k ever changes without updating the table above
)

# ------------------------------------------------------------
# 5. Tier: a composite production/efficiency score, percentile-ranked
#    WITHIN each archetype only - but WHAT that composite rewards
#    differs by archetype, since "caliber" means something different
#    per role (rebounding/blocks for a rim protector vs scoring/
#    efficiency for an offensive hub). Mechanically: z-score each raw
#    component WITHIN the cluster first (so weights reflect actual
#    value judgments, not each stat's raw scale - blocks no longer
#    need an arbitrary "x2" just to be visible next to points), then
#    take a role-specific weighted sum of those z-scores.
#
#    Weights are a hand-set, transparent heuristic (not fit/optimized)
#    reflecting what actually defines quality within that role - not
#    universal, and easy to retune per-archetype as real output is
#    reviewed. tov is always negative (turnovers never help), its
#    magnitude reflecting how costly turnovers are to that role (worse
#    for a primary ballhandler than a low-usage rim protector).
# ------------------------------------------------------------
TIER_WEIGHTS <- list(
  `1` = c(pts = 1.8, efg = 1.0, ast = 0.3, reb = 2.5, stl = 0.5, blk = 2.0, tov = -0.5),  # Paint Anchor: rebounding/blocks first, scoring volume second (efg trimmed to make room)
  `2` = c(pts = 2.0, efg = 1.5, ast = 1.5, reb = 0.3, stl = 0.5, blk = 0.1, tov = -1.0),  # Scoring Initiator: scoring/efficiency, then assists
  `3` = c(pts = 1.0, efg = 1.0, ast = 2.5, reb = 0.2, stl = 1.0, blk = 0.1, tov = -1.5),  # Primary Facilitator: assists + ball security first
  `4` = c(pts = 1.0, efg = 1.0, ast = 0.5, reb = 1.5, stl = 2.0, blk = 1.5, tov = -0.5),  # Two-Way Defender: defensive activity + rebounding first
  `5` = c(pts = 1.0, efg = 1.5, ast = 0.2, reb = 2.0, stl = 0.5, blk = 2.5, tov = -0.3),  # Stretch Rim Protector: blocks/rebounding first
  `6` = c(pts = 1.0, efg = 2.5, ast = 0.3, reb = 0.5, stl = 1.5, blk = 1.0, tov = -0.5),  # 3&D Specialist: efficiency + steals first, low usage expected
  `7` = c(pts = 3.0, efg = 2.5, ast = 1.5, reb = 0.3, stl = 0.2, blk = 0.2, tov = -1.0),  # Primary Offensive Engine: scoring/efficiency first, then assists, defense/reb barely matter (per user direction)
  `8` = c(pts = 3.0, efg = 2.5, ast = 0.8, reb = 0.3, stl = 0.5, blk = 0.2, tov = -1.0),  # High-Usage Scorer: scoring/efficiency dominant, little else
  `9` = c(pts = 0.8, efg = 1.5, ast = 0.5, reb = 1.5, stl = 1.0, blk = 1.0, tov = -0.5)   # Balanced Role Player: rebounding/efficiency/defense over volume
)
DEFAULT_TIER_WEIGHTS <- c(pts = 1.5, efg = 1.5, ast = 1.0, reb = 1.0, stl = 1.0, blk = 1.0, tov = -1.0)  # fallback for any cluster number missing from the table above

tier_components <- c("pts_per36", "efg_pct", "ast_per36", "reb_per36", "stl_per36", "blk_per36", "tov_per36")

score_one_cluster <- function(df, cluster_id) {
  w <- TIER_WEIGHTS[[as.character(cluster_id)]]
  if (is.null(w)) w <- DEFAULT_TIER_WEIGHTS

  z <- df %>%
    dplyr::transmute(
      pts = as.numeric(scale(pts_per36)),
      efg = as.numeric(scale(efg_pct)),
      ast = as.numeric(scale(ast_per36)),
      reb = as.numeric(scale(reb_per36)),
      stl = as.numeric(scale(stl_per36)),
      blk = as.numeric(scale(blk_per36)),
      tov = as.numeric(scale(tov_per36))
    )

  df$tier_score <- as.numeric(as.matrix(z) %*% w[colnames(z)])
  df$tier_pct   <- dplyr::percent_rank(df$tier_score)
  # Numeric tiers, Tier 1 = best - "Reserve"/"Starter" read as roster-role
  # labels (confusing for e.g. a bench scorer who's actually elite by the
  # stats, or a starter who's merely average for their archetype); a
  # number just says "how good for this archetype" with no implication
  # about actual playing time or role.
  # Tier cutoffs: 10% / 30% / 30% / 30% (top-down) within each archetype.
  df$tier <- dplyr::case_when(
    df$tier_pct >= 0.90 ~ "Tier 1",   # top 10%
    df$tier_pct >= 0.60 ~ "Tier 2",   # next 30%
    df$tier_pct >= 0.30 ~ "Tier 3",   # next 30%
    TRUE                ~ "Tier 4"    # bottom 30%
  )
  df
}

profiles <- profiles %>%
  dplyr::group_by(archetype_cluster) %>%
  dplyr::group_map(~ score_one_cluster(.x, .y$archetype_cluster[1]), .keep = TRUE) %>%
  dplyr::bind_rows() %>%
  dplyr::select(-tier_pct)

# ------------------------------------------------------------
# 6. Sanity-check sample: the 3 highest-tier (by tier_score) player-
#    seasons in each cluster, most recent season first - a quick way
#    to eyeball whether a cluster's "best" players make basketball
#    sense together before committing to a name.
# ------------------------------------------------------------
cat("\n=== Top 3 (by tier_score) player-seasons per cluster ===\n")
sample_rows <- profiles %>%
  dplyr::group_by(archetype_cluster) %>%
  dplyr::slice_max(tier_score, n = 3, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(archetype_cluster, dplyr::desc(tier_score)) %>%
  dplyr::select(archetype_cluster, player_name, season, tier, pts_per36, reb_per36, ast_per36, stl_per36, blk_per36)
print(as.data.frame(sample_rows))

cat("\n=== Tier distribution ===\n")
print(table(profiles$archetype_cluster, profiles$tier))

# ------------------------------------------------------------
# 7. Write output
# ------------------------------------------------------------
out <- profiles %>%
  dplyr::select(
    player_id, player_name, season, games_played, min_per_game,
    archetype_cluster, archetype_label, tier, tier_score,
    dplyr::all_of(shape_cols), efg_pct, ft_pct
  )

write_parquet(out, cfg$path_player_archetypes)
logger$log("Player archetypes: ", nrow(out), " player-seasons written to ", cfg$path_player_archetypes)
