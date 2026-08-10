# ============================================================
# Script: R/features_playbyplay_events.R
# Purpose: Derive player-game-grain event-detail stats by mining
#          play_by_play's type_text + athlete roles. Starting with
#          turnovers (see R/pull_playbyplay.R for how the athlete_id
#          role conventions were discovered - they're type-specific,
#          not a uniform rule, e.g. blocked shots have athlete_1 =
#          shooter, not the blocker).
#
#          LONG-FORMAT approach: each relevant play is expanded into
#          one row per involved player (not one row per play), with
#          the event recorded as a (player, role) pair rather than
#          separate athlete_1/athlete_2 columns. This means the final
#          player-game aggregate is a single group_by + count - no
#          repeated "aggregate athlete_1, aggregate athlete_2,
#          rename-and-join" dance needed per stat, and it generalizes
#          cleanly to future event types with 3 actors instead of 2
#          (some technical fouls involve 3 players).
# ============================================================

# A few turnover type_texts get merged into one column (same
# underlying decision-error, ESPN just also flags where the ball
# ended up - verified empirically that the "Out of Bounds -" variants
# never have a steal credited, unlike their regular counterparts, so
# merging loses no information on the steal side). "No Turnover" is
# ESPN's label for a turnover with no further subtype detail - verified
# against real rows: genuine turnovers with a named player, not
# negated/overturned calls - renamed here so the output column
# doesn't read backwards.
TOV_TYPE_MERGE <- c(
  "Out of Bounds - Bad Pass Turnover" = "Bad Pass Turnover",
  "Out of Bounds - Lost Ball Turnover" = "Lost Ball Turnover",
  "No Turnover" = "Unspecified Turnover"
)

# type_text -> a clean, column-name-safe key. Mechanical 1:1 for
# everything not in TOV_TYPE_MERGE above - deliberately not hand-
# curating every one of the ~26 turnover subtypes beyond that one
# merge decision, so the derived columns stay a faithful, complete
# reflection of what ESPN actually distinguishes (and so the
# validation against player_game_logs$tov in refresh_turnover_
# features() can be an exact match, not an approximation from
# dropped/bucketed rare types).
clean_event_key <- function(type_text) {
  type_text <- gsub("\n", " ", type_text)
  type_text <- dplyr::coalesce(TOV_TYPE_MERGE[type_text], type_text)
  type_text <- sub(" Turnover$", "", type_text)
  type_text <- tolower(type_text)
  type_text <- gsub("[^a-z0-9]+", "_", type_text)
  gsub("^_+|_+$", "", type_text)
}

# ------------------------------------------------------------
# Long-format intermediate table -> wide player-game aggregate.
# Only Bad Pass / Lost Ball ever produce a "stolen" row (the only 2
# turnover types where ESPN ever populates athlete_id_2 - verified
# empirically: every other turnover type is 0%), so tov_bad_pass and
# tov_lost_ball are the only types that naturally get a matching
# stl_* column - no special-casing needed, it falls out of the data.
# ------------------------------------------------------------
build_turnover_features <- function(cfg, logger) {
  pbp <- read_full_dataset(cfg$path_playbyplay_dataset)
  if (is.null(pbp)) {
    logger$log("Turnover event features: SKIPPED, no play_by_play data yet.")
    return(invisible(NULL))
  }
  espn_map <- read_parquet_or_null(cfg$path_espn_player_id_mapping)
  if (is.null(espn_map)) {
    logger$log("Turnover event features: SKIPPED, no espn_player_id_mapping.parquet yet.")
    return(invisible(NULL))
  }

  tov <- pbp %>%
    dplyr::filter(grepl("Turnover", type_text)) %>%
    dplyr::mutate(event_key = clean_event_key(type_text))

  committed <- tov %>%
    dplyr::filter(!is.na(athlete_id_1)) %>%
    dplyr::transmute(athlete_id_espn = as.character(athlete_id_1),
                      game_id_nba, season, event_key, role = "committed")

  stolen <- tov %>%
    dplyr::filter(!is.na(athlete_id_2)) %>%
    dplyr::transmute(athlete_id_espn = as.character(athlete_id_2),
                      game_id_nba, season, event_key, role = "stolen")

  long <- dplyr::bind_rows(committed, stolen) %>%
    dplyr::inner_join(espn_map %>% dplyr::select(athlete_id_espn, player_id), by = "athlete_id_espn")

  n_dropped <- (nrow(committed) + nrow(stolen)) - nrow(long)
  if (n_dropped > 0) {
    logger$log("  ", n_dropped, " turnover-event row(s) dropped (athlete not in espn_player_id_mapping).")
  }

  long %>%
    dplyr::mutate(col = paste0(ifelse(role == "committed", "tov_", "stl_"), event_key)) %>%
    dplyr::count(player_id, game_id_nba, season, col) %>%
    tidyr::pivot_wider(names_from = col, values_from = n, values_fill = 0)
}

# ------------------------------------------------------------
# Orchestration entry point. Joins the wide aggregate onto the FULL
# player_game_logs grain (every player-game, not just ones with a
# matched turnover event) so a player with zero bad-pass turnovers
# that game gets an explicit 0, not a missing row - matching the
# box-score convention that "0 turnovers" is real information.
#
# Also validates: the sum of every tov_* column should exactly equal
# that player-game's own trusted `tov` value from player_game_logs -
# logged every run as an ongoing correctness check, not just a one-
# time verification.
# ------------------------------------------------------------
refresh_turnover_features <- function(cfg, logger) {
  wide <- build_turnover_features(cfg, logger)
  if (is.null(wide)) return(invisible(NULL))

  logs <- read_full_dataset(cfg$path_player_logs_dataset) %>%
    dplyr::select(player_id, game_id_nba, season, tov)

  event_cols <- setdiff(names(wide), c("player_id", "game_id_nba", "season"))

  full <- logs %>%
    dplyr::left_join(wide, by = c("player_id", "game_id_nba", "season")) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(event_cols), ~ tidyr::replace_na(.x, 0)))

  tov_cols <- grep("^tov_", event_cols, value = TRUE)
  full <- full %>% dplyr::mutate(tov_derived_total = rowSums(dplyr::across(dplyr::all_of(tov_cols))))
  n_mismatch <- sum(full$tov_derived_total != full$tov)
  match_rate <- round(100 * (nrow(full) - n_mismatch) / nrow(full), 2)
  logger$log("  validation: ", match_rate, "% of player-games have tov_* columns summing exactly to player_game_logs$tov (",
             n_mismatch, "/", nrow(full), " mismatched).")
  full <- full %>% dplyr::select(-tov_derived_total)

  write_parquet(full, cfg$path_player_turnover_features)
  logger$log("  ", cfg$path_player_turnover_features, " written (", nrow(full), " player-games, ",
             length(event_cols), " event columns).")
  full
}

# ============================================================
# Blocks - structurally different from turnovers: there's no
# dedicated type_text per block (a blocked shot just carries its
# normal shot type, e.g. "Layup Shot"), and no type_id distinction
# between a blocked and unblocked miss of the same shot type either.
# Detection is a STRUCTURAL rule instead of a type_text filter -
# verified empirically (see conversation/commit history): a missed
# shot with athlete_id_2 populated can only be a block (assists only
# ever attach to made shots), and this catches a few malformed-text
# rows that a literal "blocks" text search would miss.
#
# Subdivided by shot category (dunk/layup/hook/tip/jump_shot, via
# clean_shot_category() - verified against every real blocked-shot
# type_text before building) rather than raw 50-way shot type, since
# there's no box-score reconciliation reason to keep it 1:1 the way
# turnovers did, and the bucketed categories are a more useful
# rim-protection-vs-perimeter distinction anyway.
#
# athlete_1 = the shooter (shot blocked), athlete_2 = the blocker.
# ============================================================
clean_shot_category <- function(type_text) {
  type_text <- gsub("\n", " ", type_text)
  dplyr::case_when(
    grepl("Dunk", type_text) ~ "dunk",
    grepl("Layup", type_text) ~ "layup",
    grepl("Hook", type_text) ~ "hook",
    grepl("^Tip Shot$", type_text) ~ "tip",
    TRUE ~ "jump_shot"
  )
}

build_block_features <- function(cfg, logger) {
  pbp <- read_full_dataset(cfg$path_playbyplay_dataset)
  if (is.null(pbp)) {
    logger$log("Block event features: SKIPPED, no play_by_play data yet.")
    return(invisible(NULL))
  }
  espn_map <- read_parquet_or_null(cfg$path_espn_player_id_mapping)
  if (is.null(espn_map)) {
    logger$log("Block event features: SKIPPED, no espn_player_id_mapping.parquet yet.")
    return(invisible(NULL))
  }

  blocked <- pbp %>%
    dplyr::filter(shooting_play, !scoring_play, !is.na(athlete_id_2)) %>%
    dplyr::mutate(shot_category = clean_shot_category(type_text))

  fg_blocked <- blocked %>%
    dplyr::filter(!is.na(athlete_id_1)) %>%
    dplyr::transmute(athlete_id_espn = as.character(athlete_id_1),
                      game_id_nba, season, event_key = shot_category, role = "blocked")

  blocking <- blocked %>%
    dplyr::transmute(athlete_id_espn = as.character(athlete_id_2),
                      game_id_nba, season, event_key = shot_category, role = "blocking")

  long <- dplyr::bind_rows(fg_blocked, blocking) %>%
    dplyr::inner_join(espn_map %>% dplyr::select(athlete_id_espn, player_id), by = "athlete_id_espn")

  n_dropped <- (nrow(fg_blocked) + nrow(blocking)) - nrow(long)
  if (n_dropped > 0) {
    logger$log("  ", n_dropped, " block-event row(s) dropped (athlete not in espn_player_id_mapping).")
  }

  long %>%
    dplyr::mutate(col = paste0(ifelse(role == "blocked", "fg_blocked_", "blk_"), event_key)) %>%
    dplyr::count(player_id, game_id_nba, season, col) %>%
    tidyr::pivot_wider(names_from = col, values_from = n, values_fill = 0)
}

# ------------------------------------------------------------
# Same join-onto-full-grain + validation pattern as turnovers.
# blk_* is validated against player_game_logs$blk (a real box-score
# stat). fg_blocked_* has no official box-score equivalent to
# validate against - it's a genuinely new stat this data enables
# (how often a player's OWN shot gets blocked), not something the
# NBA box score tracks at all.
# ------------------------------------------------------------
refresh_block_features <- function(cfg, logger) {
  wide <- build_block_features(cfg, logger)
  if (is.null(wide)) return(invisible(NULL))

  logs <- read_full_dataset(cfg$path_player_logs_dataset) %>%
    dplyr::select(player_id, game_id_nba, season, blk)

  event_cols <- setdiff(names(wide), c("player_id", "game_id_nba", "season"))

  full <- logs %>%
    dplyr::left_join(wide, by = c("player_id", "game_id_nba", "season")) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(event_cols), ~ tidyr::replace_na(.x, 0)))

  blk_cols <- grep("^blk_", event_cols, value = TRUE)
  full <- full %>% dplyr::mutate(blk_derived_total = rowSums(dplyr::across(dplyr::all_of(blk_cols))))
  n_mismatch <- sum(full$blk_derived_total != full$blk)
  match_rate <- round(100 * (nrow(full) - n_mismatch) / nrow(full), 2)
  logger$log("  validation: ", match_rate, "% of player-games have blk_* columns summing exactly to player_game_logs$blk (",
             n_mismatch, "/", nrow(full), " mismatched).")
  full <- full %>% dplyr::select(-blk_derived_total)

  write_parquet(full, cfg$path_player_block_features)
  logger$log("  ", cfg$path_player_block_features, " written (", nrow(full), " player-games, ",
             length(event_cols), " event columns).")
  full
}
