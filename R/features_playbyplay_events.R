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

# ============================================================
# Fouls - three views built from one shared long-format base table:
# subtype breakdown (mirrors turnovers), WHEN in the game they
# happened (period-bucketed counts + specific foul-number timing),
# since "does this player get into foul trouble early" is a timing
# question the box score's end-of-game total can never answer.
#
# PF-eligible types (which type_texts count toward the real 6-foul
# disqualification limit) chosen empirically, not from rulebook
# memory alone - tested 3 candidate sets against player_game_logs.pf
# directly: base foul types alone (95.41% match) vs base + Flagrant
# Fouls (96.05% - the winner) vs also including Double Technical
# Foul (95.51%, worse) - confirms flagrants count toward personal
# fouls but technicals (even "double" ones) don't, matching the real
# rulebook, verified rather than assumed.
#
# "Offensive Foul" vs "Offensive Foul Turnover" - verified these are
# the SAME real event logged as two separate PBP rows (100% of
# Offensive Foul rows have a matching Offensive Foul Turnover row at
# the identical game+player+instant). Using "Offensive Foul" here and
# leaving "Offensive Foul Turnover" where it already lives (the
# turnover table) - counting both here would double-count every
# charge.
#
# athlete_2 is credited too when present (Double Personal Foul is the
# only PF-eligible type where this happens in practice, ~93% of the
# time - verified empirically, so no type-specific branching needed,
# same structural-rule approach as blocks).
# ============================================================
PF_ELIGIBLE_TYPES <- c(
  "Shooting Foul", "Personal Foul", "Loose Ball Foul", "Offensive Foul",
  "Personal Take Foul", "Transition Take Foul", "Away from Play Foul",
  "Clear Path Foul", "Double Personal Foul",
  "Flagrant Foul Type 1", "Flagrant Foul Type 2"
)

clean_foul_key <- function(type_text) {
  type_text <- gsub("\n", " ", type_text)
  type_text <- sub(" Foul$", "", type_text)
  type_text <- tolower(type_text)
  type_text <- gsub("[^a-z0-9]+", "_", type_text)
  gsub("^_+|_+$", "", type_text)
}

# start_game_seconds_remaining counts down continuously across ALL of
# regulation (2880 at tip-off -> 0 at end of Q4 - verified empirically:
# period 1 alone ranges 2880 down to 2160, i.e. it does NOT reset each
# quarter), then resets to be PERIOD-relative once OT starts (each OT
# period independently ranges 300 -> 0). So "seconds elapsed since
# tipoff" is a straight subtraction in regulation, and only needs the
# period-offset adjustment for OT.
compute_elapsed_seconds <- function(period_number, start_game_seconds_remaining) {
  ifelse(period_number <= 4,
         2880 - start_game_seconds_remaining,
         2880 + (period_number - 5) * 300 + (300 - start_game_seconds_remaining))
}

# ------------------------------------------------------------
# Per player-game: elapsed game-clock seconds at the moment a player
# FIRST checked into the game - the anchor needed to turn a game-
# clock-relative stat (like elapsed_seconds_at_pf_2 below) into a
# playing-time-relative one, e.g. "did their 2nd foul come within
# their first 10 minutes ON THE FLOOR" instead of "within the game's
# first 10 minutes" (which unfairly flatters/penalizes bench players
# who simply weren't out there yet - see conversation history).
#
# Uses "Substitution" events (text is always "X enters the game for
# Y" - athlete_id_1 = entering, athlete_id_2 = leaving). Rule, applied
# per player per game, using their EARLIEST substitution appearance
# (as either role):
#   - earliest appearance is an ENTRY  -> checkin = that event's time
#     (a bench player checking in mid-game)
#   - earliest appearance is an EXIT   -> checkin = 0
#     (they were already on the floor when subbed out, i.e. a starter)
#   - no substitution appearance at all -> checkin = 0
#     (played the whole game with zero substitutions either way - an
#     iron-man starter; also covers true DNPs, harmlessly, since a
#     player who never played has no foul data downstream anyway)
# The last case is handled by the caller defaulting a left-join miss
# to 0, not inside this function - this function only returns rows
# for players with at least one substitution event.
# ------------------------------------------------------------
build_player_game_checkin_time <- function(cfg, logger) {
  pbp <- read_full_dataset(cfg$path_playbyplay_dataset)
  if (is.null(pbp)) {
    logger$log("Player check-in time: SKIPPED, no play_by_play data yet.")
    return(invisible(NULL))
  }
  espn_map <- read_parquet_or_null(cfg$path_espn_player_id_mapping)
  if (is.null(espn_map)) {
    logger$log("Player check-in time: SKIPPED, no espn_player_id_mapping.parquet yet.")
    return(invisible(NULL))
  }

  subs <- pbp %>%
    dplyr::filter(gsub("\n", " ", type_text) == "Substitution") %>%
    dplyr::mutate(elapsed_seconds = compute_elapsed_seconds(period_number, start_game_seconds_remaining))

  enters <- subs %>% dplyr::filter(!is.na(athlete_id_1)) %>%
    dplyr::transmute(athlete_id_espn = as.character(athlete_id_1), game_id_nba, elapsed_seconds, role = "enter")
  exits <- subs %>% dplyr::filter(!is.na(athlete_id_2)) %>%
    dplyr::transmute(athlete_id_espn = as.character(athlete_id_2), game_id_nba, elapsed_seconds, role = "exit")

  first_appearance <- dplyr::bind_rows(enters, exits) %>%
    dplyr::inner_join(espn_map %>% dplyr::select(athlete_id_espn, player_id), by = "athlete_id_espn") %>%
    dplyr::group_by(player_id, game_id_nba) %>%
    dplyr::slice_min(elapsed_seconds, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  first_appearance %>%
    dplyr::transmute(player_id, game_id_nba,
                      checkin_elapsed_seconds = dplyr::if_else(role == "enter", elapsed_seconds, 0))
}

# ------------------------------------------------------------
# Shared long-format base: one row per player per PF-eligible foul
# event, with the subtype key, period bucket, and elapsed-seconds
# timestamp all attached - every downstream view (subtype counts,
# period counts, milestone timing) aggregates from this same table.
# ------------------------------------------------------------
build_foul_long_base <- function(cfg, logger) {
  pbp <- read_full_dataset(cfg$path_playbyplay_dataset)
  if (is.null(pbp)) {
    logger$log("Foul event features: SKIPPED, no play_by_play data yet.")
    return(invisible(NULL))
  }
  espn_map <- read_parquet_or_null(cfg$path_espn_player_id_mapping)
  if (is.null(espn_map)) {
    logger$log("Foul event features: SKIPPED, no espn_player_id_mapping.parquet yet.")
    return(invisible(NULL))
  }

  fouls <- pbp %>%
    dplyr::filter(gsub("\n", " ", type_text) %in% PF_ELIGIBLE_TYPES) %>%
    dplyr::mutate(
      event_key = clean_foul_key(type_text),
      period_bucket = ifelse(period_number <= 4, paste0("q", period_number), "ot"),
      elapsed_seconds = compute_elapsed_seconds(period_number, start_game_seconds_remaining)
    )

  committer1 <- fouls %>% dplyr::filter(!is.na(athlete_id_1)) %>%
    dplyr::transmute(athlete_id_espn = as.character(athlete_id_1), game_id_nba, season,
                      event_key, period_bucket, elapsed_seconds)
  committer2 <- fouls %>% dplyr::filter(!is.na(athlete_id_2)) %>%
    dplyr::transmute(athlete_id_espn = as.character(athlete_id_2), game_id_nba, season,
                      event_key, period_bucket, elapsed_seconds)

  long <- dplyr::bind_rows(committer1, committer2) %>%
    dplyr::inner_join(espn_map %>% dplyr::select(athlete_id_espn, player_id), by = "athlete_id_espn")

  n_dropped <- (nrow(committer1) + nrow(committer2)) - nrow(long)
  if (n_dropped > 0) {
    logger$log("  ", n_dropped, " foul-event row(s) dropped (athlete not in espn_player_id_mapping).")
  }
  long
}

# ------------------------------------------------------------
# Orchestration: subtype counts (pf_<type>) + period counts
# (pf_q1..pf_q4/pf_ot) + milestone timing (elapsed_seconds_at_pf_2/
# 3/6 - foul trouble threshold, foul trouble by half, foul-out) all
# joined onto the full player_game_logs grain. pf_<type> columns
# validated against player_game_logs$pf (the milestone/period columns
# have no independent box-score total to check against, since the
# box score doesn't track foul timing at all - that's the whole
# point of building this from play_by_play).
# ------------------------------------------------------------
refresh_foul_features <- function(cfg, logger) {
  long <- build_foul_long_base(cfg, logger)
  if (is.null(long)) return(invisible(NULL))

  subtype_wide <- long %>%
    dplyr::count(player_id, game_id_nba, season, event_key) %>%
    dplyr::mutate(col = paste0("pf_", event_key)) %>%
    dplyr::select(-event_key) %>%
    tidyr::pivot_wider(names_from = col, values_from = n, values_fill = 0)

  period_wide <- long %>%
    dplyr::count(player_id, game_id_nba, season, period_bucket) %>%
    dplyr::mutate(col = paste0("pf_", period_bucket)) %>%
    dplyr::select(-period_bucket) %>%
    tidyr::pivot_wider(names_from = col, values_from = n, values_fill = 0)

  milestones <- long %>%
    dplyr::group_by(player_id, game_id_nba, season) %>%
    dplyr::arrange(elapsed_seconds, .by_group = TRUE) %>%
    dplyr::mutate(foul_number = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(foul_number %in% c(2, 3, 6)) %>%
    dplyr::transmute(player_id, game_id_nba, season,
                      col = paste0("elapsed_seconds_at_pf_", foul_number), elapsed_seconds) %>%
    tidyr::pivot_wider(names_from = col, values_from = elapsed_seconds)

  logs <- read_full_dataset(cfg$path_player_logs_dataset) %>%
    dplyr::select(player_id, game_id_nba, season, pf)

  checkin <- build_player_game_checkin_time(cfg, logger)

  subtype_cols <- setdiff(names(subtype_wide), c("player_id", "game_id_nba", "season"))
  period_cols  <- setdiff(names(period_wide),  c("player_id", "game_id_nba", "season"))

  full <- logs %>%
    dplyr::left_join(subtype_wide, by = c("player_id", "game_id_nba", "season")) %>%
    dplyr::left_join(period_wide,  by = c("player_id", "game_id_nba", "season")) %>%
    dplyr::left_join(milestones,   by = c("player_id", "game_id_nba", "season")) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(c(subtype_cols, period_cols)), ~ tidyr::replace_na(.x, 0))) %>%
    dplyr::mutate(fouled_out = pf >= 6)
  # elapsed_seconds_at_pf_2/3/6 are left NA when that game's pf never
  # reached that count - a real "didn't happen", not missing data.

  # On-court-time-relative versions of the same milestones - checkin
  # time missing (player never appeared in a substitution event that
  # game) defaults to 0 (played the whole game with no subs at all -
  # see build_player_game_checkin_time() header comment). Left NA
  # whenever the game-clock version is already NA (foul count never
  # reached that game) - subtracting a valid checkin time from NA
  # stays NA automatically, no explicit guard needed.
  if (!is.null(checkin)) {
    full <- full %>%
      dplyr::left_join(checkin, by = c("player_id", "game_id_nba")) %>%
      dplyr::mutate(checkin_elapsed_seconds = tidyr::replace_na(checkin_elapsed_seconds, 0)) %>%
      dplyr::mutate(
        onct_elapsed_at_pf_2 = pmax(elapsed_seconds_at_pf_2 - checkin_elapsed_seconds, 0),
        onct_elapsed_at_pf_3 = pmax(elapsed_seconds_at_pf_3 - checkin_elapsed_seconds, 0),
        onct_elapsed_at_pf_6 = pmax(elapsed_seconds_at_pf_6 - checkin_elapsed_seconds, 0)
      )
  } else {
    logger$log("  Check-in time unavailable - onct_elapsed_at_pf_* columns not added.")
  }

  n_mismatch <- sum(rowSums(dplyr::select(full, dplyr::all_of(subtype_cols))) != full$pf)
  match_rate <- round(100 * (nrow(full) - n_mismatch) / nrow(full), 2)
  logger$log("  validation: ", match_rate, "% of player-games have pf_<type> columns summing exactly to player_game_logs$pf (",
             n_mismatch, "/", nrow(full), " mismatched).")

  write_parquet(full, cfg$path_player_foul_features)
  logger$log("  ", cfg$path_player_foul_features, " written (", nrow(full), " player-games, ",
             length(subtype_cols) + length(period_cols) + 3, " event/timing columns).")
  full
}
