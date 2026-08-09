# ============================================================
# Script: R/features_rolling.R
# Purpose: Trailing N-game rolling averages, computed locally
#          from box scores already on disk.
#
#          This replaces the old approach in
#          functions_advanced_statistics.R (rolling_simple_player_stat/
#          get_date_range_last_n_games), which made a live API call
#          PER stat lookup - slow and fragile. Everything here is
#          in-memory dplyr work over data the pipeline already
#          pulled, so it's fast enough to fully recompute every run.
#
#          This is also what directly answers the open question in
#          notes/Ideas: "do teams perform below their rolling
#          average when rested less / traveled more?" - once these
#          columns exist alongside rest/travel features in
#          team_game_features.rds, that's a straightforward group_by
#          + comparison, no bespoke pipeline needed.
#
#          Windows are trailing and EXCLUDE the current game (a
#          team's rolling average going into game N is computed
#          from games N-window..N-1), matching the "prior form"
#          framing in notes/Ideas.
# ============================================================

# ------------------------------------------------------------
# Generic rolling-mean feature adder.
# group_cols: e.g. c("team_id", "season") - never rolls across
#             a season boundary.
# stat_cols:  numeric columns to compute rolling means for.
# windows:    trailing window sizes, e.g. c(5, 10, 20).
# ------------------------------------------------------------
add_rolling_stats <- function(df, group_cols, date_col, stat_cols, windows) {
  df <- df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(stat_cols), ~ suppressWarnings(as.numeric(.x)))) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(c(group_cols, date_col)))) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)))

  for (n in windows) {
    for (col in stat_cols) {
      new_col <- paste0(col, "_roll", n)
      df <- df %>%
        dplyr::mutate(
          "{new_col}" := slider::slide_dbl(
            dplyr::lag(.data[[col]]),
            mean, na.rm = TRUE,
            .before = n - 1, .complete = TRUE
          )
        )
    }
  }

  dplyr::ungroup(df)
}

# Stat columns present in both hoopR::nba_leaguegamelog outputs
# (team and player) - intersected against what's actually on disk
# so a schema hiccup upstream degrades gracefully instead of erroring.
ROLLING_STAT_CANDIDATES <- c("pts", "reb", "oreb", "dreb", "ast", "stl", "blk", "tov",
                              "plus_minus", "fg_pct", "fg3_pct", "ft_pct", "min")

compute_team_rolling_features <- function(team_logs, windows) {
  stat_cols <- intersect(ROLLING_STAT_CANDIDATES, names(team_logs))
  add_rolling_stats(
    team_logs,
    group_cols = c("team_id", "season"),
    date_col   = "game_date",
    stat_cols  = stat_cols,
    windows    = windows
  )
}

compute_player_rolling_features <- function(player_logs, windows) {
  stat_cols <- intersect(ROLLING_STAT_CANDIDATES, names(player_logs))
  add_rolling_stats(
    player_logs,
    group_cols = c("player_id", "season"),
    date_col   = "game_date",
    stat_cols  = stat_cols,
    windows    = windows
  )
}

# ============================================================
# Opponent/matchup features for rebounding prediction.
#
# Three building blocks, all reusing add_rolling_stats() above -
# no new rolling primitive, just new self-joins feeding it:
#   1. Rolling averages of the advanced rebounding splits (they
#      previously only existed as this-game raw actuals).
#   2. "Allowed" features: a team's own trailing history of what
#      OPPONENTS have put up against them (e.g. reb_allowed_roll10
#      = how many boards this team typically gives up).
#   3. Mirroring the opponent's own already-rolled columns onto a
#      team's row for that same game, so "the opponent's rolling
#      form" and "what the opponent typically allows" both sit
#      side by side with this team's own numbers.
#
# All inputs must already have team_id/opponent_id/game_id_nba as
# character (the caller's responsibility - see R/features_combine.R).
# ============================================================

ADV_REBOUNDING_STAT_COLS <- c(
  "adv_oreb", "adv_dreb", "adv_reb", "adv_c_oreb", "adv_c_dreb", "adv_c_reb",
  "adv_reb_2pt_miss", "adv_reb_3pt_miss",
  "adv_reb_0_3", "adv_reb_3_6", "adv_reb_6_10", "adv_reb_10_plus"
)

compute_adv_rebounding_rolling_features <- function(reb_with_dates, windows) {
  stat_cols <- intersect(ADV_REBOUNDING_STAT_COLS, names(reb_with_dates))
  add_rolling_stats(
    reb_with_dates,
    group_cols = c("team_id", "season"),
    date_col   = "game_date",
    stat_cols  = stat_cols,
    windows    = windows
  )
}

# raw_full: one row per team-game with team_id, opponent_id,
# game_id_nba, season, game_date, plus the raw stat_cols to mirror.
# Self-joins each row to the OPPONENT's same-game raw actuals (what
# they scored against this team that game), then rolls that per
# team - the raw same-game join result itself is discarded (using
# it directly would be leakage: it's this exact game's outcome),
# only the rolled, lag-safe *_allowed_rollN columns are kept.
compute_allowed_rolling_features <- function(raw_full, stat_cols, windows) {
  stat_cols <- intersect(stat_cols, names(raw_full))

  opponent_actuals <- raw_full %>%
    dplyr::select(game_id_nba, opp_team_id = team_id, dplyr::all_of(stat_cols)) %>%
    dplyr::rename_with(~ paste0(.x, "_allowed"), dplyr::all_of(stat_cols))

  with_allowed <- raw_full %>%
    dplyr::select(team_id, opponent_id, game_id_nba, season, game_date) %>%
    dplyr::left_join(opponent_actuals, by = c("game_id_nba", "opponent_id" = "opp_team_id"))

  allowed_cols <- paste0(stat_cols, "_allowed")
  add_rolling_stats(
    with_allowed,
    group_cols = c("team_id", "season"),
    date_col   = "game_date",
    stat_cols  = allowed_cols,
    windows    = windows
  ) %>%
    dplyr::select(team_id, game_id_nba, dplyr::matches("_allowed_roll\\d+$"))
}

# Mirrors any set of already-rolled columns from the opponent's own
# row for the same game_id_nba onto this team's row, prefixed
# "opp_". Since the source columns already exclude the current game
# via lag() inside add_rolling_stats(), mirroring them leaks
# nothing - opponent Y's opp_reb_roll10 on team X's row still only
# reflects Y's games strictly before this one.
attach_opponent_rolling_profile <- function(team_features, roll_cols) {
  roll_cols <- intersect(roll_cols, names(team_features))
  opp <- team_features %>%
    dplyr::select(game_id_nba, opp_team_id = team_id, dplyr::all_of(roll_cols)) %>%
    dplyr::rename_with(~ paste0("opp_", .x), dplyr::all_of(roll_cols))

  team_features %>%
    dplyr::left_join(opp, by = c("game_id_nba", "opponent_id" = "opp_team_id"))
}
