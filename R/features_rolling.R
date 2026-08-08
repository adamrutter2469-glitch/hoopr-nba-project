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
ROLLING_STAT_CANDIDATES <- c("pts", "reb", "ast", "stl", "blk", "tov",
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
