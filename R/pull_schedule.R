# ============================================================
# Script: R/pull_schedule.R
# Purpose: Incrementally pull the season schedule (NBA Stats API
#          only, regular season games). Historical seasons are
#          pulled once and kept; the current season is always
#          re-pulled since NBA schedules shift (postponements,
#          in-season tournament reseeding, etc).
# ============================================================

pull_nba_schedule <- function(season) {
  hoopR::nba_schedule(season = season) %>%
    janitor::clean_names() %>%

    # Remove preseason games using BOTH fields
    dplyr::filter(
      !grepl("pre", game_label, ignore.case = TRUE),
      !grepl("pre", series_text, ignore.case = TRUE)
    ) %>%

    dplyr::select(
      game_date,
      game_id,
      home_team_id,
      away_team_id,
      home_team_tricode,
      away_team_tricode,
      game_status,
      game_status_text
    ) %>%
    dplyr::rename(game_id_nba = game_id) %>%
    dplyr::mutate(
      # game_status: 1 = scheduled (not yet started), 2 = live/in
      # progress, 3 = final. This is what lets R/pull_game_logs.R
      # cross-check that a game is actually done before treating its
      # stats as real, and what lets you query "tonight's games"
      # (game_status == 1, game_date == today) for reference before
      # they've been played.
      is_final = game_status == 3,
      # Use the requested season string, not the API's own "season" field -
      # that field's type/format is inconsistent across calls (observed a
      # live integer-vs-character clash across seasons), which broke
      # bind_rows() when combining multiple seasons' pulls.
      season = season,
      .before = 1
    )
}

# ------------------------------------------------------------
# Orchestration entry point. cfg$first_season (config/config.R)
# controls how far back a first-ever run will backfill.
#
# Season-partitioned: each season to pull is deduped and written
# to its own partition, leaving every other season's file on disk
# untouched (see write_season_partition() in R/utils_io.R).
# ------------------------------------------------------------
refresh_schedule <- function(cfg, logger) {
  path <- cfg$path_schedule_dataset

  all_seasons      <- season_sequence(cfg$first_season)
  existing_seasons <- dataset_seasons_present(path)
  seasons_to_pull  <- compute_seasons_to_pull(all_seasons, existing_seasons, refresh_current = TRUE)

  if (length(seasons_to_pull) == 0) {
    logger$log("Schedule: nothing to pull, all seasons up to date.")
    return(invisible(NULL))
  }

  logger$log("Schedule: pulling ", length(seasons_to_pull), " season(s): ",
             paste(seasons_to_pull, collapse = ", "))

  for (s in seasons_to_pull) {
    logger$log("  pulling schedule for ", s, "...")
    new_rows <- try(pull_nba_schedule(s), silent = TRUE)
    if (inherits(new_rows, "try-error")) {
      logger$log("    FAILED: ", conditionMessage(attr(new_rows, "condition")))
      next
    }

    existing <- read_season_partition(path, s, required_cols = c("season", "game_id_nba"))
    combined <- combine_and_dedupe(existing, new_rows, dedupe_cols = "game_id_nba") %>%
      dplyr::mutate(game_date = as.Date(game_date), season = as.character(season))

    write_season_partition(combined, path)
    n_new <- nrow(combined) - (if (is.null(existing)) 0L else nrow(existing))
    logger$log("    season ", s, ": ", nrow(combined), " rows (", n_new, " new)")
  }

  invisible(NULL)
}
