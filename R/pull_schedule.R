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
      away_team_tricode
    ) %>%
    dplyr::rename(game_id_nba = game_id) %>%
    # Use the requested season string, not the API's own "season" field -
    # that field's type/format is inconsistent across calls (observed a
    # live integer-vs-character clash across seasons), which broke
    # bind_rows() when combining multiple seasons' pulls.
    dplyr::mutate(season = season, .before = 1)
}

# ------------------------------------------------------------
# Orchestration entry point. cfg$first_season (config/config.R)
# controls how far back a first-ever run will backfill.
# ------------------------------------------------------------
refresh_schedule <- function(cfg, logger) {
  path <- file.path(cfg$path_data_raw, "schedule.rds")
  existing <- read_existing_rds(path, required_cols = c("season", "game_id_nba"))

  all_seasons     <- season_sequence(cfg$first_season)
  existing_seasons <- if (is.null(existing)) character(0) else unique(existing$season)
  seasons_to_pull  <- compute_seasons_to_pull(all_seasons, existing_seasons, refresh_current = TRUE)

  if (length(seasons_to_pull) == 0) {
    logger$log("Schedule: nothing to pull, all seasons up to date.")
    return(existing)
  }

  logger$log("Schedule: pulling ", length(seasons_to_pull), " season(s): ",
             paste(seasons_to_pull, collapse = ", "))

  new_schedule <- purrr::map_dfr(seasons_to_pull, function(s) {
    logger$log("  pulling schedule for ", s, "...")
    out <- try(pull_nba_schedule(s), silent = TRUE)
    if (inherits(out, "try-error")) {
      logger$log("    FAILED: ", conditionMessage(attr(out, "condition")))
      return(NULL)
    }
    out
  })

  combined <- combine_and_dedupe(existing, new_schedule, dedupe_cols = "game_id_nba") %>%
    dplyr::mutate(
      game_date = as.Date(game_date),
      season    = as.character(season)
    )

  saveRDS(combined, path)
  logger$log("  schedule.rds written (", nrow(combined), " total rows)")
  combined
}
