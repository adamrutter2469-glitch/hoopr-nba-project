# ============================================================
# Script: R/pull_reference.R
# Purpose: Pull reference data (player universe, team universe)
#          from the NBA Stats API. Cheap single calls - always
#          fully refreshed and overwritten, no incremental logic
#          needed here.
# ============================================================

pull_players_raw <- function() {
  common <- hoopR::nba_commonallplayers()$CommonAllPlayers %>% janitor::clean_names()
  index  <- hoopR::nba_playerindex()$PlayerIndex %>% janitor::clean_names()
  dplyr::left_join(common, index, by = "person_id")
}

pull_teams_raw <- function() {
  hoopR::nba_teams() %>%
    janitor::clean_names() %>%
    dplyr::rename_with(~ paste0("hr_", .x))
}

# Extract unique team IDs from teams_raw
extract_team_ids <- function(teams_df) {
  teams_df$hr_team_id %>% unique()
}

# ------------------------------------------------------------
# Orchestration entry point: pull both, save both, log outcome.
# Returns the two tables invisibly (players/teams may be NULL
# if their pull failed - callers should check before relying on
# fresh data and can fall back to what's already on disk).
# ------------------------------------------------------------
refresh_reference_data <- function(cfg, logger) {
  logger$log("Reference data: pulling players + teams (full refresh)...")

  players <- try(pull_players_raw(), silent = TRUE)
  if (inherits(players, "try-error")) {
    logger$log("  FAILED to pull players: ", conditionMessage(attr(players, "condition")))
    players <- NULL
  } else {
    saveRDS(players, file.path(cfg$path_data_raw, "players_raw.rds"))
    logger$log("  players_raw.rds written (", nrow(players), " rows)")
  }

  teams <- try(pull_teams_raw(), silent = TRUE)
  if (inherits(teams, "try-error")) {
    logger$log("  FAILED to pull teams: ", conditionMessage(attr(teams, "condition")))
    teams <- NULL
  } else {
    saveRDS(teams, file.path(cfg$path_data_raw, "teams_raw.rds"))
    logger$log("  teams_raw.rds written (", nrow(teams), " rows)")
  }

  invisible(list(players = players, teams = teams))
}
