# ============================================================
# Script: 03_build_player_game_logs.R
# Purpose: Build player-by-game logs using box score v3 endpoint
#          Incremental + stop-on-failure
# Output:  data_raw/player_game_logs1.rds
# ============================================================

library(dplyr)
library(purrr)
library(janitor)
library(hoopR)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
existing_path <- "data_raw/player_game_logs1.rds"

# ------------------------------------------------------------
# Load schedule
# ------------------------------------------------------------
schedule <- readRDS("data_raw/schedule.rds")

today_date <- Sys.Date()

games_all <- schedule %>%
  filter(game_date < today_date) %>%
  distinct(game_id_nba, game_date, season)

message("Total games in schedule before today: ", nrow(games_all))

# ------------------------------------------------------------
# Load existing logs (incremental behavior)
# ------------------------------------------------------------
if (file.exists(existing_path)) {
  existing_logs <- readRDS(existing_path)
  existing_game_ids <- unique(existing_logs$game_id_nba)
  message("Loaded existing logs: ", nrow(existing_logs), " rows")
} else {
  existing_logs <- tibble()
  existing_game_ids <- character(0)
  message("No existing logs found, starting fresh")
}

# ------------------------------------------------------------
# Determine which games still need to be pulled
# ------------------------------------------------------------
games_to_pull <- games_all %>%
  filter(!(game_id_nba %in% existing_game_ids)) %>%
  arrange(game_date)

message("Games remaining to pull: ", nrow(games_to_pull))

# ------------------------------------------------------------
# Safe pull function for v3 endpoint
# ------------------------------------------------------------
pull_boxscore_safe <- function(gid) {
  message("Pulling box score (v3) for game: ", gid)
  
  out <- try(nba_boxscoretraditionalv3(game_id = gid), silent = TRUE)
  
  # Failure handling
  if (inherits(out, "try-error") ||
      is.null(out$home_team_player_traditional) ||
      is.null(out$away_team_player_traditional)) {
    
    message("  -> FAILED. Stopping ETL run.")
    return(NULL)
  }
  
  home <- out$home_team_player_traditional %>%
    clean_names() %>%
    mutate(
      team_id = as.character(team_id),
      person_id = as.character(person_id),
      game_id_nba = gid
    )
  
  away <- out$away_team_player_traditional %>%
    clean_names() %>%
    mutate(
      team_id = as.character(team_id),
      person_id = as.character(person_id),
      game_id_nba = gid
    )
  
  bind_rows(home, away)
}

# ------------------------------------------------------------
# Loop through games and stop on first failure
# ------------------------------------------------------------
for (gid in games_to_pull$game_id_nba) {
  
  ps <- pull_boxscore_safe(gid)
  
  if (is.null(ps)) {
    message("  -> FAILED. Skipping game ", gid)
    next   # instead of break
  }
  
  existing_logs <- bind_rows(existing_logs, ps)
  saveRDS(existing_logs, existing_path)
  
  message("  -> Saved ", nrow(ps), " rows for game ", gid)
}

message("ETL run complete.")