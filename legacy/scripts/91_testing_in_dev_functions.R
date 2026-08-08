# ============================================================
# Script: 91_testing_in_dev_functions
# Purpose: Test functions in process. 
# Output:  NA
# ============================================================
library(dplyr)
library(purrr)
library(janitor)
library(readr)
library(tidyr)
library(lubridate)
library(hoopR)

#returns start end dates for the last N games for a given team, relative to a given game
get_last_n_game_dates <- function(schedule, team_id_input, game_id_input, n_games) {
  
  # Get season of the target game
  target_season <- schedule %>%
    filter(game_id_nba == game_id_input, team_id == team_id_input) %>%
    pull(season)
  
  if (length(target_season) == 0) return(c("Not enough games"))
  
  # Filter to team + season BEFORE computing index
  team_games <- schedule %>%
    filter(team_id == team_id_input, season == target_season) %>%
    arrange(game_date)
  
  # Find index of the target game
  idx <- which(team_games$game_id_nba == game_id_input)
  
  # Not enough prior games
  if (length(idx) == 0 || idx <= n_games) return(c("Not enough games"))
  
  # Extract the date range for the last N games
  start_date <- min(team_games$game_date[(idx - n_games):(idx - 1)])
  end_date   <- max(team_games$game_date[(idx - n_games):(idx - 1)])
  
  return(c(start_date, end_date))
}

## EX:
game_id_ex = "0042200234"
team_id_ex = "1610612744"
schedule_ex = schedule_with_travel_per_team
n_games_ex = 3
#get_last_n_game_dates(schedule_ex, team_id_ex, game_id_ex, n_games_ex)