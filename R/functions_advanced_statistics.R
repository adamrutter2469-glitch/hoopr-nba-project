# ============================================================
# Script: functions_rolling_statistics
# Purpose: Suite of functions specifically for computing rolling team/player performance
# Output:  Functions to be used in processed data layer for rolling stats
# ============================================================


## Inputs: schedule_team_level_final dataframe, a team_id, a game_id and N (variable)
## Returns: First date and last date of last N games prior to the game provided. 
## if less than N games prior in current seasons, returns c("Not enough games")

get_date_range_last_n_games <- function(schedule, team_id_input, game_id_input, n_games) {
  
  # Identify the season of the target game
  target_season <- schedule %>%
    filter(game_id_nba == game_id_input,
           team_id == team_id_input) %>%
    pull(season)
  
  if (length(target_season) == 0) {
    return(c("Not enough games"))
  }
  
  # Filter to this team + this season only
  team_games <- schedule %>%
    filter(team_id == team_id_input,
           season == target_season) %>%
    arrange(game_date)
  
  # Locate the target game within the filtered schedule
  idx <- which(team_games$game_id_nba == game_id_input)
  
  if (length(idx) == 0 || idx <= n_games) {
    return(c("Not enough games"))
  }
  
  # Extract the date range of the previous N games
  date_from <- min(team_games$game_date[(idx - n_games):(idx - 1)])
  date_to   <- max(team_games$game_date[(idx - n_games):(idx - 1)])
  
  c(date_from, date_to)
}

  
### Pull simple stats like min, pts, reb, 3pm, etc.  
rolling_simple_player_stat <- function(schedule,
                                       player_id,
                                       team_id,
                                       game_id,
                                       n_games,
                                       stat_col) {
  
  # get date range based on TEAM schedule
  dr <- get_date_range_last_n_games(
    schedule = schedule,
    team_id_input = team_id,
    game_id_input = game_id,
    n_games = n_games
  )
  
  if (length(dr) == 1 && dr == "Not enough games") {
    return(NA_real_)
  }
  
  date_from <- dr[1]
  date_to   <- dr[2]
  
  # call the simple player dashboard
  dash <- nba_playerdashboardbygamesplits(
    player_id = player_id,
    date_from = date_from,
    date_to = date_to
  )
  
  # extract the simple stats table
  tbl <- dash$OverallPlayerDashboard
  
  # return the stat
  tbl[[stat_col]]
}

# ============================================================
# Helper: safely extract a value from a table
# ============================================================
safe_pull <- function(tbl, filter_col, filter_val, pull_col) {
  if (is.null(tbl) || nrow(tbl) == 0) return(NA_real_)
  row <- tbl %>% dplyr::filter(.data[[filter_col]] == filter_val)
  if (nrow(row) == 0) return(NA_real_)
  as.numeric(row[[pull_col]])
}

# ============================================================
# Main function: pull advanced rebounding metrics for one team-game
# ============================================================
get_team_reb_metrics <- function(team_id, game_id) {
  
  # API call
  dash <- try(hoopR::nba_teamdashptreb(team_id = team_id, game_id = game_id), silent = TRUE)
  
  # If API fails, return NA row
  if (inherits(dash, "try-error")) {
    return(tibble::tibble(
      team_id, game_id,
      OREB = NA, DREB = NA, REB = NA,
      C_OREB = NA, C_DREB = NA, C_REB = NA,
      REB_2PT_MISS = NA, REB_3PT_MISS = NA,
      REB_0_3 = NA, REB_3_6 = NA, REB_6_10 = NA, REB_10_PLUS = NA
    ))
  }
  
  # Extract sub-tables
  overall_tbl       <- dash$OverallRebounding
  numcontested_tbl  <- dash$NumContestedRebounding
  shot_type_tbl     <- dash$ShotTypeRebounding
  reb_dist_tbl      <- dash$RebDistanceRebounding
  
  # Pull from OverallRebounding (single row)
  OREB   <- if (!is.null(overall_tbl) && nrow(overall_tbl) > 0) as.numeric(overall_tbl$OREB) else NA
  DREB   <- if (!is.null(overall_tbl) && nrow(overall_tbl) > 0) as.numeric(overall_tbl$DREB) else NA
  REB    <- if (!is.null(overall_tbl) && nrow(overall_tbl) > 0) as.numeric(overall_tbl$REB) else NA
  C_OREB <- if (!is.null(overall_tbl) && nrow(overall_tbl) > 0) as.numeric(overall_tbl$C_OREB) else NA
  C_DREB <- if (!is.null(overall_tbl) && nrow(overall_tbl) > 0) as.numeric(overall_tbl$C_DREB) else NA
  C_REB  <- if (!is.null(overall_tbl) && nrow(overall_tbl) > 0) as.numeric(overall_tbl$C_REB) else NA
  
  tibble::tibble(
    team_id = team_id,
    game_id = game_id,
    
    # From OverallRebounding
    OREB = OREB,
    DREB = DREB,
    REB  = REB,
    C_OREB = C_OREB,
    C_DREB = C_DREB,
    C_REB  = C_REB,
    
    # From ShotTypeRebounding
    REB_2PT_MISS = safe_pull(shot_type_tbl, "SHOT_TYPE_RANGE", "Miss 2FG", "REB"),
    REB_3PT_MISS = safe_pull(shot_type_tbl, "SHOT_TYPE_RANGE", "Miss 3FG", "REB"),
    
    # From RebDistanceRebounding
    REB_0_3     = safe_pull(reb_dist_tbl, "REB_DIST_RANGE", "0-3 Feet", "REB"),
    REB_3_6     = safe_pull(reb_dist_tbl, "REB_DIST_RANGE", "3-6 Feet", "REB"),
    REB_6_10    = safe_pull(reb_dist_tbl, "REB_DIST_RANGE", "6-10 Feet", "REB"),
    REB_10_PLUS = safe_pull(reb_dist_tbl, "REB_DIST_RANGE", "10+ Feet", "REB")
  )
}