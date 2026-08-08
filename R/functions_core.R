# ============================================================
# Core Function Library for NBA ETL
# Store this file in: R/functions_core.R
# ============================================================

library(dplyr)
library(janitor)
library(hoopR)
library(purrr)

# ------------------------------------------------------------
# SEASON UTILITIES
# ------------------------------------------------------------
# Pull schedule for one season
# ------------------------------------------------------------
# Pull NBA schedule for one season (NBA Stats API only)
# ------------------------------------------------------------
pull_nba_schedule <- function(season) {
  
  df <- hoopR::nba_schedule(season = season) %>%
    janitor::clean_names() %>%
    
    # Remove preseason games using BOTH fields
    dplyr::filter(
      !grepl("pre", game_label, ignore.case = TRUE),
      !grepl("pre", series_text, ignore.case = TRUE)
    ) %>%
    
    # Keep only the columns you want
    dplyr::select(
      season,
      game_date,
      game_id,
      home_team_id,
      away_team_id,
      home_team_tricode,
      away_team_tricode
    ) %>%
    
    # Rename game_id → game_id_nba
    dplyr::rename(game_id_nba = game_id)
  
  df
}

# Convert numeric year (2022) → "2022-23"
normalize_season <- function(x) {
  x_chr <- as.character(x)
  if (suppressWarnings(all(!is.na(as.numeric(x_chr))))) {
    yr <- as.integer(x_chr)
    paste0(yr, "-", substr(yr + 1, 3, 4))
  } else {
    x_chr
  }
}

# Extract the starting year from "YYYY-YY"
season_start_year <- function(season_string) {
  as.integer(substr(season_string, 1, 4))
}

# Validate an existing RDS file before using it
validate_existing_table <- function(df, required_cols = NULL) {
  if (!is.data.frame(df)) return(FALSE)
  if (nrow(df) == 0) return(FALSE)
  if (!is.null(required_cols) && !all(required_cols %in% names(df))) return(FALSE)
  TRUE
}

# ------------------------------------------------------------
# PLAYER UTILITIES
# ------------------------------------------------------------

# Determine which players were active in ANY season in the schedule window
filter_players_active_in_window <- function(players_df, schedule_years) {
  players_df %>%
    mutate(
      from_year = as.integer(from_year.x),
      to_year   = as.integer(to_year.x)
    ) %>%
    filter(to_year >= min(schedule_years)) %>%
    select(person_id, from_year, to_year)
}

# Determine which players were active in a specific season
players_active_in_season <- function(players_df, season_year) {
  players_df %>%
    filter(from_year <= season_year, to_year >= season_year)
}

# ------------------------------------------------------------
# TEAM UTILITIES
# ------------------------------------------------------------

# Extract unique team IDs from teams_raw
extract_team_ids <- function(teams_df) {
  teams_df$hr_team_id %>% unique()
}

# ------------------------------------------------------------
# API PULL FUNCTIONS
# ------------------------------------------------------------

# Pull team game logs for one team in one season
pull_team_logs <- function(season, team_id) {
  df_raw <- nba_teamgamelog(
    season = season,
    team_id = team_id
  )
  
  df_raw$TeamGameLog %>%
    clean_names() %>%
    mutate(
      season = season,
      team_id = team_id
    )
}

# Pull player game logs for one player in one season
pull_player_logs <- function(season, player_id) {
  
  safe_call <- tryCatch(
    nba_playergamelog(
      season = season,
      player_id = player_id
    ),
    error = function(e) {
      message("API ERROR for player ", player_id,
              " in season ", season, ": ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(safe_call)) {
    message("No data returned for player ", player_id,
            " in season ", season, " (API failure)")
    return(NULL)
  }
  
  df <- safe_call$PlayerGameLog   # <-- your environment uses this
  
  if (is.null(df) || nrow(df) == 0) {
    message("NO GAME LOGS for player ", player_id,
            " in season ", season, " (likely did not play)")
    return(NULL)
  }
  
  df %>%
    clean_names() %>%
    mutate(
      season = season,
      player_id = player_id
    )
}
# ------------------------------------------------------------
# INCREMENTAL REFRESH UTILITIES
# ------------------------------------------------------------

# Determine which seasons need to be pulled
compute_seasons_to_pull <- function(all_seasons, existing_seasons, refresh_current = TRUE) {
  if (refresh_current) {
    current_season <- max(all_seasons)
    union(
      setdiff(all_seasons, existing_seasons),
      current_season
    ) %>% sort()
  } else {
    setdiff(all_seasons, existing_seasons) %>% sort()
  }
}

# Combine new + existing logs and dedupe
combine_and_dedupe <- function(existing_logs, new_logs) {
  bind_rows(existing_logs, new_logs) %>%
    distinct()
}

# ------------------------------------------------------------
# Compute rest days for each team across the schedule
# ------------------------------------------------------------
compute_team_rest_days <- function(schedule_df) {
  
  schedule_df %>%
    arrange(team_id, game_date) %>%
    group_by(team_id) %>%
    mutate(
      prev_game_date = lag(game_date),
      
      # True gap between games
      day_gap = as.numeric(difftime(game_date, prev_game_date, units = "days")),
      
      # New definition: rest_days = gap - 1
      rest_days = day_gap - 1,
      
      # Back-to-back: rest_days == 0
      is_b2b = rest_days == 0,
      
      # 3-in-4: two consecutive games with rest_days <= 1
      is_3in4 = rest_days <= 1 & lag(rest_days, default = Inf) <= 1
    ) %>%
    select(-day_gap) %>%
    ungroup()
}

# ------------------------------------------------------------
# Build team-by-game table using NBA IDs + tricodes
# ------------------------------------------------------------
build_team_game_locations <- function(schedule_df) {
  
  home_df <- schedule_df %>%
    transmute(
      game_id,
      game_date,
      season,
      team_id = home_team_id,
      team_abbr = home_team_tricode,
      opponent_id = away_team_id,
      opponent_abbr = away_team_tricode,
      location = "home"
    )
  
  away_df <- schedule_df %>%
    transmute(
      game_id,
      game_date,
      season,
      team_id = away_team_id,
      team_abbr = away_team_tricode,
      opponent_id = home_team_id,
      opponent_abbr = home_team_tricode,
      location = "away"
    )
  
  bind_rows(home_df, away_df)
}

# ------------------------------------------------------------
# Compute previous game location → determines where team traveled FROM
# ------------------------------------------------------------
add_previous_game_info <- function(team_games) {
  
  team_games %>%
    arrange(team_id, game_date) %>%
    group_by(team_id) %>%
    mutate(
      prev_location = lag(location),
      prev_opponent = lag(opponent_abbr),
      
      prev_city_team = case_when(
        prev_location == "home" ~ team_abbr,
        prev_location == "away" ~ prev_opponent,
        TRUE ~ NA_character_
      ),
      
      curr_city_team = case_when(
        location == "home" ~ team_abbr,
        location == "away" ~ opponent_abbr,
        TRUE ~ NA_character_
      )
    ) %>%
    ungroup()
}

# ------------------------------------------------------------
# Compute travel time using previous city → current city
# ------------------------------------------------------------
compute_travel <- function(team_games, travel_df) {
  
  travel_lookup <- travel_df %>%
    distinct(team_abbr, opponent_team_abbr, est_flight_time, timezone_shift)
  
  team_games %>%
    add_previous_game_info() %>%
    left_join(
      travel_lookup,
      by = c(
        "prev_city_team" = "team_abbr",
        "curr_city_team" = "opponent_team_abbr"
      )
    ) %>%
    mutate(
      est_flight_time = if_else(is.na(est_flight_time), 0, est_flight_time),
      timezone_shift  = if_else(is.na(timezone_shift), 0, timezone_shift)
    )
}

# ------------------------------------------------------------
# Build final game-level table (rest + travel + stats-ready)
# ------------------------------------------------------------
build_game_level_table <- function(schedule_df, travel_df) {
  
  # ----------------------------------------------------------
  # 1. Build team-by-game rows (home + away)
  # ----------------------------------------------------------
  team_games <- build_team_game_locations(schedule_df)
  
  # ----------------------------------------------------------
  # 2. Compute rest features
  # ----------------------------------------------------------
  team_games <- compute_team_rest_days(team_games)
  
  # ----------------------------------------------------------
  # 3. Compute travel features
  # ----------------------------------------------------------
  team_games <- compute_travel(team_games, travel_df)
  
  # ----------------------------------------------------------
  # 4. Collapse team rows into game-level home/away columns
  # ----------------------------------------------------------
  game_level <- team_games %>%
    select(
      game_id,
      location,                 # "home" or "away"
      rest_days,
      is_b2b,
      is_3in4,
      est_flight_time,
      timezone_shift
    ) %>%
    group_by(game_id, location) %>%
    summarise(
      dplyr::across(
        c(rest_days, is_b2b, is_3in4, est_flight_time, timezone_shift),
        ~ .x[!is.na(.x)][1],
        .names = "{.col}"
      ),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = location,
      values_from = c(
        rest_days,
        is_b2b,
        is_3in4,
        est_flight_time,
        timezone_shift
      ),
      names_glue = "{location}_{.value}"
    )
  
  # ----------------------------------------------------------
  # 5. Join back to schedule (schedule is the spine)
  # ----------------------------------------------------------
  schedule_df %>%
    left_join(game_level, by = "game_id")
}


# ------------------------------------------------------------
# Takes schedule with rest info and expands into team level data (2 rows per team)
# ------------------------------------------------------------
expand_game_table_per_team <- function(schedule_df) {
  
  # Home team rows
  home_df <- schedule_df %>%
    transmute(
      game_id_nba,
      game_date,
      season,
      team_id      = home_team_id,
      opponent_id  = away_team_id,
      home_away    = "home",
      rest_days    = home_rest_days,
      opp_rest_days = away_rest_days,
      is_b2b       = home_is_b2b,
      opp_is_b2b = away_is_b2b,
      is_3in4      = home_is_3in4,
      opp_is_3in4 = away_is_3in4,
      est_flight_time = home_est_flight_time,
      opp_est_flight_time = away_est_flight_time,
      time_zone_shift = home_timezone_shift,
      opp_time_zone_shift = away_timezone_shift
      # add other home_* fields here if needed
    )
  
  # Away team rows
  away_df <- schedule_df %>%
    transmute(
      game_id_nba,
      game_date,
      season,
      team_id      = away_team_id,
      opponent_id  = home_team_id,
      home_away    = "away",
      rest_days    = away_rest_days,
      opp_rest_days = home_rest_days,
      is_b2b       = away_is_b2b,
      opp_is_b2b = home_is_b2b,
      is_3in4      = away_is_3in4,
      opp_is_3in4 = home_is_3in4,
      est_flight_time = away_est_flight_time,
      opp_est_flight_time = home_est_flight_time,
      time_zone_shift = away_timezone_shift,
      opp_time_zone_shift = home_timezone_shift
      # add other away_* fields here if needed
    )
  
  # Combine into a tidy team-game table
  bind_rows(home_df, away_df) %>%
    arrange(game_date, game_id_nba, home_away)
}