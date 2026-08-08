# ============================================================
# Script: R/features_rest_travel.R
# Purpose: Rest-day and travel features (back-to-backs, 3-in-4,
#          estimated flight time, timezone shift). Logic carried
#          over unchanged from the original functions_core.R -
#          only the travel CSV's location changed, from a
#          machine-specific absolute path outside the repo to
#          data_raw/external/ (see config/config.R
#          path_travel_csv), so the pipeline is portable.
#
#          Always a full recompute over the whole schedule - it's
#          all local/in-memory work (~10k rows, sub-second), so
#          there's no incremental-logic payoff to chasing here.
# ============================================================

# ------------------------------------------------------------
# Compute rest days for each team across the schedule
# ------------------------------------------------------------
compute_team_rest_days <- function(schedule_df) {
  schedule_df %>%
    dplyr::arrange(team_id, game_date) %>%
    dplyr::group_by(team_id) %>%
    dplyr::mutate(
      prev_game_date = dplyr::lag(game_date),

      # True gap between games
      day_gap = as.numeric(difftime(game_date, prev_game_date, units = "days")),

      # rest_days = gap - 1 (0 rest days means the very next day)
      rest_days = day_gap - 1,

      # Back-to-back: rest_days == 0
      is_b2b = rest_days == 0,

      # 3-in-4: two consecutive games with rest_days <= 1
      is_3in4 = rest_days <= 1 & dplyr::lag(rest_days, default = Inf) <= 1
    ) %>%
    dplyr::select(-day_gap) %>%
    dplyr::ungroup()
}

# ------------------------------------------------------------
# Build team-by-game table using NBA IDs + tricodes
# ------------------------------------------------------------
build_team_game_locations <- function(schedule_df) {
  home_df <- schedule_df %>%
    dplyr::transmute(
      game_id, game_date, season,
      team_id = home_team_id, team_abbr = home_team_tricode,
      opponent_id = away_team_id, opponent_abbr = away_team_tricode,
      location = "home"
    )

  away_df <- schedule_df %>%
    dplyr::transmute(
      game_id, game_date, season,
      team_id = away_team_id, team_abbr = away_team_tricode,
      opponent_id = home_team_id, opponent_abbr = home_team_tricode,
      location = "away"
    )

  dplyr::bind_rows(home_df, away_df)
}

# ------------------------------------------------------------
# Compute previous game location -> determines where team
# traveled FROM
# ------------------------------------------------------------
add_previous_game_info <- function(team_games) {
  team_games %>%
    dplyr::arrange(team_id, game_date) %>%
    dplyr::group_by(team_id) %>%
    dplyr::mutate(
      prev_location = dplyr::lag(location),
      prev_opponent  = dplyr::lag(opponent_abbr),

      prev_city_team = dplyr::case_when(
        prev_location == "home" ~ team_abbr,
        prev_location == "away" ~ prev_opponent,
        TRUE ~ NA_character_
      ),

      curr_city_team = dplyr::case_when(
        location == "home" ~ team_abbr,
        location == "away" ~ opponent_abbr,
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::ungroup()
}

# ------------------------------------------------------------
# Compute travel time using previous city -> current city
# ------------------------------------------------------------
compute_travel <- function(team_games, travel_df) {
  travel_lookup <- travel_df %>%
    dplyr::distinct(team_abbr, opponent_team_abbr, est_flight_time, timezone_shift)

  team_games %>%
    add_previous_game_info() %>%
    dplyr::left_join(
      travel_lookup,
      by = c("prev_city_team" = "team_abbr", "curr_city_team" = "opponent_team_abbr")
    ) %>%
    dplyr::mutate(
      est_flight_time = dplyr::if_else(is.na(est_flight_time), 0, est_flight_time),
      timezone_shift  = dplyr::if_else(is.na(timezone_shift), 0, timezone_shift)
    )
}

# ------------------------------------------------------------
# Build final game-level table (rest + travel, one row per game
# with home_*/away_* columns)
# ------------------------------------------------------------
build_game_level_table <- function(schedule_df, travel_df) {
  team_games <- build_team_game_locations(schedule_df)
  team_games <- compute_team_rest_days(team_games)
  team_games <- compute_travel(team_games, travel_df)

  game_level <- team_games %>%
    dplyr::select(game_id, location, rest_days, is_b2b, is_3in4, est_flight_time, timezone_shift) %>%
    dplyr::group_by(game_id, location) %>%
    dplyr::summarise(
      dplyr::across(
        c(rest_days, is_b2b, is_3in4, est_flight_time, timezone_shift),
        ~ .x[!is.na(.x)][1],
        .names = "{.col}"
      ),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = location,
      values_from = c(rest_days, is_b2b, is_3in4, est_flight_time, timezone_shift),
      names_glue = "{location}_{.value}"
    )

  schedule_df %>% dplyr::left_join(game_level, by = "game_id")
}

# ------------------------------------------------------------
# Expand game-level table (rest info) into team-level (2 rows
# per game, one per team, with opp_* columns for context)
# ------------------------------------------------------------
expand_game_table_per_team <- function(schedule_df) {
  home_df <- schedule_df %>%
    dplyr::transmute(
      game_id_nba, game_date, season,
      team_id = home_team_id, opponent_id = away_team_id, home_away = "home",
      rest_days = home_rest_days, opp_rest_days = away_rest_days,
      is_b2b = home_is_b2b, opp_is_b2b = away_is_b2b,
      is_3in4 = home_is_3in4, opp_is_3in4 = away_is_3in4,
      est_flight_time = home_est_flight_time, opp_est_flight_time = away_est_flight_time,
      time_zone_shift = home_timezone_shift, opp_time_zone_shift = away_timezone_shift
    )

  away_df <- schedule_df %>%
    dplyr::transmute(
      game_id_nba, game_date, season,
      team_id = away_team_id, opponent_id = home_team_id, home_away = "away",
      rest_days = away_rest_days, opp_rest_days = home_rest_days,
      is_b2b = away_is_b2b, opp_is_b2b = home_is_b2b,
      is_3in4 = away_is_3in4, opp_is_3in4 = home_is_3in4,
      est_flight_time = away_est_flight_time, opp_est_flight_time = home_est_flight_time,
      time_zone_shift = away_timezone_shift, opp_time_zone_shift = home_timezone_shift
    )

  dplyr::bind_rows(home_df, away_df) %>%
    dplyr::arrange(game_date, game_id_nba, home_away)
}

# ------------------------------------------------------------
# Orchestration entry point: schedule.rds -> schedule_with_
# travel_detail.rds -> schedule_team_level_final.rds
# ------------------------------------------------------------
refresh_rest_travel_features <- function(cfg, logger) {
  schedule_path <- file.path(cfg$path_data_raw, "schedule.rds")
  if (!file.exists(schedule_path)) {
    logger$log("Rest/travel features: SKIPPED, no schedule.rds found yet.")
    return(invisible(NULL))
  }
  if (!file.exists(cfg$path_travel_csv)) {
    logger$log("Rest/travel features: SKIPPED, travel CSV not found at ", cfg$path_travel_csv)
    return(invisible(NULL))
  }

  logger$log("Rest/travel features: recomputing from full schedule...")

  schedule <- readRDS(schedule_path) %>%
    dplyr::mutate(game_id = game_id_nba, game_date = as.Date(game_date)) %>%
    dplyr::select(season, game_date, game_id_nba, game_id, dplyr::everything())

  travel_df <- readr::read_csv(cfg$path_travel_csv, show_col_types = FALSE) %>%
    janitor::clean_names()

  with_travel <- build_game_level_table(schedule_df = schedule, travel_df = travel_df)
  saveRDS(with_travel, file.path(cfg$path_data_processed, "schedule_with_travel_detail.rds"))
  logger$log("  schedule_with_travel_detail.rds written (", nrow(with_travel), " rows)")

  team_level <- expand_game_table_per_team(with_travel)
  saveRDS(team_level, file.path(cfg$path_data_processed, "schedule_team_level_final.rds"))
  logger$log("  schedule_team_level_final.rds written (", nrow(team_level), " rows)")

  team_level
}
