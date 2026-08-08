# ============================================================
# Script: 04_build_schedule_with_travel.R
# Purpose: Build game-level fact table (rest + travel)
# Output:  data_processed/schedule_with_travel_detail.rds
# ============================================================

library(dplyr)
library(purrr)
library(janitor)
library(readr)
library(tidyr)
library(lubridate)

# ------------------------------------------------------------
# Load core functions (including travel + rest + stats functions)
# ------------------------------------------------------------
purrr::walk(list.files("R", full.names = TRUE), source)

# ------------------------------------------------------------
# Load inputs
# ------------------------------------------------------------
schedule <- readRDS("data_raw/schedule.rds")

travel_path <- "C:/Users/12623/OneDrive/Documents/NBA Stats/NBA Travel Times/NBA_Airport_Flight_Matrix_Estimated.csv"

travel_df <- read_csv(travel_path) %>%
  clean_names()

# ------------------------------------------------------------
# Prepare schedule for game-level builder
# ------------------------------------------------------------
schedule_clean <- schedule %>%
  mutate(
    game_id = game_id_nba,        # NBA ID is now the canonical key
    game_date = as.Date(game_date)
  ) %>%
  select(
    game_id_nba,
    season,
    game_date,
    home_team_id,
    away_team_id,
    home_team_tricode,
    away_team_tricode,
    everything()
  )

# ------------------------------------------------------------
# Build game-level table (rest + travel + stats)
# ------------------------------------------------------------
processed_game_level <- build_game_level_table(
  schedule_df = schedule_clean,
  travel_df = travel_df
)

# ------------------------------------------------------------
# Save output
# ------------------------------------------------------------
saveRDS(processed_game_level, "data_processed/schedule_with_travel_detail.rds")

message("Game-level table created and saved.")