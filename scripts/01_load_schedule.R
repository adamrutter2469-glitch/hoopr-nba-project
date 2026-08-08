# ============================================================
# Script: 01_build_schedule.R
# Purpose: Incrementally build clean NBA schedule using NBA IDs
# Output:  data_raw/schedule.rds
# ============================================================

library(dplyr)
library(purrr)
library(janitor)
library(lubridate)
library(hoopR)

# ------------------------------------------------------------
# Load core functions
# ------------------------------------------------------------
purrr::walk(list.files("R", full.names = TRUE), source)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
schedule_path <- "data_raw/schedule.rds"

# ------------------------------------------------------------
# 1. Load existing schedule (if present)
# ------------------------------------------------------------
if (file.exists(schedule_path)) {
  
  existing <- readRDS(schedule_path)
  
  if (!validate_existing_table(existing, required_cols = c("season", "game_id_nba"))) {
    message("Existing schedule invalid — ignoring it.")
    existing <- NULL
    existing_seasons <- integer(0)
  } else {
    existing_seasons <- sort(unique(existing$season))
    message("Existing seasons found: ", paste(existing_seasons, collapse = ", "))
  }
  
} else {
  message("No existing schedule found.")
  existing <- NULL
  existing_seasons <- integer(0)
}

# ------------------------------------------------------------
# 2. Determine seasons to pull
# ------------------------------------------------------------
target_seasons <- 2022:2025

seasons_to_pull <- setdiff(target_seasons, existing_seasons)

message("Seasons to pull: ", paste(seasons_to_pull, collapse = ", "))

# ------------------------------------------------------------
# 3. Pull schedule for missing seasons
# ------------------------------------------------------------
new_schedule <- purrr::map_dfr(seasons_to_pull, function(season) {
  message("Pulling schedule for season ", season)
  pull_nba_schedule(season)   # <-- UPDATED: use nba_schedule() wrapper
})

# ------------------------------------------------------------
# 4. Combine with existing schedule
# ------------------------------------------------------------
combined <- bind_rows(existing, new_schedule)

# ------------------------------------------------------------
# 5. Clean + dedupe (temporary cleanup step)
# ------------------------------------------------------------
schedule_raw <- combined %>%
  mutate(
    game_date = as.Date(game_date),
    season = as.integer(season)
  ) %>%
  distinct(game_id_nba, .keep_all = TRUE)   # REMOVE AFTER FIRST CLEAN RUN

# ------------------------------------------------------------
# 6. Save output
# ------------------------------------------------------------
saveRDS(schedule_raw, schedule_path)

message("Schedule updated and saved to: ", schedule_path)