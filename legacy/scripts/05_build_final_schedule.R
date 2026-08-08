# ============================================================
# Script: 05_build_final_schedule.R
# Purpose: Expand schedule to team level with rest/travel information
# Output:  data_processed/schedule_with_travel_details.rds
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
# Load schedule_with_travel_details
# ------------------------------------------------------------
schedule_with_travel <- readRDS("data_processed/schedule_with_travel_detail.rds")

# ------------------------------------------------------------
# apply pivot function to schedule
# ------------------------------------------------------------

schedule_with_travel_per_team = expand_game_table_per_team(schedule_with_travel)

# ------------------------------------------------------------
# Save output
# ------------------------------------------------------------
saveRDS(schedule_with_travel_per_team, "data_processed/schedule_team_level_final.rds")

message("Schedule with travel details at team LOD created and saved.")