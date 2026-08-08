# ============================================================
# Script: 02_build_raw_teams.R
# Purpose: Pull and save raw NBA team metadata from hoopR
# Output:  data_raw/teams_raw.rds
# ============================================================

library(dplyr)
library(janitor)
library(hoopR)

message("Pulling NBA team metadata from hoopR...")

# nba_teams() already returns a flat tibble
teams_raw <- nba_teams() %>%
  clean_names() %>%
  rename_with(~ paste0("hr_", .x))

# Save
output_path <- "data_raw/teams_raw.rds"
saveRDS(teams_raw, output_path)

message("Team table saved to: ", output_path)
message("Done.")