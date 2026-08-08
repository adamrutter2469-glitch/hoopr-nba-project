# ============================================================
# Script: 00_build_raw_players.R
# Purpose: Pull and save raw NBA player metadata
# Output:  data_raw/players_raw.rds
# ============================================================

library(dplyr)
library(janitor)
library(hoopR)

message("Pulling NBA player metadata...")

# nba_commonallplayers() gives the canonical player list
players_core <- nba_commonallplayers()$CommonAllPlayers %>%
  clean_names()

# nba_playerindex() gives supplemental fields (height, weight, position)
players_index <- nba_playerindex()$PlayerIndex %>%
  clean_names()

# Merge on person_id
players_raw <- players_core %>%
  left_join(players_index, by = c("person_id" = "person_id"))

# Save
output_path <- "data_raw/players_raw.rds"
saveRDS(players_raw, output_path)

message("Player table saved to: ", output_path)
message("Done.")