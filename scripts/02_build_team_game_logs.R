# ============================================================
# Script: 02_build_team_game_logs.R
# Purpose: Incrementally pull team-by-game stats for ALL teams
# Output:  data_raw/team_game_logs.rds
# ============================================================

library(dplyr)
library(purrr)
library(janitor)
library(hoopR)
library(stringr)
library(lubridate)

# ------------------------------------------------------------
# Load all functions from R/ folder
# ------------------------------------------------------------
purrr::walk(list.files("R", full.names = TRUE), source)

# ------------------------------------------------------------
# 1. Load schedule and extract seasons
# ------------------------------------------------------------
schedule <- readRDS("data_raw/schedule.rds")

seasons <- schedule$season %>% unique() %>% sort()

message("Target seasons: ", paste(seasons, collapse = ", "))

# ------------------------------------------------------------
# 2. Load existing logs (if valid)
# ------------------------------------------------------------
existing_path <- "data_raw/team_game_logs.rds"

if (file.exists(existing_path)) {
  tmp <- readRDS(existing_path)
  
  if (validate_existing_table(tmp, required_cols = c("season"))) {
    existing_logs <- tmp
    existing_seasons <- unique(existing_logs$season)
    message("Existing seasons found: ", paste(existing_seasons, collapse = ", "))
  } else {
    message("Existing file invalid or empty. Ignoring it.")
    existing_logs <- NULL
    existing_seasons <- integer(0)
  }
  
} else {
  message("No existing team logs found.")
  existing_logs <- NULL
  existing_seasons <- integer(0)
}

# ------------------------------------------------------------
# 3. Determine seasons to pull (incremental)
# ------------------------------------------------------------
seasons_to_pull <- compute_seasons_to_pull(
  all_seasons = seasons,
  existing_seasons = existing_seasons,
  refresh_current = TRUE   # always refresh the most recent season
)

message("Seasons to pull: ", paste(seasons_to_pull, collapse = ", "))

# ------------------------------------------------------------
# 4. Load team IDs
# ------------------------------------------------------------
teams <- readRDS("data_raw/teams_raw.rds")
team_ids <- extract_team_ids(teams)

# ------------------------------------------------------------
# 5. Pull only needed seasons
# ------------------------------------------------------------
new_logs <- map_dfr(seasons_to_pull, function(season) {
  message("Pulling season: ", season)
  map_dfr(team_ids, function(team_id) {
    Sys.sleep(0.2)
    pull_team_logs(season, team_id)
  })
})

# ------------------------------------------------------------
# 6. Combine with existing logs and dedupe
# ------------------------------------------------------------
existing_logs <- existing_logs %>% 
  filter(!is.na(wl))
final_team_stat_logs <- combine_and_dedupe(existing_logs, new_logs)

# ------------------------------------------------------------
# 7. Save output
# ------------------------------------------------------------
saveRDS(final_team_stat_logs, existing_path)

message("Team game logs updated and saved to: ", existing_path)
message("Done.")