# =====================================================================
#  TEAM REBOUNDING DASHBOARD RAW STORAGE (20-GAME TEST, FAST VERSION)
# =====================================================================

library(dplyr)
library(hoopR)
library(progressr)

handlers(global = TRUE)

schedule <- readRDS("data_processed/schedule_team_level_final.rds")

schedule_subset <- schedule %>%
  slice_head(n = 20)

n <- nrow(schedule_subset)

# Preallocate list for speed
results <- vector("list", n)

success_count <- 0
fail_count <- 0

with_progress({
  p <- progressor(steps = n)
  
  for (i in seq_len(n)) {
    p()

    team_id <- schedule_subset$team_id[i]
    game_id <- schedule_subset$game_id_nba[i]
    
    out <- try(nba_teamdashptreb(team_id = team_id, game_id = game_id), silent = TRUE)
    
    if (inherits(out, "try-error")) {
      fail_count <- fail_count + 1
      results[[i]] <- NULL
    } else {
      success_count <- success_count + 1
      results[[i]] <- list(
        game_id_nba = game_id,
        team_id = team_id,
        opponent_id = schedule_subset$opponent_id[i],
        dash_raw = out
      )
    }
  }
})

# Drop NULLs
clean_results <- results[!sapply(results, is.null)]

# Convert to tibble
raw_dash_clean <- bind_rows(clean_results)

saveRDS(raw_dash_clean, "data_raw/team_reb_dashboards_TEST20.rds")

message("====================================================")
message(" TEAM REBOUNDING DASHBOARD TEST PULL COMPLETE")
message("----------------------------------------------------")
message(" Successful pulls: ", success_count)
message(" Failed pulls:     ", fail_count)
message(" Total attempted:  ", n)
message(" Saved dashboards: ", length(clean_results))
message("====================================================")