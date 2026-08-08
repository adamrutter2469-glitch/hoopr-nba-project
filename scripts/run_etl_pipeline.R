# ============================================================
# Script: run_etl_pipeline.R
# Purpose: Orchestrate full NBA ETL pipeline in correct order
# ============================================================

message("=== NBA ETL Pipeline Starting ===")

# -----------------------------
# 1. Load Schedule
# -----------------------------
message("Step 1: Loading schedule...")
tryCatch({
  source("scripts/01_load_schedule.R")
  message("✓ Schedule loaded")
}, error = function(e) {
  message("✗ Schedule load failed: ", e$message)
  stop("Pipeline aborted")
})

# -----------------------------
# 2. Team Game Logs
# -----------------------------
message("Step 2: Pulling team game logs...")
tryCatch({
  source("scripts/02_build_team_game_logs.R")
  message("✓ Team logs updated")
}, error = function(e) {
  message("✗ Team logs failed: ", e$message)
  stop("Pipeline aborted")
})

# -----------------------------
# 3. Player Game Logs
# -----------------------------
message("Step 3: Pulling player game logs...")
tryCatch({
  source("scripts/03_build_player_game_logs.R")
  message("✓ Player logs updated")
}, error = function(e) {
  message("✗ Player logs failed: ", e$message)
  stop("Pipeline aborted")
})

# -----------------------------
# 4. Player Data (optional)
# -----------------------------
message("Step 4: Updating player metadata...")
tryCatch({
  source("scripts/00_build_raw_players.R")
  message("✓ Player metadata updated")
}, error = function(e) {
  message("✗ Player metadata failed: ", e$message)
  stop("Pipeline aborted")
})

# -----------------------------
# 5. Schedule Enrichment
# -----------------------------
message("Step 5: Updating schedule with travel...")
tryCatch({
  source("scripts/04_build_schedule_with_travel.R")
  message("✓ Player metadata updated")
}, error = function(e) {
  message("✗ Schedule With Travel failed: ", e$message)
  stop("Pipeline aborted")
})

# -----------------------------
# 5. Final Schedule
# -----------------------------
message("Step 6: Updating final schedule...")
tryCatch({
  source("scripts/05_build_final_schedule.R")
  message("✓ Player metadata updated")
}, error = function(e) {
  message("✗ Final Schedule failed: ", e$message)
  stop("Pipeline aborted")
})
message("=== NBA ETL Pipeline Complete ===")