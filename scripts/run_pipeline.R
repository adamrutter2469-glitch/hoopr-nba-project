# ============================================================
# Script: scripts/run_pipeline.R
# Purpose: Main pipeline entry point. Pulls any games that have
#          occurred since the last run and rebuilds the
#          comprehensive team/player feature tables. Launched via
#          run_pipeline.bat (double-click, or schedule it), or
#          directly with `Rscript scripts/run_pipeline.R` from the
#          project root.
#
# Optional flag:
#   --skip-rebounding   Skip the slow advanced-rebounding stage
#                        for THIS run only (does not change
#                        config/config.R - useful for a fast
#                        verification run or a routine run when
#                        you don't want to wait on it).
# ============================================================

suppressMessages({
  library(dplyr)
  library(janitor)
  library(hoopR)
  library(purrr)
  library(tidyr)
  library(readr)
  library(jsonlite)
  library(slider)
  library(tibble)
  library(arrow)
})

source("config/config.R")
invisible(purrr::walk(list.files("R", full.names = TRUE, pattern = "\\.R$"), source))

args <- commandArgs(trailingOnly = TRUE)
if ("--skip-rebounding" %in% args) {
  cfg$advanced_rebounding_enabled <- FALSE
}

logger <- init_logger(cfg$path_logs)
on.exit(logger$close(), add = TRUE)

logger$log("=== NBA Pipeline run starting ===")
start_time <- Sys.time()
failures <- 0L

run_stage <- function(name, expr) {
  logger$log("--- ", name, " ---")
  tryCatch(
    expr,
    error = function(e) {
      logger$log("  STAGE FAILED: ", conditionMessage(e))
      failures <<- failures + 1L
      NULL
    }
  )
}

run_stage("Reference data",       refresh_reference_data(cfg, logger))
run_stage("Schedule",             refresh_schedule(cfg, logger))
run_stage("Team game logs",       refresh_team_game_logs(cfg, logger))
run_stage("Player game logs",     refresh_player_game_logs(cfg, logger))
run_stage("Rest/travel features", refresh_rest_travel_features(cfg, logger))
run_stage("Advanced rebounding",  refresh_rebounding_features(cfg, logger))
run_stage("Combine features",     combine_all_features(cfg, logger))

elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 1)

if (failures > 0) {
  logger$log("=== Pipeline finished with ", failures, " stage failure(s) in ", elapsed, " min ===")
  quit(status = 1, save = "no")
} else {
  logger$log("=== NBA Pipeline run complete (", elapsed, " min) ===")
}
