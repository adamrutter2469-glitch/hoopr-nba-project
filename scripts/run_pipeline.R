# ============================================================
# Script: scripts/run_pipeline.R
# Purpose: Main pipeline entry point. Pulls any games that have
#          occurred since the last run and rebuilds the
#          comprehensive team/player feature tables. Launched via
#          run_pipeline.bat (double-click, or schedule it), or
#          directly with `Rscript scripts/run_pipeline.R` from the
#          project root.
#
# Optional flags:
#   --skip-rebounding          Skip the slow team-level advanced-
#                               rebounding stage for THIS run only.
#   --skip-player-rebounding   Same, for the player-level advanced-
#                               rebounding stage.
#   --skip-r2-sync             Skip mirroring data_raw/ and
#                               data_processed/ to Cloudflare R2.
#   --skip-bbs-mapping         Skip rebuilding the Big Balls Sports
#                               Data id-mapping bridge tables.
#   --skip-bbs-odds            Skip today's rebounds-prop archive
#                               snapshot.
#   --skip-playbyplay          Skip the play-by-play pull AND the ESPN
#                               player-id mapping stage (shares the
#                               same cfg$playbyplay_enabled toggle,
#                               since the mapping depends on it).
#   None of these change config/config.R - useful for a fast
#   verification run or a routine run when you don't want to wait on
#   one of them.
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
  library(httr2)
})

source("config/config.R")
invisible(purrr::walk(list.files("R", full.names = TRUE, pattern = "\\.R$"), source))

args <- commandArgs(trailingOnly = TRUE)
if ("--skip-rebounding" %in% args) {
  cfg$advanced_rebounding_enabled <- FALSE
}
if ("--skip-player-rebounding" %in% args) {
  cfg$player_rebounding_enabled <- FALSE
}
if ("--skip-r2-sync" %in% args) {
  cfg$r2_sync_enabled <- FALSE
}
if ("--skip-bbs-mapping" %in% args) {
  cfg$bbs_mapping_enabled <- FALSE
}
if ("--skip-bbs-odds" %in% args) {
  cfg$bbs_odds_enabled <- FALSE
}
if ("--skip-playbyplay" %in% args) {
  cfg$playbyplay_enabled <- FALSE
}

logger <- init_logger(cfg$path_logs)
on.exit(logger$close(), add = TRUE)

# Snapshot BEFORE any stage runs, so the end-of-run summary can report
# what changed relative to where things stood at the start.
manifest_before <- read_manifest(cfg$path_manifest)

logger$log("=== NBA Pipeline run starting ===")
start_time <- Sys.time()
failures <- 0L
stage_failures <- list()   # stage name -> error message, for the summary
stage_results  <- list()   # holds the two rebounding stages' return values

run_stage <- function(name, expr) {
  logger$log("--- ", name, " ---")
  tryCatch(
    expr,
    error = function(e) {
      logger$log("  STAGE FAILED: ", conditionMessage(e))
      failures <<- failures + 1L
      stage_failures[[name]] <<- conditionMessage(e)
      NULL
    }
  )
}

run_stage("Reference data",       refresh_reference_data(cfg, logger))
run_stage("BBS id mapping",       refresh_bigballsdata_mapping(cfg, logger))
run_stage("Schedule",             refresh_schedule(cfg, logger))
run_stage("Team game logs",       refresh_team_game_logs(cfg, logger))
run_stage("Player game logs",     refresh_player_game_logs(cfg, logger))
run_stage("Play-by-play",         refresh_playbyplay(cfg, logger))
run_stage("ESPN player mapping",  refresh_espn_player_mapping(cfg, logger))
run_stage("Turnover event features", refresh_turnover_features(cfg, logger))
run_stage("Block event features", refresh_block_features(cfg, logger))
run_stage("Rest/travel features", refresh_rest_travel_features(cfg, logger))
stage_results$team_rebounding   <- run_stage("Advanced rebounding",  refresh_rebounding_features(cfg, logger))
stage_results$player_rebounding <- run_stage("Player rebounding",    refresh_player_rebounding_features(cfg, logger))
run_stage("Combine features",     combine_all_features(cfg, logger))
run_stage("BBS odds snapshot",    refresh_bigballsdata_odds(cfg, logger))
run_stage("R2 sync",              sync_to_r2(cfg, logger))

elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 1)

print_run_summary(cfg, logger, manifest_before, stage_failures, stage_results, elapsed)

if (failures > 0) {
  logger$log("=== Pipeline finished with ", failures, " stage failure(s) in ", elapsed, " min ===")
  quit(status = 1, save = "no")
} else {
  logger$log("=== NBA Pipeline run complete (", elapsed, " min) ===")
}
