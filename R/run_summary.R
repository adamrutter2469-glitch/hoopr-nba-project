# ============================================================
# Script: R/run_summary.R
# Purpose: The end-of-run report: how many games were newly added
#          this run and what date range they cover, any errors (full
#          stage failures plus per-row pull failures in the
#          rebounding stages), and current R2 storage utilization
#          against the free-tier cap. Printed to console AND written
#          to the log file (via logger$raw - untimestamped, so it
#          reads as one scannable block rather than a wall of
#          per-line timestamps), so it's visible whether the run was
#          watched live or checked afterward.
# ============================================================

# ------------------------------------------------------------
# Count of newly-added games this run, and their date range, by
# comparing the CURRENT team_game_logs against the manifest snapshot
# taken before this run started. "Newly added" = game_date after
# whatever was already known last time - on a first-ever run
# (no prior manifest), everything counts as new.
# ------------------------------------------------------------
games_added_this_run <- function(cfg, prev_last_game_date) {
  team_logs <- read_full_dataset(cfg$path_team_logs_dataset)
  if (is.null(team_logs)) {
    return(list(n_games = 0L, from_date = NA_character_, to_date = NA_character_))
  }

  had_prior_cutoff <- !is.null(prev_last_game_date) && !is.na(prev_last_game_date)
  new_rows <- if (had_prior_cutoff) {
    team_logs %>% dplyr::filter(game_date > as.Date(prev_last_game_date))
  } else {
    team_logs
  }

  if (nrow(new_rows) == 0) {
    return(list(n_games = 0L, from_date = NA_character_, to_date = NA_character_))
  }

  list(
    n_games   = dplyr::n_distinct(new_rows$game_id_nba),
    from_date = as.character(min(new_rows$game_date, na.rm = TRUE)),
    to_date   = as.character(max(new_rows$game_date, na.rm = TRUE))
  )
}

# ------------------------------------------------------------
# Assemble and print the summary block.
#   manifest_before  - read_manifest() result captured BEFORE this
#                      run's stages executed (run_pipeline.R does this).
#   stage_failures   - named list, stage name -> error message, built
#                      by run_stage()'s tryCatch in run_pipeline.R.
#   stage_results    - named list holding the two rebounding stages'
#                      return values (list(pulled=, failed=, data=) or
#                      NULL if that stage was skipped/disabled).
# ------------------------------------------------------------
print_run_summary <- function(cfg, logger, manifest_before, stage_failures, stage_results, elapsed_min) {
  games <- games_added_this_run(cfg, manifest_before$last_game_date_pulled)

  storage_gb <- tryCatch(local_sync_size_gb(cfg), error = function(e) NA_real_)
  pct_used   <- if (!is.na(storage_gb)) round(100 * storage_gb / cfg$r2_free_tier_gb, 1) else NA_real_

  logger$raw("")
  logger$raw("==================== RUN SUMMARY ====================")
  logger$raw(sprintf("Duration:          %s min", elapsed_min))

  if (games$n_games > 0) {
    logger$raw(sprintf("Games added:       %d (%s to %s)", games$n_games, games$from_date, games$to_date))
  } else {
    logger$raw("Games added:       0 (already up to date)")
  }

  report_rebounding_stage <- function(label, result) {
    if (is.null(result)) {
      logger$raw(sprintf("%s: skipped/disabled", label))
    } else if (result$pulled == 0L && result$failed == 0L) {
      logger$raw(sprintf("%s: nothing new to pull", label))
    } else if (result$failed > 0L) {
      logger$raw(sprintf("%s: %d pulled, %d FAILED  <-- check log for which ones", label, result$pulled, result$failed))
    } else {
      logger$raw(sprintf("%s: %d pulled, 0 failed", label, result$pulled))
    }
  }
  report_rebounding_stage("Team rebounding   ", stage_results$team_rebounding)
  report_rebounding_stage("Player rebounding ", stage_results$player_rebounding)

  if (length(stage_failures) > 0) {
    logger$raw(sprintf("Errors:            %d stage(s) failed:", length(stage_failures)))
    for (nm in names(stage_failures)) {
      logger$raw(sprintf("  - %s: %s", nm, stage_failures[[nm]]))
    }
  } else {
    logger$raw("Errors:            none")
  }

  if (!is.na(storage_gb)) {
    logger$raw(sprintf(
      "R2 storage:        %.3f GB / %d GB (%.1f%% utilized)",
      storage_gb, cfg$r2_free_tier_gb, pct_used
    ))
  } else {
    logger$raw("R2 storage:        unavailable (check R2 sync stage above)")
  }

  logger$raw("=======================================================")
  logger$raw("")
}
