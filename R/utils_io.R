# ============================================================
# Script: R/utils_io.R
# Purpose: Generic read/write/logging/dedupe helpers used by
#          every pull and feature stage. Nothing NBA-specific
#          lives here.
# ============================================================

# ------------------------------------------------------------
# Validate an existing RDS-backed table before trusting it as
# "already have this data" for incremental logic.
# ------------------------------------------------------------
validate_existing_table <- function(df, required_cols = NULL) {
  if (!is.data.frame(df)) return(FALSE)
  if (nrow(df) == 0) return(FALSE)
  if (!is.null(required_cols) && !all(required_cols %in% names(df))) return(FALSE)
  TRUE
}

# Read an RDS file if it exists and passes validate_existing_table(),
# otherwise return NULL. Centralizes the "does usable data already
# exist on disk" check used at the top of every pull stage.
read_existing_rds <- function(path, required_cols = NULL) {
  if (!file.exists(path)) return(NULL)
  df <- readRDS(path)
  if (!validate_existing_table(df, required_cols)) return(NULL)
  df
}

# ------------------------------------------------------------
# Combine existing + newly-pulled rows and drop duplicates.
# dedupe_cols = NULL dedupes on the full row (old default
# behavior); pass key columns (e.g. "game_id_nba") to dedupe on
# identity instead, keeping the first occurrence of each key.
# ------------------------------------------------------------
combine_and_dedupe <- function(existing_logs, new_logs, dedupe_cols = NULL) {
  combined <- dplyr::bind_rows(existing_logs, new_logs)
  if (is.null(dedupe_cols)) {
    dplyr::distinct(combined)
  } else {
    dplyr::distinct(combined, dplyr::across(dplyr::all_of(dedupe_cols)), .keep_all = TRUE)
  }
}

# ------------------------------------------------------------
# Parquet storage helpers.
#
# Single-file tables (reference data, feature tables - small,
# cheaply rewritten whole every run) just use arrow::read_parquet/
# write_parquet directly via these thin wrappers.
#
# Season-partitioned datasets (schedule, team/player game logs -
# unbounded history) are Hive-style directories written via
# arrow::write_dataset(): <path>/season=2022-23/part-0.parquet.
# existing_data_behavior = "delete_matching" means writing one
# season's data only touches that season's partition - every other
# season's file is left untouched on disk, so a routine run's cost
# (and the data it rewrites) stays proportional to what changed,
# not to how much history has piled up.
# ------------------------------------------------------------
read_parquet_or_null <- function(path, required_cols = NULL) {
  if (!file.exists(path)) return(NULL)
  df <- arrow::read_parquet(path)
  if (!validate_existing_table(df, required_cols)) return(NULL)
  df
}

write_parquet <- function(df, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  arrow::write_parquet(df, path)
}

# Which season= partitions currently exist in a dataset directory.
dataset_seasons_present <- function(path) {
  if (!dir.exists(path)) return(character(0))
  parts <- list.dirs(path, full.names = FALSE, recursive = FALSE)
  sub("^season=", "", parts[grepl("^season=", parts)])
}

# Read just one season's partition (NULL if that partition doesn't
# exist yet or fails validation) - this is what makes per-season
# incremental dedupe possible without ever loading full history.
read_season_partition <- function(path, season, required_cols = NULL) {
  if (!(season %in% dataset_seasons_present(path))) return(NULL)
  df <- arrow::open_dataset(path) %>%
    dplyr::filter(season == .env$season) %>%
    dplyr::collect()
  if (!validate_existing_table(df, required_cols)) return(NULL)
  df
}

# Write a data frame into a season-partitioned dataset. Only the
# season(s) present in `df` are touched; every other partition
# already on disk is left alone.
write_season_partition <- function(df, path) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  arrow::write_dataset(
    df, path,
    format = "parquet",
    partitioning = "season",
    existing_data_behavior = "delete_matching"
  )
}

# Read the full dataset across all seasons (NULL if none exist yet).
# Still cheap at this data's size - only the raw pull/write side
# needs the per-season scoping above.
read_full_dataset <- function(path) {
  if (!dir.exists(path) || length(dataset_seasons_present(path)) == 0) return(NULL)
  df <- arrow::open_dataset(path) %>% dplyr::collect()
  if (nrow(df) == 0) return(NULL)
  df
}

# ------------------------------------------------------------
# Manifest: the pipeline's memory of what it already pulled.
# Stored as human-readable JSON in state/manifest.json so it can
# be eyeballed or hand-edited (e.g. to force a re-pull window).
# ------------------------------------------------------------
default_manifest <- function() {
  list(
    last_run_at            = NA_character_,
    last_game_date_pulled  = NA_character_,
    seasons_fully_loaded   = character(0),
    rebounding_pairs_done  = 0L
  )
}

read_manifest <- function(path) {
  if (!file.exists(path)) return(default_manifest())
  m <- jsonlite::fromJSON(path)
  utils::modifyList(default_manifest(), m)
}

write_manifest <- function(manifest, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(manifest, path, auto_unbox = TRUE, pretty = TRUE, na = "null")
}

# ------------------------------------------------------------
# Logging: mirrors every message() to both the console and a
# timestamped file under logs/, so a batch-scheduled run leaves
# a record even when nobody's watching the console.
# Usage:
#   logger <- init_logger("logs")
#   logger$log("Step 1: ...")
#   ...
#   logger$close()
# ------------------------------------------------------------
init_logger <- function(log_dir = "logs") {
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_path <- file.path(log_dir, paste0("run_", ts, ".log"))
  con <- file(log_path, open = "a")

  log_fn <- function(...) {
    txt <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste0(..., collapse = ""))
    message(txt)
    writeLines(txt, con)
  }

  list(
    log   = log_fn,
    path  = log_path,
    close = function() close(con)
  )
}
