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
