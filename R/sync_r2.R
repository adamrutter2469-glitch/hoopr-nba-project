# ============================================================
# Script: R/sync_r2.R
# Purpose: Mirror data_raw/ and data_processed/ to a private
#          Cloudflare R2 bucket after every pipeline run, via
#          rclone. `rclone sync` makes the remote match the local
#          folder exactly (adds/updates/deletes to match) - so R2
#          usage tracks local disk usage, it never accumulates
#          separate snapshots on top of what's already there.
#
#          Safety-first: a hard cap (cfg$r2_max_storage_gb) is
#          checked against LOCAL size before any network call is
#          made. If a bug ever caused some table to explode in row
#          count, this stage refuses to upload anything at all and
#          fails loudly (counted as a stage failure by
#          scripts/run_pipeline.R, same as any other stage failure)
#          rather than silently racking up a storage bill.
#
#          Credentials come from .env (R2_ACCOUNT_ID,
#          R2_ACCESS_KEY_ID, R2_SECRET_ACCESS_KEY, R2_BUCKET_NAME) -
#          never hardcoded, never passed as literal command-line
#          arguments to rclone (which would be visible in a process
#          listing) - they're written to a temp rclone config file
#          instead, outside the repo, cleaned up after each run.
# ============================================================

# ------------------------------------------------------------
# Local size of everything this stage would sync, in GB.
# ------------------------------------------------------------
local_sync_size_gb <- function(cfg) {
  dirs <- c(cfg$path_data_raw, cfg$path_data_processed)
  files <- list.files(dirs, recursive = TRUE, full.names = TRUE)
  bytes <- sum(file.info(files)$size, na.rm = TRUE)
  bytes / 1e9
}

# ------------------------------------------------------------
# Pre-flight safety check. Raises an error (not just a log line) if
# the cap is exceeded, so it's impossible to accidentally proceed
# to the actual upload past this point.
# ------------------------------------------------------------
check_storage_safety <- function(cfg, logger) {
  size_gb <- local_sync_size_gb(cfg)
  logger$log("R2 sync: local data_raw/ + data_processed/ = ", round(size_gb, 4), " GB")

  if (size_gb >= cfg$r2_max_storage_gb) {
    stop(sprintf(
      "R2 sync ABORTED: local size (%.4f GB) has met or exceeded the safety cap (%.1f GB). Nothing was uploaded. Investigate before raising cfg$r2_max_storage_gb.",
      size_gb, cfg$r2_max_storage_gb
    ))
  }
  if (size_gb >= cfg$r2_warn_storage_gb) {
    logger$log("  WARNING: local size is approaching the safety cap (", cfg$r2_max_storage_gb, " GB).")
  }
  invisible(size_gb)
}

# ------------------------------------------------------------
# Write a throwaway rclone config file (outside the repo, in the
# system temp dir) so credentials never appear as literal
# command-line arguments. Caller is responsible for unlink()ing it.
# ------------------------------------------------------------
write_rclone_config <- function(account_id, access_key, secret_key) {
  endpoint <- sprintf("https://%s.r2.cloudflarestorage.com", account_id)
  conf <- c(
    "[r2]",
    "type = s3",
    "provider = Cloudflare",
    paste0("access_key_id = ", access_key),
    paste0("secret_access_key = ", secret_key),
    paste0("endpoint = ", endpoint),
    "acl = private"
  )
  conf_path <- tempfile(pattern = "rclone_", fileext = ".conf")
  writeLines(conf, conf_path)
  conf_path
}

# ------------------------------------------------------------
# Orchestration entry point. dry_run = TRUE runs rclone's own
# --dry-run (reports what WOULD happen, transfers nothing) - use
# this to verify credentials/connectivity/paths before a real sync.
# ------------------------------------------------------------
sync_to_r2 <- function(cfg, logger, dry_run = FALSE) {
  if (!isTRUE(cfg$r2_sync_enabled)) {
    logger$log("R2 sync: SKIPPED (cfg$r2_sync_enabled = FALSE).")
    return(invisible(NULL))
  }

  if (file.exists(".env")) readRenviron(".env")

  required_env <- c("R2_ACCOUNT_ID", "R2_ACCESS_KEY_ID", "R2_SECRET_ACCESS_KEY", "R2_BUCKET_NAME")
  env_vals <- Sys.getenv(required_env)
  missing_env <- required_env[env_vals == ""]
  if (length(missing_env) > 0) {
    logger$log("R2 sync: SKIPPED, missing .env value(s): ", paste(missing_env, collapse = ", "))
    return(invisible(NULL))
  }

  if (!file.exists(cfg$path_rclone_exe)) {
    stop("R2 sync: rclone.exe not found at cfg$path_rclone_exe (", cfg$path_rclone_exe, ")")
  }

  # Safety check BEFORE anything network-related happens.
  check_storage_safety(cfg, logger)

  account_id <- Sys.getenv("R2_ACCOUNT_ID")
  access_key <- Sys.getenv("R2_ACCESS_KEY_ID")
  secret_key <- Sys.getenv("R2_SECRET_ACCESS_KEY")
  bucket     <- Sys.getenv("R2_BUCKET_NAME")

  conf_path <- write_rclone_config(account_id, access_key, secret_key)
  on.exit(unlink(conf_path), add = TRUE)

  sync_one <- function(local_dir) {
    remote_dir <- paste0("r2:", bucket, "/", basename(local_dir))
    logger$log("R2 sync: ", local_dir, " -> ", remote_dir, if (dry_run) " (DRY RUN)" else "", "...")

    args <- c("--config", conf_path, "sync", local_dir, remote_dir, "--stats-one-line")
    if (dry_run) args <- c(args, "--dry-run")

    result <- system2(cfg$path_rclone_exe, args = args, stdout = TRUE, stderr = TRUE)
    status <- attr(result, "status")
    logger$log("  ", paste(result, collapse = "\n  "))

    if (!is.null(status) && status != 0) {
      stop("R2 sync FAILED for ", local_dir, " (rclone exit status ", status, ") - see log above.")
    }
  }

  sync_one(cfg$path_data_raw)
  sync_one(cfg$path_data_processed)

  logger$log("R2 sync: complete", if (dry_run) " (dry run - nothing was actually uploaded)" else "", ".")
  invisible(NULL)
}
