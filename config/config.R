# ============================================================
# Script: config/config.R
# Purpose: Single source of truth for pipeline settings.
#          Source this file to load `cfg`, a named list used
#          throughout R/ and scripts/. Edit values here, not
#          in the pipeline code, when tuning behavior.
# ============================================================

cfg <- list(

  # ------------------------------------------------------------
  # Season range
  # ------------------------------------------------------------
  # Earliest season the pipeline will backfill on a first run.
  # The current season (auto-detected via current_season() in
  # R/utils_season.R) is always included/refreshed on top of this.
  first_season = "2022-23",

  # ------------------------------------------------------------
  # Paths (all relative to project root)
  # ------------------------------------------------------------
  path_data_raw        = "data_raw",
  path_data_processed  = "data_processed",
  path_state            = "state",
  path_logs             = "logs",
  path_travel_csv        = "data_raw/external/nba_airport_flight_matrix.csv",
  path_manifest          = "state/manifest.json",

  # ------------------------------------------------------------
  # API throttling (seconds slept between calls to be polite to
  # the NBA Stats API and avoid rate-limit errors)
  # ------------------------------------------------------------
  throttle_team_logs_sec  = 0.2,
  throttle_rebounding_sec = 0.6,

  # ------------------------------------------------------------
  # Advanced rebounding stage (R/pull_rebounding.R)
  # ------------------------------------------------------------
  # No bulk endpoint exists for this tracking data - it's one API
  # call per team per game. Set to FALSE to skip this stage
  # entirely (e.g. if a historical backfill proves too slow)
  # without touching any code.
  advanced_rebounding_enabled = TRUE,

  # Save progress to disk every N team-game pulls instead of
  # after every single one (old script wrote the full file after
  # every row - far too much disk I/O once the table is large).
  rebounding_checkpoint_every = 25,

  # ------------------------------------------------------------
  # Rolling average windows (trailing N games), used by
  # R/features_rolling.R for both team and player stats
  # ------------------------------------------------------------
  rolling_windows = c(5, 10, 20)
)
