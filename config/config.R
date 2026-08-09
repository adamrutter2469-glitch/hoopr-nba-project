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
  # Storage: everything is parquet, EXCEPT the advanced rebounding
  # raw cache (nested API responses don't fit parquet's flat/
  # columnar model - see path_rebounding_raw_cache below).
  #
  # Datasets ending in a bare name (no extension) are season-
  # partitioned directories written via arrow::write_dataset() -
  # e.g. path_team_logs_dataset/season=2022-23/part-0.parquet.
  # Each pipeline run only touches the season(s) it actually
  # pulled, so cost stays proportional to what changed rather
  # than to how much history has accumulated. Everything else is
  # a single .parquet file, fully rewritten each run since it's
  # cheap at this data's size.
  # ------------------------------------------------------------
  path_data_raw       = "data_raw",
  path_data_processed = "data_processed",
  path_state          = "state",
  path_logs           = "logs",

  path_travel_csv     = "data_raw/external/nba_airport_flight_matrix.csv",
  path_manifest       = "state/manifest.json",

  path_schedule_dataset    = "data_raw/schedule",              # partitioned by season
  path_team_logs_dataset   = "data_raw/team_game_logs",         # partitioned by season
  path_player_logs_dataset = "data_raw/player_game_logs",       # partitioned by season

  path_players_raw    = "data_raw/players_raw.parquet",
  path_teams_raw      = "data_raw/teams_raw.parquet",

  path_rebounding_raw_cache = "data_raw/team_rebounding_dashboards.rds",  # nested - stays RDS
  path_rebounding_features  = "data_processed/team_rebounding_features.parquet",

  path_player_rebounding_raw_cache = "data_raw/player_rebounding_dashboards.rds",  # nested - stays RDS
  path_player_rebounding_features  = "data_processed/player_rebounding_features.parquet",

  path_schedule_with_travel = "data_processed/schedule_with_travel_detail.parquet",
  path_schedule_team_level  = "data_processed/schedule_team_level_final.parquet",
  path_team_game_features   = "data_processed/team_game_features.parquet",
  path_player_game_features = "data_processed/player_game_features.parquet",

  path_team_id_mapping   = "data_processed/team_id_mapping.parquet",
  path_player_id_mapping = "data_processed/player_id_mapping.parquet",

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
  # Player-level advanced rebounding stage (R/pull_player_rebounding.R)
  # ------------------------------------------------------------
  # Same idea as the team-level stage above (one API call per row, no
  # bulk endpoint), but at player-game grain - ~11x more rows than the
  # team version (105k+ vs ~9.8k), so scoped down by default:
  player_rebounding_enabled = TRUE,

  # Only these seasons are in scope for now - current season only, so
  # the first backfill is ~5 hours instead of ~23. Widen this later
  # (e.g. to season_sequence(cfg$first_season) for full history) once
  # you're ready to let a longer backfill run.
  player_rebounding_seasons = c("2025-26"),

  # Skip garbage-time cameos below this many minutes played that game.
  player_rebounding_min_minutes = 10,

  throttle_player_rebounding_sec = 0.6,
  player_rebounding_checkpoint_every = 100,

  # ------------------------------------------------------------
  # Rolling average windows (trailing N games), used by
  # R/features_rolling.R for both team and player stats
  # ------------------------------------------------------------
  rolling_windows = c(5, 10, 20),

  # ------------------------------------------------------------
  # Cloudflare R2 sync (R/sync_r2.R) - mirrors data_raw/ and
  # data_processed/ to a private R2 bucket after every pipeline run.
  # Credentials (R2_ACCOUNT_ID, R2_ACCESS_KEY_ID, R2_SECRET_ACCESS_KEY,
  # R2_BUCKET_NAME) live in .env, never here - see .env.example.
  # ------------------------------------------------------------
  r2_sync_enabled = TRUE,

  # Full path to rclone.exe - not on the system PATH on this machine,
  # so referenced directly rather than assumed to be findable.
  path_rclone_exe = "C:/Users/12623/bin/rclone.exe",

  # Safety cap: R2's free tier is 10 GB. Before every sync, the
  # pipeline checks the LOCAL size of data_raw/ + data_processed/
  # (rclone sync mirrors, so remote usage tracks local size closely)
  # and refuses to upload anything at all once that meets/exceeds
  # r2_max_storage_gb, leaving a 1 GB buffer under the real cap. A
  # run that blows past it fails loudly (counted as a stage failure,
  # not a silent skip) instead of uploading first and asking
  # questions later - so a future bug that caused some table to
  # explode in row count can't turn into a surprise storage bill.
  # r2_warn_storage_gb logs a warning before that point so you see it
  # coming.
  r2_warn_storage_gb = 5,
  r2_max_storage_gb  = 9,

  # R2's actual free-tier quota, used only for the end-of-run summary's
  # "X GB / 10 GB (Y% utilized)" line (R/run_summary.R) - kept separate
  # from r2_max_storage_gb so the abort trigger and the reported quota
  # can't drift apart by accident if one is ever tuned independently.
  r2_free_tier_gb = 10,

  # ------------------------------------------------------------
  # Big Balls Sports Data id-mapping bridge tables (R/pull_bigballsdata_mapping.R)
  # ------------------------------------------------------------
  # Optional - skips gracefully if BBS_API_KEY isn't set in .env.
  # Free tier is 100 req/min, 1000/day - this stage uses ~55 calls
  # total (1 for teams, ~54 paginated for players), well within it.
  bbs_mapping_enabled = TRUE,
  throttle_bbs_sec    = 0.3
)
