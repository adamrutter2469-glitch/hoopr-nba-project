# ============================================================
# Script: R/pull_bigballsdata_odds.R
# Purpose: Build our own historical archive of NBA rebounds prop
#          lines, one snapshot per day. bigballsdata's TRUE historical
#          odds endpoints (/v1/odds/historical, /v1/odds/closing-lines)
#          require a paid plan - confirmed by reading their docs, not
#          available on the free tier. This is the free-tier
#          workaround: pull the CURRENT line for tonight's relevant
#          players every day and keep every day's snapshot, so the
#          archive grows into our own point-in-time record over time
#          instead of buying access to theirs.
#
#          Each run APPENDS a new pulled_date partition rather than
#          overwriting older ones (arrow::write_dataset with
#          existing_data_behavior="delete_matching" only touches
#          TODAY's partition - a re-run on the same day replaces
#          today's snapshot idempotently; every other day's file is
#          untouched, same pattern as the season-partitioned
#          datasets in R/pull_schedule.R / R/pull_game_logs.R, just
#          partitioned by pulled_date instead of season).
#
#          Scope: only players from TODAY's scheduled games (nothing
#          to pull on off-days or in the offseason), filtered to
#          recent rotation players (min_roll10 >= cfg$player_rebounding_min_minutes,
#          reusing the exact threshold already used for the player
#          rebounding stage) - keeps this comfortably within the free
#          daily quota even on a busy multi-game night. A player with
#          no rolling-minutes history yet (e.g. a rookie) is included
#          rather than excluded - fail open, not closed, on missing
#          data. Rebounds market only for now; the API supports
#          points/assists/threes/points_rebounds_assists too if this
#          proves useful and you want to broaden it later.
# ============================================================

fetch_bbs_player_prop <- function(bbs_player_id, market = "rebounds") {
  bbs_get(paste0("/nba/players/", bbs_player_id, "/props"), query = list(market = market))
}

# ------------------------------------------------------------
# Flatten one player's prop response into one row PER SPORTSBOOK
# (long format - keeps every book's line, not just the featured
# "current_line" pick, at no extra API cost - this is what makes
# line-shopping / cross-book comparison possible later). Returns a
# 0-row tibble if no book has posted a line for this player yet.
# ------------------------------------------------------------
flatten_prop_response <- function(resp, player_id, player_name, team_id, opponent_id,
                                   game_id_nba, game_date, pulled_at) {
  books <- resp$data$current_line$all_books
  if (is.null(books) || nrow(books) == 0) return(tibble::tibble())

  tibble::as_tibble(books) %>%
    dplyr::transmute(
      pulled_date    = as.character(as.Date(pulled_at)),
      pulled_at      = pulled_at,
      game_date      = as.character(game_date),
      game_id_nba    = game_id_nba,
      player_id      = player_id,
      player_name    = player_name,
      bbs_player_id  = resp$data$player$bbs_id,
      team_id        = team_id,
      opponent_id    = opponent_id,
      market         = resp$data$market,
      sportsbook     = sportsbook,
      sportsbook_display = display,
      line           = as.numeric(line),
      over_juice     = as.numeric(over_juice),
      under_juice    = as.numeric(under_juice)
    )
}

# ------------------------------------------------------------
# Today's (or as_of_date's) scheduled games, home/away team_id cast
# to character to match the id-mapping tables. as_of_date defaults to
# Sys.Date() for real use; overridable for testing against a real
# past date's slate when nothing is actually scheduled right now.
# ------------------------------------------------------------
get_games_on <- function(cfg, as_of_date = Sys.Date()) {
  schedule <- read_full_dataset(cfg$path_schedule_dataset)
  if (is.null(schedule)) return(tibble::tibble())
  schedule %>%
    dplyr::filter(game_date == as.Date(as_of_date)) %>%
    dplyr::mutate(home_team_id = as.character(home_team_id), away_team_id = as.character(away_team_id))
}

# ------------------------------------------------------------
# Players on `team_id`'s roster worth pulling a prop for - recent
# rotation players, or true rookies without a full 10-game window yet.
#
# player_id_mapping's bbs_team_id is bigballsdata's CURRENT-roster
# assignment, but it's built from THEIR full player list, which
# includes every player who ever wore that franchise's jersey (Larry
# Bird, Danny Ainge, ...), not just the active roster - bigballsdata
# has no "is_active" flag we filtered on. The bug this fixes: an
# inner_join against a player's CURRENT-SEASON appearance in
# player_game_features is what actually narrows this down to real
# current players. The old code left_joined instead, so any retired
# player (no row at all, ever, in player_game_features) got a NA
# min_roll10, hit the "fail open, might be a rookie" clause, and got
# pulled anyway - 208 "players" per team instead of ~10-15. That 15x
# fan-out is what blew through bigballsdata's per-minute AND daily
# quota during testing.
#
# The inner_join fixes that: only players with at least one row in
# THIS season's player_game_features are considered at all (excludes
# anyone who hasn't played this season, retired or otherwise). Within
# that real, current set, min_roll10 still being NA genuinely means
# "not enough games yet for a 10-game window" (a true rookie/debut
# case) rather than "never played for this franchise" - so failing
# open on NA is safe again once it's scoped to this season.
# ------------------------------------------------------------
get_relevant_players_for_team <- function(team_id, cfg, as_of_date = Sys.Date()) {
  team_map   <- read_parquet_or_null(cfg$path_team_id_mapping)
  player_map <- read_parquet_or_null(cfg$path_player_id_mapping)
  if (is.null(team_map) || is.null(player_map)) return(tibble::tibble())

  bbs_team_id <- team_map %>% dplyr::filter(team_id == !!team_id) %>% dplyr::pull(bbs_team_id)
  if (length(bbs_team_id) == 0) return(tibble::tibble())

  team_players <- player_map %>% dplyr::filter(bbs_team_id == !!bbs_team_id[1])
  if (nrow(team_players) == 0) return(team_players)

  player_features <- read_parquet_or_null(cfg$path_player_game_features)
  if (is.null(player_features)) return(tibble::tibble())  # can't confirm anyone is currently active - fail CLOSED, not open

  current_season_players <- player_features %>%
    dplyr::filter(season == current_season(as_of_date)) %>%
    dplyr::group_by(player_id) %>%
    dplyr::slice_max(game_date, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(player_id, min_roll10)

  team_players %>%
    dplyr::inner_join(current_season_players, by = "player_id") %>%
    dplyr::filter(is.na(min_roll10) | min_roll10 >= cfg$player_rebounding_min_minutes)
}

# ------------------------------------------------------------
# Append today's snapshot to the archive. Not reusing
# write_season_partition() (R/utils_io.R) - that helper is hardcoded
# to partition by a column literally named "season"; this table
# partitions by pulled_date instead. Small enough to duplicate the
# few lines rather than risk generalizing an already-stable helper
# every other pull stage depends on.
# ------------------------------------------------------------
write_props_snapshot <- function(df, path) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  arrow::write_dataset(
    df, path,
    format = "parquet",
    partitioning = "pulled_date",
    existing_data_behavior = "delete_matching"
  )
}

read_props_history <- function(cfg) {
  path <- cfg$path_props_history
  if (!dir.exists(path) || length(list.dirs(path, recursive = FALSE)) == 0) return(NULL)
  arrow::open_dataset(path) %>% dplyr::collect()
}

# ------------------------------------------------------------
# Orchestration entry point. as_of_date overridable for testing.
# ------------------------------------------------------------
refresh_bigballsdata_odds <- function(cfg, logger, as_of_date = Sys.Date()) {
  if (!isTRUE(cfg$bbs_odds_enabled)) {
    logger$log("BBS odds snapshot: SKIPPED (cfg$bbs_odds_enabled = FALSE).")
    return(invisible(NULL))
  }

  if (file.exists(".env")) readRenviron(".env")
  if (Sys.getenv("BBS_API_KEY") == "") {
    logger$log("BBS odds snapshot: SKIPPED, BBS_API_KEY not set in .env.")
    return(invisible(NULL))
  }

  games <- get_games_on(cfg, as_of_date)
  if (nrow(games) == 0) {
    logger$log("BBS odds snapshot: no games scheduled on ", as_of_date, " - nothing to pull.")
    return(invisible(NULL))
  }
  logger$log("BBS odds snapshot: ", nrow(games), " game(s) scheduled on ", as_of_date, ".")

  pulled_at <- as.character(Sys.time())
  all_rows <- list()
  success_count <- 0L
  fail_count <- 0L
  no_line_count <- 0L

  for (i in seq_len(nrow(games))) {
    matchup <- list(
      list(team = games$home_team_id[i], opp = games$away_team_id[i]),
      list(team = games$away_team_id[i], opp = games$home_team_id[i])
    )
    for (side in matchup) {
      players <- get_relevant_players_for_team(side$team, cfg, as_of_date)
      if (nrow(players) == 0) next

      for (j in seq_len(nrow(players))) {
        Sys.sleep(cfg$throttle_bbs_sec)
        resp <- try(fetch_bbs_player_prop(players$bbs_player_id[j], market = "rebounds"), silent = TRUE)
        if (inherits(resp, "try-error")) {
          fail_count <- fail_count + 1L
          next
        }
        rows <- try(
          flatten_prop_response(resp, players$player_id[j], players$player_name[j],
                                 side$team, side$opp, games$game_id_nba[i], games$game_date[i], pulled_at),
          silent = TRUE
        )
        if (inherits(rows, "try-error") || nrow(rows) == 0) {
          no_line_count <- no_line_count + 1L
          next
        }
        success_count <- success_count + 1L
        all_rows[[length(all_rows) + 1]] <- rows
      }
    }
  }

  logger$log("  players: ", success_count, " with a line, ", no_line_count, " with none posted, ", fail_count, " failed")

  if (length(all_rows) == 0) {
    logger$log("BBS odds snapshot: no lines found for any relevant player.")
    return(invisible(list(pulled = success_count, failed = fail_count, no_line = no_line_count)))
  }

  snapshot <- dplyr::bind_rows(all_rows)
  write_props_snapshot(snapshot, cfg$path_props_history)
  logger$log("  ", cfg$path_props_history, " updated (", nrow(snapshot), " book-level rows for ", as_of_date, ")")

  invisible(list(pulled = success_count, failed = fail_count, no_line = no_line_count, data = snapshot))
}
