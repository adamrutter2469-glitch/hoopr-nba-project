# ============================================================
# Script: R/pull_bigballsdata_mapping.R
# Purpose: Bridge tables connecting our own NBA-Stats-sourced IDs to
#          Big Balls Sports Data's (bigballsdata.com) internal IDs,
#          so any future odds/props pull from that API can join back
#          to our own team_game_features / player_game_features.
#
#          Two real mismatches, confirmed against the live API:
#          - Players: their id is a UUID (bbs_id), ours is NBA's own
#            numeric person_id - no shared key, matched by name.
#          - Teams: their canonical /v1/teams short_name differs from
#            NBA.com's tricode for 5 franchises (GS/NO/SA/UTAH/WSH vs
#            our GSW/NOP/SAS/UTA/WAS) - matched by full team name,
#            not abbreviation.
#
#          Cheap compared to the rebounding stages: ~55 total API
#          calls (1 for teams, ~54 paginated for players; free tier
#          is 100/min, 1000/day), full refresh each run - no
#          checkpointing needed at this scale. Skips gracefully (not
#          an error) if BBS_API_KEY isn't set in .env, since this
#          integration is optional.
# ============================================================

BBS_BASE_URL <- "https://api.bigballsdata.com/v1"

# ------------------------------------------------------------
# Normalize a name for matching: strip diacritics, lowercase, drop
# periods, collapse whitespace. Applied to BOTH sides before
# comparing, so "Luka Doncic" (accented on one side or the other
# depending on source) and "A. J. Green" / "A.J. Green" both line up.
# ------------------------------------------------------------
normalize_name <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("\\.", "", x)
  x <- gsub("\\s+", " ", trimws(x))
  x
}

# ------------------------------------------------------------
# Authenticated GET against the BBS gateway. Returns the parsed body
# with $data already simplified to a data frame where possible.
# ------------------------------------------------------------
bbs_get <- function(path, query = list()) {
  key <- Sys.getenv("BBS_API_KEY")
  req <- httr2::request(paste0(BBS_BASE_URL, path)) %>%
    httr2::req_headers(Authorization = paste("Bearer", key)) %>%
    httr2::req_url_query(!!!query)
  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp, simplifyVector = TRUE)
}

fetch_bbs_teams <- function() {
  result <- bbs_get("/teams", query = list(sport = "basketball", league = "nba"))
  tibble::as_tibble(result$data)
}

# Paginated - /v1/players has no bulk "give me everyone" mode, so
# this walks every page at the given limit (100 is the max observed
# to work) until pagination$total is exhausted.
fetch_bbs_players <- function(logger, throttle_sec = 0.3, page_limit = 100) {
  first_page <- bbs_get("/players", query = list(sport = "basketball", league = "nba", limit = page_limit, offset = 0))
  total <- first_page$pagination$total
  n_pages <- ceiling(total / page_limit)
  logger$log("  fetching ", total, " players across ", n_pages, " page(s)...")

  pages <- vector("list", n_pages)
  pages[[1]] <- tibble::as_tibble(first_page$data)

  if (n_pages > 1) {
    for (i in seq(2, n_pages)) {
      Sys.sleep(throttle_sec)
      offset <- (i - 1) * page_limit
      page <- bbs_get("/players", query = list(sport = "basketball", league = "nba", limit = page_limit, offset = offset))
      pages[[i]] <- tibble::as_tibble(page$data)
      if (i %% 10 == 0 || i == n_pages) logger$log("  ...page ", i, "/", n_pages)
    }
  }

  dplyr::bind_rows(pages)
}

# ------------------------------------------------------------
# Team bridge table.
# ------------------------------------------------------------
build_team_mapping <- function(cfg, logger) {
  bbs_teams <- fetch_bbs_teams() %>%
    dplyr::filter(short_name != "TBD") %>%   # a placeholder "team" bigballsdata uses for unscheduled playoff slots
    dplyr::mutate(match_key = normalize_name(name))

  our_teams <- read_parquet_or_null(cfg$path_teams_raw)
  if (is.null(our_teams)) {
    logger$log("Bigballsdata team mapping: SKIPPED, no teams_raw.parquet yet.")
    return(invisible(NULL))
  }
  our_teams <- our_teams %>% dplyr::mutate(match_key = normalize_name(hr_team_name_full))

  matched <- dplyr::inner_join(our_teams, bbs_teams, by = "match_key")

  n_our_unmatched <- nrow(our_teams) - nrow(matched)
  n_bbs_unmatched <- nrow(bbs_teams) - nrow(matched)

  mapping <- matched %>%
    dplyr::transmute(
      team_id           = as.character(hr_team_id),
      team_abbreviation = hr_team_abbreviation,
      team_name_full    = hr_team_name_full,
      bbs_team_id       = id,
      bbs_short_name    = short_name
    )

  write_parquet(mapping, cfg$path_team_id_mapping)
  logger$log("  ", cfg$path_team_id_mapping, " written (", nrow(mapping), " teams matched",
             if (n_our_unmatched > 0) paste0(", ", n_our_unmatched, " of ours unmatched") else "",
             if (n_bbs_unmatched > 0) paste0(", ", n_bbs_unmatched, " of theirs unmatched") else "",
             ")")
  mapping
}

# ------------------------------------------------------------
# Player bridge table. Same-name collisions (a normalized name that
# maps to more than one distinct person on EITHER side - e.g. two
# players sharing a name across NBA history) are excluded from the
# match rather than guessed at, and counted separately so they're
# visible without silently producing a wrong link.
# ------------------------------------------------------------
build_player_mapping <- function(cfg, logger) {
  logger$log("Bigballsdata player mapping: fetching all players (paginated)...")
  bbs_players <- fetch_bbs_players(logger, throttle_sec = cfg$throttle_bbs_sec) %>%
    dplyr::mutate(match_key = normalize_name(name))

  our_players <- read_parquet_or_null(cfg$path_players_raw)
  if (is.null(our_players)) {
    logger$log("Bigballsdata player mapping: SKIPPED, no players_raw.parquet yet.")
    return(invisible(NULL))
  }
  our_players <- our_players %>% dplyr::mutate(match_key = normalize_name(display_first_last))

  our_dupe_keys <- our_players %>% dplyr::count(match_key) %>% dplyr::filter(n > 1) %>% dplyr::pull(match_key)
  bbs_dupe_keys <- bbs_players %>% dplyr::count(match_key) %>% dplyr::filter(n > 1) %>% dplyr::pull(match_key)
  ambiguous_keys <- union(our_dupe_keys, bbs_dupe_keys)

  our_safe <- our_players %>% dplyr::filter(!match_key %in% ambiguous_keys)
  bbs_safe <- bbs_players %>% dplyr::filter(!match_key %in% ambiguous_keys)

  matched <- dplyr::inner_join(our_safe, bbs_safe, by = "match_key")

  n_ambiguous     <- length(ambiguous_keys)
  n_our_unmatched <- nrow(our_safe) - nrow(matched)
  n_bbs_unmatched <- nrow(bbs_safe) - nrow(matched)

  mapping <- matched %>%
    dplyr::transmute(
      player_id      = as.character(person_id),
      player_name    = display_first_last,
      bbs_player_id  = id,
      bbs_player_name = name,
      bbs_team_id    = team_id
    )

  write_parquet(mapping, cfg$path_player_id_mapping)
  logger$log("  ", cfg$path_player_id_mapping, " written (", nrow(mapping), " players matched, ",
             n_ambiguous, " ambiguous name(s) excluded, ",
             n_our_unmatched, " of ours unmatched, ", n_bbs_unmatched, " of theirs unmatched)")
  mapping
}

# ------------------------------------------------------------
# Orchestration entry point.
# ------------------------------------------------------------
refresh_bigballsdata_mapping <- function(cfg, logger) {
  if (!isTRUE(cfg$bbs_mapping_enabled)) {
    logger$log("Bigballsdata mapping: SKIPPED (cfg$bbs_mapping_enabled = FALSE).")
    return(invisible(NULL))
  }

  if (file.exists(".env")) readRenviron(".env")
  if (Sys.getenv("BBS_API_KEY") == "") {
    logger$log("Bigballsdata mapping: SKIPPED, BBS_API_KEY not set in .env (optional integration).")
    return(invisible(NULL))
  }

  team_result <- try(build_team_mapping(cfg, logger), silent = TRUE)
  if (inherits(team_result, "try-error")) {
    logger$log("  Team mapping FAILED: ", conditionMessage(attr(team_result, "condition")))
    team_result <- NULL
  }

  player_result <- try(build_player_mapping(cfg, logger), silent = TRUE)
  if (inherits(player_result, "try-error")) {
    logger$log("  Player mapping FAILED: ", conditionMessage(attr(player_result, "condition")))
    player_result <- NULL
  }

  invisible(list(teams = team_result, players = player_result))
}
