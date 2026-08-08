# ============================================================
# Script: R/utils_season.R
# Purpose: Season-string helpers shared by every pull/feature
#          stage. "Season" is always the NBA convention string
#          e.g. "2022-23", never a bare year.
# ============================================================

# Convert numeric year (2022) -> "2022-23". Passes strings through
# unchanged so it's safe to call on values that are already formatted.
normalize_season <- function(x) {
  x_chr <- as.character(x)
  if (suppressWarnings(all(!is.na(as.numeric(x_chr))))) {
    yr <- as.integer(x_chr)
    paste0(yr, "-", substr(yr + 1, 3, 4))
  } else {
    x_chr
  }
}

# Extract the starting year from "YYYY-YY"
season_start_year <- function(season_string) {
  as.integer(substr(season_string, 1, 4))
}

# ------------------------------------------------------------
# Determine the current NBA season from a calendar date.
# NBA seasons start in October, so Jan-Sep belongs to the season
# that started the previous October.
# ------------------------------------------------------------
current_season <- function(as_of_date = Sys.Date()) {
  yr <- as.integer(format(as_of_date, "%Y"))
  mo <- as.integer(format(as_of_date, "%m"))
  start_yr <- if (mo >= 10) yr else yr - 1
  normalize_season(start_yr)
}

# ------------------------------------------------------------
# Build the full season sequence the pipeline should know about,
# from cfg$first_season through the current season (inclusive).
# ------------------------------------------------------------
season_sequence <- function(first_season, last_season = current_season()) {
  first_yr <- season_start_year(normalize_season(first_season))
  last_yr  <- season_start_year(normalize_season(last_season))
  if (last_yr < first_yr) return(character(0))
  normalize_season(first_yr:last_yr)
}

# ------------------------------------------------------------
# Determine which seasons need to be (re-)pulled this run.
# refresh_current = TRUE always includes the most recent season
# even if it's already present, since an in-progress season's
# schedule/games keep changing.
# ------------------------------------------------------------
compute_seasons_to_pull <- function(all_seasons, existing_seasons, refresh_current = TRUE) {
  if (refresh_current) {
    current <- all_seasons[which.max(season_start_year(all_seasons))]
    union(
      setdiff(all_seasons, existing_seasons),
      current
    ) %>% sort()
  } else {
    setdiff(all_seasons, existing_seasons) %>% sort()
  }
}
