# ============================================================
# Script: 90_explore_document_hoopR_functions
# Purpose: Explore what's available in hoopR natively. Take notes on data returned and use cases.
# Output:  NA
# ============================================================

# Format
# ============================================================
# function: function_name
# Purpose: what does it do
# Output:  relevant details, col names, output type, etc.
# ============================================================
# Examples

library(dplyr)
library(purrr)
library(janitor)
library(readr)
library(tidyr)
library(lubridate)
library(hoopR)


# ============================================================
# function: nba_playergamestreakfinder
# Purpose: identify player streaks in any nba season
# Output:  output$PlayerGameStreakFinderResults is a dataframe
# returns player id, startdate, enddate, activestreak (1/0), 
# pts (e.g. total pts in streak, total stat), pts_pg (e.g. pts per game in streak, per game stat rate)
# ============================================================
# For Precious Achuiwa, return streaks where he's had >= 9 pts per game
df <- nba_playergamestreakfinder(
  player_id = 1630173,
  season = "2025-26",
  season_type = "Regular Season",
  gt_pts = 9
)

x=df$PlayerGameStreakFinderResults

# For 2025-26 nba season, return streaks where any player has >= 35 pts per game
df2 <- nba_playergamestreakfinder(
  season = "2025-26",
  season_type = "Regular Season",
  gt_pts = 35
)

y=df2$PlayerGameStreakFinderResults

# ============================================================
# function: load_nba_pbp
# Purpose: returns details play by play data for each game in a season (~600k records per season)
# Output:  too much detail to specify. court coordinates, players involved, etc.
# NOTE: would require a manually generated game_id to map to other data (only espn game ID available)
# ============================================================

raw <- load_nba_pbp(seasons = 2025)

### Player Dash

# ============================================================
# function: nba_playerdashptreb
# Purpose: returns several different tables - all of which provide details around a players reb, see below
# Output:  see below
# ============================================================
# Example: Precious Achuiwa. Note season is 2024-25 format

# Returns List of 5 tibbles: 
test = nba_playerdashptreb(player_id = 1630173, date_from = "01-01-2026", date_to = "01-11-2026")
test2 = nba_playerdashboardbyteamperformance(player_id = 1630173, date_from = "01-01-2026", date_to = "01-11-2026")
test3 = nba_playerdashboardbylastngames(player_id = 1630173, date_from = "01-01-2026", date_to = "01-11-2026")
# 1. Shot Type Rebounding: 
shot_type_reb = test$ShotTypeRebounding
playerdashboardbyteamperformance_test1 = test2$PontsAgainstPlayerDashboard
nba_playerdashboardbygamesplits_test = test3$OverallPlayerDashboard
colnames(nba_playerdashboardbygamesplits_test)
# Reb Freq is the % of rebounds player collects that they have a chance at (within 3.5 ft)
# C_ or UC_ references the nature of the rebound, contested or uncontested
# the percentages for each tell you, for the shot type, 

# 2. rebounding splits by 0, 1, 2+ players competing for rebound
contested_reb = test$NumContestedRebounding

#3. split by distance shot taken from (more detailed version of shot type)
reb_by_shot_dist = test$ShotDistanceRebounding

#4. Rebounds by distance from basket
reb_distance = test$RebDistanceRebounding


### Team Dash

# ============================================================
# function: nba_teamdashptreb
# Purpose: returns several different tables - all of which provide details around a teams reb, see below
# Output:  see below (mirrors player structure)
# ============================================================
# Example: MIL. Note season is 2024-25 format, default current season
nba_team_reb_dash = nba_teamdashptreb(team_id = "1610612749",date_from = "01-01-2026", date_to = "01-11-2026")

team_reb = nba_team_reb_dash$OverallRebounding
View(team_reb)

### to compute opponent averages for advanced rebounding stats, you need to query each possible
# opponent and aggregate performance. can build a function to do this
reb_opp_example = nba_teamdashptreb(
  team_id = "1610612737",              # Hawks
  opponent_team_id = "1610612749",     # Spurs
  season = "2023-24",
  season_type = "Regular Season"
)
View(reb_opp_example$ nbRebDistanceRebounding)

### Lineups
## use start position to determine the starters
box <- hoopR::nba_boxscoretraditionalv2(game_id = "0022300001")$PlayerStats
View(box)

rolling_simple_player_stat_test = rolling_simple_player_stat(schedule_with_travel_per_team, player_id = 1630173, game_id = "0022500234", n_games = 3, stat_col = "REB")

print(rolling_simple_player_stat_test)
playerstats = nba_boxscoretraditionalv2(game_id = "0022300001")
playerstats_player = playerstats$PlayerStats


testing = nba_boxscoreadvancedv3(game_id = "0052400101")
check = testing$home_team_player_advanced



n = nba_boxscoretraditionalv3(game_id = "0052400101")

check = n$home_team_player_traditional
check_awa = n$away_team_player_traditional
colnames(check)
colnames(check_awa)
type(check$team_id)
type(check_awa$team_id)
