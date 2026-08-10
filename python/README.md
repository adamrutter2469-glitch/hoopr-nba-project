# Python exploration scripts

Read-only exploration of the R pipeline's parquet output. Nothing here
writes back to the data. Run `run_pipeline.bat` from the project root
first if `data_processed/` is empty.

## Setup (PyCharm)

1. Open this `python/` folder as a PyCharm project - or open the repo
   root in PyCharm and mark `python/` as a **Sources Root**
   (right-click the folder > Mark Directory as > Sources Root) so
   `import load_data` resolves from any script in this folder.
2. Give it an interpreter: **File > Settings > Project > Python
   Interpreter > Add Interpreter > Add Local Interpreter > venv**,
   pointing at this `python/` folder. Or from a terminal:
   ```
   cd python
   python -m venv .venv
   .venv\Scripts\activate
   pip install -r requirements.txt
   ```
3. Run `explore.py` (right-click > Run) to get oriented, or open a
   PyCharm Python Console and:
   ```python
   from load_data import load_team_game_features
   df = load_team_game_features()
   ```

## Files

- **`load_data.py`** - loader functions for every table the pipeline
  produces. Import from here rather than hardcoding paths - it
  resolves the project root automatically regardless of where a
  script actually runs from.
- **`explore.py`** - a runnable starting point: shape/columns/seasons,
  the back-to-back rebounding dip, top teams and players by rolling
  average.
- **`query_with_duckdb.py`** - the same kind of exploration via SQL
  instead of pandas, including querying the season-partitioned raw
  datasets under `data_raw/` directly without loading them fully.
- **`data_dictionary.py`** - explore `docs/data_dictionary/*.csv` (what
  the R side generates - see that folder's own README for the full
  column reference) instead of hardcoding column knowledge here:
  ```python
  from data_dictionary import list_tables, list_fields

  list_tables()                          # every table + description
  list_fields("team_game_features")      # every field in one table
  list_fields("team_game_features", leakage_risk="UNSAFE")   # just the columns
  list_fields("team_game_features", feature_family="matchup")  # unsafe as a same-game predictor
  ```

## Table reference

| Function | File | Grain |
|---|---|---|
| `load_team_game_features()` | `data_processed/team_game_features.parquet` | 1 row/team-game - **start here.** Box score, rest/travel, rolling averages, opponent/matchup features, advanced rebounding. 278 columns. |
| `load_player_game_features()` | `data_processed/player_game_features.parquet` | 1 row/player-game - box score + rolling averages. |
| `load_team_rebounding_features()` | `data_processed/team_rebounding_features.parquet` | 1 row/team-game - parsed advanced rebounding splits on their own (already joined into the table above with an `adv_` prefix). |
| `load_schedule_with_travel()` | `data_processed/schedule_with_travel_detail.parquet` | 1 row/game. |
| `load_schedule()`, `load_team_game_logs()`, `load_player_game_logs()`, `load_players()`, `load_teams()` | `data_raw/*` | the pipeline's own raw input cache - usually you want the processed tables above instead. |
| `load_playbyplay(season=None, game_id_nba=None)` | `data_raw/play_by_play/` | 1 row/play - event-level, ESPN-sourced. 2.5M+ rows total - pass `season` and/or `game_id_nba` to filter (pushed down at read time) unless you want everything at once. `game_id_nba` is bridged in from the schedule; ESPN's own game id is kept as `game_id_espn`. |

Column-name reference for `team_game_features` (see the main repo
README for how these are built): `<stat>_rollN` = this team's own
trailing N-game average; `opp_<stat>_rollN` = the opponent's own
trailing average; `<stat>_allowed_rollN` = this team's history of what
opponents have scored against them; `opp_<stat>_allowed_rollN` = what
the opponent typically allows; `<stat>_matchup_edge_roll10` = the
explicit comparison between the two. Raw (non-`_roll`) columns like
`reb`, `oreb`, `adv_reb` are that exact game's actual result - useful
as a label/target, never as a predictive feature for that same game.
