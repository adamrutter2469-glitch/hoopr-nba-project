# hoopr-nba-project

Incremental NBA data pipeline built on [`hoopR`](https://hoopr.sportsdataverse.org/) (NBA Stats API).
One command pulls whatever games have happened since the last run and rebuilds two
analysis-ready tables: comprehensive team-game stats and comprehensive player-game stats,
each including rest days, back-to-back/3-in-4 flags, travel (flight time + timezone shift),
and trailing rolling averages.

## Quick start

```bash
Rscript scripts/00_install_packages.R   # first time only, or after pulling new deps
run_pipeline.bat                        # pulls anything new and rebuilds the feature tables
```

`run_pipeline.bat` is safe to run as often as you like (daily, after every night's games, etc.) -
everything is incremental. Pass `--skip-rebounding`, `--skip-player-rebounding`,
`--skip-r2-sync`, `--skip-bbs-mapping`, `--skip-bbs-odds`, and/or `--skip-playbyplay` to skip any
of the slower/optional stages for a single run: `run_pipeline.bat --skip-rebounding`.

### Cloud backup (Cloudflare R2)

Every run ends by mirroring `data_raw/` and `data_processed/` to a private R2 bucket
(`R/sync_r2.R`, via [rclone](https://rclone.org/)) - mainly so the rebounding pulls (hours of
throttled API calls) never have to be re-run from scratch just because the local copy was lost.
Requires `R2_ACCOUNT_ID`, `R2_ACCESS_KEY_ID`, `R2_SECRET_ACCESS_KEY`, `R2_BUCKET_NAME` in `.env`
(see `.env.example`) - the stage silently skips itself if any are missing, so it's opt-in.

**Safety cap**: before any upload, the stage checks the *local* size of `data_raw/` +
`data_processed/` against `cfg$r2_max_storage_gb` (default 9, a 1 GB buffer under R2's 10 GB
free tier) and refuses to sync anything at all if that's met or exceeded - loudly, as a failed
stage, not a silent skip. `rclone sync` mirrors rather than accumulates, so remote usage tracks
local usage; the cap exists so a future bug that caused some table to balloon in row count can't
turn into a surprise storage bill. `cfg$r2_warn_storage_gb` (default 5) logs a warning before
that point.

### Odds/props data (Big Balls Sports Data)

`R/pull_bigballsdata_mapping.R` builds two bridge tables against [bigballsdata.com](https://bigballsdata.com)
(free tier: 1,000 req/day) - `data_processed/team_id_mapping.parquet` and
`player_id_mapping.parquet` - so a future odds/props pull from that API can join back to our own
NBA-Stats-sourced ids. Neither source shares a key with ours (their ids are UUIDs; their team
abbreviations differ from NBA.com's tricode for 5 franchises), so both are matched by normalized
name rather than id, with same-name collisions on either side excluded rather than guessed at.
Requires `BBS_API_KEY` in `.env` - skips itself if it's not set, same as the R2 sync.

### Play-by-play

`R/pull_playbyplay.R` pulls event-level play-by-play (every shot/foul/turnover/rebound, with
shot coordinates, the players involved, and game-clock context) via `hoopR::load_nba_pbp()` - a
bulk per-season download from sportsdataverse's own hosted release files, not the live NBA Stats
API. It's the practical choice right now: the NBA-Stats-native PBP and shot-location endpoints
(`nba_pbp()`, `nba_data_pbp()`, `nba_leaguedashplayershotlocations()`, `nba_shotchartdetail()`)
are all currently returning errors/blocked on NBA's side.

This data is ESPN-sourced, so it carries ESPN's own `game_id` and team abbreviations, not this
project's `game_id_nba`. `game_id_nba` is bridged in deterministically by matching
`(game_date, home team abbrev, away team abbrev)` against the schedule - a team plays a given
opponent at most once per date, so this key is unambiguous (no fuzzy matching needed, unlike the
bigballsdata bridge tables above). ~99.9% of games match; the rare miss is typically an
All-Star/exhibition game with no NBA Stats regular-season/playoff counterpart. ESPN's team
abbreviations differ from NBA.com's tricode for 6 franchises (`NY`/`GS`/`SA`/`UTAH`/`WSH`/`NO`
vs. our `NYK`/`GSW`/`SAS`/`UTA`/`WAS`/`NOP`) - see `ESPN_ABBREV_FIX` in `R/pull_playbyplay.R`.

Same problem exists at the *player* level: play-by-play events reference ESPN's own
`athlete_id`/`athlete_name` (in `athlete_id_1/2/3`), not our `player_id`. Bridged the same way
as the bigballsdata mapping - normalized-name matching, same-name collisions on either side
excluded rather than guessed at - into `data_processed/espn_player_id_mapping.parquet`
(`build_espn_player_id_mapping()` in `R/pull_playbyplay.R`). Verified empirically before
building: zero name collisions at any grain (team+game, team+season, or fully global) within
this project's season scope, though the exclusion logic stays in as a safety net regardless.

### Play-by-play event mining

`R/features_playbyplay_events.R` mines play_by_play's `type_text` + athlete roles into
player-game grain detail tables - `data_processed/player_turnover_features.parquet` (turnover
subtype: bad pass, lost ball, offensive foul, traveling, ... plus steals forced) and
`player_block_features.parquet` (blocks by shot type: dunk/layup/hook/tip/jump_shot, plus
`fg_blocked_*` - how often a player's own shot gets blocked, which the box score never tracks
at all). Both use a **long-format** intermediate representation - each relevant play expands
into one row per involved player rather than staying one row per play with separate
`athlete_1`/`athlete_2` columns - so the final aggregate is a single `group_by` + count, and it
generalizes to future event types with 3 actors without any new join logic.

The athlete-role convention is **type-specific, not uniform** - `athlete_1` means "who
committed it" for turnovers but "the shooter" (not the blocker!) for blocked shots - discovered
empirically before building anything, not assumed. Blocks in particular have no dedicated
`type_text` at all (a blocked shot just carries its normal shot type); detection is a
structural rule instead - a missed shot with a 2nd player attached can only be a block, since
assists only ever attach to made shots.

Both tables are validated every run against `player_game_logs`' own trusted totals
(`tov`/`blk`) - turnovers land at 92.83% exact match (the gap is a real, structural ESPN
limitation: 5 turnover types, like shot-clock violations, never name a player at all in ESPN's
feed even though the NBA box score credits someone - not a bug), blocks at 99.6%.

A third table, `player_foul_features.parquet`, adds foul detail the box score can never
provide at all: not just *how many* fouls (subtype breakdown, same pattern as turnovers/
blocks) but *when* - `pf_q1`..`pf_q4`/`pf_ot` (period-bucketed counts) and
`elapsed_seconds_at_pf_2`/`_3`/`_6` (the exact game-clock moment - continuous across
regulation and OT - a player reached that many fouls, `NA` if they never did). Which
`type_text` values actually count toward the real 6-foul disqualification limit was decided
empirically, not from rulebook memory: tested 3 candidate sets against
`player_game_logs.pf` directly - base foul types + Flagrant Fouls won (96.05% exact match);
Technical Fouls (even "double" ones) don't count and are excluded. Also caught and handled:
`"Offensive Foul"` and `"Offensive Foul Turnover"` are the *same* real event logged as two
separate rows (100% overlap verified) - only one is counted here to avoid double-counting,
since the other already lives in the turnover table.

## What you get

- `data_processed/team_game_features.parquet` - one row per team per game: box score, rest/travel
  context, trailing 5/10/20-game rolling averages, opponent/matchup features (what the opponent's
  own rolling form looks like, what each team typically allows, explicit comparison columns), and
  (if enabled) advanced rebounding splits. 269 columns - see `python/README.md` for the naming
  convention (`_rollN` / `opp_*` / `*_allowed` / `*_matchup_edge`).
- `data_processed/player_game_features.parquet` - one row per player per game: box score, trailing
  rolling averages, and their team's rest/travel context for that game.

Read either with `arrow::read_parquet("data_processed/team_game_features.parquet")` in R (or
`arrow::open_dataset()` for the season-partitioned raw tables under `data_raw/` - see below), or
from Python/PyCharm via `python/` - see `python/README.md`.

For what every column means, see [`docs/data_dictionary/`](docs/data_dictionary/README.md) -
one CSV per table, auto-generated from the live schema (`Rscript scripts/build_data_dictionary.R`),
including which columns are safe pre-game predictors vs. that game's actual result.

## Layout

```
config/config.R          settings: season range, paths, API throttle, feature toggles
R/                        function library, one file per concern (see header comments)
scripts/
  00_install_packages.R   installs required packages
  run_pipeline.R           orchestrator - sources R/, runs each stage, logs to logs/
  wip_injuries.R           unfinished injury scraper, not yet wired into the pipeline
data_raw/
  schedule/                season-partitioned parquet dataset (season=2022-23/part-0.parquet, ...)
  team_game_logs/          season-partitioned parquet dataset
  player_game_logs/        season-partitioned parquet dataset
  play_by_play/            season-partitioned parquet dataset - event-level, ESPN-sourced (see above)
  players_raw.parquet, teams_raw.parquet     single files, fully refreshed each run
  team_rebounding_dashboards.rds             raw nested API cache - stays RDS, see below
  player_rebounding_dashboards.rds           same, player grain - scoped down by default,
                                              see cfg$player_rebounding_seasons/_min_minutes
  external/                 static reference data (travel-time CSV) - tracked in git
data_processed/            joined, feature-engineered parquet output (git-ignored)
state/manifest.json        what's already been pulled (git-ignored, rebuilds automatically)
logs/                      one timestamped log per run (git-ignored)
legacy/                    the pre-redesign scripts/functions/data, kept for reference
models/
  train_rebounds_model.R   REB prediction (elastic net + xgboost) - separate, user-run, not
                           wired into run_pipeline.bat. Trained model files are git-ignored.
python/                    read-only exploration from PyCharm/Python - see python/README.md
docs/data_dictionary/      one CSV per table describing every column - see its own README.md
streamlit_app/             UI, reading straight from R2 (not local files) - see its own README.md.
                           Currently just a connection smoke test (displays the 30 teams); the
                           real dashboard comes once the analysis layer exists.
notes/                     unchanged from before the redesign
```

Data under `data_raw/` and `data_processed/` (except `data_raw/external/`), plus `state/`
and `logs/`, is git-ignored - it's regenerated by the pipeline, not versioned. Run
`00_install_packages.R` then `run_pipeline.bat` on a fresh clone to build it locally (first
run backfills from the 2022-23 season, see `config/config.R`).

### Why parquet, and why one cache is still RDS

Everything is parquet except `data_raw/team_rebounding_dashboards.rds`, which caches the
*raw* advanced-rebounding API response (several nested sub-tables per team-game) - parquet
is a flat/columnar format and doesn't represent that kind of nested blob, so that one file
stays RDS. Its *parsed*, flat output (`data_processed/team_rebounding_features.parquet`) is
parquet like everything else.

`schedule/`, `team_game_logs/`, and `player_game_logs/` are Hive-style directories
partitioned by season (`season=2022-23/part-0.parquet`) rather than single files. Every
pipeline run only touches the season(s) it actually pulled - historical seasons' files are
never re-read or rewritten - so a routine run's cost stays proportional to what changed,
not to how much history has piled up. At current data volumes (~2MB/season for the full
player-game table) this data would stay small for decades even as plain files, but
partitioning is what keeps *write* cost flat as history grows, and parquet's columnar
compression + interoperability (queryable from Python/duckdb/Polars, not just R) are why
it's the format going forward instead of RDS.

## How incremental pulls work

`state/manifest.json` plus each dataset's own season partitions record what's already been
pulled. Every run: reference data (players/teams) is cheaply refreshed in full; the schedule
and game logs pull any season not yet loaded plus always re-pull the current season (since it
keeps changing) - each pulled season is deduped and written to just its own partition; rest/
travel and rolling-average features are cheap local recomputes; advanced rebounding (both
`R/pull_rebounding.R` at team grain and `R/pull_player_rebounding.R` at player grain) only
pulls game pairs it doesn't already have, since that's the one stage with no bulk API endpoint.
Play-by-play (`R/pull_playbyplay.R`) is a bulk per-season pull like the schedule/game logs - same
"pull any season not yet loaded, always re-pull the current season" pattern, and fast (seconds
per season, not the hours the per-game rebounding pulls take) since it isn't throttled against a
rate-limited API.

On any key collision during a re-pull, the freshly-pulled row always wins over whatever was
cached (see `combine_and_dedupe()` in `R/utils_io.R`) - so a stat correction the API issues
after the fact, or a game whose final box score wasn't available on an earlier run, gets
picked up and overwritten rather than getting stuck on a stale value forever.

### Only finalized games are ingested

`data_raw/schedule` carries the NBA Stats API's own game status (`game_status`: 1 = scheduled,
2 = live, 3 = final; see `is_final` = `game_status == 3`) - and it's the *schedule* table, so it
deliberately includes every game in a season, played or not, which is what lets you look up
tonight's games ahead of time (`schedule %>% filter(game_date == today, !is_final)`) even though
box scores for them don't exist yet.

Box scores are the opposite: `R/pull_game_logs.R` cross-checks every pulled team/player row
against that season's schedule and drops anything not marked `is_final`, regardless of whether
the underlying box-score endpoint (`hoopR::nba_leaguegamelog()`) would have included it anyway -
this makes "only ingest finalized games" true by construction rather than an assumption about
API behavior. The advanced-rebounding stages only ever pull for `game_id`s already present in
the (already-filtered) game logs, so they inherit the same guarantee automatically.

See `notes/Ideas` for the open research question this pipeline was built to make easy to
answer: does team performance dip below its rolling average with less rest or more travel?
