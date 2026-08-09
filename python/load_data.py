"""
Reusable loaders for the R pipeline's parquet output.

Path convention: this file lives in <project_root>/python/, so
PROJECT_ROOT resolves correctly regardless of where a script is
actually run from - open python/ as a PyCharm source root (or run
scripts from anywhere) and the paths below just work.

Nothing here writes back to the data - read-only exploration only.
Run run_pipeline.bat (from the project root) first if data_processed/
is empty.
"""
from pathlib import Path

import pandas as pd

PROJECT_ROOT = Path(__file__).resolve().parent.parent
DATA_RAW = PROJECT_ROOT / "data_raw"
DATA_PROCESSED = PROJECT_ROOT / "data_processed"


def _read(path: Path) -> pd.DataFrame:
    """Read a single parquet file OR a season-partitioned parquet
    directory (e.g. data_raw/schedule/season=2022-23/part-0.parquet,
    ...) - pandas + pyarrow handle both the same way."""
    if not path.exists():
        raise FileNotFoundError(
            f"{path} not found - run run_pipeline.bat from the project "
            f"root first to build the data."
        )
    return pd.read_parquet(path)


# ---- The two "comprehensive" analysis-ready tables ----------------------

def load_team_game_features() -> pd.DataFrame:
    """One row per team per game: box score, rest/travel, rolling
    averages, opponent/matchup features, advanced rebounding.
    This is almost always the table you want to start with."""
    return _read(DATA_PROCESSED / "team_game_features.parquet")


def load_player_game_features() -> pd.DataFrame:
    """One row per player per game: box score + rolling averages +
    their team's rest/travel context that game."""
    return _read(DATA_PROCESSED / "player_game_features.parquet")


# ---- Other processed tables -----------------------------------------------

def load_team_rebounding_features() -> pd.DataFrame:
    """Parsed advanced rebounding splits (contested, shot-distance),
    one row per team-game. Already joined into team_game_features()
    with an 'adv_' prefix - load this directly only if you want it on
    its own."""
    return _read(DATA_PROCESSED / "team_rebounding_features.parquet")


def load_schedule_with_travel() -> pd.DataFrame:
    """One row per game, home_*/away_* rest and travel columns."""
    return _read(DATA_PROCESSED / "schedule_with_travel_detail.parquet")


# ---- Raw, season-partitioned pulls -----------------------------------------
# The pipeline's own input cache. Usually you want the processed
# tables above instead - these are lower-level (e.g. no rolling
# averages, no opponent context).

def load_schedule() -> pd.DataFrame:
    return _read(DATA_RAW / "schedule")


def load_team_game_logs() -> pd.DataFrame:
    return _read(DATA_RAW / "team_game_logs")


def load_player_game_logs() -> pd.DataFrame:
    return _read(DATA_RAW / "player_game_logs")


def load_players() -> pd.DataFrame:
    return _read(DATA_RAW / "players_raw.parquet")


def load_teams() -> pd.DataFrame:
    return _read(DATA_RAW / "teams_raw.parquet")


if __name__ == "__main__":
    # Quick sanity check when run directly: `python load_data.py`
    for name, fn in [
        ("team_game_features", load_team_game_features),
        ("player_game_features", load_player_game_features),
    ]:
        df = fn()
        print(f"{name}: {df.shape[0]:,} rows x {df.shape[1]} cols")
