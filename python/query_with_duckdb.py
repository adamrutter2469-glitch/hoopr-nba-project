"""
Query the parquet files directly with SQL via duckdb - no pandas load
step needed, and it reads the season-partitioned directories under
data_raw/ natively via glob patterns. Handy for quick ad hoc questions,
or when a table's too big to comfortably pull fully into memory first.

Every query here returns a pandas DataFrame (`.df()`), so you can mix
this freely with load_data.py / regular pandas code.
"""
from pathlib import Path

import duckdb

PROJECT_ROOT = Path(__file__).resolve().parent.parent
DATA_RAW = PROJECT_ROOT / "data_raw"
DATA_PROCESSED = PROJECT_ROOT / "data_processed"

con = duckdb.connect()


def q(sql: str):
    """Run a SQL string, return a pandas DataFrame."""
    return con.execute(sql).df()


def _pq(*parts) -> str:
    """Build a read_parquet()-ready path string (forward slashes, so
    it works inside a SQL string on Windows too)."""
    return (PROJECT_ROOT.joinpath(*parts)).as_posix()


if __name__ == "__main__":
    team_features = _pq("data_processed", "team_game_features.parquet")

    print("--- row count ---")
    print(q(f"SELECT COUNT(*) AS n FROM read_parquet('{team_features}')"))

    print("\n--- average rebounds, home vs. away ---")
    print(q(f"""
        SELECT home_away, ROUND(AVG(reb), 2) AS avg_reb, COUNT(*) AS n
        FROM read_parquet('{team_features}')
        GROUP BY home_away
    """))

    print("\n--- average rebounds, back-to-back vs. not ---")
    print(q(f"""
        SELECT is_b2b, ROUND(AVG(reb), 2) AS avg_reb, COUNT(*) AS n
        FROM read_parquet('{team_features}')
        WHERE is_b2b IS NOT NULL
        GROUP BY is_b2b
    """))

    # Querying a season-partitioned raw dataset directly - the '**'
    # glob picks up every season=.../part-0.parquet file at once.
    team_logs_glob = _pq("data_raw", "team_game_logs", "**", "*.parquet")
    print("\n--- games per season, straight from the partitioned raw dataset ---")
    print(q(f"""
        SELECT season, COUNT(*) / 2 AS games
        FROM read_parquet('{team_logs_glob}')
        GROUP BY season
        ORDER BY season
    """))
