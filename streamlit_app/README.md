# streamlit_app

A minimal Streamlit app whose only job is to prove the R2 connection pattern works -
`app.py` reads `data_raw/teams_raw.parquet` straight out of the Cloudflare R2 bucket via
DuckDB's S3-compatible reader and displays the 30 teams in a table. This is groundwork,
not the real dashboard - once the analysis layer (predictions, matchup reports, season
summaries - see the project root README.md) exists, the same `read_parquet('s3://...')`
pattern points at those tables instead.

## Setup

```bash
cd streamlit_app
pip install -r requirements.txt
streamlit run app.py
```

Needs `.streamlit/secrets.toml` (gitignored, already present locally) with your R2
credentials - copy `.streamlit/secrets.toml.example` if setting this up somewhere new:

```toml
[r2]
account_id = "..."
access_key_id = "..."
secret_access_key = "..."
bucket = "..."
```

**Security note**: the credentials in `.streamlit/secrets.toml` right now are the same
read-write ones `R/sync_r2.R` uses to write to the bucket. That's fine for local
development, but before deploying this anywhere public (Streamlit Community Cloud, etc.),
generate a **separate, read-only R2 API token** scoped to just this bucket and use that
instead - a deployed app only ever needs to read.

## How it works

- `get_connection()` (`@st.cache_resource`): one DuckDB connection per session, configured
  with R2's endpoint/credentials via `SET s3_*` pragmas. `s3_url_style='path'` and
  `s3_region='auto'` are both required specifically for R2 (it isn't AWS S3, even though
  the API is compatible).
- `load_teams()` (`@st.cache_data(ttl=3600)`): the actual query, cached for an hour so
  Streamlit doesn't re-fetch from R2 on every rerun (every widget interaction re-runs the
  whole script by default - without caching this would hit R2 constantly).
- Everything else is just `st.dataframe()` displaying the result.

## Extending this

To pull a different table, change the `read_parquet()` path in `load_teams()` (or add a
new cached function) - e.g. `s3://<bucket>/data_processed/team_game_features.parquet` for
the full feature table. Same pattern, same caching approach.
