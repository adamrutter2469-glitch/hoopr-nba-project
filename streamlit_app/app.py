"""
Minimal Streamlit + R2 connectivity sample.

Purpose: establish the R2 read pattern this project will build on -
NOT the real analysis UI yet (that comes once the analysis layer is
built - see the project root README.md). Displays the 30 NBA teams
straight from data_raw/teams_raw.parquet in the R2 bucket, as a smoke
test that credentials/connectivity/caching all work end to end.

Run from this folder (so .streamlit/secrets.toml is found):
    streamlit run app.py
"""
import duckdb
import pandas as pd
import streamlit as st

st.set_page_config(page_title="NBA Data - R2 Connection Test", page_icon="🏀")


@st.cache_resource
def get_connection() -> duckdb.DuckDBPyConnection:
    """One DuckDB connection per Streamlit session, configured to talk
    to the R2 bucket over its S3-compatible API. @st.cache_resource
    (not @st.cache_data) because a database connection is a live
    resource to reuse across reruns, not data to be pickled."""
    con = duckdb.connect()
    con.sql("INSTALL httpfs; LOAD httpfs;")
    r2 = st.secrets["r2"]
    con.sql(f"""
        SET s3_endpoint='{r2["account_id"]}.r2.cloudflarestorage.com';
        SET s3_access_key_id='{r2["access_key_id"]}';
        SET s3_secret_access_key='{r2["secret_access_key"]}';
        SET s3_region='auto';
        SET s3_url_style='path';
    """)
    return con


@st.cache_data(ttl=3600)
def load_teams() -> pd.DataFrame:
    """Cached for an hour - team reference data barely changes, no
    reason to re-fetch from R2 on every rerun. To pull any other
    table in the bucket, swap the read_parquet() path below - same
    pattern works for data_processed/team_game_features.parquet,
    the eventual analysis-layer tables, etc."""
    con = get_connection()
    bucket = st.secrets["r2"]["bucket"]
    return con.sql(f"""
        SELECT
            hr_team_id AS team_id,
            hr_team_city AS city,
            hr_team_name AS name,
            hr_team_abbreviation AS abbreviation,
            hr_conference AS conference,
            hr_division AS division
        FROM read_parquet('s3://{bucket}/data_raw/teams_raw.parquet')
        ORDER BY hr_conference, hr_division, hr_team_city
    """).df()


st.title("🏀 NBA Data - R2 Connection Test")
st.caption(
    "A minimal sample confirming Streamlit can read the pipeline's data straight out of "
    "Cloudflare R2. This isn't the real dashboard yet - just the connection groundwork."
)

with st.spinner("Reading teams_raw.parquet from R2..."):
    teams = load_teams()

st.success(
    f"Connected - loaded {len(teams)} teams from "
    f"`r2://{st.secrets['r2']['bucket']}/data_raw/teams_raw.parquet`"
)

st.dataframe(teams, width="stretch", hide_index=True)

st.divider()
st.caption(
    "Next step: once the analysis layer (predictions, matchup reports, season summaries) "
    "is built, point this same pattern - read_parquet('s3://.../data_processed/<table>.parquet') "
    "- at those tables instead."
)
