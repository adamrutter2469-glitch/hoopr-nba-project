"""
Starter exploration script. Run it directly, or open it in PyCharm and
run cell-by-cell (# %% cells work with PyCharm's Scientific Mode), or
import load_data in a PyCharm Python Console and poke around live.
"""
import pandas as pd

from load_data import load_team_game_features, load_player_game_features

pd.set_option("display.width", 160)
pd.set_option("display.max_columns", 20)


def main():
    team = load_team_game_features()
    player = load_player_game_features()

    print(f"team_game_features:   {team.shape[0]:,} rows x {team.shape[1]} cols")
    print(f"player_game_features: {player.shape[0]:,} rows x {player.shape[1]} cols")

    print("\n--- team_game_features: first 30 columns (269 total - see README) ---")
    print(list(team.columns[:30]))

    print("\n--- games per season ---")
    print(team.groupby("season")["game_id_nba"].nunique().sort_index())

    # Recreates the "does performance dip below rolling average on a
    # back-to-back?" check from earlier analysis.
    print("\n--- rebounds vs. rolling average, back-to-back vs. not ---")
    sample = team.dropna(subset=["is_b2b", "reb_roll10"]).copy()
    sample["diff_vs_roll10"] = sample["reb"] - sample["reb_roll10"]
    print(sample.groupby("is_b2b")["diff_vs_roll10"].agg(["count", "mean"]).round(2))

    # Most recent rolling rebound average per team - a snapshot of
    # current form.
    print("\n--- top 10 teams by most recent reb_roll10 ---")
    latest = (
        team.dropna(subset=["reb_roll10"])
        .sort_values("game_date")
        .groupby("team_id")
        .tail(1)
        .sort_values("reb_roll10", ascending=False)
        [["team_abbreviation", "game_date", "reb_roll10", "reb_allowed_roll10"]]
        .head(10)
    )
    print(latest.to_string(index=False))

    # Same idea at the player level: current top rebounders by rolling average.
    print("\n--- top 10 players by most recent reb_roll10 ---")
    latest_players = (
        player.dropna(subset=["reb_roll10"])
        .sort_values("game_date")
        .groupby("player_id")
        .tail(1)
        .sort_values("reb_roll10", ascending=False)
        [["player_name", "team_abbreviation", "game_date", "reb_roll10"]]
        .head(10)
    )
    print(latest_players.to_string(index=False))


if __name__ == "__main__":
    main()
