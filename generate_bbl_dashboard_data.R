####################################################################################
#  Script-file:   generate_bbl_dashboard_data.R
#  Project:       CricketStatsHub
#  Author:        Cooper Denny
#
#  Purpose:       Fetch BBL data for the most recent season and write JSON files
#                 consumed by bbl_dashboard.html.
#
#  Usage:         source("generate_bbl_dashboard_data.R")
#                 # OR from terminal: Rscript generate_bbl_dashboard_data.R
#
#  Output:        bbl_dashboard_data.json  (in the same directory)
####################################################################################

library(tidyverse)
library(jsonlite)

source("fetch_cricket_data.R")

cat("Fetching BBL batting data...\n")
batting_all <- fetch_cricket_data("bbl", type = "batting")

cat("Fetching BBL bowling data...\n")
bowling_all <- fetch_cricket_data("bbl", type = "bowling")

cat("Fetching BBL match data...\n")
matches_all <- fetch_cricket_data("bbl", type = "match")

# ── Identify most recent season ──────────────────────────────────────────────
latest_season <- batting_all |> pull(season) |> unique() |> sort(decreasing = TRUE) |> first()
cat("Most recent season:", latest_season, "\n")

batting  <- batting_all  |> filter(season == latest_season)
bowling  <- bowling_all  |> filter(season == latest_season)
matches  <- matches_all  |> filter(season == latest_season)

# ── Batting leaderboard ──────────────────────────────────────────────────────
# Aggregate across all innings for each batter
batting_leader <- batting |>
  group_by(batter) |>
  summarise(
    team         = first(batting_team),
    innings      = n(),
    runs         = sum(runs),
    balls_faced  = sum(balls_faced),
    fours        = sum(fours),
    sixes        = sum(sixes),
    not_outs     = sum(not_out),
    highest      = max(runs),
    average      = round(runs / max(innings - not_outs, 1), 2),
    strike_rate  = round(runs / balls_faced * 100, 2),
    .groups      = "drop"
  ) |>
  filter(innings >= 3) |>   # min 3 innings for leaderboard
  arrange(desc(runs))

# ── Bowling leaderboard ──────────────────────────────────────────────────────
bowling_leader <- bowling |>
  group_by(bowler) |>
  summarise(
    team          = first(bowling_team),
    innings       = n(),
    balls         = sum(balls),
    runs_conceded = sum(runs_conceded),
    wickets       = sum(wickets),
    wides         = sum(wides_bowled),
    no_balls      = sum(no_balls_bowled),
    best_wickets  = max(wickets),
    economy       = round(runs_conceded / (balls / 6), 2),
    average       = round(runs_conceded / max(wickets, 1), 2),
    .groups       = "drop"
  ) |>
  mutate(overs = paste0(balls %/% 6, ".", balls %% 6)) |>
  filter(innings >= 3) |>
  arrange(desc(wickets), economy)

# ── Match results ────────────────────────────────────────────────────────────
match_results <- matches |>
  select(
    match_id,
    date,
    venue,
    team1,
    team2,
    winner,
    winner_runs,
    winner_wickets,
    player_of_match,
    toss_winner,
    toss_decision
  ) |>
  mutate(
    date       = as.character(date),
    player_of_match = sapply(player_of_match, \(x) paste(x, collapse = ", ")),
    margin     = case_when(
      !is.na(winner_runs)    ~ paste0(winner_runs, " runs"),
      !is.na(winner_wickets) ~ paste0(winner_wickets, " wickets"),
      TRUE                   ~ "N/A"
    )
  ) |>
  arrange(desc(date))

# ── Team standings ───────────────────────────────────────────────────────────
# Derive from match results
team_results <- bind_rows(
  match_results |> mutate(team = team1, won = team1 == winner),
  match_results |> mutate(team = team2, won = team2 == winner)
) |>
  filter(!is.na(winner)) |>
  group_by(team) |>
  summarise(
    played = n(),
    wins   = sum(won),
    losses = played - wins,
    .groups = "drop"
  ) |>
  mutate(win_pct = round(wins / played * 100, 1)) |>
  arrange(desc(wins), desc(win_pct))

# ── Compile and write JSON ───────────────────────────────────────────────────
dashboard_data <- list(
  season         = latest_season,
  generated_at   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  batting        = batting_leader,
  bowling        = bowling_leader,
  matches        = match_results,
  standings      = team_results
)

output_path <- "bbl_dashboard_data.json"
write_json(dashboard_data, output_path, pretty = TRUE, na = "null", auto_unbox = TRUE)
cat("Data written to:", output_path, "\n")
cat("Done! Now open bbl_dashboard.html in your browser.\n")
