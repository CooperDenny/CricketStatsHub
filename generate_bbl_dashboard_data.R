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
# cricketdata 0.3.0 can fail to parse Cricsheet's match files. Batting and
# bowling come from ball-by-ball data and don't depend on this, so if it fails
# keep the previous run's results and standings for the same season.
matches_all <- tryCatch(
  fetch_cricket_data("bbl", type = "match"),
  error = function(e) {
    warning("Match data fetch failed (", conditionMessage(e), ")", call. = FALSE)
    NULL
  }
)

# ── Identify most recent season ──────────────────────────────────────────────
latest_season <- batting_all |> pull(season) |> unique() |> sort(decreasing = TRUE) |> first()
cat("Most recent season:", latest_season, "\n")

batting  <- batting_all  |> filter(season == latest_season)
bowling  <- bowling_all  |> filter(season == latest_season)

# ── Batting leaderboard ──────────────────────────────────────────────────────
# Aggregate across all innings for each batter. Per-innings maxima (highest,
# best figures) are computed before the totals: inside summarise(), a later
# expression sees the already-summarised column, so max(runs) after
# runs = sum(runs) would return the season total.
batting_leader <- batting |>
  group_by(batter) |>
  summarise(
    team         = first(batting_team),
    innings      = n(),
    highest      = max(runs),
    highest_not_out = any(runs == max(runs) & not_out),
    not_outs     = sum(not_out),
    runs         = sum(runs),
    balls_faced  = sum(balls_faced),
    fours        = sum(fours),
    sixes        = sum(sixes),
    .groups      = "drop"
  ) |>
  mutate(
    # Batting average is undefined when a batter was never dismissed
    average     = if_else(innings > not_outs, round(runs / (innings - not_outs), 2), NA_real_),
    strike_rate = if_else(balls_faced > 0, round(runs / balls_faced * 100, 2), NA_real_),
    highest_label = paste0(highest, if_else(highest_not_out, "*", ""))
  ) |>
  filter(innings >= 3) |>   # min 3 innings for leaderboard
  arrange(desc(runs))

# ── Bowling leaderboard ──────────────────────────────────────────────────────
# Best figures: most wickets in an innings, fewest runs as the tie-break
best_figures <- bowling |>
  arrange(bowler, desc(wickets), runs_conceded) |>
  group_by(bowler) |>
  slice(1) |>
  ungroup() |>
  transmute(bowler, best_wickets = wickets, best_runs = runs_conceded,
            best = paste0(wickets, "/", runs_conceded))

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
    .groups       = "drop"
  ) |>
  left_join(best_figures, by = "bowler") |>
  mutate(
    overs   = paste0(balls %/% 6, ".", balls %% 6),
    economy = if_else(balls > 0, round(runs_conceded / (balls / 6), 2), NA_real_),
    # Bowling average and strike rate are undefined without a wicket
    average = if_else(wickets > 0, round(runs_conceded / wickets, 2), NA_real_),
    strike_rate = if_else(wickets > 0, round(balls / wickets, 1), NA_real_)
  ) |>
  filter(innings >= 3) |>
  arrange(desc(wickets), economy)

output_path <- "bbl_dashboard_data.json"
previous    <- if (file.exists(output_path)) read_json(output_path, simplifyVector = TRUE) else NULL

if (!is.null(matches_all)) {
  # ── Match results ────────────────────────────────────────────────────────────
  match_results <- matches_all |>
    filter(season == latest_season) |>
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
} else if (!is.null(previous) && identical(previous$season, latest_season)) {
  message("Keeping match results and standings from the previous run (", previous$generated_at, ")")
  match_results <- previous$matches
  team_results  <- previous$standings
} else {
  stop("No match data for ", latest_season, " and no previous run to fall back on.")
}

# ── Compile and write JSON ───────────────────────────────────────────────────
dashboard_data <- list(
  season         = latest_season,
  generated_at   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  batting        = batting_leader,
  bowling        = bowling_leader,
  matches        = match_results,
  standings      = team_results
)

write_json(dashboard_data, output_path, pretty = TRUE, na = "null", auto_unbox = TRUE)
cat("Data written to:", output_path, "\n")
cat("Done! Now open bbl_dashboard.html in your browser.\n")
