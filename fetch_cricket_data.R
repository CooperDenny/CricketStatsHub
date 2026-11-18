####################################################################################
#  Script-file:   fetch_cricket_data.R
#  Project:       CricketStatsHub
#  Author:        Cooper Denny
#
#  Purpose:       Single function to fetch and return clean cricket data.
#                 Run `cricsheet_codes` to see all available competition codes.
####################################################################################

library(tidyverse)
library(cricketdata)

# Dismissals credited to the bowler under the Laws of Cricket.
# Run out, obstructing the field, hit the ball twice, handled the ball, timed out
# and retirements are not.
bowler_wicket_types <- c("bowled", "caught", "caught and bowled", "lbw",
                         "stumped", "hit wicket")

# Ways of leaving the crease that do not count as a dismissal (the batter is not out)
not_out_exits <- c("retired hurt", "retired not out")

# Competitions with more than two innings. In every other competition, innings 3
# onwards are super overs, which are not counted in player statistics.
multi_day_codes <- c("tests", "mdms", "bwt", "cch", "pks", "ssh")

# Competitions Cricsheet only publishes for women: gender defaults to "female" for these
women_only_codes <- c("cec", "frb", "rhf", "wbb", "wcl", "wod", "wpl", "wsl", "wtb", "wtc")

# Cricsheet encodes "nothing" as "" rather than NA in its character columns
has_value <- function(x) !is.na(x) & x != ""
is_true   <- function(x) x %in% c(TRUE, "true", "True", "TRUE")

# Fetch cricket data for a given competition and return a clean tibble.
#
# competition : Cricsheet code (e.g. "bbl", "ipl", "t20s"). Run `cricsheet_codes`.
# type        : "bbb" = ball-by-ball | "batting" = batting stats per innings |
#               "bowling" = bowling stats per innings | "match" = match metadata
# gender      : "male" or "female". Defaults to "female" for women-only competitions
#               (e.g. "wbb"), otherwise "male".
fetch_cricket_data <- function(
    competition,
    type   = c("bbb", "batting", "bowling", "match"),
    gender = c("male", "female")
) {
  type <- match.arg(type)
  if (missing(gender) && competition %in% women_only_codes) gender <- "female"
  gender <- match.arg(gender)

  if (type == "match") {
    return(fetch_cricsheet("match", gender, competition))
  }

  bbb <- fetch_cricsheet("bbb", gender, competition)

  if (type == "bbb") {
    return(bbb)
  }

  # Player statistics exclude super overs
  if (!competition %in% multi_day_codes) {
    bbb <- filter(bbb, innings <= 2)
  }

  if (type == "batting") summarise_batting(bbb) else summarise_bowling(bbb)
}

# Batting stats per batter per innings, from ball-by-ball data.
summarise_batting <- function(bbb) {
  innings_keys <- c("match_id", "season", "start_date", "venue", "innings",
                    "batting_team", "bowling_team")

  # Everyone who batted, including batters who never faced a ball
  # (e.g. run out at the non-striker's end, or not out at the other end)
  batters <- bind_rows(
    distinct(bbb, across(all_of(innings_keys)), batter = striker),
    distinct(bbb, across(all_of(innings_keys)), batter = non_striker)
  ) |>
    distinct()

  # Runs and balls are credited to the striker. Wides are not balls faced;
  # no-balls are. Fours run without reaching the boundary are not boundaries.
  faced <- bbb |>
    group_by(match_id, innings, batter = striker) |>
    summarise(
      runs        = sum(runs_off_bat, na.rm = TRUE),
      balls_faced = sum(is.na(wides)),
      fours       = sum(runs_off_bat == 4 & !is_true(non_boundary), na.rm = TRUE),
      sixes       = sum(runs_off_bat == 6, na.rm = TRUE),
      .groups     = "drop"
    )

  # Dismissals can fall on either batter, and a ball can (rarely) dismiss two
  dismissed <- bind_rows(
    bbb |>
      filter(has_value(player_dismissed), !wicket_type %in% not_out_exits) |>
      transmute(match_id, innings, batter = as.character(player_dismissed)),
    bbb |>
      filter(has_value(other_player_dismissed), !other_wicket_type %in% not_out_exits) |>
      # as.character: a column with no values (e.g. no double dismissals) is read as logical
      transmute(match_id, innings, batter = as.character(other_player_dismissed))
  ) |>
    distinct() |>
    mutate(out = TRUE)

  batters |>
    left_join(faced, by = c("match_id", "innings", "batter")) |>
    left_join(dismissed, by = c("match_id", "innings", "batter")) |>
    mutate(
      across(c(runs, balls_faced, fours, sixes), \(x) as.integer(coalesce(x, 0L))),
      not_out     = is.na(out),
      strike_rate = if_else(balls_faced > 0, round(runs / balls_faced * 100, 2), NA_real_)
    ) |>
    select(-out) |>
    arrange(match_id, innings, desc(runs))
}

# Bowling stats per bowler per innings, from ball-by-ball data.
summarise_bowling <- function(bbb) {
  bbb |>
    group_by(match_id, season, start_date, venue, innings,
             bowling_team, batting_team, bowler) |>
    summarise(
      # Wides and no-balls are not legal deliveries
      balls           = sum(is.na(wides) & is.na(noballs)),
      # Byes and leg byes are not charged to the bowler
      runs_conceded   = sum(runs_off_bat, na.rm = TRUE) +
                        sum(wides, na.rm = TRUE) +
                        sum(noballs, na.rm = TRUE),
      wickets         = sum(wicket_type %in% bowler_wicket_types),
      wides_bowled    = sum(!is.na(wides)),
      no_balls_bowled = sum(!is.na(noballs)),
      .groups         = "drop"
    ) |>
    mutate(
      overs   = paste0(balls %/% 6, ".", balls %% 6),
      economy = if_else(balls > 0, round(runs_conceded / (balls / 6), 2), NA_real_)
    ) |>
    arrange(match_id, innings, desc(wickets), runs_conceded)
}
