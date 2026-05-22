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

# Fetch cricket data for a given competition and return a clean tibble.
#
# competition : Cricsheet code (e.g. "bbl", "ipl", "t20s"). Run `cricsheet_codes`.
# type        : "bbb" = ball-by-ball | "batting" = batting stats per innings |
#               "bowling" = bowling stats per innings | "match" = match metadata
# gender      : "male" or "female"
fetch_cricket_data <- function(
    competition,
    type   = c("bbb", "batting", "bowling", "match"),
    gender = c("male", "female")
) {
  type   <- match.arg(type)
  gender <- match.arg(gender)

  if (type == "match") {
    return(fetch_cricsheet("match", gender, competition))
  }

  bbb <- fetch_cricsheet("bbb", gender, competition)

  if (type == "bbb") {
    return(bbb)
  }

  if (type == "batting") {
    bbb |>
      group_by(match_id, season, start_date, venue, innings,
               batting_team, bowling_team, striker) |>
      summarise(
        runs        = sum(runs_off_bat, na.rm = TRUE),
        balls_faced = sum(is.na(wides)),
        fours       = sum(runs_off_bat == 4, na.rm = TRUE),
        sixes       = sum(runs_off_bat == 6, na.rm = TRUE),
        not_out     = !any(player_dismissed == striker, na.rm = TRUE),
        .groups     = "drop"
      ) |>
      mutate(strike_rate = round(runs / balls_faced * 100, 2)) |>
      rename(batter = striker) |>
      arrange(match_id, innings, desc(runs))
  } else {
    # bowling
    bbb |>
      group_by(match_id, season, start_date, venue, innings,
               bowling_team, batting_team, bowler) |>
      summarise(
        balls           = sum(is.na(wides) & is.na(noballs)),
        runs_conceded   = sum(runs_off_bat, na.rm = TRUE) +
                          sum(wides, na.rm = TRUE) +
                          sum(noballs, na.rm = TRUE),
        wickets         = sum(
          !is.na(player_dismissed) &
            !wicket_type %in% c("run out", "retired hurt", "retired out",
                                "obstructing the field"),
          na.rm = TRUE
        ),
        wides_bowled    = sum(!is.na(wides)),
        no_balls_bowled = sum(!is.na(noballs)),
        .groups         = "drop"
      ) |>
      mutate(
        overs   = paste0(balls %/% 6, ".", balls %% 6),
        economy = round(runs_conceded / (balls / 6), 2)
      ) |>
      arrange(match_id, innings, desc(wickets), runs_conceded)
  }
}
