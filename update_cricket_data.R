####################################################################################
#  Script-file:   update_cricket_data.R
#  Project:       CricketStatsHub
#  Author:        Cooper Denny
#
#  Purpose:       Example usage of fetch_cricket_data().
#                 Run `cricsheet_codes` to see all available competition codes.
####################################################################################

source("fetch_cricket_data.R")

# Ball-by-ball
bbl_bbb <- fetch_cricket_data("bbl", type = "bbb")

# Batting stats per innings
bbl_batting <- fetch_cricket_data("bbl", type = "batting")

# Bowling stats per innings
bbl_bowling <- fetch_cricket_data("bbl", type = "bowling")

# Match metadata
bbl_matches <- fetch_cricket_data("bbl", type = "match")

# Women's competition — specify gender
wbbl_batting <- fetch_cricket_data("wbbl", type = "batting", gender = "female")

# International — gender matters
mens_t20_batting  <- fetch_cricket_data("t20s", type = "batting", gender = "male")
womens_t20_batting <- fetch_cricket_data("t20s", type = "batting", gender = "female")
