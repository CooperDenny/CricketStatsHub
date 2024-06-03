####################################################################################
#	Script-file:   update_cricket_data.R
#	Project:       CricketStatsHub
# Author:        Cooper Denny
#
# Purpose:  	   Code to update cricket player data
####################################################################################

source("Data Download Scripts/load_mens_limited_overs_data.R")
print("✅ Mens Limited Overs Data")
rm(list=ls())

source("Data Download Scripts/load_womens_limited_overs_data.R")
print("✅ Womens Limited Overs Data")
rm(list=ls())

source("Data Download Scripts/load_mens_red_ball_data.R")
print("✅ Mens Red Ball Data")
rm(list=ls())

source("Data Download Scripts/load_womens_red_ball_data.R")
print("✅ Womens Red Ball Data")
rm(list=ls())

source("Data Download Scripts/load_the_hundred_data.R")
print("✅ The Hundred Data")
rm(list=ls())

