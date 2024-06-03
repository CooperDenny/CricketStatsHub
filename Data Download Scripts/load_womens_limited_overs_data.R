####################################################################################
#	Script-file:   load_womens_limited_overs_data.R
#	Project:       CricketStatsHub
# Author:        Cooper Denny
#
# Purpose:  	   Code to download and process womens limited overs player data
####################################################################################

#Load necessary libraries
library(tidyverse)
library(cricketdata)
library(lubridate)

#Gather the womens limited overs tournaments with data available
tournaments <- cricsheet_codes %>% 
  filter(code %in% c("odis","odms","t20s","it20s","cec","frb","rhf","wbb","wcl","wsl","wpl", "wtc")) %>% 
  filter(!code %in% c("blz", "sft"))

#Loop through each tournament and gather data
for(i in 1:nrow(tournaments)){
  
  #Check if folder path exists and create folder path if it doesn't
  folder_path <- paste0("Database/",tournaments$competition[i])
  if (!file.exists(folder_path)) {
    dir.create(folder_path)
  } 
  
  #Gather cricsheet and match summary data
  cricsheet <- fetch_cricsheet(type = "bbb", gender = "female", competition = tournaments$code[i]) %>% arrange(match_id)
  match_summary <- fetch_cricsheet(type = "match", gender = "female", competition = tournaments$code[i])
  match_summary_reduced <- match_summary %>% select(-season, -date, -venue)
  
  #merge the match summary with the cricsheet
  cricsheet <- merge(cricsheet, match_summary_reduced, by = "match_id")
  
  #Clean up the date
  cricsheet$start_date = ymd(cricsheet$start_date)
  
  #Clean up some NAs with 0
  cricsheet$wides <- cricsheet$wides  %>% replace(is.na(.), 0)
  cricsheet$noballs <- cricsheet$noballs  %>% replace(is.na(.), 0)
  cricsheet$byes <- cricsheet$byes  %>% replace(is.na(.), 0)
  cricsheet$legbyes <- cricsheet$legbyes  %>% replace(is.na(.), 0)
  cricsheet$penalty <- cricsheet$penalty  %>% replace(is.na(.), 0)
  
  #Add DotBallBowler column
  cricsheet <- cricsheet %>%
    mutate(DotBallBowler = runs_off_bat + wides + noballs)
  cricsheet$DotBallBowler<-replace(cricsheet$DotBallBowler, cricsheet$DotBallBowler==0, -1)
  cricsheet$DotBallBowler<-replace(cricsheet$DotBallBowler, cricsheet$DotBallBowler>0, 0)
  cricsheet$DotBallBowler<-replace(cricsheet$DotBallBowler, cricsheet$DotBallBowler==-1, 1)
  
  ####################################################################################
  ####################################################################################
  ###################################### BATTING #####################################
  ####################################################################################
  ####################################################################################
  
  #Gather innings batting data
  innings_batting <- cricsheet %>% 
    group_by(striker, match_id, start_date, season, event, venue, city, 
             team1, team2, innings, batting_team, bowling_team, toss_winner, 
             toss_decision, player_of_match, winner, winner_wickets,  
             winner_runs) %>% 
    summarise(RunsScored = sum(runs_off_bat, na.rm = TRUE),
              BallsFaced = n() - sum(wides > 0, na.rm = TRUE),
              Fours = sum(runs_off_bat == 4, na.rm = TRUE),
              Sixes = sum(runs_off_bat == 6, na.rm = TRUE),
              StrikeRate = 100*sum(runs_off_bat, na.rm = TRUE)/(n() - sum(wides > 0, na.rm = TRUE)),
              NotOut = if(sum(player_dismissed == striker, na.rm = TRUE) > 0) {FALSE} else {TRUE})
  
  #Identify times when a player has been run out when being the non-striker    
  innings_batting_run_out_non_striker <- cricsheet %>% 
    group_by(non_striker, match_id, start_date, season, event, venue, city, 
             team1, team2, innings, batting_team, bowling_team, toss_winner, 
             toss_decision, player_of_match, winner, winner_wickets,  
             winner_runs) %>% 
    summarise(NotOut_non_striker = if(sum(player_dismissed == non_striker, na.rm = TRUE) > 0) {FALSE} else {TRUE})
  
  #Replace the "non_striker" column name with "striker" so that it can be merged with innings_batting
  names(innings_batting_run_out_non_striker)[1] <- "striker"
  
  #Merge innings_batting with innings_batting_run_out_non_striker
  innings_batting <- merge(innings_batting, 
                           innings_batting_run_out_non_striker, 
                           by = c("striker", "match_id", "start_date", "season", "event", "venue", "city", 
                                  "team1", "team2", "innings", "batting_team", "bowling_team", "toss_winner", 
                                  "toss_decision", "player_of_match", "winner", "winner_wickets",  
                                  "winner_runs"),
                           all.x=TRUE, 
                           all.y=TRUE)
  
  #Clean up the "NotOut" column
  innings_batting$NotOut <-replace(innings_batting$NotOut, innings_batting$NotOut_non_striker == FALSE, FALSE)
  innings_batting$NotOut <-replace(innings_batting$NotOut, is.na(innings_batting$NotOut), TRUE)
  
  #Clean up some NAs with 0
  innings_batting$RunsScored <-replace(innings_batting$RunsScored, is.na(innings_batting$RunsScored), 0)
  innings_batting$BallsFaced <-replace(innings_batting$BallsFaced, is.na(innings_batting$BallsFaced), 0)
  innings_batting$Fours <-replace(innings_batting$Fours, is.na(innings_batting$Fours), 0)
  innings_batting$Sixes <-replace(innings_batting$Sixes, is.na(innings_batting$Sixes), 0)
  
  #Select only important columns and arrange according to runs scored and balls faced
  innings_batting <- innings_batting %>% 
    select(striker, start_date, batting_team, bowling_team, 
           innings, RunsScored, BallsFaced, Fours, 
           Sixes, StrikeRate, NotOut, event, 
           season, match_id, venue, city,
           team1, team2, toss_winner, toss_decision,
           player_of_match, winner, winner_wickets, winner_runs) %>% 
    arrange(desc(RunsScored), BallsFaced)
  
  ####################################################################################
  ####################################################################################
  ###################################### BOWLNG ######################################
  ####################################################################################
  ####################################################################################
  
  #Clean up the "over" and "ball" columns 
  if("over" %in% colnames(cricsheet)) {
    cricsheet$over <- cricsheet$over
    cricsheet$ball <- cricsheet$ball
  } else if("ball" %in% colnames(cricsheet)) {
    cricsheet$over <- substr(cricsheet$ball, 1, regexpr("\\.", cricsheet$ball) - 1)
    cricsheet$ball <- sub(".*\\.", "", cricsheet$ball)
  }
  
  #Gather innings bowling data
  innings_bowling <- cricsheet %>% 
    group_by(bowler, match_id, start_date, season, event, venue, city, 
             team1, team2, innings, batting_team, bowling_team, toss_winner, 
             toss_decision, player_of_match, winner, winner_wickets,  
             winner_runs) %>% 
    summarise(Balls = (n() - sum(wides > 0, na.rm = TRUE) - sum(noballs > 0, na.rm = TRUE)),
              Overs = paste(toString((n() - sum(wides > 0, na.rm = TRUE) - sum(noballs > 0, na.rm = TRUE))%/%6), toString((n() - sum(wides > 0, na.rm = TRUE) - sum(noballs > 0, na.rm = TRUE))%%6), sep= "."),
              Runs = sum(runs_off_bat, na.rm = TRUE) + 
                sum(wides, na.rm = TRUE) + 
                sum(noballs, na.rm = TRUE),
              Wickets = sum(wicket_type == "caught", na.rm = TRUE) + 
                sum(wicket_type == "bowled", na.rm = TRUE) + 
                sum(wicket_type == "lbw", na.rm = TRUE) + 
                sum(wicket_type == "stumped", na.rm = TRUE) +
                sum(wicket_type == "hit wicket", na.rm = TRUE) +
                sum(wicket_type == "caught and bowled", na.rm = TRUE),
              Economy = Runs/((n() - sum(wides > 0, na.rm = TRUE) - sum(noballs > 0, na.rm = TRUE))/6),
              DotBalls = sum(DotBallBowler == 1, na.rm = TRUE))
  
  #Identify times when a bowler has bowled a maiden
  innings_bowling_maidens <- cricsheet %>% 
    group_by(bowler, match_id, start_date, season, event, venue, city, 
             team1, team2, innings, batting_team, bowling_team, over, toss_winner, 
             toss_decision, player_of_match, winner, winner_wickets,  
             winner_runs) %>% 
    summarise(balls_in_over = max(ball),
              first_ball = min(ball),
              runs_conceded = sum(runs_off_bat, na.rm = TRUE) + sum(wides, na.rm = TRUE) + sum(noballs, na.rm = TRUE)) %>% 
    filter(runs_conceded == 0, 
           first_ball < 2, 
           balls_in_over > 5) %>% 
    mutate(maidens = 1) %>% 
    group_by(bowler, match_id, start_date, season, event, venue, city, 
             team1, team2, innings, batting_team, bowling_team, toss_winner, 
             toss_decision, player_of_match, winner, winner_wickets,  
             winner_runs) %>%
    summarise(Maidens = sum(maidens))
  
  #Merge innings_bowling with innings_bowling_maidens
  innings_bowling <- merge(x=innings_bowling,
                           y=innings_bowling_maidens,
                           by=c("bowler", "match_id", "start_date", "season", "event", "venue", "city", 
                                "team1", "team2", "innings", "batting_team", "bowling_team", "toss_winner", 
                                "toss_decision", "player_of_match", "winner", "winner_wickets",  
                                "winner_runs"),
                           all.x=TRUE, 
                           all.y=FALSE)
  
  #Clean up some NAs with 0
  innings_bowling$Maidens <- innings_bowling$Maidens %>% replace(is.na(.), 0)
  
  #Select only important columns and arrange according to wickets taken and runs conceded
  innings_bowling <- innings_bowling %>% 
    select(bowler, start_date, batting_team, bowling_team, 
           innings, Overs, Maidens, Runs,
           Wickets, Economy, DotBalls, event, 
           season, match_id, venue, city,
           team1, team2, toss_winner, toss_decision,
           player_of_match, winner, winner_wickets, winner_runs) %>% 
    arrange(desc(Wickets), Runs)
  
  ####################################################################################
  ####################################################################################
  ################################## MATCH SUMMARY ###################################
  ####################################################################################
  ####################################################################################
  
  #Clean up data types for some columns
  match_summary$date <- as.Date(match_summary$date)
  match_summary$match_id <- as.numeric(match_summary$match_id)
  match_summary$winner_wickets <- as.numeric(match_summary$winner_wickets)
  match_summary$winner_runs <- as.numeric(match_summary$winner_runs)
  match_summary$balls_per_over <- as.numeric(match_summary$balls_per_over)
  
  #Arrange match_summary according to date and match_id
  match_summary <- match_summary %>% 
    arrange(date, match_id)
  
  ####################################################################################
  ####################################################################################
  
  #Save files in csv format
  write.csv(innings_batting, paste0("Database/",tournaments$competition[i],"\\",tournaments$code[i],"_women_innings_batting.csv"), row.names=FALSE)
  write.csv(innings_bowling, paste0("Database/",tournaments$competition[i],"\\",tournaments$code[i],"_women_innings_bowling.csv"), row.names=FALSE)
  write.csv(match_summary, paste0("Database/",tournaments$competition[i],"\\",tournaments$code[i],"_women_matches_in_dataset.csv"), row.names=FALSE)
  
  ####################################################################################
  
}



