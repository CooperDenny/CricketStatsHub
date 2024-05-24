####################################################################################
#	Script-file:   load_womens_limited_overs_data.R
#	Project:       Cricket Stats
# Author:        Cooper Denny
#
# Purpose:  	   Code to download and process womens limited overs player data
####################################################################################

library(tidyverse)
library(cricketdata)
library(lubridate)

tournaments <- cricsheet_codes %>% 
  filter(code %in% c("odis","odms","t20s","it20s","cec","frb","rhf","wbb","wcl","wsl","wpl", "wtc")) %>% 
  filter(!code %in% c("blz", "sft"))

for(i in 1:nrow(tournaments)){
  
  
  cricsheet <- fetch_cricsheet(type = "bbb", gender = "female", competition = tournaments$code[i]) %>% arrange(match_id)
  match_summary <- fetch_cricsheet(type = "match", gender = "female", competition = tournaments$code[i])
  match_summary_reduced <- match_summary %>% select(-season, -date, -venue)
  
  cricsheet <- merge(cricsheet, match_summary_reduced, by = "match_id")
  
  
  cricsheet$start_date = ymd(cricsheet$start_date)
  cricsheet$wides <- cricsheet$wides  %>% replace(is.na(.), 0)
  cricsheet$noballs <- cricsheet$noballs  %>% replace(is.na(.), 0)
  cricsheet$byes <- cricsheet$byes  %>% replace(is.na(.), 0)
  cricsheet$legbyes <- cricsheet$legbyes  %>% replace(is.na(.), 0)
  cricsheet$penalty <- cricsheet$penalty  %>% replace(is.na(.), 0)
  
  cricsheet <- cricsheet %>%
    mutate(
      DotBallBowler = runs_off_bat + wides + noballs)
  
  cricsheet$DotBallBowler<-replace(cricsheet$DotBallBowler, cricsheet$DotBallBowler==0, -1)
  cricsheet$DotBallBowler<-replace(cricsheet$DotBallBowler, cricsheet$DotBallBowler>0, 0)
  cricsheet$DotBallBowler<-replace(cricsheet$DotBallBowler, cricsheet$DotBallBowler==-1, 1)
  
  
  ####################################################################################
  ####################################################################################
  ###################################### BATTING #####################################
  ####################################################################################
  ####################################################################################
  
  innings_batting <- cricsheet %>% group_by(striker, match_id, start_date, season, event, venue, city, 
                                            team1, team2, innings, batting_team, bowling_team, toss_winner, 
                                            toss_decision, player_of_match, winner, winner_wickets,  
                                            winner_runs) %>% 
    summarise(
      RunsScored = sum(runs_off_bat),
      BallsFaced = n() - sum(wides > 0, na.rm = TRUE),
      Fours = sum(runs_off_bat == 4, na.rm = TRUE),
      Sixes = sum(runs_off_bat == 6, na.rm = TRUE),
      StrikeRate = 100*sum(runs_off_bat)/(n() - sum(wides > 0, na.rm = TRUE)),
      NotOut = if(sum(player_dismissed == striker, na.rm = TRUE) > 0) {
        FALSE
      } else {
        TRUE
      }
    )
  
  innings_batting_run_out_non_striker <- cricsheet %>% group_by(non_striker, match_id, start_date, season, event, venue, city, 
                                                                team1, team2, innings, batting_team, bowling_team, toss_winner, 
                                                                toss_decision, player_of_match, winner, winner_wickets,  
                                                                winner_runs) %>% 
    summarise(
      NotOut_non_striker = if(sum(player_dismissed == non_striker, na.rm = TRUE) > 0) {
        FALSE
      } else {
        TRUE
      }
    )
  names(innings_batting_run_out_non_striker)[1] <- "striker"
  innings_batting <- merge(innings_batting, innings_batting_run_out_non_striker, by = c("striker", "match_id", "start_date", "season", "event", "venue", "city", 
                                                                                        "team1", "team2", "innings", "batting_team", "bowling_team", "toss_winner", 
                                                                                        "toss_decision", "player_of_match", "winner", "winner_wickets",  
                                                                                        "winner_runs"),all.x=TRUE, all.y=TRUE)
  innings_batting$NotOut <-replace(innings_batting$NotOut, innings_batting$NotOut_non_striker == FALSE, FALSE)
  innings_batting$NotOut <-replace(innings_batting$NotOut, is.na(innings_batting$NotOut), TRUE)
  innings_batting$RunsScored <-replace(innings_batting$RunsScored, is.na(innings_batting$RunsScored), 0)
  innings_batting$BallsFaced <-replace(innings_batting$BallsFaced, is.na(innings_batting$BallsFaced), 0)
  innings_batting$Fours <-replace(innings_batting$Fours, is.na(innings_batting$Fours), 0)
  innings_batting$Sixes <-replace(innings_batting$Sixes, is.na(innings_batting$Sixes), 0)
  
  innings_batting <- innings_batting %>% select(striker, start_date, batting_team, bowling_team, 
                                                innings, RunsScored, BallsFaced, Fours, 
                                                Sixes, StrikeRate, NotOut, event, 
                                                season, match_id, venue, city,
                                                team1, team2, toss_winner, toss_decision,
                                                player_of_match, winner, winner_wickets, winner_runs
  ) %>% arrange(desc(RunsScored), desc(BallsFaced))
  
  ####################################################################################
  ####################################################################################
  ###################################### BOWLNG ######################################
  ####################################################################################
  ####################################################################################
  
  if("over" %in% colnames(cricsheet)) {
    cricsheet$over <- cricsheet$over
    cricsheet$ball <- cricsheet$ball
  } else if("ball" %in% colnames(cricsheet)) {
    cricsheet$over <- substr(cricsheet$ball, 1, regexpr("\\.", cricsheet$ball) - 1)
    cricsheet$ball <- sub(".*\\.", "", cricsheet$ball)
  }
  
  innings_bowling <- cricsheet %>% group_by(bowler, match_id, start_date, season, event, venue, city, 
                                            team1, team2, innings, batting_team, bowling_team, toss_winner, 
                                            toss_decision, player_of_match, winner, winner_wickets,  
                                            winner_runs) %>% 
    summarise(
      Balls = (n() - sum(wides > 0, na.rm = TRUE) - sum(noballs > 0, na.rm = TRUE)),
      Overs = paste(toString((n() - sum(wides > 0, na.rm = TRUE) - sum(noballs > 0, na.rm = TRUE))%/%6), toString((n() - sum(wides > 0, na.rm = TRUE) - sum(noballs > 0, na.rm = TRUE))%%6), sep= "."),
      Runs = sum(runs_off_bat, na.rm = TRUE) + sum(wides, na.rm = TRUE) + sum(noballs, na.rm = TRUE),
      Wickets = sum(wicket_type == "caught", na.rm = TRUE) + 
        sum(wicket_type == "bowled", na.rm = TRUE) + 
        sum(wicket_type == "lbw", na.rm = TRUE) + 
        sum(wicket_type == "stumped", na.rm = TRUE) +
        sum(wicket_type == "hit wicket", na.rm = TRUE) +
        sum(wicket_type == "caught and bowled", na.rm = TRUE),
      Economy = Runs/((n() - sum(wides > 0, na.rm = TRUE) - sum(noballs > 0, na.rm = TRUE))/6),
      DotBalls = sum(DotBallBowler == 1, na.rm = TRUE)
    )
  
  innings_bowling_maidens <- cricsheet %>% group_by(bowler, match_id, start_date, season, event, venue, city, 
                                                    team1, team2, innings, batting_team, bowling_team, over, toss_winner, 
                                                    toss_decision, player_of_match, winner, winner_wickets,  
                                                    winner_runs) %>% 
    summarise(
      balls_in_over = max(ball),
      first_ball = min(ball),
      runs_conceded = sum(runs_off_bat, na.rm = TRUE) + sum(wides, na.rm = TRUE) + sum(noballs, na.rm = TRUE)
    ) %>% filter(runs_conceded == 0, first_ball < 2, balls_in_over > 5) %>% mutate(
      maidens = 1
    ) %>% group_by(bowler, match_id, start_date, season, event, venue, city, 
                   team1, team2, innings, batting_team, bowling_team, toss_winner, 
                   toss_decision, player_of_match, winner, winner_wickets,  
                   winner_runs) %>%
    summarise(
      Maidens = sum(maidens)
    )
  
  innings_bowling <- merge(x=innings_bowling,y=innings_bowling_maidens,by=c("bowler", "match_id", "start_date", "season", "event", "venue", "city", 
                                                                            "team1", "team2", "innings", "batting_team", "bowling_team", "toss_winner", 
                                                                            "toss_decision", "player_of_match", "winner", "winner_wickets",  
                                                                            "winner_runs"),all.x=TRUE, all.y=FALSE)
  
  innings_bowling$Maidens <- innings_bowling$Maidens %>% replace(is.na(.), 0)
  
  innings_bowling <- innings_bowling %>% select(bowler, start_date, batting_team, bowling_team, 
                                                innings, Overs, Maidens, Runs,
                                                Wickets, Economy, DotBalls, event, 
                                                season, match_id, venue, city,
                                                team1, team2, toss_winner, toss_decision,
                                                player_of_match, winner, winner_wickets, winner_runs
  ) %>% arrange(desc(Wickets), Runs)
  
  ####################################################################################
  ####################################################################################
  
  match_summary$date <- as.Date(match_summary$date)
  match_summary$match_id <- as.numeric(match_summary$match_id)
  match_summary$winner_wickets <- as.numeric(match_summary$winner_wickets)
  match_summary$winner_runs <- as.numeric(match_summary$winner_runs)
  match_summary$balls_per_over <- as.numeric(match_summary$balls_per_over)
  
  match_summary <- match_summary %>% arrange(date, match_id)
  
  ####################################################################################
  ####################################################################################
  
  folder_path <- paste0("Database/",tournaments$competition[i])
  if (!file.exists(folder_path)) {
    dir.create(folder_path)
  } 
  
  write.csv(innings_batting, paste0("Database/",tournaments$competition[i],"\\",tournaments$code[i],"_women_innings_batting.csv"), row.names=FALSE)
  
  write.csv(innings_bowling, paste0("Database/",tournaments$competition[i],"\\",tournaments$code[i],"_women_innings_bowling.csv"), row.names=FALSE)
  
  write.csv(match_summary, paste0("Database/",tournaments$competition[i],"\\",tournaments$code[i],"_women_matches_in_dataset.csv"), row.names=FALSE)
  
  ####################################################################################
  
}



