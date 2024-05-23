# CricketStatsHub

## Overview

CricketStatsHub is a repository containing summarised batting and bowling cricket data from a variety of international and domestic competitions. The data is sourced using the `cricketdata` R package, which facilitates the retrieval of cricket match data from providers like Cricsheet.

## Competitions with Available Data

CricketStatsHub repository contains data from various cricket competitions, providing a comprehensive dataset for analysis and research purposes. Here's a list of competitions for which data is available:

- Afghanistan Premier League
- Bangladesh Premier League
- Big Bash League
- CSA T20 Challenge
- Caribbean Premier League
- Charlotte Edwards Cup
- County Championship
- Cricket Ireland Inter-Provincial Limited Over Cup
- Cricket Ireland Inter-Provincial Twenty20 Trophy
- FairBreak Invitational Tournament
- Indian Premier League
- International League T20
- Lanka Premier League
- Major League Cricket
- Multi-day matches
- Mzansi Super League
- Non-official T20 internationals
- One-day internationals
- One-day matches
- Pakistan Super League
- Rachael Heyhoe Flint Trophy
- Royal London One-Day Cup
- SA20
- Sheffield Shield
- Super Smash
- Syed Mushtaq Ali Trophy
- T20 Blast
- T20 internationals
- Test matches
- The Hundred
- Women's Big Bash League
- Women's Caribbean Premier League
- Women's Cricket Super League
- Women's Premier League
- Women's T20 Challenge

By providing access to data from a wide range of competitions, CricketStatsHub facilitates comprehensive analysis and insights generation, contributing to the advancement of cricket analytics and research.

## Repository Navigation

- Data Download Scripts: Contains scripts for downloading and updating cricket data.
- Database: Stores the cricket data in a structured format.
- .gitignore: Specifies intentionally untracked files to ignore.
- CricketStatsHub.Rproj: R project file for the CricketStatsHub project.
- update_cricket_data.R: Script for updating cricket data.
- README.md: This file, providing an overview of the repository.

## Updating Data

To update the cricket data in this repository, run the `update_cricket_data.R` script. This script fetches the latest data using the `cricketdata` package and updates the repository with the newly acquired data.