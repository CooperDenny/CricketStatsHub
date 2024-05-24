# CricketStatsHub

## Overview

CricketStatsHub is a repository containing summarised batting and bowling cricket data from a variety of international and domestic competitions. The data is sourced using the `cricketdata` R package, which facilitates the retrieval of cricket match data from providers like Cricsheet.

## Competitions with Available Data

CricketStatsHub repository contains data from various cricket competitions, providing a comprehensive dataset for analysis and research purposes. Here's a list of competitions for which data is available:

* ***note that there may be some missing matches in the dataset***

- Afghanistan Premier League (2018-10-05 - )
- Bangladesh Premier League (2012-02-11 - )
- Big Bash League (2011-12-16 - )
- Caribbean Premier League (2013-07-30 - )
- Charlotte Edwards Cup (2021-06-26 - )
- County Championship (2014-04-06 - )
- Cricket Ireland Inter-Provincial Limited Over Cup (2016-05-02 - )
- Cricket Ireland Inter-Provincial Twenty20 Trophy (2016-05-27 - )
- CSA T20 Challenge (2012-02-15 - )
- FairBreak Invitational Tournament (2022-05-04 - )
- Indian Premier League (2008-04-18 )
- International League T20 (2023-01-13 - )
- Lanka Premier League (2020-11-26 - )
- Major League Cricket (2023-07-13 - )
- Multi-day matches (2006-12-05 - )
- Mzansi Super League (2018-11-16 -)
- Non-official T20 internationals (Male: 2010-06-04 - , Female: 2013-07-23 - )
- One-day internationals (Male: 2002-12-29 - , Female: 2007-01-22 -)
- One-day matches (Male: 2010-05-28 - , Female: 2016-07-15 - )
- Pakistan Super League (2016-02-04 - )
- Rachael Heyhoe Flint Trophy (2020-08-29 -)
- Royal London One-Day Cup (2014-07-26 - )
- SA20 (2023-01-10 - )
- Sheffield Shield (2017-03-26 - )
- Super Smash (2013-01-20 - )
- Syed Mushtaq Ali Trophy (2016-01-02 - )
- T20 Blast (2014-05-16 - )
- T20 internationals (Male: 2005-02-17 - , Female: 2009-06-18 - )
- Test matches (Male: 2002-12-26 - , Female: 2003-02-22 - )
- The Hundred (Male: 2021-07-22 - , Female: 2021-07-21 - )
- Women's Big Bash League (2015-12-18 - )
- Women's Caribbean Premier League (2022-08-31 - )
- Women's Cricket Super League (2016-07-30 - )
- Women's Premier League (2023-03-04 - )
- Women's T20 Challenge (2018-05-22 - )

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
