# CricketStatsHub

R-based data pipeline that downloads, cleans, and stores batting and bowling cricket statistics from international and domestic competitions.

## Structure

```
CricketStatsHub/
├── update_cricket_data.R            # Master script — runs all download scripts in sequence
├── Data Download Scripts/
│   ├── load_mens_limited_overs_data.R
│   ├── load_womens_limited_overs_data.R
│   ├── load_mens_red_ball_data.R
│   ├── load_womens_red_ball_data.R
│   └── load_the_hundred_data.R
└── Database/                        # Output CSVs organised by competition
```

## Usage

Run `update_cricket_data.R` to refresh all datasets. Each sub-script sources data for its category, cleans it, and writes to `Database/`. The master script runs them in sequence and clears the environment between each to avoid memory issues.

## Data source

`cricketdata` R package, which pulls from Cricsheet (ball-by-ball data aggregated to innings level).

## Competitions covered

Includes 30+ competitions: BBL, IPL, PSL, CPL, ODI, T20I, Test matches, Sheffield Shield, County Championship, women's competitions, and more. See README.md for the full list.

## Packages

`tidyverse`, `cricketdata`

## Notes

- Some matches may be missing from datasets (Cricsheet coverage gaps)
- Data is summarised by innings — not ball-by-ball
