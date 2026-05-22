# CricketStatsHub

R-based library for fetching clean cricket data from international and domestic competitions on demand.

## Structure

```
CricketStatsHub/
├── fetch_cricket_data.R   # Core function — source this to use the library
├── update_cricket_data.R  # Example usage of fetch_cricket_data()
└── Database/              # Historical output CSVs (legacy)
```

## Usage

Source `fetch_cricket_data.R` and call `fetch_cricket_data()`:

```r
source("fetch_cricket_data.R")

# Returns a clean tibble — no files written to disk
bbl_batting <- fetch_cricket_data("bbl", type = "batting")
ipl_bowling <- fetch_cricket_data("ipl", type = "bowling")
t20_matches <- fetch_cricket_data("t20s", type = "match", gender = "male")
```

Run `cricsheet_codes` to see all available competition codes.

## Function: fetch_cricket_data()

```r
fetch_cricket_data(competition, type = c("bbb", "batting", "bowling", "match"), gender = c("male", "female"))
```

| type | Returns |
|------|---------|
| `"bbb"` | Ball-by-ball data (one row per delivery) |
| `"batting"` | Batting stats aggregated per batter per innings |
| `"bowling"` | Bowling stats aggregated per bowler per innings |
| `"match"` | Match metadata (teams, venue, date, umpires) |

**Batting columns:** `batter`, `runs`, `balls_faced`, `fours`, `sixes`, `not_out`, `strike_rate`

**Bowling columns:** `bowler`, `balls`, `overs`, `runs_conceded`, `wickets`, `economy`, `wides_bowled`, `no_balls_bowled`

## Data source

`cricketdata` R package, which pulls from Cricsheet (ball-by-ball data). Batting and bowling stats are derived by aggregating ball-by-ball data.

## Competitions covered

30+ competitions: BBL, WBBL, IPL, PSL, CPL, ODI, T20I, Test matches, Sheffield Shield, County Championship, women's competitions, and more.

## Packages

`tidyverse`, `cricketdata`

## Notes

- Some matches may be missing (Cricsheet coverage gaps)
- Run outs, retired hurt, and retired out are excluded from bowler wicket counts
- Byes and leg byes are not charged to the bowler in `runs_conceded`
