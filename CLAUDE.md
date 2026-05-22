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

## Competition codes

Pass these as the `competition` argument. For M/F competitions, also set `gender`.

### International

| Code | Competition | Gender |
|------|-------------|--------|
| `tests` | Test matches | M/F |
| `odis` | One-day internationals | M/F |
| `t20s` | T20 internationals | M/F |
| `mdms` | Multi-day matches | M/F |
| `odms` | One-day matches | M/F |
| `it20s` | Non-official T20 internationals | M/F |

### Men's domestic & franchise

| Code | Competition |
|------|-------------|
| `bbl` | Big Bash League |
| `blz` | T20 Blaze |
| `bpl` | Bangladesh Premier League |
| `bwt` | Bob Willis Trophy |
| `cch` | County Championship |
| `cpl` | Caribbean Premier League |
| `ctc` | CSA T20 Challenge |
| `hnd` | The Hundred |
| `ilt` | International League T20 |
| `ipo` | Cricket Ireland Inter-Provincial Limited Over Cup |
| `ipl` | Indian Premier League |
| `ipt` | Cricket Ireland Inter-Provincial Twenty20 Trophy |
| `lpl` | Lanka Premier League |
| `mcl` | Major Clubs Limited Over Tournament |
| `mct` | Major Clubs T20 Tournament |
| `mlc` | Major League Cricket |
| `mlt` | Major League Tournament |
| `msl` | Mzansi Super League |
| `npl` | Nepal Premier League |
| `ntb` | T20 Blast |
| `odc` | One-Day Cup (Australia) |
| `pks` | Plunket Shield |
| `psl` | Pakistan Super League |
| `rlc` | Royal London One-Day Cup |
| `sat` | SA20 |
| `sft` | Super 50 |
| `sma` | Syed Mushtaq Ali Trophy |
| `ssh` | Sheffield Shield |
| `ssm` | Super Smash |

### Women's domestic & franchise

| Code | Competition |
|------|-------------|
| `cec` | Charlotte Edwards Cup |
| `frb` | FairBreak Invitational Tournament |
| `hnd` | The Hundred (use `gender = "female"`) |
| `rhf` | Rachael Heyhoe Flint Trophy |
| `wbb` | Women's Big Bash League |
| `wcl` | Women's Caribbean Premier League |
| `wod` | ECB Women's One-Day Cup |
| `wpl` | Women's Premier League |
| `wsl` | Women's Cricket Super League |
| `wtb` | Women's T20 Blast |
| `wtc` | Women's T20 Challenge |

## Packages

`tidyverse`, `cricketdata`

## Notes

- Some matches may be missing (Cricsheet coverage gaps)
- Run outs, retired hurt, and retired out are excluded from bowler wicket counts
- Byes and leg byes are not charged to the bowler in `runs_conceded`
