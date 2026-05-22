# CricketStatsHub

## Overview

CricketStatsHub is a repository containing batting and bowling cricket data (summarised by innings) from a variety of international and domestic competitions. The data is sourced using the `cricketdata` R package, which facilitates the retrieval of cricket match data from providers like Cricsheet.

## Competition Codes

Pass these codes as the `competition` argument to `fetch_cricket_data()`. For competitions marked **M/F**, also specify `gender = "male"` or `gender = "female"`.

> Note: some matches may be missing due to Cricsheet coverage gaps.

### International

| Code | Competition | Gender |
|------|-------------|--------|
| `tests` | Test matches | M/F |
| `odis` | One-day internationals | M/F |
| `t20s` | T20 internationals | M/F |
| `mdms` | Multi-day matches | M/F |
| `odms` | One-day matches | M/F |
| `it20s` | Non-official T20 internationals | M/F |

### Men's Domestic & Franchise

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

### Women's Domestic & Franchise

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

## Repository Navigation

- `fetch_cricket_data.R`: Core function — source this to fetch data.
- `update_cricket_data.R`: Example usage of `fetch_cricket_data()`.
- `Database`: Historical output CSVs (legacy).
- `CricketStatsHub.Rproj`: R project file.

## Usage

```r
source("fetch_cricket_data.R")

# Batting stats for the BBL
bbl_batting <- fetch_cricket_data("bbl", type = "batting")

# Bowling stats for men's T20 internationals
t20_bowling <- fetch_cricket_data("t20s", type = "bowling", gender = "male")

# Ball-by-ball data for the IPL
ipl_bbb <- fetch_cricket_data("ipl", type = "bbb")

# Match metadata
bbl_matches <- fetch_cricket_data("bbl", type = "match")
```
