# CricketStatsHub

## Overview

CricketStatsHub is an R function, `fetch_cricket_data()`, that returns clean ball-by-ball, batting, bowling or match data for any competition Cricsheet covers, plus a self-contained BBL dashboard built from it. Data comes from [Cricsheet](https://cricsheet.org) via the `cricketdata` R package; nothing is written to disk unless you ask.

## Competition Codes

Pass these codes as the `competition` argument to `fetch_cricket_data()`. For competitions marked **M/F**, also specify `gender = "male"` or `gender = "female"` (the default is `"male"`). Women-only competitions default to `"female"`, so `gender` can be left out.

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
- `generate_bbl_dashboard_data.R`: Builds `bbl_dashboard_data.json` for the latest BBL season.
- `bbl_dashboard.html`: Sortable, filterable BBL leaderboards, results and standings. Serve the folder (e.g. `python3 -m http.server`) and open it; it loads the JSON with `fetch`, which browsers block from `file://`.
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

# Women-only competitions default to gender = "female"
wbbl_batting <- fetch_cricket_data("wbb", type = "batting")
```

## How the stats are calculated

Batting and bowling figures are aggregated from ball-by-ball data, following the Laws of Cricket and the usual scorecard conventions. Every innings' batting runs plus extras reconcile to the team total.

**Batting** (one row per batter per innings)
- Every batter who came to the crease is included, even without facing a ball.
- A batter is out if dismissed at either end, so a run out at the non-striker's end counts. Retired hurt (or retired not out) is not out; retired out is out.
- Balls faced exclude wides and include no-balls. Fours exclude all-run fours.
- `strike_rate` is `NA` when no balls were faced.

**Bowling** (one row per bowler per innings)
- Wickets count only dismissals credited to the bowler: bowled, caught, caught and bowled, lbw, stumped and hit wicket. Run outs, obstructing the field and retirements are not.
- Runs conceded include wides and no-balls but not byes or leg byes.
- Balls exclude wides and no-balls; `economy` is `NA` with no legal balls.

**Super overs** are excluded from batting and bowling stats (innings 3+ in every competition except the multi-day ones: `tests`, `mdms`, `bwt`, `cch`, `pks`, `ssh`). They remain in `type = "bbb"`.

## Known issues

- Some matches may be missing (Cricsheet coverage gaps).
- `type = "match"` reads Cricsheet's match files directly, because `cricketdata` 0.3.0's `fetch_cricsheet("match")` fails on current files (they now carry their own `match_id`). `winner` includes ties won in a super over (`super_over = TRUE`), and `result` is `"won"`, `"tie"` or `"no result"`.
