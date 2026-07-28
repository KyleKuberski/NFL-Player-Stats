# NFL Player Stats Pipeline (R)

Pulls play-by-play and roster data for the NFL and aggregates it into
season-level player stat totals across passing, rushing, receiving,
defense, and special teams.

## What it does

1. **Loads play-by-play and roster data** via
   [`nflreadr`](https://github.com/nflverse/nflreadr) for each configured
   season, filtered to regular season games (and, for the current season,
   only games played through the most recent completed week).

2. **Aggregates player stats by category:**
   - **Passing** — attempts, completions, yards, TDs, interceptions,
     sacks, EPA per dropback, CPOE
   - **Rushing** — attempts, yards, TDs, first downs, EPA per play
   - **Receiving** — targets, receptions, yards, TDs, air yards, YAC
   - **Defense** — sacks, interceptions, forced fumbles
   - **Special teams** — field goal/extra point attempts and makes,
     punting totals (gross/net yards)

3. **Joins in roster context** — position, team, and age at time of season.

4. **Outputs one Excel file per season** (`AllRanksNFL_<year>.xlsx`)
   containing a `Totals` sheet with one row per player for that season.
   These files are the input to the companion Python ranking script.

## Requirements

- R with the following packages: `dplyr`, `tidyr`, `nflreadr`, `openxlsx`,
  `lubridate`

## Configuration

- `seasons` — list of seasons to process; needs a manual update each
  year to include the upcoming season.
- Output files are written to the current working directory
  (`AllRanksNFL_<year>.xlsx`).

## Notes

- For the current season, the script automatically determines the most
  recent completed regular-season week and limits play-by-play data
  accordingly, so partial-season totals stay accurate as the season
  progresses.
- Player stat categories were chosen to be broadly useful for general
  ranking/rating purposes; additional or more granular stat categories
  can be layered on as needed.