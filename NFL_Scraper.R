# NFL-Player-Stats
# Copyright (C) 2026 Kyle Kuberski
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <https://www.gnu.org/licenses/>.

# install.packages(c("dplyr","tidyr","nflreadr","openxlsx","lubridate"))
library(dplyr)
library(tidyr)
library(nflreadr)
library(openxlsx)
library(lubridate)
message("WD = ", normalizePath(getwd()))

# --- Helpers ---
sum_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}
mean_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

# --- Seasons to process ---
seasons <- c(2022, 2023, 2024, 2025)

# --- Current week for 2025 (REG only, up to today) ---
sched <- load_schedules(2025)
valid_game_types <- c("REG")

current_week <- max(
  sched$week[sched$gameday <= Sys.Date() & sched$game_type %in% valid_game_types],
  na.rm = TRUE
)

for (season in seasons) {
  message("Processing season: ", season)

  # --- Play-by-play (REG only) ---
  pbp <- load_pbp(season)

  if ("season_type" %in% names(pbp)) {
    pbp <- pbp %>% filter(season_type == "REG")
  } else if ("game_type" %in% names(pbp)) {
    pbp <- pbp %>% filter(game_type == "REG")
  } else {
    stop("Neither season_type nor game_type exists in pbp for season ", season)
  }

  if (season == 2025) {
    pbp <- pbp %>% filter(week <= current_week)
  }

  # --- Roster ---
  roster <- load_rosters(season) %>%
    mutate(
      season_ref_date = as.Date(paste0(season, "-09-01")),
      age = floor(time_length(interval(birth_date, season_ref_date), "years"))
    ) %>%
    transmute(
      player_id = gsis_id,
      full_name = paste(first_name, last_name),
      position,
      team,
      age
    )

  # --- Team totals (for receiving shares) ---
  team_totals <- pbp %>%
    group_by(season, team_play = posteam) %>%
    summarise(
      team_targets   = sum(!is.na(receiver_player_id), na.rm = TRUE),
      team_air_yards = sum_or_na(air_yards),
      .groups = "drop"
    )

  # --- Passing (QB) ---
  pass <- pbp %>%
    filter(!is.na(passer_player_id)) %>%
    group_by(player_id = passer_player_id, player_name = passer_player_name, posteam) %>%
    summarise(
      pass_attempts   = sum_or_na(pass_attempt == 1 & sack == 0 & qb_spike == 0 & qb_kneel == 0),
      dropbacks       = sum_or_na(qb_dropback == 1),
      completions     = sum_or_na(complete_pass == 1),
      pass_yards      = sum_or_na(passing_yards),
      pass_tds        = sum_or_na(pass_touchdown == 1),
      interceptions   = sum_or_na(interception == 1),
      sacks_taken     = sum_or_na(sack == 1),
      pass_air_yards  = sum_or_na(air_yards),
      epa_per_dropback = mean_or_na(ifelse(qb_dropback == 1, epa, NA_real_)),
      cpoe_avg        = mean_or_na(cpoe),
      .groups = "drop"
    ) %>%
    rename(team = posteam)

  # --- Rushing (RB/QB/WR) ---
  rush <- pbp %>%
    filter(!is.na(rusher_player_id)) %>%
    group_by(player_id = rusher_player_id, player_name = rusher_player_name) %>%
    summarise(
      rush_attempts     = n(),
      rush_yards        = sum_or_na(rushing_yards),
      rush_tds          = sum_or_na(rush_touchdown == 1),
      rush_first_downs  = sum_or_na(first_down_rush == 1),
      rush_epa_per_play = mean_or_na(epa),
      fumbles           = sum_or_na(fumble == 1),
      .groups = "drop"
    )

  # --- Receiving (WR/TE/RB) ---
  recv <- pbp %>%
    filter(!is.na(receiver_player_id)) %>%
    group_by(player_id = receiver_player_id, player_name = receiver_player_name, team = posteam) %>%
    summarise(
      targets         = n(),
      receptions      = sum_or_na(complete_pass == 1),
      rec_yards       = sum_or_na(passing_yards),
      rec_tds         = sum_or_na(pass_touchdown == 1),
      air_yards       = sum_or_na(air_yards),
      yac_yards       = sum_or_na(yards_after_catch),
      rec_first_downs = sum_or_na(first_down_pass == 1),
      .groups = "drop"
    ) %>%
    left_join(team_totals, by = c("team" = "team_play")) %>%
    select(-team_targets, -team_air_yards)

  # --- Defense (core categories only) ---
  sacks <- pbp %>%
    transmute(
      sack_player_id, sack_player_name,
      half_sack_1_player_id, half_sack_1_player_name,
      half_sack_2_player_id, half_sack_2_player_name
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = c("stat", ".value"),
      names_pattern = "(.*)_(player_id|player_name)"
    ) %>%
    filter(!is.na(player_id)) %>%
    mutate(sack_credit = ifelse(grepl("^half_sack_", stat), 0.5, 1.0)) %>%
    group_by(player_id, player_name) %>%
    summarise(sacks = sum(sack_credit, na.rm = TRUE), .groups = "drop")

  ints <- pbp %>%
    select(interception_player_id, interception_player_name) %>%
    filter(!is.na(interception_player_id)) %>%
    group_by(player_id = interception_player_id, player_name = interception_player_name) %>%
    summarise(interceptions_def = n(), .groups = "drop")

  ff <- pbp %>%
    select(forced_fumble_player_1_player_id, forced_fumble_player_1_player_name,
           forced_fumble_player_2_player_id, forced_fumble_player_2_player_name) %>%
    pivot_longer(cols = everything(),
                 names_to = c("stat", ".value"),
                 names_pattern = "(.*)_(player_id|player_name)") %>%
    filter(!is.na(player_id)) %>%
    group_by(player_id, player_name) %>%
    summarise(forced_fumbles = n(), .groups = "drop")

  defense <- sacks %>%
    full_join(ints, by = c("player_id", "player_name")) %>%
    full_join(ff,   by = c("player_id", "player_name"))

  # --- Special Teams (core categories only) ---
  kicking <- pbp %>%
    filter(!is.na(kicker_player_id)) %>%
    group_by(player_id = kicker_player_id, player_name = kicker_player_name) %>%
    summarise(
      fg_att  = sum_or_na(field_goal_attempt == 1),
      fg_made = sum_or_na(field_goal_attempt == 1 & field_goal_result == "made"),
      xp_att  = sum_or_na(extra_point_attempt == 1),
      xp_made = sum_or_na(extra_point_attempt == 1 & extra_point_result == "good"),
      .groups = "drop"
    )

  punting <- pbp %>%
    filter(play_type == "punt", !is.na(punter_player_id)) %>%
    mutate(
      punt_dist = suppressWarnings(as.numeric(kick_distance)),
      ret_yds   = if ("return_yards" %in% names(.)) suppressWarnings(as.numeric(return_yards)) else NA_real_,
      tb_yds    = ifelse(touchback == 1, 20, 0),
      net_calc  = punt_dist - coalesce(ret_yds, 0) - tb_yds
    ) %>%
    group_by(player_id = punter_player_id, player_name = punter_player_name) %>%
    summarise(
      att_punts        = dplyr::n(),
      punt_yards_gross = sum_or_na(punt_dist),
      net_punt_yards   = sum_or_na(net_calc),
      .groups = "drop"
    )

  special <- kicking %>%
    full_join(punting, by = c("player_id", "player_name"))

  # --- Assemble master table ---
  player_stats <- pass %>%
    full_join(rush,    by = c("player_id", "player_name")) %>%
    full_join(recv,    by = c("player_id", "player_name", "team")) %>%
    full_join(defense, by = c("player_id", "player_name")) %>%
    full_join(special, by = c("player_id", "player_name")) %>%
    left_join(roster,  by = "player_id") %>%
    mutate(
      player_name = coalesce(full_name, player_name),
      team = coalesce(team.x, team.y)
    ) %>%
    select(-full_name, -team.x, -team.y)

  # --- Totals ---
  player_stats_totals <- player_stats %>%
    group_by(player_id, player_name, position) %>%
    summarise(
      across(where(is.numeric) & !any_of("age"), sum_or_na),
      age = if (all(is.na(age))) NA_real_ else max(age, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      id = row_number(),
      season = !!season
    ) %>%
    select(-player_id) %>%
    relocate(id, .before = 1) %>%
    relocate(season, .after = last_col())

  # --- Save each season ---
  out_file <- file.path(getwd(), sprintf("AllRanksNFL_%s.xlsx", season))

  openxlsx::write.xlsx(
    list(Totals = player_stats_totals),
    file = out_file,
    overwrite = TRUE
  )

  message("Saved to: ", normalizePath(out_file))
}
