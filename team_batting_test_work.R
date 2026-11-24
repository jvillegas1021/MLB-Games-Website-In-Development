library(tidyverse)
library(DBI)
library(RPostgres)
library(glmnet)
library(caret)
library(baseballr)

game_date = as.Date(format(Sys.time(), tz = "America/New_York"))
current_season = as.numeric(format(Sys.Date(), "%Y"))
sports_id = 1
mlb_batting_roster_gameday_df = NULL

# extract mlb roster lineups gameday
if (is.null(mlb_batting_roster_gameday_df) || nrow(mlb_batting_roster_gameday_df) == 0) {
  mlb_batting_roster_gameday_df <- get_data_from_database('mlb_batting_rosters_gameday')
} else {
  mlb_batting_roster_gameday_df <- mlb_batting_roster_gameday_df
}

# extract teams
if (is.null(mlb_teams) || nrow(mlb_teams) == 0) {
  mlb_teams <- mlb_teams(season = current_season, sport_ids = sports_id)
} else {
  mlb_teams <- mlb_teams
}

historical_table_name <- paste0('team_batting_overall_stats_', current_season)

year_range <- seq(from = current_season, to = (current_season - 2))

mlb_team_batting_stats_df_list <- c()

# for year loop
for (year in year_range) {
  
  statsapi_table <- paste0('batter_statsapi_counts_', year)
  statcast_table <- paste0('batter_statcast_counts_', year)
  
  batter_statsapi_df <- get_data_from_database(statsapi_table)
  batter_statcast_df <- get_data_from_database(statcast_table)
  
  mlb_team_year_df_list <- c()
  
  # for team loop
  for (team in 1:nrow(mlb_teams)) {
    
    mlb_team_id <- as.character(mlb_teams[team, 2])
    mlb_team_name <- as.character(mlb_teams[team, 3])
    mlb_team_abbv <-as.character(mlb_teams[team, 8])
    
    lineup_ids <- mlb_batting_roster_gameday_df %>%
      filter(roster_date == game_date,
             team_id == mlb_team_id) %>%
      pull(roster_lineup_ids)
    
    
    split_up_linup_ids <- str_split(lineup_ids, ",") %>% unlist()
    
    filtered_batter_statsapi <- batter_statsapi_df %>%
      filter(xMLBAMID %in% split_up_linup_ids)
    
    filtered_batter_statcast <- batter_statcast_df %>%
      filter(xMLBAMID %in% split_up_linup_ids)
    
    batter_identifier_df <- create_batter_identifier_df(filtered_batter_statsapi, filtered_batter_statcast)
    
    prepared_batter_stat_counts_df <- prepare_batter_stats_counts_df(filtered_batter_statsapi, filtered_batter_statcast)
    
    max_year <- current_season
    multiplier <- 5
    
    if (max_year - year == 2) {
      multiplier <- 2
    } else if (max_year - year == 1) {
      multiplier <- 3
    } else {
      multiplier <- 5
    }
    
    prepared_batter_stat_counts_df <- process_batter_statcast_decay(prepared_batter_stat_counts_df, multiplier = multiplier)
    
    final_batter_stat_counts_df <- batter_identifier_df %>%
      inner_join(
        prepared_batter_stat_counts_df,
        by = ('xMLBAMID')
      )
    
    mlb_team_year_df_list[[as.character(mlb_team_id)]] <- list(
      team_id = mlb_team_id,
      team_name = mlb_team_name,
      team_abbv = mlb_team_abbv,
      roster_df = final_batter_stat_counts_df
    )
  }
  
  mlb_team_batting_stats_df_list[[as.character(year)]] <- mlb_team_year_df_list
}

mlb_team_batting_stats_df_list[["2024"]][["133"]]$roster_df
mlb_team_batting_stats_df_list[["2025"]][["133"]]$roster_df
mlb_team_batting_stats_df_list[["2026"]][["133"]]$roster_df




































































create_batter_identifier_df <- function(statsapi_df, statcast_df) {
  
  batter_statsapi_identifier_df <- statsapi_df %>%
    select(
      xMLBAMID,
      player_name,
      gamesPlayed
    )
  
  batter_statcast_identifier_df <- statcast_df %>%
    select(
      xMLBAMID,
      raw_plate_appearances = plate_appearances
    )
  
  batter_identifier_df <- batter_statsapi_identifier_df %>%
    inner_join(
      batter_statcast_identifier_df,
      by = c('xMLBAMID' = 'xMLBAMID'),
      relationship = 'one-to-one'
    )
  
  return(batter_identifier_df)
}

prepare_batter_stats_counts_df <- function(statsapi_df, statcast_df) {
  
  # join statsapi and statcast
  combined_batter_data <- statsapi_df %>%
    inner_join(
      statcast_df,
      by = c('xMLBAMID' = 'xMLBAMID'),
      relationship = 'one-to-one'
    )
  
  cleaned_batter_data <- combined_batter_data %>%
    select(xMLBAMID,
           runs,
           doubles,
           triples,
           plate_appearances,
           atBats,
           homeRuns,
           strikeouts,
           baseOnBalls,
           intentionalWalks,
           hits,
           hitByPitch,
           stolenBases,
           rbi,
           groundIntoDoublePlay,
           totalBases,
           sacBunts,
           sacFlies,
           pitches,
           walks,
           pitches_outside_zone,
           swings_outside_zone,
           pitches_inside_zone,
           swings_inside_zone,
           whiffs,
           swings,
           first_pitch_strikes,
           called_strikes,
           batted_balls,
           fly_balls,
           ground_balls,
           line_drives,
           home_runs,
           pop_ups,
           barreled_balls,
           hard_hits_balls,
           contacted_balls,
           contacted_balls_inside_zone,
           contacted_balls_outside_zone,
           ev_sum,
           la_sum,
           estimated_ba_using_speedangle_bip,
           estimated_woba_using_speedangle_bip,
           iso_value_bip,
           estimated_slg_using_speedangle_bip,
           babip_value_bip
    )
  
  return(cleaned_batter_data)
}

process_batter_statcast_decay <- function(statcast_df = NULL, multiplier = 5) {
  
  numeric_cols <- names(statcast_df %>% select(where(function(x) is.numeric(x))))
  
  decayed_statcast_df <- statcast_df %>%
    mutate(
      across(
        all_of(numeric_cols),
        ~.x * multiplier)
    )
}
