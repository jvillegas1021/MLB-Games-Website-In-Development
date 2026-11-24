update_mlb_team_batting_stats_gameday <- function(game_date = as.Date(format(Sys.time(), tz = "America/New_York")),
                                                  current_season = as.numeric(format(Sys.Date(), "%Y")),
                                                  sports_id = 1,
                                                  mlb_batting_roster_gameday_df = NULL,
                                                  mlb_teams = NULL) {
  
  
  print(paste0("Processing ", game_date, ", ", current_season))
  # extract batting_rosters_gameday
  if (is.null(mlb_batting_roster_gameday_df) || nrow(mlb_batting_roster_gameday_df) == 0) {
    mlb_batting_roster_gameday_df <- get_data_from_database('mlb_batting_rosters_gameday')
  } else {
    mlb_batting_roster_gameday_df <- mlb_batting_roster_gameday_df
  }
  # extract statsapi data
  if (is.null(batter_stats_statsapi_df) || nrow(batter_stats_statsapi_df) == 0) {
    batter_stats_statsapi_df <- get_data_from_database('batter_seasonal_data_statcast')
  } else {
    batter_stats_statcast_df <- batter_stats_statcast_df
  }
  # extract statcast data
  if (is.null(batter_stats_statcast_df) || nrow(batter_stats_statcast_df) == 0) {
    batter_stats_statcast_df <- get_data_from_database('batter_seasonal_data_statcast')
  } else {
    batter_stats_statcast_df <- batter_stats_statcast_df
  }
  # extract teams
  if (is.null(mlb_teams) || nrow(mlb_teams) == 0) {
    mlb_teams <- mlb_teams(season = current_season, sport_ids = sports_id)
  } else {
    mlb_teams <- mlb_teams
  }
  
  
  
  
  historical_table_name <- paste0('team_batting_overall_stats_', current_season)
  
  year_range <- seq(from = current_year, to = (current_season - 2))
  
  team_batting_stats_counts_df_list <- c()
  
  for (year in year_range) {
    
    statsapi_table <- paste0('batter_statsapi_counts_', year)
    statcast_table <- paste0('batter_statcast_counts_', year)
    
    batter_statsapi_df <- get_data_from_database(statsapi_table)
    batter_statcast_df <- get_data_from_database(statcast_table)
    
    batter_identifier_df <- create_batter_identifier_df(batter_statsapi_df, batter_statcast_df)
    
    prepared_batter_stat_counts_df <- prepare_batter_stats_counts_df(batter_statsapi_df, batter_statcast_df)
    
    max_year <- current_year
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
    
    batter_stats_counts_df_list[[length(batter_stats_counts_df_list) + 1]] <- final_batter_stat_counts_df    
    
  }
  
  batter_stat_counts_df <- bind_rows(batter_stats_counts_df_list)
  
  processed_batter_stats_df <- process_batter_statcast_stats(batter_stat_counts_df)
  
  sample_sized_batter_stats_df <- process_batter_statcast_small_sample_size_batters(processed_batter_stats_df)
  
  batters_to_update <- sample_sized_batter_stats_df$xMLBAMID
  
  delete_batter_stats_from_df(historical_table_name, batters_to_update)
  
  write_df_to_sql_append(sample_sized_batter_stats_df, historical_table_name)
  
}
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #process data
  mlb_teams_batting_df <- create_mlb_teams_batting_df(game_date,
                                                      mlb_batting_roster_gameday_df,
                                                      batter_stats_statcast_df,
                                                      mlb_teams)
  # load data
  write_df_to_sql_append('mlb_team_batting_stats_gameday', mlb_teams_batting_df)
  
  print(paste0("Completed ", game_date, ", ", current_season))
}
