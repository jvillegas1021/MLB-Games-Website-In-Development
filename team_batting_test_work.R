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

# process team stats
data_years <- names(mlb_team_batting_stats_df_list)

mlb_team_batting_overall_stats_df_list <- c()

# team loop
for (team in 1:nrow(mlb_teams)) {
  
  team_batting_df_list <- c()
  
  mlb_team_id <- as.character(mlb_teams[team, 2])
  mlb_team_name <- as.character(mlb_teams[team, 3])
  mlb_team_abbv <-as.character(mlb_teams[team, 8])

  # year loop
  for (year in data_years) {
  
  batting_df <- mlb_team_batting_stats_df_list[[year]][[mlb_team_id]][["roster_df"]]
  
  team_batting_df_list[[length(team_batting_df_list) + 1]] <- batting_df
  
  complete_team_batting_df <- bind_rows(team_batting_df_list)
  }
  
  processed_team_batting_df <- process_team_batting_stats(mlb_team_id,
                                                          mlb_team_name,
                                                          mlb_team_abbv,
                                                          game_date,
                                                          complete_team_batting_df)
  
  mlb_team_batting_overall_stats_df_list[[length(mlb_team_batting_overall_stats_df_list) + 1]] <- processed_team_batting_df
  
}
  
complete_team_batting_overall_stats <- bind_rows(mlb_team_batting_overall_stats_df_list)


  


