
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

process_batter_statcast_small_sample_size_batters <- function(statcast_df = NULL, threshold = 150) {
  
  static_cols <- c('xMLBAMID', 'player_name', 'season_count', 'total_games_started', 
                   'total_games_pitched', 'total_plate_appearances', 'update_date')
  
  average_cols <- setdiff(names(statcast_df), static_cols)
  
  league_averages_df <- statcast_df %>%
    filter(total_plate_appearances >= threshold) %>%
    summarise(across(all_of(average_cols), ~mean(.x, na.rm = TRUE)))
  
  curated_statcast_df <- statcast_df %>%
    mutate(
      sample_weight = pmin(total_plate_appearances / threshold, 1),
      across(
        all_of(average_cols),
        ~ (sample_weight * .x) + ((1 - sample_weight) * league_averages_df[[cur_column()]])
      )
    ) %>%
    select(-sample_weight)
  
  return(curated_statcast_df)
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

process_batter_statcast_stats <- function(statcast_df = NULL) {
  processed_batter_statcast_df <- statcast_df %>%
    group_by(xMLBAMID, player_name) %>%
    mutate(
      babip_denom_row = atBats - strikeouts - homeRuns + sacFlies,
      babip_num_row   = hits - homeRuns,
      lob_denom_row   = hits + baseOnBalls + hitByPitch - (1.4 * homeRuns),
      lob_num_row     = hits + baseOnBalls + hitByPitch - runs,
      fip_num_row     = (13 * homeRuns) + (3 * (baseOnBalls + hitByPitch)) - (2 * strikeouts),
      obp_denom_row   = atBats + baseOnBalls + hitByPitch + sacFlies,
      obp_num_row     = hits + baseOnBalls + hitByPitch
    ) %>%
    summarise(
      season_count = n(),
      total_games_started = sum(gamesStarted),
      total_games_pitched = sum(gamesPitched),
      total_plate_appearances = sum(raw_plate_appearances),
      # Control block
      `BB%`       = if_else(sum(plate_appearances) == 0, 0, sum(walks) / sum(plate_appearances)),
      `K%`        = if_else(sum(plate_appearances) == 0, 0, sum(strikeouts) / sum(plate_appearances)),
      `K/9`       = if_else(sum(innings_pitched) == 0, 0, sum(strikeouts) * 9 / sum(innings_pitched)),
      `BB/9`      = if_else(sum(innings_pitched) == 0, 0, sum(walks) * 9 / sum(innings_pitched)),
      `K-BB%`     = `K%` - `BB%`,
      `Chase%`    = if_else(sum(pitches_outside_zone) == 0, 0, sum(swings_outside_zone) / sum(pitches_outside_zone)),
      `Whiff%`    = if_else(sum(swings) == 0, 0, sum(whiffs) / sum(swings)),
      `Zone%`     = if_else(sum(pitches) == 0, 0, sum(pitches_inside_zone) / sum(pitches)),
      `F-Strike%` = if_else(sum(plate_appearances) == 0, 0, sum(first_pitch_strikes) / sum(plate_appearances)),
      `CStr%`     = if_else(sum(pitches) == 0, 0, sum(called_strikes) / sum(pitches)),
      `SwStr%`    = if_else(sum(pitches) == 0, 0, sum(whiffs) / sum(pitches)),
      `C+SwStr%`  = if_else(sum(pitches) == 0, 0, (sum(called_strikes) + sum(whiffs)) / sum(pitches)),
      
      # Contact block
      `GB%`        = if_else(sum(batted_balls) == 0, 0, sum(ground_balls) / sum(batted_balls)),
      `FB%`        = if_else(sum(batted_balls) == 0, 0, sum(fly_balls) / sum(batted_balls)),
      `LD%`        = if_else(sum(batted_balls) == 0, 0, sum(line_drives) / sum(batted_balls)),
      `PU%`        = if_else(sum(batted_balls) == 0, 0, sum(pop_ups) / sum(batted_balls)),
      `GB/FB`      = if_else(sum(fly_balls) == 0, 0, sum(ground_balls) / sum(fly_balls)),
      `HR/FB`      = if_else(sum(fly_balls) == 0, 0, sum(home_runs) / sum(fly_balls)),
      `IFFB%`      = if_else(sum(fly_balls) == 0, 0, sum(pop_ups) / sum(fly_balls)),
      `Barrel%`    = if_else(sum(batted_balls) == 0, 0, sum(barreled_balls) / sum(batted_balls)),
      `HardHit%`   = if_else(sum(batted_balls) == 0, 0, sum(hard_hits_balls) / sum(batted_balls)), 
      `Z-Contact%` = if_else(sum(swings_inside_zone) == 0, 0, sum(contacted_balls_inside_zone) / sum(swings_inside_zone)),
      `O-Contact%` = if_else(sum(swings_outside_zone) == 0, 0, sum(contacted_balls_outside_zone) / sum(swings_outside_zone)),
      `Contact%`   = if_else(sum(swings) == 0, 0, sum(contacted_balls) / sum(swings)),
      `EV`         = if_else(sum(batted_balls) == 0, 0, sum(ev_sum) / sum(batted_balls)),
      `LA`         = if_else(sum(batted_balls) == 0, 0, sum(la_sum) / sum(batted_balls)),
      
      # Results block
      `H/9`   = if_else(sum(inningsPitched) == 0, 0, (sum(hits) * 9) / sum(inningsPitched)),
      `HR/9`  = if_else(sum(inningsPitched) == 0, 0, (sum(homeRuns) * 9) / sum(inningsPitched)),
      `ERA`   = if_else(sum(inningsPitched) == 0, 0, (sum(earnedRuns) * 9) / sum(inningsPitched)),
      `AVG`   = if_else(sum(atBats) == 0, 0, sum(hits) / sum(atBats)),
      `WHIP`  = if_else(sum(inningsPitched) == 0, 0, (sum(hits) + sum(baseOnBalls)) / sum(inningsPitched)),
      `BABIP` = if_else(sum(babip_denom_row) == 0, 0, sum(babip_num_row) / sum(babip_denom_row)),
      `LOB%`  = if_else(sum(lob_denom_row) == 0, 0, sum(lob_num_row) / sum(lob_denom_row)),
      `FIP`   = if_else(sum(inningsPitched) == 0, 0, sum(fip_num_row) / sum(inningsPitched) + 4.00),
      `OBP`   = if_else(sum(obp_denom_row) == 0, 0, sum(obp_num_row) / sum(obp_denom_row)),
      `SLG`   = if_else(sum(atBats) == 0, 0, sum(totalBases) / sum(atBats)),  
      `OPS`   = `OBP` + `SLG`,
      
      # Expected block
      `xBA`    = if_else(sum(batted_balls) == 0, 0, sum(estimated_ba_using_speedangle_bip) / sum(batted_balls)),
      `xwOBA`  = if_else(sum(batted_balls) == 0, 0, sum(estimated_woba_using_speedangle_bip) / sum(batted_balls)),
      `xSLG`   = if_else(sum(batted_balls) == 0, 0, sum(estimated_slg_using_speedangle_bip) / sum(batted_balls)),
      `xISO`   = if_else(sum(batted_balls) == 0, 0, sum(iso_value_bip) / sum(batted_balls)),
      `xBABIP` = if_else(sum(batted_balls) == 0, 0, sum(babip_value_bip) / sum(batted_balls)),
      
      .groups = "drop"
    ) %>%
    mutate(
      update_date = Sys.Date()
    )
  
  return(processed_batter_statcast_df)
}













create_team_batting_df <- function(team_name,
                                   team_id,
                                   team_abbv,
                                   game_date,
                                   mlb_batting_roster_gameday_df,
                                   batter_stats_statcast_df) {
  
  lineup_ids <- mlb_batting_roster_gameday_df %>%
    filter(roster_date == game_date,
           team_id == !!team_id) %>%
    pull(roster_lineup_ids)
  
  
  split_up_linup_ids <- str_split(lineup_ids, ",") %>% unlist()
  
  filtered_batter_statcast <- batter_stats_statcast_df %>%
    filter(xMLBAMID %in% split_up_linup_ids)
  
  if (nrow(filtered_batter_statcast) == 0 || length(filtered_batter_statcast) == 0){
    return(tibble())
  }
  
  wBB <- 0.698
  wHBP <- 0.729
  w1B <- 0.890
  w2B <- 1.261
  w3B <- 1.596
  wHR <- 2.049
  
  league_woba <- 0.316
  wOBAScale <- 1.238
  R_PA_lg <- 0.118
  
  # create team df
  team_batting_df <- data.frame(team_name = team_name,
                                team_id = team_id,
                                team_abbv = team_abbv,
                                game_date = game_date)
  
  batter_stats_summarized <- filtered_batter_statcast %>%
    summarise(
      # --- Intermediate Sums needed for calculations ---
      # (These will automatically be dropped at the end, leaving only your rates)
      pa  = as.numeric(sum(plate_appearances, na.rm = TRUE)),
      so  = as.numeric(sum(strikeouts, na.rm = TRUE)),
      bb  = as.numeric(sum(walks, na.rm = TRUE)),
      hbp = as.numeric(sum(hit_by_pitches, na.rm = TRUE)),
      h   = as.numeric(sum(hits, na.rm = TRUE)),
      b1  = as.numeric(sum(singles, na.rm = TRUE)),
      b2  = as.numeric(sum(doubles, na.rm = TRUE)),
      b3  = as.numeric(sum(triples, na.rm = TRUE)),
      hr  = as.numeric(sum(home_runs, na.rm = TRUE)),
      sf  = as.numeric(sum(sac_flies, na.rm = TRUE)),
      
      gb  = as.numeric(sum(GB, na.rm = TRUE)),
      fb  = as.numeric(sum(FB, na.rm = TRUE)),
      ld  = as.numeric(sum(LD, na.rm = TRUE)),
      pu  = as.numeric(sum(PU, na.rm = TRUE)),
      
      # --- Calculations ---
      `K%` = so / pa,
      `BB%` = bb / pa,
      `BB/K` = bb / so,
      
      `GB%` = gb / (gb + fb + ld),
      `FB%` = fb / (gb + fb + ld),
      `LD%` = ld / (gb + fb + ld),
      `HR/FB` = hr / fb,
      `GB/FB` = gb / fb,
      `IFFB%` = pu / fb,
      
      at_bats = pa - sum(non_at_bats, na.rm = TRUE), 
      AVG = h / at_bats,
      SLG = ((b1 * 1) + (b2 * 2) + (b3 * 3) + (hr * 4)) / at_bats,
      ISO = SLG - AVG,
      
      BABIP = (h - hr) / (at_bats - so - hr + sf),
      OBP = (h + bb + hbp) / (at_bats + bb + hbp + sf),
      OPS = OBP + SLG,
      
      # Statcast averages
      EV = sum(launch_speed_sum, na.rm = TRUE) / sum(batted_balls, na.rm = TRUE),
      LA = sum(launch_angle_sum, na.rm = TRUE) / sum(batted_balls, na.rm = TRUE),
      xBA = sum(ba_speedangle_sum, na.rm = TRUE) / sum(batted_balls, na.rm = TRUE),
      xWOBA = sum(woba_speedangle_sum, na.rm = TRUE) / sum(batted_balls, na.rm = TRUE),
      xSLG = sum(slg_speedangle_sum, na.rm = TRUE) / sum(batted_balls, na.rm = TRUE),
      xISO = sum(iso_value_sum, na.rm = TRUE) / sum(batted_balls, na.rm = TRUE),
      xBABIP = sum(babip_value_sum, na.rm = TRUE) / sum(batted_balls, na.rm = TRUE),
      
      `HardHit%` = sum(hard_hit_balls, na.rm = TRUE) / sum(batted_balls, na.rm = TRUE),
      `Barrel%` = sum(barrel_balls, na.rm = TRUE) / sum(batted_balls, na.rm = TRUE),
      
      # Plate Discipline
      `Z-Swing%` = sum(swings_in_zone, na.rm = TRUE) / sum(pitches_in_zone, na.rm = TRUE),
      `Z-Contact%` = sum(contacted_balls_in_zone, na.rm = TRUE) / sum(swings_in_zone, na.rm = TRUE),
      `Contact%` = sum(contacted_balls, na.rm = TRUE) / sum(swings, na.rm = TRUE),
      `O-Contact%` = sum(contacted_balls_outside_zone, na.rm = TRUE) / sum(swings_outside_zone, na.rm = TRUE),
      `O-Swing%` = sum(swings_outside_zone, na.rm = TRUE) / sum(pitches_outside_zone, na.rm = TRUE),
      `Chase%` = sum(swings_outside_zone, na.rm = TRUE) / sum(pitches_outside_zone, na.rm = TRUE), 
      `Whiff%` = sum(whiffs, na.rm = TRUE) / sum(swings, na.rm = TRUE),
      `SwStr%` = sum(whiffs, na.rm = TRUE) / sum(pitches, na.rm = TRUE),
      `F-Strike%` = sum(first_pitch_strikes, na.rm = TRUE) / pa, 
      
      # Advanced Linear Weights
      woba_num = (wBB * bb) + (wHBP * hbp) + (w1B * b1) + (w2B * b2) + (w3B * b3) + (wHR * hr),
      wOBA = if_else(pa == 0, 0, woba_num / pa),
      wRAA = if_else(pa == 0, 0, ((wOBA - league_woba) / wOBAScale) * pa),
      wRC = wRAA + (R_PA_lg * pa),
      `wRC+` = if_else(pa == 0, 0, 100 * (((wRAA / pa) / R_PA_lg) + 1))
    ) %>%
    select(
      'K%', 'BB%', 'BB/K', 'O-Swing%', 'Z-Swing%', 'Chase%', 'Whiff%',
      'GB%', 'FB%', 'LD%', 'GB/FB', 'HR/FB', 'IFFB%', 'Barrel%', 'HardHit%',
      'Z-Contact%', 'O-Contact%', 'Contact%', 'EV', 'LA',
      'ISO', 'OPS', 'BABIP', 'AVG', 'OBP', 'SLG', 'wOBA', 'wRAA', 'wRC', 'wRC+',
      'xWOBA', 'xBA', 'xSLG', 'xISO', 'xBABIP'
    ) %>%
    mutate(
      batter_ids = lineup_ids,
      update_date = Sys.time()
    )
  
  final_team_batting_df <- bind_cols(team_batting_df, batter_stats_summarized)
  
  return(final_team_batting_df)
  
}

create_mlb_teams_batting_df <- function(game_day = as.Date(format(Sys.time(), tz = "America/New_York")),
                                        mlb_roster_df,
                                        batter_statcast_df,
                                        mlb_teams) {
  current_date <- game_day
  
  batting_df_list <- c()
  
  for (team in 1:nrow(mlb_teams)) {
    mlb_team_id <- as.character(mlb_teams[team, 2])
    mlb_team_name <- mlb_teams[team, 3]
    mlb_team_abbv <- mlb_teams[team, 8]
    
    mlb_team_batting_df <- create_team_batting_df(
      team_name = mlb_team_name,
      team_id = mlb_team_id,
      team_abbv = mlb_team_abbv,
      game_date = current_date,
      mlb_batting_roster_gameday_df = mlb_batting_roster_gameday_df,
      batter_stats_statcast_df = batter_stats_statcast_df
    )
    
    batting_df_list[[length(batting_df_list) + 1]] <- mlb_team_batting_df
  }
  
  final_mlb_teams_batting_df <- bind_rows(batting_df_list)
  
  return(final_mlb_teams_batting_df)
}