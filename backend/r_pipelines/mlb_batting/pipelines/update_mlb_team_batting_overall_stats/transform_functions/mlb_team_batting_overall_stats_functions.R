
create_batter_identifier_df <- function(statsapi_df, statcast_df) {
  
  batter_statsapi_identifier_df <- statsapi_df %>%
    select(
      xMLBAMID,
      player_name,
      gamesPlayed
    )
  
  batter_statcast_identifier_df <- statcast_df %>%
    select(
      xMLBAMID
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

process_team_batting_stats <- function(mlb_team_name,
                                       mlb_team_id,
                                       mlb_team_abbv,
                                       game_date,
                                       team_batting_stats_df = NULL) {
  
  # create team df
  team_batting_df <- data.frame(team_name = mlb_team_name,
                                team_id = mlb_team_id,
                                team_abbv = mlb_team_abbv,
                                game_date = game_date)
  
  
  wBB <- 0.698
  wHBP <- 0.729
  w1B <- 0.890
  w2B <- 1.261
  w3B <- 1.596
  wHR <- 2.049
  
  league_woba <- 0.316
  wOBAScale <- 1.238
  R_PA_lg <- 0.118
  
  
  processed_batter_statcast_df <- team_batting_stats_df %>%
    mutate(
      babip_denom_row = atBats - strikeouts - homeRuns + sacFlies,
      babip_num_row   = hits - homeRuns,
      obp_denom_row   = atBats + baseOnBalls + hitByPitch + sacFlies,
      obp_num_row     = hits + baseOnBalls + hitByPitch
    ) %>%
    summarise(
      pa        = as.numeric(sum(plate_appearances, na.rm = TRUE)),
      ab        = as.numeric(sum(atBats, na.rm = TRUE)),
      bb        = as.numeric(sum(baseOnBalls, na.rm = TRUE)),
      hbp       = as.numeric(sum(hitByPitch, na.rm = TRUE)),
      h         = as.numeric(sum(hits, na.rm = TRUE)),
      b2        = as.numeric(sum(doubles, na.rm = TRUE)),
      b3        = as.numeric(sum(triples, na.rm = TRUE)),
      hr        = as.numeric(sum(homeRuns, na.rm = TRUE)),
      b1        = h - (b2 + b3 + hr),
      woba_denom = as.numeric(sum(obp_denom_row, na.rm = TRUE)),
      
      # Control block
      `BB%`       = if_else(pa == 0, 0, bb / pa),
      `K%`        = if_else(pa == 0, 0, sum(strikeouts, na.rm = TRUE) / pa),
      `Chase%`    = if_else(sum(pitches_outside_zone, na.rm = TRUE) == 0, 0, sum(swings_outside_zone, na.rm = TRUE) / sum(pitches_outside_zone, na.rm = TRUE)),
      `Whiff%`    = if_else(sum(swings, na.rm = TRUE) == 0, 0, sum(whiffs, na.rm = TRUE) / sum(swings, na.rm = TRUE)),
      `Z-Swing%`  = if_else(sum(pitches_inside_zone, na.rm = TRUE) == 0, 0, sum(swings_inside_zone, na.rm = TRUE) / sum(pitches_inside_zone, na.rm = TRUE)),
      `F-Strike%` = if_else(pa == 0, 0, sum(first_pitch_strikes, na.rm = TRUE) / pa),
      `CStr%`     = if_else(sum(pitches, na.rm = TRUE) == 0, 0, sum(called_strikes, na.rm = TRUE) / sum(pitches, na.rm = TRUE)),
      `C+SwStr%`  = if_else(sum(pitches, na.rm = TRUE) == 0, 0, (sum(called_strikes, na.rm = TRUE) + sum(whiffs, na.rm = TRUE)) / sum(pitches, na.rm = TRUE)),
      
      # Contact block
      `GB%`        = if_else(sum(batted_balls, na.rm = TRUE) == 0, 0, sum(ground_balls, na.rm = TRUE) / sum(batted_balls, na.rm = TRUE)),
      `FB%`        = if_else(sum(batted_balls, na.rm = TRUE) == 0, 0, sum(fly_balls, na.rm = TRUE) / sum(batted_balls, na.rm = TRUE)),
      `LD%`        = if_else(sum(batted_balls, na.rm = TRUE) == 0, 0, sum(line_drives, na.rm = TRUE) / sum(batted_balls, na.rm = TRUE)),
      `PU%`        = if_else(sum(batted_balls, na.rm = TRUE) == 0, 0, sum(pop_ups, na.rm = TRUE) / sum(batted_balls, na.rm = TRUE)),
      `GB/FB`      = if_else(sum(fly_balls, na.rm = TRUE) == 0, 0, sum(ground_balls, na.rm = TRUE) / sum(fly_balls, na.rm = TRUE)),
      `HR/FB`      = if_else(sum(fly_balls, na.rm = TRUE) == 0, 0, hr / sum(fly_balls, na.rm = TRUE)),
      `IFFB%`      = if_else(sum(fly_balls, na.rm = TRUE) == 0, 0, sum(pop_ups, na.rm = TRUE) / sum(fly_balls, na.rm = TRUE)),
      `Barrel%`    = if_else(sum(batted_balls, na.rm = TRUE) == 0, 0, sum(barreled_balls, na.rm = TRUE) / sum(batted_balls, na.rm = TRUE)),
      `HardHit%`   = if_else(sum(batted_balls, na.rm = TRUE) == 0, 0, sum(hard_hits_balls, na.rm = TRUE) / sum(batted_balls, na.rm = TRUE)), 
      `Z-Contact%` = if_else(sum(swings_inside_zone, na.rm = TRUE) == 0, 0, sum(contacted_balls_inside_zone, na.rm = TRUE) / sum(swings_inside_zone, na.rm = TRUE)),
      `O-Contact%` = if_else(sum(swings_outside_zone, na.rm = TRUE) == 0, 0, sum(contacted_balls_outside_zone, na.rm = TRUE) / sum(swings_outside_zone, na.rm = TRUE)),
      `Contact%`   = if_else(sum(swings, na.rm = TRUE) == 0, 0, sum(contacted_balls, na.rm = TRUE) / sum(swings, na.rm = TRUE)),
      `EV`         = if_else(sum(batted_balls, na.rm = TRUE) == 0, 0, sum(ev_sum, na.rm = TRUE) / sum(batted_balls, na.rm = TRUE)),
      `LA`         = if_else(sum(batted_balls, na.rm = TRUE) == 0, 0, sum(la_sum, na.rm = TRUE) / sum(batted_balls, na.rm = TRUE)),
      
      # Run creating block
      woba_num = (wBB * bb) + (wHBP * hbp) + (w1B * b1) + (w2B * b2) + (w3B * b3) + (wHR * hr),
      wOBA     = if_else(woba_denom == 0, 0, woba_num / woba_denom),
      wRAA     = if_else(pa == 0, 0, ((wOBA - league_woba) / wOBAScale) * pa),
      wRC      = wRAA + (R_PA_lg * pa),
      `wRC+`   = if_else(pa == 0, 0, 100 * (((wRAA / pa) / R_PA_lg) + 1)),
      
      # Results block
      `AVG`   = if_else(ab == 0, 0, h / ab),
      `BABIP` = if_else(sum(babip_denom_row, na.rm = TRUE) == 0, 0, sum(babip_num_row, na.rm = TRUE) / sum(babip_denom_row, na.rm = TRUE)),
      `OBP`   = if_else(woba_denom == 0, 0, sum(obp_num_row, na.rm = TRUE) / woba_denom),
      `SLG`   = if_else(ab == 0, 0, sum(totalBases, na.rm = TRUE) / ab),  
      `OPS`   = `OBP` + `SLG`,
      
      # Expected block
      `xBA`    = if_else(ab == 0, 0, sum(estimated_ba_using_speedangle_bip, na.rm = TRUE) / ab),
      `xwOBA`  = if_else(woba_denom == 0, 0, ((wBB * bb) + (wHBP * hbp) + sum(estimated_woba_using_speedangle_bip, na.rm = TRUE)) / woba_denom),
      `xSLG`   = if_else(ab == 0, 0, sum(estimated_slg_using_speedangle_bip, na.rm = TRUE) / ab),
      `xISO`   = if_else(ab == 0, 0, sum(iso_value_bip, na.rm = TRUE) / ab),
      `xBABIP` = if_else(sum(batted_balls, na.rm = TRUE) == 0, 0, sum(babip_value_bip, na.rm = TRUE) / sum(batted_balls, na.rm = TRUE)),
      
      .groups = "drop"
    ) %>%
    mutate(
      update_date = Sys.Date()
    )
  
  complete_team_batting_df <- bind_cols(team_batting_df, processed_batter_statcast_df)
  
  final_team_batting_overall_stats_df <- complete_team_batting_df %>%
    select(
      team_name,
      team_id,
      team_abbv,
      game_date,
      `K%`,
      `BB%`,
      `Chase%`,
      `Whiff%`,
      `Z-Swing%`,
      `F-Strike%`,
      `CStr%`,
      `C+SwStr%`,
      `GB%`,
      `FB%`,
      `LD%`,
      `PU%`,
      `GB/FB`,
      `HR/FB`,
      `IFFB%`,
      `Barrel%`,
      `HardHit%`,
      `Z-Contact%`,
      `O-Contact%`,
      `Contact%`,
      EV,
      LA,
      `wRC+`,
      wOBA,
      wRAA,
      wRC,
      AVG,
      OBP,
      SLG,
      OPS,
      BABIP,
      xBA,
      xwOBA,
      xSLG,
      xISO,
      xBABIP
    )
  
  return(final_team_batting_overall_stats_df)
}












  
 