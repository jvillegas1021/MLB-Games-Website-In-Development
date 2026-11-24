library(tidyverse)
library(DBI)
library(RPostgres)
library(glmnet)
library(caret)
library(baseballr)

################ CALCULATE BATTING SCORE WEIGHTS FOR STATS ##################



# 1. CORE DISCIPLINE 
block_discipline <- c('K%', 'BB%', 'Z-Swing%', 'Chase%', 'Whiff%', 'F-Strike%', 'CStr%', 'C+SwStr%')
block_contact     <- c('GB%', 'FB%', 'LD%', 'PU', 'GB/FB', 'HR/FB', 'IFFB%', 'Barrel%', 'HardHit%',
                       'Z-Contact%', 'O-Contact%', 'Contact%', 'EV', 'LA')
block_creation  <- c('wRC+', 'wOBA', 'wRAA', 'wRC')
block_results <- c('AVG', 'OBP', 'SLG', 'OPS', 'BABIP')
block_expected   <- c('xwOBA', 'xBA', 'xSLG', 'xISO', 'xBABIP')


# Create a master list of all tracking stats to clean in the next step
all_stats_to_clean <- c(block_discipline, block_contact, block_creation, block_results, block_expected)

# 1. Combine all your block stats plus runs_scored into one master list
all_numeric_cols <- c(all_stats_to_clean, 'wOBA_fg')

# 2. Force every stat column to be numeric (fixes character/factor text issues)
team_batting_df[all_numeric_cols] <- lapply(
  team_batting_df[all_numeric_cols], 
  function(x) as.numeric(as.character(x))
)

# 3. Clean up NA values by replacing them with the league average for that stat
for (col in all_numeric_cols) {
  if (any(is.na(team_batting_df[[col]]))) {
    league_avg <- mean(team_batting_df[[col]], na.rm = TRUE)
    
    # If a stat is completely blank for some reason, default to 0
    if (is.na(league_avg)) league_avg <- 0 
    
    team_batting_df[[col]][is.na(team_batting_df[[col]])] <- league_avg
  }
}


# 1. Create a structured list linking each block to its specific stats
blocks_list <- list(
  Discipline = block_discipline,
  Contact   = block_contact,
  Creation = block_creation,
  Results = block_results,
  Expected   = block_expected
)


# 2. Define a master vector of stats where LOWER values are better.
# Any stat NOT on this list will automatically be treated as "Positive/Higher is better".
negative_stats <- c(
  'K%', 'Chase%', 'Whiff%', 'GB%', 'IFFB%', 'GB/FB')

# Loop through each of the 7 skill blocks
for (block_name in names(blocks_list)) {
  
  # 1. Pull the specific stats for this current block
  block_stats <- blocks_list[[block_name]]
  
  # 2. Extract and format the matrix safely
  X_raw <- team_batting_df %>% select(all_of(block_stats))
  X_mat <- data.matrix(X_raw)
  
  # Safety Check: If a column has 0 variance (all values are identical), 
  # scaling it creates NaNs. We replace those NaNs with 0 to prevent glmnet from crashing.
  X_scaled <- scale(X_mat)
  X_scaled[is.na(X_scaled)] <- 0
  
  y_raw <- team_batting_df$wOBA_fg
  y_scaled <- as.vector(scale(y_raw))
  
  # 3. Fit the Ridge Regression model for this block
  ridge_model <- cv.glmnet(X_scaled, y_scaled, alpha = 0)
  
  # 4. Extract importance weights (coefficients) and normalize them to sum to 1.00
  raw_coefficients <- as.vector(as.matrix(coef(ridge_model, s = "lambda.min")))[-1] 
  
  # Safety Check: If a coefficient is NaN due to zero variance, force it to 0
  raw_coefficients[is.na(raw_coefficients)] <- 0
  
  # Ensure we don't divide by zero if all weights happen to be zero
  sum_coefs <- sum(abs(raw_coefficients))
  if (sum_coefs == 0) sum_coefs <- 1
  
  normalized_weights <- abs(raw_coefficients) / sum_coefs
  
  # 5. Build an internal matrix to store component points for each team
  team_component_points <- matrix(0, nrow = nrow(team_batting_df), ncol = length(block_stats))
  
  # 6. Calculate the scores for each individual stat in the block
  for (i in 1:length(block_stats)) {
    stat_name <- block_stats[i]
    weight    <- normalized_weights[i]
    
    # Calculate team Z-scores for this stat
    stat_values <- team_batting_df[[stat_name]]
    
    stat_mean <- mean(stat_values, na.rm = TRUE)
    stat_sd   <- sd(stat_values, na.rm = TRUE)
    
    # Safety Check for Z-Score standard deviation dividing by zero
    if (is.na(stat_sd) || stat_sd == 0) {
      z_scores <- rep(0, length(stat_values))
    } else {
      z_scores <- (stat_values - stat_mean) / stat_sd
    }
    
    # If the stat is on our negative list (like K% or Whiff%), flip the sign 
    if (stat_name %in% negative_stats) {
      z_scores <- z_scores * -1
    }
    
    # Multiply by the ridge importance weight
    team_component_points[, i] <- z_scores * weight
  }
  
  # 7. Sum the individual components together to get the raw composite score
  raw_composite_score <- rowSums(team_component_points, na.rm = TRUE)
  
  # 8. Scale to our classic Index scale (Mean = 100, Standard Deviation = 15)
  avg_comp <- mean(raw_composite_score, na.rm = TRUE)
  sd_comp  <- sd(raw_composite_score, na.rm = TRUE)
  
  if (is.na(sd_comp) || sd_comp == 0) sd_comp <- 1
  
  index_score <- 100 + ((raw_composite_score - avg_comp) / sd_comp) * 15
  
  # 9. Apply your custom logic: Divide by 10 to turn the 100-baseline index into a clean points scale!
  final_points_score <- round(index_score / 10, 2)
  
  # 10. Append this new score column to your final scores dataframe
  column_output_name <- paste0("score_", tolower(block_name))
  final_team_scores_df[[column_output_name]] <- final_points_score
}


final_team_scores_df <- final_team_scores_df %>%
  mutate(
    update_date = Sys.time())




