library(tidyverse)
library(DBI)
library(RPostgres)
library(glmnet)
library(caret)
library(baseballr)

################ CALCULATE BATTING SCORE WEIGHTS FOR STATS ##################
fg_team_batting_df <- fg_team_batter(startseason = 2026, endseason = 2026)

cleaned_fg_team_batting_df <- fg_team_batting_df %>%
  mutate(
    TB   = `1B` + (2 * `2B`) + (3 * `3B`) + (4 * HR),
    RC_A = H + BB - CS + HBP - GDP,
    RC_B = TB + (0.26 * (BB - IBB + HBP)) + (0.52 * (SH + SF + SB)),
    RC_C = AB + BB + HBP + SH + SF,
    RC   = if_else(RC_C == 0, 0, (RC_A * RC_B) / RC_C),
    `wRC/PA` = if_else(PA == 0, 0, wRC / PA),
    lg_wSB = (sum(SB, na.rm = TRUE) * 0.200 + sum(CS, na.rm = TRUE) * (-0.410)) /
      sum(`1B` + BB + HBP - IBB, na.rm = TRUE),
    wSB    = (SB * 0.200) + (CS * (-0.410)) - (lg_wSB * (`1B` + BB + HBP - IBB)),
    xISO = xSLG - xAVG,
    xBABIP = ((xAVG * AB) - HR) / (AB - HR - SO + SF),
  ) %>%
  select(
    `K%` = K_pct,
    `BB%` = BB_pct,
    `Chase%` = `O-Swing_pct`,
    `Whiff%` = SwStr_pct,
    `Z-Swing%` = `Z-Swing_pct`,
    `F-Strike%` = `F-Strike_pct`,
    `SwStr%` = SwStr_pct,
    `Zone%` = Zone_pct,
    `CStr%` = CStr_pct,
    `C+SwStr%` = `C+SwStr_pct`,
    `GB%` = GB_pct,
    `FB%` = FB_pct,
    `LD%` = LD_pct,
    `GB/FB` = GB_FB,
    `HR/FB` = HR_FB,
    `IFFB%` = IFFB_pct,
    `Barrel%` = Barrel_pct,
    `HardHit%` = HardHit_pct,
    `Z-Contact%` = `Z-Contact_pct`,
    `O-Contact%` = `O-Contact_pct`,
    `Contact%` = Contact_pct,
    EV = EV,
    LA = LA,
    wOBA = wOBA,
    wRAA = wRAA,
    wRC = wRC,
    `wRC/PA`,
    RC,
    wSB,
    AVG = AVG,
    OBP = OBP,
    SLG = SLG,
    OPS = OPS,
    BABIP = BABIP,
    ISO = ISO,
    xBA = xAVG,
    xwOBA = xwOBA,
    xSLG = xSLG,
    xISO = xISO,
    xBABIP = xBABIP,
    wRC_plus
  )

# 1. CORE DISCIPLINE 
block_discipline <- c('K%', 'BB%', 'Z-Swing%', 'Chase%', 'Whiff%', 'Zone%', 'SwStr%', 'F-Strike%', 'CStr%', 'C+SwStr%')
block_contact     <- c('GB%', 'FB%', 'LD%', 'GB/FB', 'HR/FB', 'IFFB%', 'Barrel%', 'HardHit%',
                       'Z-Contact%', 'O-Contact%', 'Contact%', 'EV', 'LA')
block_creation  <- c("wOBA", "wRAA", "wRC", "wRC/PA", "RC", "wSB")
block_results <- c('AVG', 'OBP', 'SLG', 'OPS', 'ISO', 'BABIP')
block_expected   <- c('xwOBA', 'xBA', 'xSLG', 'xISO', 'xBABIP')

target_metric <- 'wRC_plus'

# Create a master list of all tracking stats to clean in the next step
all_stats_to_clean <- c(block_discipline, block_contact, block_creation, block_results, block_expected)

# 1. Combine all your block stats plus runs_scored into one master list
all_numeric_cols <- c(all_stats_to_clean, target_metric)

# 2. Force every stat column to be numeric (fixes character/factor text issues)
cleaned_fg_team_batting_df[all_numeric_cols] <- lapply(
  cleaned_fg_team_batting_df[all_numeric_cols], 
  function(x) as.numeric(as.character(x))
)

# 3. Clean up NA values by replacing them with the league average for that stat
for (col in all_numeric_cols) {
  if (any(is.na(cleaned_fg_team_batting_df[[col]]))) {
    league_avg <- mean(cleaned_fg_team_batting_df[[col]], na.rm = TRUE)
    
    # If a stat is completely blank for some reason, default to 0
    if (is.na(league_avg)) league_avg <- 0 
    
    cleaned_fg_team_batting_df[[col]][is.na(cleaned_fg_team_batting_df[[col]])] <- league_avg
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
# Define negative stats matching your exact 40 stat names
negative_stats <- c(
  "K%", "Chase%", "Whiff%", "SwStr%", "F-Strike%", 
  "CStr%", "C+SwStr%", "GB%", "IFFB%", "GB/FB"
)

weights_list <- list()

for (block_name in names(blocks_list)) {
  
  # 1. Pull the specific stats for this current block
  block_stats <- blocks_list[[block_name]]
  
  # 2. Extract and scale X and Y
  X_mat <- data.matrix(cleaned_fg_team_batting_df %>% select(all_of(block_stats)))
  X_scaled <- scale(X_mat)
  X_scaled[is.na(X_scaled)] <- 0
  
  y_scaled <- as.vector(scale(cleaned_fg_team_batting_df$wRC_plus))
  
  # 3. Fit Ridge Regression
  ridge_model <- cv.glmnet(X_scaled, y_scaled, alpha = 0)
  
  # 4. Extract importance weights and normalize to sum to 1.00
  raw_coefficients <- as.vector(as.matrix(coef(ridge_model, s = "lambda.min")))[-1]
  raw_coefficients[is.na(raw_coefficients)] <- 0
  
  sum_coefs <- sum(abs(raw_coefficients))
  if (sum_coefs == 0) sum_coefs <- 1
  
  normalized_weights <- abs(raw_coefficients) / sum_coefs
  
  # 5. Store clean stat + weight table for this block
  weights_list[[block_name]] <- tibble(
    block  = block_name,
    stat   = block_stats,
    weight = normalized_weights
  )
}

# Combine all blocks into one master weights table
batting_weights_df <- bind_rows(weights_list)

batting_weights_df <- batting_weights_df %>%
  mutate(
    direction     = if_else(stat %in% negative_stats, -1, 1),
    signed_weight = weight * direction
  )

saveRDS(batting_weights_df, "backend/r_pipelines/data_files/mlb_team_batting_weights.rds")
