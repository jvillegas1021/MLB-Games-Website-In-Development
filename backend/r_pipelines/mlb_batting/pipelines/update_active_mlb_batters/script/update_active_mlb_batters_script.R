library(baseballr)
library(tidyverse)
library(httr)
library(jsonlite)
library(DBI)
library(RPostgres)


source("backend/r_pipelines/mlb_batting/pipelines/update_active_mlb_batters/extract_functions/extract_mlb_player_info.R")
source("backend/r_pipelines/mlb_batting/pipelines/update_active_mlb_batters/load_functions/load_data_to_database.r")
source("backend/r_pipelines/mlb_batting/pipelines/update_active_mlb_batters/pipeline/update_active_mlb_batters_pipeline.r")

update_active_mlb_batters()
print("Updated Active MLB batters.")