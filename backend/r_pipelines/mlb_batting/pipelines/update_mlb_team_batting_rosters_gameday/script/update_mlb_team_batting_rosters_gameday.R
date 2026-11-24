library(baseballr)
library(tidyverse)
library(httr)
library(jsonlite)
library(DBI)
library(RPostgres)

source("backend/r_pipelines/mlb_batting/pipelines/update_mlb_team_batting_rosters_gameday/transform_functions/mlb_team_batting_rosters_gameday_functions.R")
source("backend/r_pipelines/mlb_batting/pipelines/update_mlb_team_batting_rosters_gameday/pipeline/mlb_team_batting_rosters_gameday_pipeline.R")
source("backend/r_pipelines/mlb_batting/pipelines/update_mlb_team_batting_rosters_gameday/load_functions/load_data_to_database.r")

update_mlb_batting_rosters_gameday()


