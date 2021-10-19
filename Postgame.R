library(gt)
library(gtExtras)
library(stringi)
library(tidyverse)
library(glue)
library(cfbfastR)
source("~/Desktop/Projects/CFB Composite/Repo/CFB-Composite/Functions.R")

week <- CFBWeek()

info <- cfbd_game_info(
  2021,
  week = week-1,
  season_type = "regular",
  team = "Oklahoma") 

pbp <- cfbd_pbp_data(
  2021,
  season_type = "regular",
  week = week-1,
  team = "Oklahoma",
  epa_wpa = T,
)

fg <- pbp %>% filter(play_type=="Field Goal Missed"|play_type=="Field Goal Good",pos_team=="Oklahoma")

id <- info %>% select(game_id) %>% as.integer()

adv <- cfbd_game_box_advanced(id, long = FALSE, verbose = FALSE)

