library(gt)
library(gtExtras)
library(stringi)
library(tidyverse)
library(glue)
library(fontawesome)
library(cfbfastR)
source("Functions.R")

cweek <- CFBWeek()

info <- cfbd_game_info(
  2021,
  week = week-1,
  season_type = "regular",
  team = "Oklahoma") 

pbp <- data.frame()
for(i in 1:cweek-1){
df <- cfbd_pbp_data(
  2021,
  week=i,
  season_type = "regular",
  team = "Oklahoma",
  epa_wpa = T,
) %>% mutate(week=i)
pbp <- bind_rows(pbp,df)
}

fg <- pbp %>% 
  filter(play_type=="Field Goal Missed"|play_type=="Field Goal Good"|
         play_type=="Blocked Field Goal"|play_type=="Missed Field Goal Return",
         pos_team=="Oklahoma") %>% select(week,yds_fg,fg_make_prob,play_type) %>% 
  mutate(xPts = 3*fg_make_prob,
         Pts = if_else(play_type=="Field Goal Good",3,0),
         PtsAdded = Pts-xPts)

table <- fg %>% filter(week==cweek-1) %>% select(-week) %>% gt() %>% 
  tab_header(title="Field Goal Attempts") %>% 
  text_transform(
    locations = cells_body(columns=play_type),
    fn = function(x){
        ifelse(x == "Field Goal Good",
               gt::html(fontawesome::fa("check", fill = "#35b03b", height = "1em")),
               gt::html(fontawesome::fa("times", fill = "#DA2A2A", height = "1em")))}
    ) %>% 
  fmt_percent(fg_make_prob, decimals=1) %>% 
  fmt_number(xPts, decimals=1) %>% 
  fmt_number(PtsAdded, decimals=1) %>% 
  cols_label(yds_fg="Distance",fg_make_prob="")

id <- info %>% select(game_id) %>% as.integer()

adv <- cfbd_game_box_advanced(id, long = FALSE, verbose = FALSE)

