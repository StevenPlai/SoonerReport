library(tidyverse)
library(cfbfastR)
source("Functions.R")

cweek <- CFBWeek()

pbp <- data.frame()
for(x in 2015:2021){
  for(i in 1:16){
    df <- cfbd_pbp_data(
      x,
      week=i,
      season_type = "regular",
      team = "Oklahoma",
      epa_wpa = T)
    if(is.data.frame(df)){ df <- df %>% mutate(week=i)}
    pbp <- bind_rows(pbp,df)
  }
}

write.csv(pbp, "Data/AllPBP.csv", row.names = F)

pbp21 <- data.frame()
for(i in 1:16){
  df <- cfbd_pbp_data(
    2021,
    week=i,
    season_type = "regular",
    epa_wpa = T)
  if(is.data.frame(df)){ df <- df %>% mutate(week=i)}
  pbp21 <- bind_rows(pbp21,df)}
  
write.csv(pbp21, "Data/2021PBP.csv")

fg <- pbp %>% 
  filter(play_type=="Field Goal Missed"|play_type=="Field Goal Good"|
           play_type=="Blocked Field Goal"|play_type=="Missed Field Goal Return",
         pos_team=="Oklahoma") %>% select(fg_kicker_player_name,season,wk,yds_fg,fg_make_prob,play_type,def_pos_team) %>% 
  mutate(xPts = 3*fg_make_prob,
         Pts = if_else(play_type=="Field Goal Good",3,0),
         PtsAdded = Pts-xPts) %>% filter(fg_kicker_player_name=="Brkic"|fg_kicker_player_name=="Gabe Brkic",
                                              !is.na(fg_make_prob))

write.csv(fg,"Data/BrkicFG.csv", row.names = F)

fg_games <- fg %>% group_by(season,wk) %>% summarise(attempts = n(),
                                                    totalYards = sum(yds_fg),
                                                    xPts = sum(xPts),
                                                    Pts = sum(Pts),
                                                    PtsAdded = sum(PtsAdded),
                                                    opponent = first(def_pos_team))

write.csv(fg_games,"Data/BrkicGames.csv", row.names = F) 

conf <- cfbd_team_info(only_fbs=T) %>% select(school, conference)

fg_fbs <- pbp21 %>% left_join(conf,by=c("pos_team"="school")) %>% 
  filter(play_type=="Field Goal Missed"|play_type=="Field Goal Good"|
           play_type=="Blocked Field Goal"|play_type=="Missed Field Goal Return"|
           play_type=="Blocked Field Goal Touchdown",!is.na(conference)) %>% 
  select(yds_fg,fg_make_prob,play_type,pos_team,fg_kicker_player_name) %>% 
  separate(fg_kicker_player_name, into=c("first","last"),fill="left",sep=" ",remove=T) %>% 
  select(-first) %>% filter(!is.na("last")) %>% 
  mutate(xPts = 3*fg_make_prob,
         Pts = if_else(play_type=="Field Goal Good",3,0),
         PtsAdded = Pts-xPts) %>% filter(!is.na(fg_make_prob)) %>% 
  group_by(pos_team,last) %>% summarise(attempts = n(),
                                   totalYards = sum(yds_fg),
                                   xPts = sum(xPts),
                                   Pts = sum(Pts),
                                   PtsAdded = sum(PtsAdded),
                                   per = PtsAdded/n(),
                                   team = first(pos_team)) %>% ungroup() %>% 
  mutate(rk=n()-rank(PtsAdded,ties.method = "last")+1) 

write.csv(fg_fbs,"Data/FG2021.csv", row.names = F) 
