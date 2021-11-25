library(tidyverse)
library(cfbfastR)
source("Functions.R")

lweek <- CFBWeek()-1

info <- cfbd_team_info(year=2021,only_fbs = F) %>% select(school,conference)

pbp <- data.frame()
for(x in 2015:2021){
  for(i in 1:16){
    df <- cfbd_pbp_data(
      x,
      week=i,
      season_type = "regular",
      team = "Oklahoma",
      epa_wpa = T)
    pbp <- bind_rows(pbp,df)
  }
}

pbp21 <- data.frame()
for(i in 1:16){
  df <- cfbd_pbp_data(
    2021,
    week=i,
    season_type = "regular",
    epa_wpa = T)
  pbp21 <- bind_rows(pbp21,df)}

offense <- pbp %>% filter(pos_team=="Oklahoma",!is.na(EPA),play_type %notin% c("Kickoff", "Uncategorized", "NA"))

offGameRanks <- offense %>% group_by(drive_id,game_id) %>% summarise(opp = first(def_pos_team),
                                                                     EPA = mean(EPA)) %>% 
  ungroup() %>% group_by(game_id) %>% summarise(opp = first(opp),
                                                EPA = mean(EPA)) %>% ungroup()

write.csv(offGameRanks, "Data/OffGameRanks.csv", row.names = F)

offense21 <- pbp21 %>% filter(!is.na(EPA),play_type %notin% c("Kickoff", "Uncategorized","NA"))

weekOff <- offense21 %>% filter(pos_team=="Oklahoma",wk==lweek)

write.csv(weekOff, "Data/WeekOffense.csv", row.names = F)

offenseRanks <- offense21 %>% group_by(drive_id,game_id) %>% summarise(EPA = mean(EPA),
                                                                school = first(pos_team)) %>% 
  ungroup() %>% group_by(school) %>% summarise(EPA = mean(EPA)) %>% left_join(info, by="school") %>% 
  ungroup() %>% filter(!is.na(conference)) %>% select(-conference)

write.csv(offenseRanks, "Data/OffenseRanks.csv", row.names = F)

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
