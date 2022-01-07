library(rvest, warn.conflicts = F)
library(lubridate, warn.conflicts = F)
library(tidyverse, warn.conflicts = F)
library(logging, warn.conflicts = F)
library(glue, warn.conflicts = F)
library(stringi, warn.conflicts = F)
source("~/desktop/Projects/Sooner Report/Repo/SoonerReport/Functions.R")

cycle <- rcycle()

for(i in 0:2){
  year <- as.character(as.numeric(cycle)+i)
  compteamrank <- read_html(glue("https://247sports.com/Season/{year}-Football/CompositeTeamRankings/"))
  
  ratings <- compteamrank %>% 
    html_elements(".number") %>% 
    html_text2()
  
  ratings <- compteamrank %>% 
    html_elements(".number") %>% 
    html_text2()
  
  teams <- compteamrank %>% 
    html_elements(".rankings-page__name-link") %>% 
    html_text2()
  
  teams <- compteamrank %>% 
    html_elements(".rankings-page__name-link") %>% 
    html_text2()
  
  time <- Sys.time()
  
  team_rankings <- data.frame(team = teams,
                              rating = as.double(ratings),
                              time = time)
  
  team_rankings <- team_rankings %>% group_by(time) %>% mutate(rank = n()+1-rank(rating, ties.method="max"))
  
  running <- read.csv(glue("~/desktop/CompScraper/RunningCompositeRankings{year}.csv"))
  running$time <- ymd_hms(running$time, tz = "America/Chicago")
  
  running <- bind_rows(running,team_rankings)
  
  write.csv(running, glue("~/desktop/CompScraper/RunningCompositeRankings{year}.csv"),
            row.names = F)
  
  obj <- read_html(glue("https://247sports.com/college/oklahoma/Season/{year}-Football/Commits/"))
  commits <- data.frame(name = trimws(obj %>% html_nodes(".ri-page__name-link") %>% html_text()),
                        pos = trimws(obj %>% html_nodes(".position") %>% html_text()),
                        rating = trimws(obj %>% html_nodes(".score") %>% html_text())[-1],
                        date = obj %>% html_nodes(".commit-date.withDate") %>% html_text(),
                        time = NA) %>%
    mutate(date = trimws(gsub("Commit\n", "", date)))
  commits$date <- mdy(commits$date)
  
  runningcommits <- read.csv(glue("~/desktop/CompScraper/RunningCommits{year}.csv"))
  runningcommits$rating <- as.factor(runningcommits$rating)
  runningcommits$date <- ymd(runningcommits$date)
  runningcommits$time <- ymd_hms(runningcommits$time)
  new <- anti_join(commits, runningcommits, by=c("name", "date"))
  
  if(nrow(new)>0){
    commits <- bind_rows(new, runningcommits)
  } 
  
  
  write.csv(commits, glue("~/desktop/CompScraper/RunningCommits{year}.csv"),
            row.names = F)
  
  loginfo(glue("Found {nrow(new)} new commits on 247 page for c/o {year}"))
  
  loginfo(glue("Added {nrow(team_rankings)} obs to running list for c/o{year}"))
}
