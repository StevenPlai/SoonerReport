library(rvest, warn.conflicts = F)
library(lubridate, warn.conflicts = F)
library(tidyverse, warn.conflicts = F)
library(logging, warn.conflicts = F)
library(glue, warn.conflicts = F)

rcycle <- function() {
  date <- Sys.Date()
  year <- as.numeric(date %>% stri_sub(1,4))
  month <- as.numeric(date %>% stri_sub(6,7))
  mday <- as.numeric(date %>% stri_sub(9,10))
  wday <- wday(date)
  if (month>2) {as.character(year+1)
  } else {
    if (month==1) {as.character(year)} else {
      if(mday>7) {as.character(year+1)} else{
        if(mday==1) {as.character(year)} else {
          if(mday==2) {
            if(wday==5){as.character(year+1)
            } else{as.character(year)}} else {
              if(mday==3) {
                if(wday==5|wday==6){as.character(year+1)
                } else{as.character(year)}} else{
                  if(mday==4) {
                    if(wday>4){as.character(year+1)
                    } else{as.character(year)}} else{
                      if(mday==5) {
                        if(wday>4|wday==1){as.character(year+1)
                        } else{as.character(year)}} else{
                          if(mday==6) {
                            if(wday==3|wday==4){as.character(year)
                            } else{as.character(year+1)}} else{
                              if(mday==7) {
                                if(wday==4){as.character(year)
                                } else{as.character(year+1)}}}}}}}}}}}}

cycle <- rcycle()

for(i in 0:1)
{
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
  
  team_rankings <- team_rankings %>% group_by(time) %>% mutate(rank = round(51-rank(rating), digits = 0))
  
  running <- read.csv(glue("~/desktop/Composite Scrapes/RunningCompositeRankings{year}.csv"))
  running$time <- ymd_hms(running$time, tz = "America/Chicago")
  
  running <- bind_rows(running,team_rankings)
  
  write.csv(running, glue("~/desktop/Composite Scrapes/RunningCompositeRankings{year}.csv"),
            row.names = F)
  
  commits <- read_html(glue("https://247sports.com/college/oklahoma/Season/{year}-Football/Commits/"))
  commits <- data.frame(name = trimws(commits %>% html_nodes(".ri-page__name-link") %>% html_text()),
                        pos = trimws(commits %>% html_nodes(".position") %>% html_text()),
                        rating = trimws(commits %>% html_nodes(".score") %>% html_text())[-1],
                        date = commits %>% html_nodes(".commit-date.withDate") %>% html_text(),
                        time = NA) %>%
    mutate(date = trimws(gsub("Commit\n", "", date)))
  commits$date <- mdy(commits$date)
  
  runningcommits <- read.csv(glue("~/desktop/Composite Scrapes/RunningCommits{year}.csv"))
  runningcommits$rating <- as.factor(runningcommits$rating)
  runningcommits$date <- ymd(runningcommits$date)
  new <- anti_join(commits, runningcommits, by=c("name", "date"))
  
  if(nrow(new)>0){
    new$time <- time
  }
  
  commits <- bind_rows(new, runningcommits)
  
  write.csv(commits, glue("~/desktop/Composite Scrapes/RunningCommits{year}.csv"),
            row.names = F)
  
  loginfo(glue("Found {nrow(new)} new commits on 247 page for c/o {year}"))
  
  loginfo(glue("Added {nrow(team_rankings)} obs to running list for c/o{year}"))
}

