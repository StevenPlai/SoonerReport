library(rvest, warn.conflicts = F)
library(lubridate, warn.conflicts = F)
library(tidyverse, warn.conflicts = F)
library(logging, warn.conflicts = F)
library(glue, warn.conflicts = F)

compteamrank <- read_html("https://247sports.com/Season/2022-Football/CompositeTeamRankings/")

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

running <- read.csv("~/desktop/Composite Scrapes/RunningCompositeRankings2022.csv")
running$time <- ymd_hms(running$time, tz = "America/Chicago")

running <- bind_rows(running,team_rankings)

write.csv(running, "~/desktop/Composite Scrapes/RunningCompositeRankings2022.csv",
          row.names = F)

commits <- read_html("https://247sports.com/college/oklahoma/Season/2022-Football/Commits/")
commits <- data.frame(name = trimws(commits %>% html_nodes(".ri-page__name-link") %>% html_text()),
                      pos = trimws(commits %>% html_nodes(".position") %>% html_text()),
                      rating = trimws(commits %>% html_nodes(".score") %>% html_text())[-1],
                      date = commits %>% html_nodes(".commit-date.withDate") %>% html_text(),
                      time = NA) %>%
  mutate(date = trimws(gsub("Commit\n", "", date)))
commits$date <- mdy(commits$date)

runningcommits <- read.csv("~/desktop/Composite Scrapes/RunningCommits2022.csv")
runningcommits$rating <- as.factor(runningcommits$rating)
runningcommits$date <- ymd(runningcommits$date)
new <- anti_join(commits, runningcommits, by=c("name", "date"))

if(nrow(new)>0){
 new$time <- time
}

commits <- bind_rows(new, runningcommits)

write.csv(commits, "~/desktop/Composite Scrapes/RunningCommits2022.csv",
          row.names = F)

loginfo(glue("Found {nrow(new)} new commits on 247 page"))

loginfo(glue("Added {nrow(team_rankings)} obs to running list"))
