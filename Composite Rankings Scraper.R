library(rvest)
library(lubridate)
library(tidyverse)

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

team_rankings <- data.frame(team = teams22,
                            rating = as.double(ratings22),
                            time = time)

team_rankings <- team_rankings %>% group_by(time) %>% mutate(rank = round(51-rank(rating), digits = 0))

running <- read.csv("~/desktop/Composite Scrapes/RunningCompositeRankings2022.csv")
running$time <- ymd_hms(running$time, tz = "America/Chicago")

running <- bind_rows(running,team_rankings)

write.csv(running, "~/desktop/Composite Scrapes/RunningCompositeRankings2022.csv",
          row.names = F)

print(paste0("running total = ",nrow(running22)))
