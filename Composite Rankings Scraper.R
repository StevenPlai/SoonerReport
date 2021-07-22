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

date <- Sys.Date()

team_rankings <- data.frame(team = teams22,
                            rating = as.double(ratings22),
                            date = date)

running <- read.csv("~/desktop/Composite Scrapes/RunningCompositeRankings2022.csv")

running <- bind_rows(running,team_rankings)

write.csv(running, "~/desktop/Composite Scrapes/RunningCompositeRankings2022.csv",
          row.names = F)

print(paste0("running total = ",nrow(running22)))
