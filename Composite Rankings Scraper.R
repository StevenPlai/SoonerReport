library(rvest)
library(lubridate)
library(tidyverse)

compteamrank22 <- read_html("https://247sports.com/Season/2022-Football/CompositeTeamRankings/")
compteamrank23 <- read_html("https://247sports.com/Season/2023-Football/CompositeTeamRankings/")

ratings22 <- compteamrank22 %>% 
  html_elements(".number") %>% 
  html_text2()

ratings23 <- compteamrank23 %>% 
  html_elements(".number") %>% 
  html_text2()

teams22 <- compteamrank22 %>% 
  html_elements(".rankings-page__name-link") %>% 
  html_text2()

teams23 <- compteamrank23 %>% 
  html_elements(".rankings-page__name-link") %>% 
  html_text2()

time <- Sys.time()

time <- gsub(" |:","",time)
time <- gsub("-","",time)

team_rankings22 <- data.frame(team = teams22,
                              rating = as.double(ratings22),
                              time = as.double(time))

team_rankings23 <- data.frame(team = teams23,
                              rating = as.double(ratings23),
                              time = as.double(time))

running22 <- read.csv("/Users/andersoninman/desktop/Composite Scrapes/RunningCompositeRankings2022.csv")

running22 <- bind_rows(running22,team_rankings22)

write.csv(running22, "/Users/andersoninman/desktop/Composite Scrapes/RunningCompositeRankings2022.csv",
          row.names = F)

print(paste0("running total = ",nrow(running22)))
