library(rvest)
library(lubridate)

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
                            rating = ratings22,
                            time = time)

team_rankings23 <- data.frame(team = teams23,
                            rating = ratings23,
                            time = time)


write.csv(team_rankings22, paste0("/Users/stevenplaisance/desktop/Projects/SoonerBot/CompositeScrapes/",time,".csv"))



