library(rvest)
library(RSelenium)
library(tidyverse)
library(wdman)
library(glue)
library(httr)
library(stringr)

#Configure Selenium Server
#httr::with_config(config(ssl_verifypeer=0L),wdman::selenium(retcommand=TRUE))
  
posList247 <- c("QB","RB","WR","TE","OT","IOL","EDGE","DL","LB","CB","S","ATH","K","P","LS")

list247 <- data.frame()

for(p in posList247){
  page <- read_html(glue("https://247sports.com/Season/2022-Football/CompositeRecruitRankings/?InstitutionGroup=HighSchool&Position={p}")) 
  df <- data.frame(name = page %>% 
                     html_elements(".rankings-page__list-item") %>% 
                     html_elements(".rankings-page__name-link") %>% html_text(),
                   id = page %>% 
                     html_elements(".rankings-page__list-item") %>% 
                     html_elements(".rankings-page__name-link") %>% html_attr("href") %>% 
                     str_extract_all("([0-9]+).*$") %>% as.character())
  if(nrow(df)==50){
    page2 <- read_html(glue("https://247sports.com/Season/2022-Football/CompositeRecruitRankings/?ViewPath=~%2FViews%2FSkyNet%2FPlayerSportRanking%2F_SimpleSetForSeason.ascx&Position={p}&InstitutionGroup=HighSchool&Page=2")) 
    df2 <- data.frame(name = page2 %>% 
                        html_elements(".rankings-page__list-item") %>% 
                        html_elements(".rankings-page__name-link") %>% html_text(),
                      id = page2 %>% 
                        html_elements(".rankings-page__list-item") %>% 
                        html_elements(".rankings-page__name-link") %>% html_attr("href") %>% 
                        str_extract_all("([0-9]+).*$") %>% as.character())
    page3 <- read_html(glue("https://247sports.com/Season/2022-Football/CompositeRecruitRankings/?ViewPath=~%2FViews%2FSkyNet%2FPlayerSportRanking%2F_SimpleSetForSeason.ascx&Position={p}&InstitutionGroup=HighSchool&Page=3")) 
    df3 <- data.frame(name = page3 %>% 
                        html_elements(".rankings-page__list-item") %>% 
                        html_elements(".rankings-page__name-link") %>% html_text(),
                      id = page3 %>% 
                        html_elements(".rankings-page__list-item") %>% 
                        html_elements(".rankings-page__name-link") %>% html_attr("href") %>% 
                        str_extract_all("([0-9]+).*$") %>% as.character())
    df <- bind_rows(df,df2,df3)
    }
  list247 <- bind_rows(list247,df)
}

posListRivals <- c("APB","ATH","C","CB","DT","DUAL","ILB","K","OG","OLB","OT","PRO","RB","S","SDE","TE","WDE","WR")

listRivals <- data.frame()

server <- phantomjs(port=4444L)

browser <- remoteDriver(browserName = "phantomjs", port=4444)

browser$open()

for(p in posListRivals){
  
  browser$navigate(glue("https://n.rivals.com/position_rankings/Football/2022/{p}"))
  
  df <- data.frame(fname = read_html(browser$getPageSource()[[1]]) %>% html_elements(".name-star") %>% 
                     html_elements("a") %>% html_elements(".first-name") %>% html_text() %>% 
                     trimws(),
                   lname = read_html(browser$getPageSource()[[1]]) %>% html_elements(".name-star") %>% 
                     html_elements("a") %>% html_elements(".last-name") %>% html_text() %>% 
                     trimws(),
                   id = read_html(browser$getPageSource()[[1]]) %>% html_elements(".name-star") %>% 
                     html_elements("a") %>% html_attr("href") %>%
                     str_extract_all("([0-9]+).*$") %>% str_sub(-6,-1) %>% as.character()) %>%
    mutate(name = glue("{fname} {lname}")) %>% select(name,id)
    
    listRivals <- bind_rows(listRivals,df)
}

browser$close()

join <- full_join(list247,listRivals,by="name",suffix=c("247","Rivals"))
