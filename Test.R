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

server <- phantomjs(port=5555L)

browser <- remoteDriver(browserName = "phantomjs", port=5555)

browser$open()

for(p in posListRivals){
  
  browser$navigate(glue("https://n.rivals.com/search#?formValues=%7B%22sport%22:%22Football%22,%22prospect_profiles.position_group_abbreviation%22:%5B%22{p}%22%5D,%22page_number%22:1,%22page_size%22:50,%22recruit_year%22:2022,%22sort%22:%7B%22prospect_profiles.stars%22:%7B%22order%22:%22desc%22%7D%7D%7D"))
  
  button <- browser$findElements(using="class",value="btn-primary")
  
  button <- button[[1]]
  
  button$clickElement()
  
  button <- browser$findElements(using="xpath",value='/html/body/main/div[2]/div[1]/div/div[5]/ul/li[9]/a')
  button <- button[[1]]
  
  button$clickElement()
  
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

posListon3 <- c("ot","s","qb","rb","wr","te","iol","edge","dl","lb","cb","ath","k","p","ls")

listOn3 <- data.frame()

for(p in posListon3){
  
  if(p=="ot"|p=="s"){
    browser$navigate(glue("https://www.on3.com/db/rankings/consensus-player/football/2022/?position={p}"))
    buttons <- browser$findElements(using="tag name",value="button")
    buttons <- buttons[[6]]
    buttons$clickElement()
    buttons$clickElement()
    buttons$clickElement()
    Sys.sleep(25)
    
    df <- data.frame(name = read_html(browser$getPageSource()[[1]]) %>% html_elements(".PlayerList_name__2YV_C") %>% 
                       html_elements("a") %>% html_text() %>% trimws(),
                     id = read_html(browser$getPageSource()[[1]]) %>% html_elements(".PlayerList_name__2YV_C") %>% 
                       html_elements("a") %>% html_attr("href") %>%
                       str_extract_all("\\d+") %>% as.character())
    print(c(p,nrow(df)))
    listOn3 <- bind_rows(listOn3,df)
  } else{
    browser$navigate(glue("https://www.on3.com/db/rankings/consensus-player/football/2022/?position={p}"))
    
    l <- 6
    while(l==6){
      tryCatch(expr = {
        buttons <- browser$findElements(using="tag name",value="button")
        l <- length(buttons)
        if(l==6){
          buttons <- buttons[[6]]
          buttons$clickElement()
        }},
        error = function(e){print("error")})
    }
    
    df <- data.frame(name = read_html(browser$getPageSource()[[1]]) %>% html_elements(".PlayerList_name__2YV_C") %>% 
                       html_elements("a") %>% html_text() %>% trimws(),
                     id = read_html(browser$getPageSource()[[1]]) %>% html_elements(".PlayerList_name__2YV_C") %>% 
                       html_elements("a") %>% html_attr("href") %>%
                       str_extract_all("\\d+") %>% as.character(),
                     p = p)
    print(c(p,nrow(df)))
    listOn3 <- bind_rows(listOn3,df)
  }
}

browser$close()
server$stop()

join <- full_join(list247,listRivals,by="name",suffix=c("247","Rivals")) %>% 
  full_join(listOn3,by="name") %>% rename("idOn3"=id)


