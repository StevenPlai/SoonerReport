library(rtweet)
library(rvest)
library(tidyverse)
library(glue)
source("~/desktop/Projects/Sooner Report/Repo/SoonerReport/Functions.R")

min <- as.POSIXct(Sys.time(),tz="America/Chicago") %>% substr(16,16)

if(min==0){
  token <- read.csv("~/desktop/Projects/Sooner Report/Repo/SoonerReport/Token.csv") %>% convert_token()
  
  offerlist <- data.frame()
  
  for(i in 2022:2025){
    offers <- read_html(glue("https://www.on3.com/college/oklahoma-sooners/football/{i}/offers/")) 
    
    df <- data.frame(name = offers %>% html_elements(".PlayerCommit_nameContainer__23j6_") %>% 
                       html_element("a") %>% html_text() %>% trimws(),
                     link =  offers %>% html_elements(".PlayerCommit_nameContainer__23j6_") %>% 
                       html_element("a") %>% html_attr("href"),
                     year = i)
    
    offerlist <- bind_rows(offerlist,df)
  }
  
  predlist <- data.frame()
  
  for(i in 1:nrow(offerlist)){
    link <- offerlist[i,2]
    name <- offerlist[i,1]
    page <- read_html(glue("https://on3.com/{link}")) %>% html_elements(".PlayerRpmModule_footer__a1Yzw")
    if(length(page)>0){
      nlink <- page %>% html_elements("a") %>% html_attr("href")
      npred <- read_html(glue("https://on3.com/{nlink}")) %>% html_elements("ul")
      preds <- data.frame(time = npred %>% html_elements("p") %>% html_text(),
                          predictor = npred %>% html_elements(".InsiderPredictions_expertContainer__CZEfD") %>% 
                            html_elements("img") %>% html_attr("alt") %>% trimws())
      teams <- data.frame(team = npred %>% html_elements(".InsiderPredictions_logosContainer__S275_") %>% 
                            html_elements(".InsiderPredictions_teamLogo__P9jlm") %>% html_attr("alt") %>% trimws(),
                          class = npred %>% html_elements(".InsiderPredictions_logosContainer__S275_") %>% 
                            html_elements(".InsiderPredictions_teamLogo__P9jlm") %>% html_attr("class")) %>% 
        filter(class=="InsiderPredictions_teamLogo__P9jlm") %>% select(team)
      if(nrow(preds)>0){
        n <- nrow(preds)
        confidence <- npred %>% html_elements(".InsiderPredictions_confidenceContainer__wUPrD") %>% 
          html_elements("span") %>% html_text()
        confidence <- confidence[seq(from=2,to=n*4,by=4)]
        preds <- preds %>% mutate(confidence = confidence,
                                  team = teams$team, 
                                  team = substr(team,1,nchar(team)-5),
                                  predictor = substr(predictor,1,nchar(predictor)-7),
                                  prospect = name)
        predlist <- bind_rows(predlist,preds)
      }
    }
  }
  
  predlist <- predlist %>% mutate(confidence = as.numeric(confidence))
  
  running_list <- read.csv("~/Desktop/On3Scraper/RunningO3List.csv")
  
  full_list <- read.csv("~/Desktop/On3Scraper/FullO3List.csv")
  
  new_preds <- anti_join(predlist, running_list)
  
  new_ou <- new_preds %>% filter(team=="Oklahoma")
  
  write.csv(predlist, "~/Desktop/On3Scraper/RunningO3List.csv", row.names = F)
  
  if(nrow(new_ou)>0){
    full_list <- bind_rows(full_list,new_ou)  
    write.csv(full_list, "~/Desktop/On3Scraper/FullO3List.csv", row.names = F)
    name <- new_ou$prospect
    post_message(glue("Found new On3 Prediction for {name}"), "StevenPlai", token=token)
    post_message(glue("Found new On3 Prediction for {name}"), "KeganReneau", token=token)
  } 
}





