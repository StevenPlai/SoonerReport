library(rtweet)
library(rvest)
library(dplyr,warn.conflicts = F)
library(glue, warn.conflicts = F)
library(logging)
library(lubridate, warn.conflicts = F)
source("~/Projects/SoonerReport/Functions.R")

min <- as.POSIXct(Sys.time(),tz="America/Chicago") %>% substr(16,16)

if(min==0){
  now <- Sys.time() %>% ymd_hms()
  
  token <- read.csv("~/Projects/SoonerReport/Token.csv") %>% convert_token()
  
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
        loginfo(glue("Progress: {i}/{nrow(offerlist)}"))
      }
    }
  }
  
  predlist <- predlist %>% mutate(confidence = as.numeric(confidence))
  
  running_list <- read.csv("~/Desktop/On3Scraper/RunningO3List.csv")
  
  full_list <- read.csv("~/Desktop/On3Scraper/FullO3List.csv")
  
  new_preds <- anti_join(predlist, running_list) %>% anti_join(full_list)
  
  new_ou <- new_preds %>% filter(team=="Oklahoma")
  
  write.csv(predlist, "~/Desktop/On3Scraper/RunningO3List.csv", row.names = F)
  
  if(nrow(new_ou)==0){
    status <- data.frame(time = now, 
                         connection = 1,
                         new_prediction = 0,
                         predictor = NA,
                         prospect = NA,
                         tweeted = 0,
                         text = NA)
    log <- read.csv("~/Desktop/ActivityLogs/On3Log.csv") %>% mutate(time = ymd_hms(time))
    log <- bind_rows(log, status)
    write.csv(log,"~/Desktop/ActivityLogs/On3Log.csv",row.names = F)
  } else{
    if(nrow(new_ou)>5){
      post_message(glue("Found {nrow(new_ou)} New On3 Prediction(s)"), "StevenPlai", token=token)
      status <- data.frame(time = now,
                           connection = 1,
                           new_prediction = nrow(new_ou),
                           forecaster = NA,
                           prospect = name,
                           tweeted = 1,
                           text = text)
      log <- read.csv("~/Desktop/ActivityLogs/On3Log.csv") %>% mutate(time = ymd_hms(time))
      log <- bind_rows(log, status)
      write.csv(log,"~/Desktop/ActivityLogs/On3Log.csv",row.names = F)
    } else{
      post_message(glue("Found {nrow(new_ou)} New On3 Prediction(s)"), "StevenPlai", token=token)
      full_list <- bind_rows(full_list,new_ou)  
      write.csv(full_list, "~/Desktop/On3Scraper/FullO3List.csv", row.names = F)
      for(i in nrow(new_ou)){
        row <- new_ou %>% slice(i) %>% left_join(offerlist,by=c("prospect"="name"))
        name <- row$prospect
        predictor <- row$predictor
        conf <- row$confidence
        year <- row$year
        plink <- row$link
        
        page <- read_html(glue("https://on3.com/{plink}"))
        
        objects <- page %>% html_elements(".MeasurementInfo_item__LDnHm")
        info <- objects[[2]] %>% html_elements(".MuiTypography-root") 
        info <- info[[2]] %>% html_text() %>% strsplit(split="/") %>% unlist() %>% 
          as.character() %>% trimws()
        pos <- info[1]
        ht <- info[2]
        wt <- info[3]
        hs <- objects[[3]] %>% html_elements(".MuiTypography-root") 
        hs <- hs[[2]] %>% html_text() %>% trimws()
        htown <- objects[[4]] %>% html_elements(".MuiTypography-root") 
        htown <- htown[[2]] %>% html_text() %>% trimws()
        star <- page %>% html_elements(".StarRating_starWrapper__Ofuoa") %>% html_elements("a") %>% 
          html_elements("span")
        star <- star[[1]] %>% html_attr("aria-label") %>% substr(0,1)
        
        text <-  glue(
          "
          \U0001F52E New #Sooners On3 Prediction
          
          {year} {star}-Star {pos} {name}
          {ht} / {wt}
          {hs} ({htown})
          
          By: {predictor}
          Confidence: {conf}%
          
          https://on3.com/{plink}
          ")
        post_tweet(
          status = text,
          media = NULL,
          token = token
        )
        status <- data.frame(time = now,
                             connection = 1,
                             new_prediction = 1,
                             forecaster = predictor,
                             prospect = name,
                             tweeted = 1,
                             text = text)
        log <- read.csv("~/Desktop/ActivityLogs/On3Log.csv") %>% mutate(time = ymd_hms(time))
        log <- bind_rows(log, status)
        write.csv(log,"~/Desktop/ActivityLogs/On3Log.csv",row.names = F)
      }
    }
  }
}





