library(rtweet)
library(tidyverse)
library(glue)
library(lubridate)
source("~/desktop/Projects/Sooner Report/Repo/SoonerReport/Functions.R")

token <- read.csv("~/desktop/Projects/Sooner Report/Repo/SoonerReport/Token.csv") %>% convert_token()

min <- as.POSIXct(Sys.time(),tz="America/Chicago") %>% substr(16,16)

if(min==5|min==0){cblog <- read.csv("~/Desktop/ActivityLogs/247Log.csv") %>% mutate(time = ymd_hms(time)) %>% arrange(desc(time)) %>% slice(1:6)
rflog <- read.csv("~/Desktop/ActivityLogs/RivalsLog.csv") %>% mutate(time = ymd_hms(time)) %>% arrange(desc(time)) %>% slice(1:6)

cblast <- cblog$time[1]+hours(6) 
cblast <- cblast %>% as.POSIXct()
rflast <- rflog$time[1]+hours(6) 
rflast <- rflast %>% as.POSIXct()

now <- as.POSIXct(Sys.time(),tz="America/Chicago")
bnow <- now %>% substr(6,16)
cbelapsed <- difftime(now,cblast,units="secs") %>% as.numeric() %>% round(0)
rfelapsed <- difftime(now,rflast,units="secs") %>% as.numeric() %>% round(0)

if(any(cblog$connection==0)) text <- glue("{bnow}: Failed to connect to 247") else{
  if(any(rflog$nat_connection)==0) text <- glue("{bnow}: Failed to connect to Rivals") else{
    if(any(rflog$reg_connection)==0) text <- glue("{bnow}: Failed to connect to Soonerscoop") else{
      if(any(cblog$new_prediction)>0&any(cblog$tweeted>0)) text <- glue("{bnow}: Found and tweeted new CBs") else{
        if(any(rflog$new_prediction)>0&any(rflog$tweeted>0)) text <- glue("{bnow}: Found and tweeted new RFs") else{
          if(any(cblog$new_prediction)>0&all(cblog$tweeted==0)) text <- glue("{bnow}: Found new CBs, did not tweet") else{
            if(any(rflog$new_prediction)>0&all(rflog$tweeted==0)) text <- glue("{bnow}: Found new CBs, did not tweet") else{
              if(cbelapsed<200&rfelapsed<200) text <- glue("{bnow}: All systems operational, no new instances") else{
                text <- "Error encountered. Not run"
              }
            }
          }
        }  
      }
    }
  }
}

post_message(text, "StevenPlai", token=token)
post_message(text, "KeganReneau", token=token)}

