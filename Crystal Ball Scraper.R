library(rvest, warn.conflicts = F)
library(dplyr, warn.conflicts = F)
library(tidyr, warn.conflicts = F)
library(rtweet, warn.conflicts = F)
library(lubridate, warn.conflicts = F)
library(glue, warn.conflicts = F)
library(logging, warn.conflicts = F)
source("~/Projects/SoonerReport/Functions.R")

options(warn = -1)

token <- read.csv("~/Projects/SoonerReport/Token.csv") %>% convert_token()

loginfo("Beginning Scraping...")

now <- ymd_hms(Sys.time())

tryCatch(
  expr = {
    cb <- read_html("https://247sports.com/Season/2022-Football/CurrentTargetPredictions/?OrderBy=UpdatedOn%3ADESC") 
  }, 
  error = function(e) {
    status <- data.frame(time = now,
                         connection = 0,
                         new_prediction = NA,
                         forecaster = NA,
                         prospect = NA,
                         tweeted = 0,
                         text = NA)
    log <- read.csv("~/Desktop/ActivityLogs/247Log.csv") %>% mutate(time = ymd_hms(time))
    log <- bind_rows(log, status)
    write.csv(log,"~/Desktop/ActivityLogs/247Log.csv",row.names = F)
    stop("247Sports could not be reached")
  })



loginfo("Data acquired... Parsing now")

span <- cb %>% 
  html_elements("span") %>% 
  html_attr("class")

image_names<-cb %>% html_nodes("img") %>% html_attr("alt")
image_height<-cb %>% html_nodes("img") %>% html_attr("height")
image_class<-cb %>% html_nodes("img") %>% html_attr("class")
plinks<-cb %>% html_nodes("a") %>% html_attr("href")
pred_date<-cb %>% html_elements(".prediction-date") %>% html_text()
player_names<-cb %>% html_nodes(".name")
names <- html_children(player_names)
predictor_names<-cb %>% html_nodes(".predicted-by") %>% html_nodes("a") %>% 
  html_nodes("span") %>% html_text()
forecaster_links <- cb %>% html_elements(".predicted-by") %>% html_elements("a") %>% html_attr("href")
flinks <- data.frame(link = forecaster_links, number = 1:50)
confidence<-cb %>% html_nodes(".confidence") %>% html_nodes(".confidence-wrap") %>% 
  html_text()
confidence <- data.frame(confidence <- confidence)
sep <- confidence %>% separate(col = confidence....confidence, into = c("A", "B", "C"), sep = "                ")
confidence <- sep$B
confidence <- gsub("\n","",confidence)

player_info <- data.frame(name = NA,
                          pos = NA, 
                          rank = NA)

for(i in 0:49){
  info <- data.frame(name = html_text(names[[i*3+1]]),
                     pos = html_text(names[[((i*3)+2)]]),
                     rank = html_text(names[[((i*3)+3)]]))
  
  player_info <- bind_rows(player_info, info)
}

player_info <- player_info %>% slice(2:51)

images <- data.frame(names = image_names, height = as.integer(image_height), class = image_class)
images$class <- replace_na(images$class, "")
teams <- images %>% dplyr::filter(height == 24) 
changed <- if_else(teams$class=="old",1,0) %>% lag()
changed[1] <- 0
teams <- teams %>% mutate(changed = if_else(changed==1,1,0),
                          previous = if_else(changed==1,lag(names),"NA")) %>% 
  filter(class!="old")

zero <- data.frame(name = span)
zero <- zero %>% dplyr::slice(31:731)

zeroes <- which(zero == "icon-zero")
emptys <- floor(zeroes/14)
emptys <- sort(emptys)

if(length(emptys!=0)) {
  teams$number <- 1:nrow(teams)
  for(i in 1:length(emptys)) {
    current_empty <- emptys[i]
    cut <- teams %>% dplyr::filter(number>(current_empty-1))
    teams <- teams %>% dplyr::filter(number<(current_empty))
    cut <- cut %>% mutate(new = number+1) %>% select(-number, number = new)
    teams <- bind_rows(teams, cut)
    new_row <- data.frame(names = "icon-zero", height = 24, number = current_empty, changed=0, previous="NA")
    teams <- bind_rows(teams, new_row)
  }
} else{teams$number <- 1:50}
pred_date <- as.data.frame(pred_date)
pred_date$number = 1:50
teams <- left_join(teams, pred_date, by="number")

targets <- data.frame(plink = plinks)
sep <- targets %>% separate(col = plink, into = c("prefix", "body"), sep = 8)
sep <- sep %>% separate(col = "body", into = c("site", "body"), sep = 9)
sep <- sep %>% separate(col = "body", into = c("suffix", "body"), sep = 5)
sep <- sep %>% separate(col = "body", into = c("type", "body"), sep = 6)
targets <- targets  %>% 
  mutate(site = sep$site, type = sep$type) %>% filter(type == "Player")

if(any(targets$plink=="https://247sports.com/Player/Kaevon-Freshwater-46101385/")){
  targets <- targets %>% filter(plink!="https://247sports.com/Player/Kaevon-Freshwater-46101385/") %>%
    mutate(number = 1:49)
} else {  targets <- targets %>% mutate(number = 1:50)}

new_names <- data.frame(name = player_info$name)
sep <- new_names %>% separate(col = name, into = c("A", "B", "C", "D", "E"), sep = "                ")
player_info$name <- sep$B
player_info$year <- sep$D %>% trimws() %>% substr(2,5)
player_info$name <- gsub("\n","",player_info$name)

new_pos <- data.frame(pos = player_info$pos)
sep <- new_pos %>% separate(col = pos, into = c("A", "B", "C"), sep = "/")
new_pos$pos <- sep$A
new_pos$ht <- sep$B
new_pos$wt <- sep$C
new_pos$pos <- gsub("\n                ","",new_pos$pos)
sep3 <- new_pos %>% separate(col = wt, into = c("A", "B"), sep = "            ")
player_info$ht <- sep3$ht
player_info$ht <- gsub(" ","",player_info$ht)
player_info$wt <- sep3$A
player_info$wt <- gsub(" |\n","",player_info$wt)
player_info <- player_info %>% mutate(wt = as.integer(wt))
player_info$pos <- new_pos$pos
player_info <- player_info %>% mutate(rank = as.numeric(trimws(rank)),
                                      star = if_else(rank=="NA", "NR",
                                                     if_else(rank>0.9833, "5-Star",
                                                             if_else(rank>0.8900, "4-Star",
                                                                     "3-Star"))),
                                      number = 1:50) 

cb_list <- left_join(teams, targets, by="number")
cb_list$pred_date=mdy_hm(cb_list$pred_date, truncated = 1, tz = "America/Chicago")
cb_list$pred_date<- as.numeric(as.character(gsub("-|:| ","",cb_list$pred_date)))
cb_list$pred_date <- as.numeric(as.character(gsub(".{2}$","",cb_list$pred_date)))
cb_list <- left_join(cb_list, player_info, by="number") 
seqA <- seq(1,99, by = 2)
seqB <- seq(2,100, by=2)
predictor_info <- data.frame(predictor = predictor_names[seqA],
                             title = predictor_names[seqB],
                             flink = flinks$link, number = 1:50) 
predictor_info$confidence <- as.integer(confidence)
cb_list <- left_join(cb_list, predictor_info, by="number") 

running_list <- read.csv("~/desktop/CBScraper/RunningCBList.csv")
full_list <- read.csv("~/desktop/CBScraper/FullCBList.csv") %>% mutate(rank = as.numeric(rank),
                                                                       time = ymd_hms(time))

new_pred <- anti_join(cb_list, running_list, by=c("names", "pred_date", "name", "changed")) %>% 
  anti_join(full_list, by=c("names", "pred_date", "name"))

new_ou <- new_pred %>% filter(names == "Oklahoma") 

if(nrow(new_ou>0)){
  new <- new_ou %>% mutate(time = ymd_hms(now), rank = as.numeric(rank), year = as.numeric(year))
  full_list <- bind_rows(new,full_list)
  write.csv(full_list, "~/desktop/CBScraper/FullCBList.csv",
            row.names = F)
} else{
  status <- data.frame(time = now, 
                       connection = 1,
                       new_prediction = 0,
                       forecaster = NA,
                       prospect = NA,
                       tweeted = 0,
                       text = NA)
  log <- read.csv("~/Desktop/ActivityLogs/247Log.csv") %>% mutate(time = ymd_hms(time))
  log <- bind_rows(log, status)
  write.csv(log,"~/Desktop/ActivityLogs/247Log.csv",row.names = F)
}

write.csv(cb_list, "~/desktop/CBScraper/RunningCBList.csv",
          row.names = F)

if(nrow(new_ou)>5) {
  loginfo(glue("Found more than 3 new instances ({nrow(new_ou)}). Not tweeting"))
  status <- data.frame(time = now, 
                       connection = 1,
                       new_prediction = total,
                       predictor = NA,
                       prospect = NA,
                       tweeted = 0,
                       text = NA)
  log <- read.csv("~/Desktop/ActivityLogs/247Log.csv") %>% mutate(time = ymd_hms(time))
  log <- bind_rows(log, status)
  write.csv(log,"~/Desktop/ActivityLogs/247Log.csv",row.names = F)
} else{
  if(nrow(new_ou)>0) {
    
    status <- data.frame(time = now, 
                         connection = 1,
                         new_prediction = nrow(new_ou),
                         predictor = NA,
                         prospect = NA,
                         tweeted = .5,
                         text = NA)
    log <- read.csv("~/Desktop/ActivityLogs/247Log.csv") %>% mutate(time = ymd_hms(time))
    log <- bind_rows(log, status)
    write.csv(log,"~/Desktop/ActivityLogs/247Log.csv",row.names = F)
    
    loginfo(glue("Found {nrow(new_ou)} new instances. Tweeting Now"))
    
    for(i in 1:nrow(new_ou)){
      
      pred <- new_ou %>% slice(i)
      
      name <- pred$name
      plink <- as.character(pred$plink)
      flink <- as.character(pred$flink)
      pos <- pred$pos
      year <- pred$year
      rank <- pred$rank
      ht <- pred$ht
      wt <- pred$wt
      predictor <- pred$predictor
      title <- pred$title
      star <- pred$star
      confidence <- pred$confidence
      
      forecaster_info <- read_html(flink) %>% html_nodes(".picks") %>% 
        html_nodes("li") %>% html_nodes("span") %>% html_text()
      
      acc <- round(as.numeric(sub("%", "",forecaster_info[2],fixed=TRUE))/100, digits = 3)*100
      
      player_info <- read_html(plink) %>% html_nodes(".upper-cards") %>% html_nodes(".details") %>%
        html_nodes("li") %>% html_nodes("span") %>% html_text()
      hs <- trimws(player_info[2], which = "both") 
      hometown <- player_info[4]
      
      player_predictions_link <- read_html(plink) %>% html_nodes(".link-block") %>% 
        html_nodes("a") %>% 
        html_attr("href")
      
      player_prediction_teams <- read_html(paste0("https:",player_predictions_link[1])) %>% html_nodes(".prediction") %>%
        html_nodes("img")  %>% html_attr("alt")
      player_prediction_times <- read_html(paste0("https:",player_predictions_link[1])) %>% html_nodes(".prediction") %>%
        html_nodes(".date-time")  %>% html_text()
      player_prediction_conf <- read_html(paste0("https:",player_predictions_link[1])) %>% html_nodes(".confidence-wrap") %>%
        html_text()
      test <- read_html(paste0("https:",player_predictions_link[1])) %>% 
        html_nodes(".prediction-percentage") %>%
        html_nodes("ul") %>% html_attr("class")
      
      if(test == "list cb-1") {
        player_prediction_conf <- player_prediction_conf[-1]
      } else {
        player_prediction_conf <- player_prediction_conf[-1] 
        player_prediction_conf <- player_prediction_conf[-2]
      }
      
      #player_predictions <- data.frame(team = trimws(player_prediction_teams),
      #time = trimws(player_prediction_times),
      #conf = trimws(player_prediction_conf))
      #sep <- player_predictions %>% separate(col = conf,into = c("A", "B"), sep = "\n")
      #player_predictions$conf <- sep$A
      #sep <- player_predictions %>% separate(col = time,into = c("A", "B"), sep = "\n")
      #player_predictions$time <- mdy_hm(player_predictions$time, truncated = 1, tz = "America/Chicago")
      #player_predictions <- player_predictions %>% arrange(time)
      #player_predictions$cume <- cumsum(player_predictions$conf)
      
      #ggplot(data = player_predictions, aes(x = time, y = cume)) +
      #geom_step(aes(group = team), direction = "hv") +
      #scale_y_continuous(limits = c(0,max(player_predictions$cume))) +
      #scale_x_datetime(limits = c(ymd_hm("2021-02-01 01:01"), max(player_predictions$time)))
      
      if(star == "NR" | rank == "NA" | is.na(rank)){
        text <-  glue(
          "
          \U0001F52E New #Sooners 247 Crystal Ball
          
          {year} {pos}{name}
          {ht} / {wt}
          {hs} ({hometown})
          
          By: {title} {predictor} ({acc}%)
          Confidence: {confidence}/10
          
          {plink}
          ")
      } else{
        text <-  glue(
          "
          \U0001F52E New #Sooners 247 Crystal Ball
          
          {year} {star} {pos}{name}
          {ht} / {wt}
          {hs} ({hometown})
          
          By: {title} {predictor} ({acc}%)
          Confidence: {confidence}/10
          
          {plink}
          ")
      }
      
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
      log <- read.csv("~/Desktop/ActivityLogs/247Log.csv") %>% mutate(time = ymd_hms(time)) %>% 
        slice(2:nrow(log))
      log <- bind_rows(log, status)
      write.csv(log,"~/Desktop/ActivityLogs/247Log.csv",row.names = F)
    }
  } else {
    loginfo("Found 0 new instances")
  } 
  
}

