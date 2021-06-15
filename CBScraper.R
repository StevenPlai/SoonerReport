library(rvest)
library(tidyverse)
library(rtweet)
library(lubridate)
library(glue)

cb <- read_html("https://247sports.com/Season/2022-Football/TargetPredictions/")

span <- cb %>% 
  html_elements("span") %>% 
  html_attr("class")

image_names<-cb %>% html_nodes("img") %>% html_attr("alt")
image_height<-cb %>% html_nodes("img") %>% html_attr("height")
links<-cb %>% html_nodes("a") %>% html_attr("href")
pred_date<-cb %>% html_elements(".prediction-date") %>% html_text()
player_names<-cb %>% html_nodes(".name")
names <- html_children(player_names)
predictor_names<-cb %>% html_nodes(".predicted-by") %>% html_nodes("a") %>% 
  html_nodes(".jsonly") %>% html_attr("alt")
predictor_stats <- cb %>% html_nodes(".accuracy") %>% html_nodes("span") %>% html_text()
p_stats <- data.frame(acc = predictor_stats)
seq <- seq(to = 150, from = 3, by = 3)
p_stats <- p_stats[seq, ]
p_stats <- gsub(" ","",p_stats)
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
player_info$number <- 1:50

images <- data.frame(names = image_names, height = image_height)
teams <- images %>% dplyr::filter(height == 24)

zero <- data.frame(name = span)
zero <- zero %>% dplyr::slice(31:731)

zeroes <- which(zero == "icon-zero")
emptys <- floor(zeroes/14)

if(length(emptys!=0)) {
  teams$number <- 1:nrow(teams)
  cut <- teams %>% dplyr::filter(number>(emptys-1))
  teams <- teams %>% dplyr::filter(number<(emptys))
  cut <- cut %>% mutate(new = number+1) %>% select(-number, number = new)
  teams <- bind_rows(teams, cut)
  new_row <- data.frame(names = "icon-zero", height = as.factor(24), number = emptys)
  teams <- bind_rows(teams, new_row)
} else{teams$number <- 1:50}
pred_date <- as.data.frame(pred_date)
pred_date$number = 1:50
teams <- left_join(teams, pred_date, by="number")

targets <- data.frame(link = links)
sep <- targets %>% separate(col = link, into = c("prefix", "body"), sep = 8)
sep <- sep %>% separate(col = "body", into = c("site", "body"), sep = 9)
sep <- sep %>% separate(col = "body", into = c("suffix", "body"), sep = 5)
sep <- sep %>% separate(col = "body", into = c("type", "body"), sep = 6)
targets <- targets %>% mutate(site = sep$site, type = sep$type) %>% filter(type == "Player") %>% mutate(number = 1:50)

new_names <- data.frame(name = player_info$name)
sep <- new_names %>% separate(col = name, into = c("A", "B", "C", "D", "E"), sep = "                ")
player_info$name <- sep$B
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
player_info$pos <- new_pos$pos

new_rank <- data.frame(rank = player_info$rank)
sep <- new_rank %>% separate(col = rank, into = c("A", "B"), sep = "                \n                ")
new_rank$rank <- sep$B
sep <- new_rank %>% separate(col = rank, into = c("A", "B"), sep = "            ")
player_info$rank <- sep$A
player_info <- player_info %>% mutate(star = if_else(rank>0.9830, "5-Star",
                                                     if_else(rank>0.8900, "4-Star",
                                                             "3-Star")))

cb_list <- left_join(teams, targets, by="number")
now <- now(tzone = "America/Chicago")
now <- as.numeric(as.character(gsub("-|:| ","",now)))
cb_list$pred_date=mdy_hm(cb_list$pred_date, truncated = 1, tz = "America/Chicago")
cb_list$pred_date<- as.numeric(gsub("-|:| ","",cb_list$pred_date))
cb_list <- cb_list %>% mutate(elapsed = now-pred_date)
cb_list <- left_join(cb_list, player_info, by="number") 
predictor_info <- data.frame(predictor = predictor_names, acc = p_stats) 
predictor_info$number <- 1:50
predictor_info$confidence <- confidence
cb_list <- left_join(cb_list, predictor_info, by="number") 

new_pred <- cb_list %>% filter(elapsed < 130 & names == Oklahoma) 

if(nrow(new_pred)>0) {
  
  for(i in 1:nrow(new_pred)){
    
    pred <- new_pred %>% slice(i)
    
    name <- pred$name
    link <- as.character(pred$link)
    pos <- pred$pos
    rank <- pred$star
    ht <- pred$ht
    wt <- pred$wt
    predictor <- pred$predictor
    acc <- pred$acc
    star <- pred$star
    confidence <- pred$confidence
    
    player_page <- read_html(link) %>% html_nodes(".upper-cards") %>% html_nodes(".details") %>%
      html_nodes("li") %>% html_nodes("span") %>% html_text()
    hs <- player_page[2] 
    hs <- gsub("\n                            ","",hs)
    hs <- gsub("\n                        ","",hs)
    hometowm <- data.frame(state = player_page[4])
    sep <- hometowm %>% separate(col = state, into = c("Town", "State"), sep = ", ")
    state <- sep$State
    
    token <- create_token(
      app = "SoonerBot",
      "rv9GhbZ7h78cBJqToYNZbxZTP",
      "Tv604bevC3uXaobOq7bihCkA21PjUOkvB5ObhxYDYgKVtFWzVk",
      access_token = "1392790570125479937-LOobHwkoGZRbARnJzt1cymJgtkJhDS",
      access_secret = "IyVzV0pkApw6LgEiO1aP6jSjby4zT250LleG5YjnynrEG",
      set_renv = F
    )
    
    if(is.na(rank)){
      text <-  glue(
        "
        \U0001f6A8 New #Sooners Crystal Ball
        
        2022 {pos}{name}
        {ht} / {wt}
        {{hs} ({state})
        
        By: {predictor} ({acc} in 2022)
        Confidence: {confidence}/10
        
        {link}
        ")
        } else{
          text <-  glue(
            "
            \U0001f6A8 New #Sooners Crystal Ball
            
            2022 {star} {pos}{name}
            {ht} / {wt}
            {hs} ({state})
            
            By: {predictor} ({acc} in 2022)
            Confidence: {confidence}/10
            
            {link}
            ")
        }
    
    post_tweet(
      status = text,
      media = NULL,
      token = token
    )
        }
}           

