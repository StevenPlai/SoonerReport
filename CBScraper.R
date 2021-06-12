library(rvest)
library(rtweet)
library(lubridate)
library(grepl)

cb22 <- read_html("https://247sports.com/Season/2022-Football/TargetPredictions/")

span22 <- cb22 %>% 
  html_elements("span") %>% 
  html_attr("class")

image_names<-cb22 %>% html_nodes("img") %>% html_attr("alt")
image_height<-cb22 %>% html_nodes("img") %>% html_attr("height")
links<-cb22 %>% html_nodes("a") %>% html_attr("href")
pred_date<-cb22 %>% html_elements(".prediction-date") %>% html_text()

images22 <- data.frame(names = image_names, height = image_height)
teams22 <- images22 %>% filter(height == 24)

zero <- data.frame(name = span22)
zero <- zero %>% slice(31:731)

zeroes <- which(zero == "icon-zero")
emptys <- floor(zeroes/14)

if(length(emptys)!=0) {
teams22$number <- 1:nrow(teams22)
cut <- teams22 %>% filter(number>(emptys-1))
teams22 <- teams22 %>% filter(number<(emptys))
cut <- cut %>% mutate(new = number+1) %>% select(-number, number = new)
teams22 <- bind_rows(teams22, cut)
new_row <- data.frame(names = "icon-zero", height = as.factor(24), number = emptys)
teams22 <- bind_rows(teams22, new_row)
} else{teams22$number <- 1:50}
pred_date <- as.data.frame(pred_date)
pred_date$number = 1:50
teams22 <- left_join(teams22, pred_date, by="number")

targets <- data.frame(link = links)
sep <- targets %>% separate(col = link, into = c("prefix", "body"), sep = 8)
sep <- sep %>% separate(col = "body", into = c("site", "body"), sep = 9)
sep <- sep %>% separate(col = "body", into = c("suffix", "body"), sep = 5)
sep <- sep %>% separate(col = "body", into = c("type", "body"), sep = 6)
targets <- targets %>% mutate(site = sep$site, type = sep$type) %>% filter(type == "Player") %>% mutate(number = 1:50)

cb_list22 <- left_join(teams22, targets, by="number")
now <- now(tzone = "America/Chicago")
now <- as.numeric(as.character(gsub("-|:| ","",now)))
cb_list22$pred_date=mdy_hm(cb_list22$pred_date, truncated = 1, tz = "America/Chicago")
cb_list22$pred_date<- as.numeric(gsub("-|:| ","",cb_list22$pred_date))
cb_list22 <- cb_list22 %>% mutate(elapsed = now-pred_date)

new_pred <- cb_list22 %>% filter(elapsed > -130 & names =="Oklahoma")




if(nrow(new_pred)>0) {
  
  token <- create_token(
    app = "SoonerBot",
    "rv9GhbZ7h78cBJqToYNZbxZTP",
    "Tv604bevC3uXaobOq7bihCkA21PjUOkvB5ObhxYDYgKVtFWzVk",
    access_token = "1392790570125479937-LOobHwkoGZRbARnJzt1cymJgtkJhDS",
    access_secret = "IyVzV0pkApw6LgEiO1aP6jSjby4zT250LleG5YjnynrEG",
    set_renv = TRUE
  )
  
  post_tweet(
    status = paste0("New Crystal Ball ",new_pred$link),
    media = NULL,
    token = token
  )
}

               