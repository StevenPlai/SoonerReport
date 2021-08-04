library(tidyverse, warn.conflicts = F)
library(ggplot2, warn.conflicts = F)
library(stringi, warn.conflicts = F)
library(lubridate, warn.conflicts = F)
library(ggthemes, warn.conflicts = F)
library(zoo, warn.conflicts = F)
library(png, warn.conflicts = F)
library(ggimage, warn.conflicts = F)
library(gridExtra, warn.conflicts = F)
library(grid, warn.conflicts = F)
library(rtweet, warn.conflicts = F)
library(glue, warn.conflicts = F)
library(magick, warn.conflicts = F)
library(extrafont, warn.conflicts = F)
library(toOrdinal, warn.conflicts = F)

rcycle <- function() {
  date <- Sys.Date()
  year <- as.numeric(date %>% stri_sub(1,4))
  month <- as.numeric(date %>% stri_sub(6,7))
  mday <- as.numeric(date %>% stri_sub(9,10))
  wday <- wday(date)
  if (month>2) {as.character(year+1)
  } else {
    if (month==1) {as.character(year)} else {
      if(mday>7) {as.character(year+1)} else{
        if(mday==1) {as.character(year)} else {
          if(mday==2) {
            if(wday==5){as.character(year+1)
            } else{as.character(year)}} else {
              if(mday==3) {
                if(wday==5|wday==6){as.character(year+1)
                } else{as.character(year)}} else{
                  if(mday==4) {
                    if(wday>4){as.character(year+1)
                    } else{as.character(year)}} else{
                      if(mday==5) {
                        if(wday>4|wday==1){as.character(year+1)
                        } else{as.character(year)}} else{
                          if(mday==6) {
                            if(wday==3|wday==4){as.character(year)
                            } else{as.character(year+1)}} else{
                              if(mday==7) {
                                if(wday==4){as.character(year)
                                } else{as.character(year+1)}}}}}}}}}}}}

cycle <- rcycle()

comp <- read.csv(glue("~/desktop/Composite Scrapes/RunningCompositeRankings{cycle}.csv"))

teams <- sort(c("Oklahoma", "Alabama", "Ohio State", "Clemson", "Georgia", "Texas"))
colors <- c("#9E1B32", "#F56600", "black", "#666666", "#841617", "#BF5700")
borders <- c("#9E1B32", "white", "white", "white", "white", "white")
logos <- "NULL"
for(x in 1:6){
  x <- teams[x]
  n <- paste0("~/desktop/Logos/",x,".png")
  logos <- c(logos,n)
}

logos <- logos[-1]

report <- comp %>% filter(team %in% teams) %>% group_by(team) %>% 
  mutate(rmean = rollmean(rating, 3, align = "right", fill = 0)) %>% ungroup()
report$time <- ymd_hms(report$time, tz = "America/Chicago")
window <- Sys.time()
window <- window-days(30)
report <- report %>% mutate(test = time-window) %>% filter(test>0) %>% select(-test) 
initials <- as_vector(report %>% slice(1:6) %>% arrange(team) %>% select(rating))

commits <- read.csv(glue("~/desktop/Composite Scrapes/RunningCommits{cycle}.csv")) %>% 
  mutate(time = ymd_hms(time)) %>%
  filter(time>min(report$time) & time<max(report$time))

p <- ggplot(data = report, aes(x = time, y=rating)) 

if(nrow(commits)>0){
  p <- p + 
    geom_vline(xintercept = commits$time, linetype="dashed")
  for(i in 1:nrow(commits)){
    p <- p + geom_label(y=min(report$rating)+((i-1)*5), x=commits$time[i], family = "SF Pro Display Regular", 
                        label=glue("{commits$name[i]} commits"), color = "black") 
  }
}

p <- p +
    scale_color_manual(values = colors) +
    geom_line(aes(color = team), size = 2) +
    geom_line(data=report[report$team=="Alabama",], color="white", size =1) +
    geom_line(data=report[report$team=="Oklahoma",], color="#841617", size = 4) +
    labs(title = "30-Day Recruiting Report",
         subtitle = glue("Class of {cycle}"),
         caption = "Data: 247Sports\n@SoonerReport",
         y = "247 Composite Team Rating",
         x = "Date") +
    theme_fivethirtyeight() +
    theme(
      plot.title = element_text(size = 45, hjust = .5, family = "SF Pro Display Regular", face = "plain", vjust = .7),
      plot.subtitle = element_text(size = 30, hjust = .5, family = "SF Pro Display Light", face = "plain"),
      plot.caption = element_text(size=15, family = "Courier New", face = "bold", hjust = 1),
      axis.title = element_text(size = 18, family = "SF Pro Display Light", color = "black"),
      axis.text = element_text(size = 15, family = "SF Pro Display Light"),
      panel.grid.major = element_line(color = "#e6f0f9"),
      plot.background = element_rect(fill="white"),
      panel.background = element_rect(fill="white"),
      legend.position = "none") +
    annotation_custom(rasterGrob(readPNG(logos[1])), 
                      xmax=min(report$time), ymin=initials["rating1"]-4, ymax =initials["rating1"]+4) +
    annotation_custom(rasterGrob(readPNG(logos[2])), 
                      xmax=min(report$time), ymin=initials["rating2"]-4, ymax =initials["rating2"]+4) +
    annotation_custom(rasterGrob(readPNG(logos[3])),
                      xmax=min(report$time), ymin=initials["rating3"]-4, ymax =initials["rating3"]+4) +
    annotation_custom(rasterGrob(readPNG(logos[4])), 
                      xmax=min(report$time), ymin=initials["rating4"]-4, ymax =initials["rating4"]+4) +
    annotation_custom(rasterGrob(readPNG(logos[5])), 
                      xmax=min(report$time), ymin=initials["rating5"]-6, ymax =initials["rating5"]+6) +
    annotation_custom(rasterGrob(readPNG(logos[6])), 
                      xmax=min(report$time), ymin=initials["rating6"]-4, ymax =initials["rating6"]+4)

ggsave(glue("~/desktop/CompositeReports/30DayReport{cycle}.png"), 
       plot = egg::set_panel_size(p=p, width=unit(10, "in"), height=unit(7, "in")),
       height = 9.5, width = 11)

p <- image_read(glue("~/desktop/CompositeReports/30DayReport{cycle}.png"))
l <- image_read("~/desktop/LOGO.png")
i <- image_composite(p,image_scale(l,"550"),offset="+5+2530")

image_write(i, path = glue("~/desktop/CompositeReports/30DayReport{cycle}.png"), format = "png")

rank <- comp %>% filter(team == "Oklahoma") %>% slice_max(time) %>% select(rank) %>% as.numeric() %>% toOrdinal()

text <-  glue(
  "
  #Sooners 30-Day Recruiting Report
  
  Oklahoma currently ranks {rank} in 247 Composite for Class of {cycle}
  ")

post_tweet(status = text,
           media = glue("~/desktop/CompositeReports/30DayReport{cycle}.png"),
           token = token)

  
rasterGrob(readPNG("~/desktop/LOGO.png"))


