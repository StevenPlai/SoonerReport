library(gt)
library(gtExtras)
library(stringi)
library(tidyverse)
library(glue)
library(fontawesome)
library(cfbfastR)
library(grid)
library(extrafont)
library(ggimage)
library(ggthemes)
library(toOrdinal)
library(png)
library(egg)
library(magick)
library(webshot2)
library(ggtext)
library(kableExtra)
source("Functions.R")

lweek <- CFBWeek()-1

weekOff <- read.csv("Data/WeekOffense.csv")
drives <- weekOff %>% group_by(drive_id) %>% summarise(xPts = first(ep_before),
                                                       EPA = mean(EPA),
                                                       start = first(yard_line),
                                                       last_play = last(play_type),
                                                       plays = n(),
                                                       positions = list(yard_line),
                                                       lead_before = first(pos_team_score)-first(def_pos_team_score),
                                                       time_after = last(clock.minutes)+(last(clock.seconds)*60)) %>% 
  mutate(game_end = if_else(lead_before>0&time_after==0,1,0),
         result = if_else()) %>% filter(game_end==0)

logos <-   logos <- cfbd_team_info(year=2021,only_fbs = F) %>% select(school,logos) %>%
  separate(col=logos,sep=",",into=c("A","B")) %>% select(-B)
logos$A <- logos$A %>% stri_sub(from=4,to=nchar(logos$A)-1)

info <- cfbd_team_info(year=2021,only_fbs = F) %>% select(school,conference)

SR <- image_read("Images/Logo.png")

xpos <- as.list(drives[1,7])
xpos <- xpos[[1]]
xpos <- xpos[[1]]

line_color <- if_else(drives[1,])

p <- ggplot(data = drives[1,]) +
  xlim(0,100) + ylim(-1,1) +
  geom_vline(xintercept = 0,size=2) +
  geom_vline(xintercept = 10) +
  geom_vline(xintercept = 20,color="red") +
  geom_vline(xintercept = 30) +
  geom_vline(xintercept = 40) +
  geom_vline(xintercept = 50,size=1.25) +
  geom_vline(xintercept = 60) +
  geom_vline(xintercept = 70) +
  geom_vline(xintercept = 80,color="red") +
  geom_vline(xintercept = 90) +
  geom_vline(xintercept = 100,size=2) +
  theme(line = element_blank(),
        text = element_blank(),
        title = element_blank(),
        panel.background = element_rect(fill ="white"),
        plot.background = element_rect(fill="white"),
        panel.grid = element_blank()) +
  geom_segment(x=first(xpos),xend=last(xpos),y=0,yend=0,size=2,color=line_color)

for(i in 1:length(xpos)){
  p <- p + geom_point(x=xpos[i],y=0,size=5)
}



