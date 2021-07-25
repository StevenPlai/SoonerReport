library(tidyverse)
library(ggplot2)
library(stringi)
library(lubridate)
library(ggthemes)
library(zoo)
library(png)
library(ggimage)
library(grid)

comp <- read.csv("~/desktop/Composite Scrapes/RunningCompositeRankings2022.csv")

teams <- sort(c("Oklahoma", "Alabama", "Ohio State", "Clemson", "Georgia"))
colors <- c("#9E1B32", "#F56600", "#BA0C2F", "#BB0000", "#841617")
logos <- "NULL"
for(x in 1:5){
  x <- teams[x]
  n <- paste0("~/desktop/Logos/",x,".png")
  logos <- c(logos,n)
}

logos <- logos[-1]


report <- comp %>% filter(team %in% teams) %>% group_by(team) %>% 
  mutate(rmean = rollmean(rating, 3, align = "right", fill = 0)) %>% ungroup()
report$time <- ymd_hms(report$time, tz = "America/Chicago")
window <- Sys.time()
window <- window-weeks(2)
report <- report %>% mutate(test = time-window) %>% filter(test>0) %>% select(-test) 
initials <- as_vector(report %>% slice(1:5) %>% arrange(team) %>% select(rating))


ggplot(data = report, aes(x = time, y=rating)) +
  scale_color_manual(values = colors) +
  geom_line(aes(color = team), size = 4) +
  labs(title = "Two-Week Performance Report",
       subtitle = "Class of 2022",
       caption = paste0("Data: 247Sports\n@SoonerReport"),
       y = "247 Composite Rating",
       x = "Date") +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(size = 45, hjust = .5, family = "Arial", face = "bold", vjust = .7),
    plot.subtitle = element_text(size = 30, hjust = .5, family = "Arial", face = "plain"),
    plot.caption = element_text(size=15, family = "Courier New", face = "bold", hjust = 1),
    axis.title = element_text(size = 18, family = "Arial", face = "bold"),
    axis.text = element_text(size = 15, family = "Arial"),
    plot.background = element_rect(fill="white"),
    panel.background = element_rect(fill="white"),
    legend.position = "none") +
  annotation_custom(rasterGrob(readPNG(logos[1])), 
                    xmax=min(report$time), ymin=initials["rating1"]-5, ymax =initials["rating1"]+5) +
  annotation_custom(rasterGrob(readPNG(logos[2])), 
                    xmax=min(report$time), ymin=initials["rating2"]-5, ymax =initials["rating2"]+5) +
  annotation_custom(rasterGrob(readPNG(logos[3])), 
                    xmax=min(report$time), ymin=initials["rating3"]-5, ymax =initials["rating3"]+5) +
  annotation_custom(rasterGrob(readPNG(logos[4])), 
                    xmax=min(report$time), ymin=initials["rating4"]-5, ymax =initials["rating4"]+5) +
  annotation_custom(rasterGrob(readPNG(logos[5])), 
                    xmax=min(report$time), ymin=initials["rating5"]-5, ymax =initials["rating5"]+5)
  



