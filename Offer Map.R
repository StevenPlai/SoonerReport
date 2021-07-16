library(rvest)
library(tidyverse)
library(stringr)
library(RCurl)
library(usmap)
library(ggthemes)
library(extrafont)
library(lubridate)
library(gridExtra)
library(egg)
library(maptools)
library(rgdal)
library(rtweet)
library(png)
library(ggpubr)

na_cities <- data.frame()

now <- ymd_hms(Sys.time())
now <- format(as.Date(now),'%m/%d/%Y')

for(i in 2022:2025)
{ 
cycle <- i

x <- getURL("https://raw.githubusercontent.com/StevenPlai/SoonerReport/main/uscities.csv")
city_data <- read.csv(text = x)

offers <- read_html(paste0("https://247sports.com/Season/",cycle,"-Football/Offers?ViewPath=~/Views/SkyNet/RecruitInterest/_SimpleDetailedSetForSeason.ascx&skey=12"))

locations <- offers %>% html_nodes(".meta") %>% html_text() %>% 
  trimws() %>% str_extract_all("(?<=\\().+?(?=\\))")

locations <- data.frame(city = matrix(unlist(locations), nrow=length(locations), byrow=TRUE))

locations$city <- locations$city %>% recode("Saint Louis, MO" = "St. Louis, MO")
locations$city <- locations$city %>% recode("East Saint Louis, IL" = "East St. Louis, IL")
locations$city <- locations$city %>% recode("Saint Charles, MO" = "St. Charles, MO")
locations$city <- locations$city %>% recode("West Chester, OH" = "Olde West Chester, OH")
locations$city <- locations$city %>% recode("Hallandale, FL" = "Hallandale Beach, FL")
locations$city <- locations$city %>% recode("Cypress, TX" = "Jersey Village, TX")
locations$city <- locations$city %>% recode("Cordova, TN" = "Lakeland, TN")
locations$city <- locations$city %>% recode("Ellenwood, GA" = "Lake City, TN")
locations$city <- locations$city %>% recode("Lees Summit, MO" = "Lee's Summit, MO")
locations$city <- locations$city %>% recode("Windsor, CT" = "Hartford, CT")
locations$city <- locations$city %>% recode("Oakdale, CT" = "Norwich, CT")
location_data <- locations %>% separate(col = city, sep = ",", into = c("city", "state_id")) 
location_data$state_id <- trimws(location_data$state_id)

geo_data <- left_join(location_data, city_data, by=c("city", "state_id")) 
old_offers <- read.csv(paste0("~/desktop/Offers/OfferList",i,".csv"))
new_offers <- anti_join(geo_data, old_offers) %>% mutate(date = now)
if(nrow(new_offers)>0)
{
  runnng_list <- read.csv(paste0("~/desktop/Offers/DatedOfferList",i,".csv"))
  new_offers <- bind_rows(runnng_list, new_offers)
  write.csv(new_offers,paste0("~/desktop/Offers/DatedOfferList",i,".csv") )
}
write.csv(geo_data, paste0("~/desktop/Offers/OfferList",i,".csv"))
n_offers <- nrow(geo_data)
na_cities <- bind_rows(na_cities, geo_data %>% filter(is.na(lng)))
geo_data <- geo_data %>% group_by(city, state_id) %>% summarise(lat = first(lat), 
                                                                lng = first(lng),
                                                                offers = n()) %>% ungroup() %>%
  filter(!is.na(lng))

coords <- geo_data %>% select(lng,lat)
geo_data <- geo_data %>% select(-lng,-lat)
coords <- coords %>% usmap_transform() %>% select(lng = lng.1, lat = lat.1)
geo_data$lng <- coords$lng
geo_data$lat <- coords$lat


p <- plot_usmap() +
  geom_point(data = geo_data, aes(x=lng, y=lat, size = offers), 
             alpha = .35, color = "red") +
  scale_size_continuous(range = c(4, 1.5*max(geo_data$offers)),
                        name = "Offers", breaks = c(sort(unique(geo_data$offers)))) +
  labs(title = "Cumulative Offer Map",
       subtitle = paste0("Oklahoma Class of ",cycle),
       caption = paste0("Data: 247Sports, USCities\nAs of ",now,"\n@SoonerReport")) +
  theme_fivethirtyeight() +
  annotation_custom(text_grob(paste0("Total Offers = ",n_offers),
                              hjust = -.8, vjust = -1, face = "plain",
                              family = "Arial", size = 20), xmin = .9, xmax = 1, ymin = .5) +
  theme(
    plot.title = element_text(size = 45, hjust = .5, family = "Arial", face = "bold", vjust = .7),
    plot.subtitle = element_text(size = 30, hjust = .5, family = "Arial", face = "plain"),
    plot.caption = element_text(size=15, family = "Courier New", face = "bold", hjust = 1),
    legend.position = c(.65,.06),
    legend.title = element_text(family = "Arial", face = "plain", size = 15),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.background = element_rect(fill="white"),
    panel.background = element_rect(fill="white"),
    legend.background = element_rect(fill="white"),
    legend.key = element_rect(fill = NA)
    
  )

grid.arrange(egg::set_panel_size(p=p, height=unit(7, "in"), 
                                 width=unit(10, "in")))

ggsave(paste0("~/desktop/Offers/CumeOfferMap",cycle,".png"), 
       plot = egg::set_panel_size(p=p, width=unit(10, "in"), height=unit(7, "in")),
       height = 9, width = 10.35)

}

post_tweet(
  status = "Updated #Sooners Cumulative Offer Maps",
  token = token,
  media = c("~/desktop/Offers/CumeOfferMap2022.png","~/desktop/Offers/CumeOfferMap2023.png"))

prev_nas <- read.csv("~/desktop/Offers/na_cities.csv")
na_cities <- bind_rows(prev_nas, na_cities)
write.csv(na_cities, "~/desktop/Offers/na_cities.csv")

