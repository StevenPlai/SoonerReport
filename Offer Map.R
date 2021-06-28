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

font_import()  

x <- getURL("https://raw.githubusercontent.com/StevenPlai/SoonerReport/main/uscities.csv")
city_data <- read.csv(text = x)

offers <- read_html("https://247sports.com/Season/2024-Football/Offers?ViewPath=~/Views/SkyNet/RecruitInterest/_SimpleDetailedSetForSeason.ascx&skey=12")

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
geo_data <- geo_data %>% group_by(city, state_id) %>% summarise(lat = first(lat), 
                                                lng = first(lng),
                                                offers = n()) %>% ungroup() %>%
  filter(!is.na(lng))

coords <- geo_data %>% select(lng,lat)
geo_data <- geo_data %>% select(-lng,-lat)
coords <- coords %>% usmap_transform() %>% select(lng = lng.1, lat = lat.1)
geo_data$lng <- coords$lng
geo_data$lat <- coords$lat

now <- ymd_hms(Sys.time())
now <- format(as.Date(now),'%m/%d/%Y')

p <- plot_usmap() +
  geom_point(data = geo_data, aes(x=lng, y=lat, size = offers), 
            alpha = .35, color = "red") +
  scale_size_continuous(range = c(4, 1.25*max(geo_data$offers)),
                        name = "Offers", breaks = c(sort(unique(geo_data$offers)))) +
  labs(title = "Cumulative Offer Map",
       subtitle = "Oklahoma Class of 2019",
       caption = paste0("@SoonerReport\nUpdated Through ",now)) +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(size = 45, hjust = .5, family = ".SF NS Rounded", vjust = .7),
    plot.subtitle = element_text(size = 30, hjust = .5, family = ".SF NS Rounded"),
    plot.caption = element_text(size=15, face="bold", family = "Courier New", hjust = 1),
    legend.position = c(.65,.06),
    legend.title = element_text(family = ".SF NS Rounded", size = 15),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
  )

grid.arrange(egg::set_panel_size(p=p, height=unit(7, "in"), 
                                 width=unit(10, "in")))

ggsave("/Users/stevenplaisance/desktop/CumeOfferMap24.png", 
       plot = egg::set_panel_size(p=p, width=unit(10, "in"), height=unit(7, "in")),
       height = 9, width = 10.35)


