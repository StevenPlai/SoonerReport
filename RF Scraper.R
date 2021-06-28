library(rvest)
library(tidyverse)
library(rtweet)
library(lubridate)
library(glue)

rf <- read_html("https://n.rivals.com/futurecast") %>% 
  html_nodes(".ForecastActivity_forecastTime__2ECSU") %>% html_text()

