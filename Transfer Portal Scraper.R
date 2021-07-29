library(rvest, warn.conflicts = F)
library(tidyverse, warn.conflicts = F)
library(lubridate, warn.conflicts = F)
library(glue, warn.conflicts = F)

target_year <- 2021

tp <- read_html(glue("https://247sports.com/Season/{target_year}-Football/TransferPortal/")) 

portal <- data.frame(name = tp %>% html_nodes(".transfer-player") %>% html_nodes("h3") %>% html_text(),
                     rating = tp %>% html_nodes(".rating") %>% html_text(),
                     pos = tp %>% html_nodes(".position") %>% html_text(),
                     eligibility = tp %>% html_nodes(".eligibility") %>% html_text(),
                     original = tp %>% html_nodes(".logo.source") %>% html_attr("alt"))



