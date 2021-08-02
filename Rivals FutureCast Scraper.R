library(rvest, warn.conflicts = F)
library(tidyverse, warn.conflicts = F)
library(lubridate, warn.conflicts = F)
library(glue, warn.conflicts = F)
library(httr, warn.conflicts = F)
library(jsonlite, warn.conflicts = F)
library(stringr, warn.conflicts = F)
library(logging, warn.conflicts = F)
library(rtweet, warn.conflicts = F)

target_school <- "Oklahoma"
target_year <- "2022"
now <- ymd_hms(Sys.time())

loginfo("Starting Rivals FutureCast scraping...")

trim <- function (x) gsub("^\\s+|\\s+$", "", gsub("^\\t|\\t$", "", x))
column <- function(x, css) x %>% html_node(css = css) %>% html_text()

fixHeight <- function(x) paste0(floor(x/12),"-",x-(floor(x/12)*12))

splitJoin <- function(x) strsplit(x, split="\\s") %>% trim() %>% .[lapply(., length) > 0] %>% unlist() %>% paste()

target_page <- tolower(target_school)
team_html <- read_html(paste0("https://",target_page,".rivals.com/futurecast"))

main_html <- read_html("https://n.rivals.com/futurecast")

# ----- FutureCast Forecasters Team -----
loginfo("Looking for Forecaster information on team page...")
forecast_nodes <- team_html %>%
  html_nodes("[class^=\"ForecasterThumbnail_forecasterInformation__\"]")

loginfo(glue("Found {length(forecast_nodes)} forecaster nodes on team page. Parsing..."))
forecasters <- data.frame(
  title = column(forecast_nodes, "[class^=\"ForecasterThumbnail_forecasterTitle__\"]"),
  first_name = column(forecast_nodes, "[class^=\"Link_link__1xDdm ForecasterThumbnail_forecasterName__\"] > div:nth_child(1)"),
  last_name = column(forecast_nodes, "[class^=\"Link_link__1xDdm ForecasterThumbnail_forecasterName__\"] > div:nth_child(2)"),
  accuracy = column(forecast_nodes, "[class^=\"ForecasterThumbnail_forecasterAccuracy__\"]"),
  stringsAsFactors = FALSE
)

loginfo(glue("Parsed {nrow(forecasters)} forecaster rows from team page. Cleaning..."))
forecasters <- forecasters %>%
  mutate(
    title = trim(title),
    first_name = trim(first_name),
    last_name = trim(last_name),
    full_name = glue("{first_name} {last_name}"),
    accuracy = as.numeric(gsub("% accuracy", "", accuracy))
  ) %>%
  select(full_name, title, accuracy)

loginfo(glue("Found/parsed/cleaned {nrow(forecasters)} forecaster rows from team page"))

# ----- FutureCasts Team -----
loginfo(glue("Looking for recruit futurecast nodes on team page..."))

fc_nodes <- team_html %>%
  html_nodes("[class^=\"ForecastActivity_forecastActivity__\"] > [class^=\"ForecastActivity_activityText__\"]")

loginfo(glue("Found {length(fc_nodes)} recruit futurecast nodes, parsing..."))
futurecasts <- data.frame(
  forecaster = column(fc_nodes, "[class^=\"ForecastActivity_forecastText__\"] > b:nth-child(1)"),
  recruit = column(fc_nodes, "[class^=\"ForecastActivity_forecastText__\"] > b:nth_child(2)"),
  profile_url = fc_nodes %>% html_node("[class^=\"ForecastActivity_forecastText__\"] > b:nth_child(2) > a") %>% html_attr('href'),
  full_text = column(fc_nodes, "[class^=\"ForecastActivity_forecastText__\"]"),
  time_since = column(fc_nodes, "[class^=\"ForecastActivity_forecastTime__\"]"),
  stringsAsFactors = FALSE
)

loginfo(glue("Found {nrow(futurecasts)} recruit futurecast rows on team page"))

now <- now(tz = "UTC")

futurecasts <- futurecasts %>%
  mutate(
    forecaster = trim(forecaster),
    recruit = trim(recruit),
    recruit = gsub("\\s+"," ",recruit),
    time_since = gsub(" ago", "", time_since),
    player_id = as.numeric(sub(".*-", "", profile_url)),
    year = str_extract(full_text, "\\((\\d{4}),"),
    year = sub("\\D","", year),
    year = sub(",","", year),
    year = as.integer(year),
    forecasted_team = str_extract(full_text, " to (.*)\\."),
    forecasted_team = sub("to ","", forecasted_team),
    forecasted_team = sub("\\.","", forecasted_team),
    forecasted_team = trimws(forecasted_team),
    update = if_else(grepl("updates", full_text, fixed = T), 1, 0),
    original_school = if_else(update == 0, "NA", str_extract(full_text, "from (.*)\\ to")),
    original_school = sub(" to","", original_school),
    original_school = sub("from ","", original_school))

t_futurecasts <- left_join(futurecasts, forecasters, by = c("forecaster"="full_name"))

# ----- FutureCast Forecasters National -----
loginfo("Looking for Forecaster information on national page...")
forecast_nodes <- main_html %>%
  html_nodes("[class^=\"ForecasterThumbnail_forecasterInformation__\"]")

loginfo(glue("Found {length(forecast_nodes)} forecaster nodes on national page. Parsing..."))
forecasters <- data.frame(
  title = column(forecast_nodes, "[class^=\"ForecasterThumbnail_forecasterTitle__\"]"),
  first_name = column(forecast_nodes, "[class^=\"Link_link__1xDdm ForecasterThumbnail_forecasterName__\"] > div:nth_child(1)"),
  last_name = column(forecast_nodes, "[class^=\"Link_link__1xDdm ForecasterThumbnail_forecasterName__\"] > div:nth_child(2)"),
  accuracy = column(forecast_nodes, "[class^=\"ForecasterThumbnail_forecasterAccuracy__\"]"),
  stringsAsFactors = FALSE
)

loginfo(glue("Parsed {nrow(forecasters)} forecaster rows from national page. Cleaning..."))
forecasters <- forecasters %>%
  mutate(
    title = trim(title),
    first_name = trim(first_name),
    last_name = trim(last_name),
    full_name = glue("{first_name} {last_name}"),
    accuracy = as.numeric(gsub("% accuracy", "", accuracy))
  ) %>%
  select(full_name, title, accuracy)

loginfo(glue("Found/parsed/cleaned {nrow(forecasters)} forecaster rows from national page"))

# ----- FutureCasts National -----
loginfo(glue("Looking for recruit futurecast nodes on national page..."))

fc_nodes <- main_html %>%
  html_nodes("[class^=\"ForecastActivity_forecastActivity__\"] > [class^=\"ForecastActivity_activityText__\"]")

loginfo(glue("Found {length(fc_nodes)} recruit futurecast nodes, parsing..."))
futurecasts <- data.frame(
  forecaster = column(fc_nodes, "[class^=\"ForecastActivity_forecastText__\"] > b:nth-child(1)"),
  recruit = column(fc_nodes, "[class^=\"ForecastActivity_forecastText__\"] > b:nth_child(2)"),
  profile_url = fc_nodes %>% html_node("[class^=\"ForecastActivity_forecastText__\"] > b:nth_child(2) > a") %>% html_attr('href'),
  full_text = column(fc_nodes, "[class^=\"ForecastActivity_forecastText__\"]"),
  time_since = column(fc_nodes, "[class^=\"ForecastActivity_forecastTime__\"]"),
  stringsAsFactors = FALSE
)

loginfo(glue("Found {nrow(futurecasts)} recruit futurecast rows on national page,."))
loginfo(glue("Cleaning and filtering based on criteria: school ({target_school})"))

futurecasts <- futurecasts %>%
  mutate(
    forecaster = trim(forecaster),
    recruit = trim(recruit),
    recruit = gsub("\\s+"," ",recruit),
    time_since = gsub(" ago", "", time_since),
    player_id = as.numeric(sub(".*-", "", profile_url)),
    year = str_extract(full_text, "\\((\\d{4}),"),
    year = sub("\\D","", year),
    year = sub(",","", year),
    year = as.integer(year),
    forecasted_team = str_extract(full_text, " to (.*)\\."),
    forecasted_team = sub("to ","", forecasted_team),
    forecasted_team = sub("\\.","", forecasted_team),
    forecasted_team = trimws(forecasted_team),
    update = if_else(grepl("updates", full_text, fixed = T), 1, 0),
    original_school = if_else(update == 0, "NA", str_extract(full_text, "from (.*)\\ to")),
    original_school = sub(" to","", original_school),
    original_school = sub("from ","", original_school))

n_futurecasts <- left_join(futurecasts, forecasters, by = c("forecaster"="full_name"))

futurecasts <- bind_rows(t_futurecasts, n_futurecasts) %>% group_by(full_text, time_since) %>%
  summarise(across(forecaster:accuracy,first)) %>% filter(forecasted_team == target_school | 
                                                            original_school == target_school) %>%
  select(colnames(t_futurecasts)) %>% ungroup() %>% mutate(forecaster = as.character(forecaster))

running_list <- read.csv("~/Desktop/RFScraper/running_list.csv")
full_list <- read.csv("~/Desktop/RFScraper/full_list.csv") %>% mutate(time = ymd_hms(time))

new_futurecasts <- anti_join(futurecasts, running_list, by=c("forecaster", "player_id", "forecasted_team"))
new_records <- new_futurecasts %>% mutate(time = ymd_hms(now))

full_list <- bind_rows(full_list, futurecasts)

write.csv(futurecasts, "~/Desktop/RFScraper/running_list.csv", row.names = F)
write.csv(full_list, "~/Desktop/RFScraper/full_list.csv", row.names = F)

projected_futurecasts <- new_futurecasts %>% filter(target_school == forecasted_team)

changed_futurecasts <- new_futurecasts %>% filter(target_school == original_school)

loginfo(glue("Found {nrow(projected_futurecasts)} New FutureCasts and {nrow(changed_futurecasts)} Changed FutureCasts"))

strip_suffix <- function(name) {
  name <- trim(name)
  result <- case_when(
    grepl(' Jr$', name) ~ gsub("Jr","", name),
    grepl(' Jr\\.$', name) ~ gsub("Jr.","", name),
    grepl(' II$', name) ~ gsub("II","", name),
    grepl(' III$', name) ~ gsub("III","", name),
    grepl(' IV$', name) ~ gsub("IV","", name),
    grepl(' V$', name) ~ gsub("V","", name),
    grepl(' VI$', name) ~ gsub("VI","", name),
    grepl(' VII$', name) ~ gsub("VII","", name),
    TRUE ~ trim(name)
  )
  return(result)
}

query_croots <- function(name, year) {
  body <- paste0("{
                 \"search\": {
                 \"member\": \"Prospect\",
                 \"query\": \",",strip_suffix(name),",\",
                 \"sport\": \"Football\",
                 \"page_number\": \"1\",
                 \"page_size\": \"50\",
                 \"recruit_year\": \"",year,"\"
}
}")
  croot_req <- POST("https://n.rivals.com/api/v1/people", add_headers(
    "User-Agent"="Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:85.0) Gecko/20100101 Firefox/85.0",
    Accept="application/json, text/plain, */*",
    "Accept-Language"="en-US,en;q=0.5",
    Referer="https://n.rivals.com/search",
    "Content-Type"="application/json;charset=utf-8",
    "X-XSRF-TOKEN"="JWSTp21Yj1wpNjaVz7W7x/wNgbUCNxLTFP0oHbOtYWUXdXMt2hvX+NQ3ejC8as5FAb1RE/KwiH3bY4SlsEl+Sg==",
    "Origin"="https://n.rivals.com",
    "Connection"="keep-alive",
    "Cookie"="A1=d=AQABBL_NHmACEKNsUb5rZatSKxxQNn_WTEQFEgEBAQEfIGAoYAAAAAAA_SMAAA&S=AQAAAkKziEj7gaF2mOKKRDUJWqg; A3=d=AQABBL_NHmACEKNsUb5rZatSKxxQNn_WTEQFEgEBAQEfIGAoYAAAAAAA_SMAAA&S=AQAAAkKziEj7gaF2mOKKRDUJWqg; A1S=d=AQABBL_NHmACEKNsUb5rZatSKxxQNn_WTEQFEgEBAQEfIGAoYAAAAAAA_SMAAA&S=AQAAAkKziEj7gaF2mOKKRDUJWqg; GUC=AQEBAQFgIB9gKEId1wSk; XSRF-TOKEN=JWSTp21Yj1wpNjaVz7W7x%2FwNgbUCNxLTFP0oHbOtYWUXdXMt2hvX%2BNQ3ejC8as5FAb1RE%2FKwiH3bY4SlsEl%2BSg%3D%3D; _rivalry_session_v2=ZWZZQVZSQVh3ZnNaSWxaZlgwNU95aXV3a3VFREM2SE9iME9wZG4yaERvd0hHNjJOekhGZG1BOHo4WDM5OFdCSnFCczRWQ0hKelRzMWJna1NDZXV3MndjNHhaZlU1S0ZEdmJFaTVUR1doQ1RHdUJhRlQxUis2cWRBblUyVlhKcXVnVHE0aFlDODZscVMvQWxwRWxQUmpwUGk4QWdXRWUwdTlIZ2lxUk5YdzlJa3RxVVdac3lBdGRiQkxBOEErYXl2djNGT1dXa1g0c3ozSHJJdDZyMkNFZmd0eDZhUmE5TVFHU2RLc045RHlqMFFjUjJvbzVVOU9MNG9ORU9qdHY4K21wTjdtVlNVVU1KWTFNWlY0MUp6eDY3cnNZMHdNYVg5aGQ5TUZoM2doZnZEK1NvWkRkNjBiVVFlYmlyTzVqb21WVFlRRXlmWEVZSmtHZTVsc0tENkdBPT0tLVNNQmpEbzFNMGI1blVuKzVyR1Q3SFE9PQ%3D%3D--0947bcffa7fd9b9703f5522cb9afa8fac7d83254; GUCS=Ae_tyY5k; ywandp=10002066977754%3A1333922239; _cb_ls=1; _ga=GA1.2.770716812.1612631490; _gid=GA1.2.1018170693.1612631490; _gat=1; A1=d=AQABBL_NHmACEKNsUb5rZatSKxxQNn_WTEQFEgEBAQEfIGAoYAAAAAAA_SMAAA&S=AQAAAkKziEj7gaF2mOKKRDUJWqg; A1S=d=AQABBL_NHmACEKNsUb5rZatSKxxQNn_WTEQFEgEBAQEfIGAoYAAAAAAA_SMAAA&S=AQAAAkKziEj7gaF2mOKKRDUJWqg; A3=d=AQABBL_NHmACEKNsUb5rZatSKxxQNn_WTEQFEgEBAQEfIGAoYAAAAAAA_SMAAA&S=AQAAAkKziEj7gaF2mOKKRDUJWqg; GUC=AQEBAQFgIB9gKEId1wSk; _rivalry_session_v2=QW9Cckk0MGF3bnZtSFBLMFVhNlp3M281bFkzajM0dW9KWjh5clJEUWNNVytqU0w5dDg4OGJFY0NiMDNLZ0N1b3g2NEhWcVNvWm40QVdDNmlROGozMXJGazdMQmsrWUo0ZitnZkhHSFcweERuQkwxNUdQdmFESng0bTdPZlJPcTlhYy94QndBNlNWdERSMzRMVWtSbHFxT1l1VlB3eC9TbGN0OFJId2R0S1AwRE16R09GcDArbDVSUVFWcldxQzFoekJibkxNWjVQOVlRZS9vZVBkcTVpWkFqYUFIRkhLMVluaktxdllUNDRUWGw2SjdteDR3K2VHbmk0TytGWFZHdzVJaUNhanN3OFNlbDlxVGMxVEFNeXUrWFVORmt2N0ZkYnVBRUVEaEtJRTBjbHUzSUxiemRiZU8wN3hqZGJDcFA4Nnk3eDJHOGVRVzdmb2pRYUNNNDJnPT0tLXJ4c1ZUUjFXY0dqUHltWE1sSitjUHc9PQ%3D%3D--ab9e9cd73c80953acce2a86a86eb53ab3b0bfabf",
    "TE"="Trailers"
    ), body = body, encode = "json")
  
  result <- content(croot_req, "text")
  result <- fromJSON(result)$people
  return(result)
}

get_croot_info <- function(name, player_id, year) {
  result <- query_croots(name, year)
  result <- result %>%
    filter(
      as.numeric(prospect_id) == as.numeric(player_id)
    ) %>%
    head(1)
  return(result)
}

#Tweet new FCs

expanded_data <- data.frame()
player_slim_list <- projected_futurecasts %>%
  select(recruit, player_id, year) %>% group_by(player_id, year) %>% 
  summarise(recruit = first(recruit))
total <- nrow(player_slim_list)

if (total > 0) {
  loginfo(glue("Retrieving expanded info from Rivals API for {nrow(projected_futurecasts)} New FutureCasts"))
  for (row in 1:total) {
    player_id <- player_slim_list[row, "player_id"]
    name  <- trim(player_slim_list[row, "recruit"])
    year  <- player_slim_list[row, "year"]
    
    
    tryCatch(
      {
        loginfo(glue("Starting loading {row}/{total}: {name} (ID: {player_id}, Year: {year})"))
        result <- get_croot_info(name, player_id, year)
        expanded_data <- rbind(expanded_data, result)
      },
      error = function(cond) {
        logwarn(paste("Error: ", cond))
      },
      finally = {
        loginfo(glue("Done loading {row}/{total}: {name} (ID: {player_id}, Year: {year})"))
      }
    )
  }
  
  
  projected_futurecasts <- left_join(projected_futurecasts, expanded_data, by = c("player_id" = "prospect_id", "year" = "year"))
  loginfo(glue("Found and joined expanded info for {nrow(futurecasts)} FutureCasts"))
  
  loginfo(glue("Tweeting New FutureCasts"))
  
  for (row in 1:total) {
    link <- as.character(projected_futurecasts[row, "profile_url"])
    
    player_profile <- read_html(link) 
    
    player_hs <- player_profile %>% html_nodes("div.new-prospect-profile > 
                                               div.prospect-personal-information > 
                                               div.location-block > 
                                               div.right-personal-information > 
                                               a > .prospect-small-information > 
                                               .vital-line-location") %>%
      html_text()
    
    name  <- trim(projected_futurecasts[row, "recruit"])
    year  <- projected_futurecasts[row, "year"]
    pos <- projected_futurecasts[row, "position_abbreviation"]
    rank <- projected_futurecasts[row, "stars"]
    ht <- fixHeight(projected_futurecasts[row, "height"])
    wt <- projected_futurecasts[row, "weight"]
    predictor <- projected_futurecasts[row, "forecaster"]
    title <- projected_futurecasts[row, "title"]
    acc <- projected_futurecasts[row, "accuracy"]
    hs <- player_hs[2]
    hometown <- projected_futurecasts[row, "hometown"]
    
    if(rank == 0){
      text <-  glue(
        "
        \U0001F52E New #Sooners FutureCast
        
        {year} {pos} {name}
        {ht} / {wt}
        {hs} ({hometown})
        
        By: {title} {predictor} ({acc}%)
        
        {link}
        ")
    } else{
      text <-  glue(
        "
        \U0001F52E New #Sooners FutureCast
        
        {year} {rank}-Star {pos} {name}
        {ht} / {wt}
        {hs} ({hometown})
        
        By: {title} {predictor} ({acc}%)
        
        {link}
        ")
    }
    
    post_tweet(
      status = text,
      media = NULL,
      token = token
    )
    
    loginfo(glue("Tweet {row}/{total} posted"))
    }
  } else {
    loginfo(glue("Did not have any FutureCasts to find expanded info for, returning empty dataframe"))
    futurecasts <- data.frame()
  }

#Tweet changed FCs

expanded_data <- data.frame()
player_slim_list <- changed_futurecasts %>%
  select(recruit, player_id, year) %>% group_by(player_id, year) %>% 
  summarise(recruit = first(recruit))
total <- nrow(player_slim_list)

if (total > 0) {
  loginfo(glue("Retrieving expanded info from Rivals API for {nrow(changed_futurecasts)} Changed FutureCasts"))
  for (row in 1:total) {
    player_id <- player_slim_list[row, "player_id"]
    name  <- trim(player_slim_list[row, "recruit"])
    year  <- player_slim_list[row, "year"]
    
    
    tryCatch(
      {
        loginfo(glue("Starting loading {row}/{total}: {name} (ID: {player_id}, Year: {year})"))
        result <- get_croot_info(name, player_id, year)
        expanded_data <- rbind(expanded_data, result)
      },
      error = function(cond) {
        logwarn(paste("Error: ", cond))
      },
      finally = {
        loginfo(glue("Done loading {row}/{total}: {name} (ID: {player_id}, Year: {year})"))
      }
    )
  }
  
  
  changed_futurecasts <- left_join(changed_futurecasts, expanded_data, by = c("player_id" = "prospect_id", "year" = "year"))
  loginfo(glue("Found and joined expanded info for {nrow(futurecasts)} FutureCasts"))
  
  loginfo(glue("Tweeting New FutureCasts"))
  
  for (row in 1:total) {
    link <- as.character(changed_futurecasts[row, "profile_url"])
    
    player_profile <- read_html(link) 
    
    player_hs <- player_profile %>% html_nodes("div.new-prospect-profile > 
                                               div.prospect-personal-information > 
                                               div.location-block > 
                                               div.right-personal-information > 
                                               a > .prospect-small-information > 
                                               .vital-line-location") %>%
      html_text()
    
    name  <- trim(changed_futurecasts[row, "recruit"])
    year  <- changed_futurecasts[row, "year"]
    pos <- changed_futurecasts[row, "position_abbreviation"]
    rank <- changed_futurecasts[row, "stars"]
    ht <- fixHeight(changed_futurecasts[row, "height"])
    wt <- changed_futurecasts[row, "weight"]
    predictor <- changed_futurecasts[row, "forecaster"]
    title <- changed_futurecasts[row, "title"]
    acc <- changed_futurecasts[row, "accuracy"]
    hs <- player_hs[2]
    hometown <- changed_futurecasts[row, "hometown"]
    og_school <- changed_futurecasts[row, "original_school"]
    new_school <- changed_futurecasts[row, "forecasted_team"]
    
    if(rank == 0){
      text <-  glue(
        "
        \U0001f6A8 #Sooners Recruiting Alert
        
        {title} {predictor} ({acc}%) updates forecast for {year} {pos} {name} from {og_school} to {new_school}
        
        {ht} / {wt}
        {hs} ({hometown})
        {link}
        ")
    } else{
      text <-  glue(
        "
        \U0001f6A8 #Sooners Recruiting Alert
        
        {title} {predictor} ({acc}%) updates forecast for {year} {rank}-Star {pos} {name} from {og_school} to {new_school}
        
        {ht} / {wt}
        {hs} ({hometown})
        {link}
        ")
    }
    
    post_tweet(
      status = text,
      media = NULL,
      token = token
    )
    
    loginfo(glue("Tweet {row}/{total} posted"))
  }
} else {
  loginfo(glue("Did not have any FutureCasts to find expanded info for, returning empty dataframe"))
}


