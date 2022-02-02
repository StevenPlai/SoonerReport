#Not In

`%notin%` <- Negate(`%in%`)

#Compute passer rating

add_pass_rating <- function(x) {
  a = ((x$completions/x$attempts)-.3)*5
  a <- if_else(a>2.375,2.375,a)
  a <- if_else(a<0,0,a)
  b = ((x$passing_yards/x$attempts)-3)*.25
  b <- if_else(b>2.375,2.375,b)
  b <- if_else(b<0,0,b)
  c = (x$passing_tds/x$attempts)*20
  c <- if_else(c>2.375,2.375,c)
  c <- if_else(c<0,0,c)
  d = 2.375-((x$interceptions/x$attempts)*25)
  d <- if_else(d>2.375,2.375,d)
  d <- if_else(d<0,0,d)
  r = ((a+b+c+d)/6)*100
  x <- x %>% mutate(rating = r)
  return(x)
}

#Get Headshot from ESPN (modified from Thomas Mock & espnscrapeR)

get_athlete <- function(athlete_id){
  
  season <- Sys.Date() %>% substr(1, 4)
  
  athlete_html <- read_html(glue("https://www.espn.com/nfl/player/_/id/{athlete_id}"))
  
  full_text <- athlete_html %>% 
    html_nodes('body') %>% 
    html_text() %>% 
    toString()
  
  links <- str_match_all(r,'espncdn":(.*?\\])')  
  
  images <- athlete_html %>% html_nodes("img") %>% 
    html_attr("src")
  
  headshot <- images[[4]]
  
  raw_get  <- base_url %>%
    glue::glue() %>%
    httr::GET()
  
  httr::stop_for_status(raw_get)
  
  raw_json <- content(raw_get)
  
  athlete_df <- raw_json %>%
    tibble::enframe() %>%
    dplyr::filter(name != "$ref") %>%
    tidyr::pivot_wider(names_from = name, values_from = value) %>%
    janitor::clean_names() %>%
    tidyr::hoist(position, pos = "abbreviation") %>%
    tidyr::hoist(headshot, headshot_url = "href") %>%
    tidyr::hoist(experience, nfl_exp = "years") %>%
    tidyr::hoist(team, team_id = list(1, "$ref")) %>%
    dplyr::select(
      full_name,
      headshot_url,
    ) %>%
    tidyr::unchop(tidyselect:::where(is.list))
  
  athlete_df
  
}

#Convert dataframe into Twitter Token

convert_token <- function(x) {
  app = x$app
  ck = x$consumer_key
  cs = x$consumer_secret
  at = x$access_token
  as = x$access_secret
  token <- create_token(app=app,ck,cs,access_token=at,access_secret=as,set_renv=F)
  return(token)
}

#Compose Tweet Body for New On3 Prediction

tweetOn3 <- function(x,titles,token){
  for(i in 1:nrow(x)){
    row <- x %>% slice(i) %>% left_join(offerlist,by=c("prospect"="name"))
    name <<- row$prospect
    predictor <- row$predictor
    conf <- row$confidence
    year <- row$year
    plink <- row$link
    
    page <- read_html(glue("https://on3.com/{plink}"))
    
    objects <- page %>% html_elements(".MeasurementInfo_item__LDnHm")
    info <- objects[[2]] %>% html_elements(".MuiTypography-root") 
    info <- info[[2]] %>% html_text() %>% strsplit(split="/") %>% unlist() %>% 
      as.character() %>% trimws()
    pos <- info[1]
    ht <- info[2]
    wt <- info[3]
    hs <- objects[[3]] %>% html_elements(".MuiTypography-root") 
    hs <- hs[[2]] %>% html_text() %>% trimws()
    htown <- objects[[4]] %>% html_elements(".MuiTypography-root") 
    htown <- htown[[2]] %>% html_text() %>% trimws()
    star <- page %>% html_elements(".StarRating_starWrapper__Ofuoa") %>% html_elements("a") %>% 
      html_elements("span")
    star <- star[[1]] %>% html_attr("aria-label") %>% substr(0,1)
    
    if(predictor %in% titles$name){
      title <- left_join(as.data.frame(predictor),titles,by=c("predictor"="name")) %>% select(title) %>% as.character()
      text <-  glue(
        "
          \U0001F52E New #Sooners On3 Prediction
          
          {year} {star}-Star {pos} {name}
          {ht} / {wt}
          {hs} ({htown})
          
          By: {title} {predictor}
          Confidence: {conf}%
          
          https://on3.com/{plink}
          ")
    } else{
      text <-  glue(
        "
          \U0001F52E New #Sooners On3 Prediction
          
          {year} {star}-Star {pos} {name}
          {ht} / {wt}
          {hs} ({htown})
          
          By: {predictor}
          Confidence: {conf}%
          
          https://on3.com/{plink}
          ")
    }
    #post_tweet(
      #status = text,
      #media = NULL,
      #token = token
    #)
    print(text)
  }}

##Find the current recruiting cycle based on system date

rcycle <- function() {
  date <- Sys.Date()
  year <- as.numeric(date %>% stri_sub(1,4))
  month <- as.numeric(date %>% stri_sub(6,7))
  mday <- as.numeric(date %>% stri_sub(9,10))
  wday <- lubridate::wday(date)
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
        

CFBWeek <- function() {
  date <- Sys.time()
  month <- as.numeric(date %>% stri_sub(6,7))
  mday <- as.numeric(date %>% stri_sub(9,10))
  hour <- as.numeric(date %>% stri_sub(12,13))
  if(month<9){0} else{
    if(month==9){
      if(mday<5){1} else{
        if(mday==5){
          if(hour>3){2}else{1}}else{
            if(mday<12){2} else{
              if(mday==12){
                if(hour>3){3}else{2}}else{
                  if(mday<19){3} else{
                    if(mday==19){
                      if(hour>3){4}else{3}}else{
                        if(mday<26){4} else{
                          if(mday==26){
                            if(hour>3){5}else{4}}else{5}}}}}}}}}else{
                              if(month==10){
                                if(mday<3){5} else{
                                  if(mday==3){
                                    if(hour>3){6}else{5}}else{
                                      if(mday<10){6} else{
                                        if(mday==10){
                                          if(hour>3){7}else{6}}else{
                                            if(mday<17){7} else{
                                              if(mday==17){
                                                if(hour>3){8}else{7}}else{
                                                  if(mday<24){8} else{
                                                    if(mday==24){
                                                      if(hour>3){9}else{8}}else{
                                                        if(mday<31){9}else{
                                                          if(mday==31){
                                                            if(hour>3){10}else{9}}else{10}}}}}}}}}}}else{
                                                              if(month==11){
                                                                if(mday<7){10}else{
                                                                  if(mday==7){
                                                                    if(hour>3){11}else{10}}else{
                                                                      if(mday<14){11}else{
                                                                        if(mday==14){
                                                                          if(hour>3){12}else{11}}else{
                                                                            if(mday<21){12}else{
                                                                              if(mday==21){
                                                                                if(hour>3){13}else{12}}else{
                                                                                  if(mday<28){13}else{
                                                                                    if(mday==28){
                                                                                      if(hour>3){14}else{13}}else{14}}}}}}}}}else{
                                                                                        if(month==12){
                                                                                          if(mday<5){14}else{
                                                                                            if(mday==5){
                                                                                              if(hour>3){15}else{14}}else{
                                                                                                if(mday<12){15}else{
                                                                                                  if(mday<19){16}else{
                                                                                                    if(mday==19){
                                                                                                      if(hour>3){17}else{17}}else{17}}}}}}}}}}}
