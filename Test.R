page <- read_html("https://247sports.com/Season/2022-Football/CompositeRecruitRankings/?InstitutionGroup=HighSchool&Position=QB") 

list <- data.frame()

df <- data.frame(names = page %>% 
                   html_elements(".rankings-page__list-item") %>% 
                   html_elements(".rankings-page__name-link") %>% html_text(),
                 ids = page %>% 
                   html_elements(".rankings-page__list-item") %>% 
                   html_elements(".rankings-page__name-link") %>% html_attr("href") %>% 
                   str_extract_all("([0-9]+).*$") %>% as.character())



test2 <- test[[1]]
