library(tidyverse)
library(nflfastR)
library(gtExtras)
library(httr)
library(gt)
library(grid)
library(patchwork)
library(magick)
library(webshot2)
library(glue)
library(rtweet)
source("Functions.R")

active_players <- data.frame(player_name = c("M.Andrews","B.Bell","M.Brown","J.Hurts",
                                             "C.Lamb", "B.Mayfield", "J.Mixon", "K.Murray",
                                             "S.Perine", "S.Shepard", "R.Stevenson", "K.Stills",
                                             "D.Westbrook", "D.Williams"),
                             id = c("3116365","2514206","4241372","4040715",
                                    "4241389","3052587","3116385","3917315",
                                    "3116389","2976592","4569173","16016",
                                    "3892889","17359"))

heads <- data.frame()
for(i in 1:nrow(active_players)){
  a <- active_players[i,2]
  df <- get_athlete(a) %>% mutate(id = a)
  heads <- bind_rows(heads,df)
}

active_players <- left_join(active_players,heads,by="id")

active_qb <- c("J.Hurts","B.Mayfield","K.Murray")

active_rb <- c("J.Mixon","S.Perine","R.Stevenson","D.Williams")

active_wr <- c("M.Andrews", "M.Brown", "C.Lamb","S.Shepard","K.Stills","D.Westbrook")

logos <- teams_colors_logos %>% select("recent_team"=team_abbr,
                                       "logo"=team_logo_espn)

stats <- load_player_stats(2021) %>% filter(season==2021) %>% 
  filter(player_name %in% active_players$player_name, player_id!="00-0031806", week == max(week))

week <- unique(stats$week)

qb_stats <- stats %>% filter(player_name %in% active_qb) %>% 
  select(player_name, recent_team, completions, attempts, passing_yards, 
         passing_tds, interceptions) %>% mutate(avg = passing_yards/attempts) %>%
  mutate(attempts = glue("{completions}/{attempts}")) %>% 
  arrange(desc(passing_yards)) %>% 
  left_join(active_players,by="player_name") %>% 
  left_join(logos,by="recent_team") %>% dplyr::select(headshot_url, full_name, logo,
                                                      attempts,avg,passing_yards:interceptions) %>% 
  gt() %>% text_transform(locations = cells_body(columns = logo),
                          fn = function(x){web_image(x,height=120)}) %>% 
  text_transform(locations = cells_body(columns = headshot_url),
                 fn = function(x){web_image(x, height=170)}) %>% 
  tab_header(title=" Quarterbacks") %>% 
  tab_style(
    style = cell_text(size = px(100), font = "SFProDisplay-Regular",
                      align = "left"),
    locations = cells_title(group = "title")) %>% 
  tab_style(
    style = cell_text(size = px(60), font = "SFProDisplay-Regular"),
    locations = cells_body(everything())) %>%
  tab_style(
    style = cell_text(
      size = px(45),
      color = "#606066",
      font = "SFProDisplay-Regular",
      transform = "uppercase"),
    locations = cells_column_labels(everything())) %>%
  tab_style(
    style=cell_text(
      align = "center"),
    locations = cells_column_labels(everything())) %>% 
  tab_style(
    style=cell_text(
      align = "center"),
    locations = cells_body(everything())) %>% 
  tab_style(
    style=cell_text(
      align = "left"),
    locations = cells_body(columns=full_name)) %>% 
  fmt_number(columns = avg, decimals=1) %>%
  cols_label(
    headshot_url = "Player",
    full_name = "",
    logo = "Team",
    attempts = "Comp/Att",
    passing_yards = "Yds",
    passing_tds = "TD",
    interceptions = "INT",
    avg = "YPA") %>% 
  tab_options(
    column_labels.border.top.style = "none",
    table.border.top.style = "none",
    table_body.border.top.style = "none",
    data_row.padding = px(0),
    table.width = px(1700),
    column_labels.border.bottom.width = 2,
    footnotes.border.bottom.style = "none",
    table.border.bottom.style = "none",
    heading.border.bottom.style = "none",
    heading.padding = px(30),
    table_body.hlines.width = px(4),
    table_body.border.top.width = px(4),
    table_body.border.bottom.width = px(4))

rb_stats <- stats %>% filter(player_name %in% active_rb) %>% 
  select(player_name, recent_team, carries, rushing_yards, rushing_tds, 
         receptions, receiving_yards, receiving_tds) %>% 
  arrange(desc(rushing_yards)) %>% 
  left_join(active_players,by="player_name") %>% 
  left_join(logos,by="recent_team") %>% select(headshot_url, full_name, logo,
                                               carries, rushing_yards,
                                               rushing_tds:receiving_yards,receiving_tds) %>% 
  gt() %>% text_transform(locations = cells_body(columns = logo),
                          fn = function(x){web_image(x,height=120)}) %>% 
  text_transform(locations = cells_body(columns = headshot_url),
                 fn = function(x){web_image(x, height=170)}) %>% 
  tab_header(title=" Running Backs") %>% 
  tab_style(
    style = cell_text(size = px(100), font = "SFProDisplay-Regular",
                      align = "left"),
    locations = cells_title(group = "title")) %>% 
  tab_style(
    style = cell_text(size = px(60), font = "SFProDisplay-Regular"),
    locations = cells_body(everything())) %>%
  tab_style(
    style = cell_text(
      size = px(45),
      color = "#606066",
      font = "SFProDisplay-Regular",
      transform = "uppercase"),
    locations = cells_column_labels(everything())) %>%
  tab_style(
    style=cell_text(
      align = "center"),
    locations = cells_column_labels(everything())) %>% 
  tab_style(
    style=cell_text(
      align = "center"),
    locations = cells_body(everything())) %>% 
  tab_style(
    style=cell_text(
      align = "left"),
    locations = cells_body(columns=full_name)) %>% 
  cols_label(
    headshot_url = "Player",
    full_name = "",
    logo = "Team",
    carries = "Att",
    rushing_yards = "Yds",
    rushing_tds = "TD",
    receptions = "Rec",
    receiving_yards = "Yds",
    receiving_tds = "TD") %>% 
  tab_spanner(
    label = "Rushing",
    columns = c(carries, rushing_yards, rushing_tds)) %>% 
  tab_spanner(
    label = "Receiving",
    columns = c(receptions, receiving_yards, receiving_tds)) %>% 
  tab_style(
    style = cell_text(
      size = px(45),
      color = "#606066",
      font = "SFProDisplay-Regular",
      transform = "uppercase"),
    locations = cells_column_spanners(everything())) %>%
  tab_options(
    column_labels.border.top.style = "none",
    table.border.top.style = "none",
    table_body.border.top.style = "none",
    data_row.padding = px(0),
    column_labels.border.bottom.width = 2,
    footnotes.border.bottom.style = "none",
    table.width = px(1700),
    table.border.bottom.style = "none",
    heading.border.bottom.style = "none",
    heading.padding = px(0),
    column_labels.padding = px(3),
    table_body.hlines.width = px(4),
    table_body.border.top.width = px(4),
    table_body.border.bottom.width = px(4))

wr_stats <- stats %>% filter(player_name %in% active_wr) %>% 
  select(player_name, recent_team, targets, receptions, receiving_yards,
         receiving_yards_after_catch, receiving_tds) %>% 
  arrange(desc(receiving_yards)) %>% 
  left_join(active_players,by="player_name") %>% 
  left_join(logos,by="recent_team") %>% select(headshot_url, full_name, logo,
                                               targets:receiving_yards,
                                               receiving_yards_after_catch,
                                               receiving_tds) %>% 
  gt() %>% text_transform(locations = cells_body(columns = logo),
                          fn = function(x){web_image(x,height=120)}) %>% 
  text_transform(locations = cells_body(columns = headshot_url),
                 fn = function(x){web_image(x, height=170)}) %>% 
  tab_header(title=" Receivers") %>% 
  tab_source_note(
    source_note = html("Images: ESPN<br>Data: nflfastR, espnscrapeR")) %>% 
  tab_style(
    style = cell_text(size = px(100), font = "SFProDisplay-Regular",
                      align = "left"),
    locations = cells_title(group = "title")) %>% 
  tab_style(
    style = cell_text(size = px(60), font = "SFProDisplay-Regular"),
    locations = cells_body(everything())) %>%
  tab_style(
    style = cell_text(
      size = px(45),
      color = "#606066",
      font = "SFProDisplay-Regular",
      transform = "uppercase"),
    locations = cells_column_labels(everything())) %>%
  tab_style(
    style=cell_text(
      align = "center"),
    locations = cells_column_labels(everything())) %>% 
  tab_style(
    style=cell_text(
      size = px(40),
      font = "SFProDisplay-Regular",
      align = "right",
      weight="bold"),
    locations = cells_source_notes()) %>% 
  tab_style(
    style=cell_text(
      align = "center"),
    locations = cells_body(everything())) %>% 
  tab_style(
    style=cell_text(
      align = "left"),
    locations = cells_body(columns=full_name)) %>% 
  tab_options(
    column_labels.border.top.style = "none",
    table.border.top.style = "none",
    table_body.border.top.style = "none",
    data_row.padding = px(0),
    column_labels.border.bottom.width = 2,
    footnotes.border.bottom.style = "none",
    table.width = px(1700),
    table.border.bottom.style = "none",
    heading.border.bottom.style = "none",
    heading.padding = px(30),
    source_notes.padding = px(20),
    table_body.hlines.width = px(4),
    table_body.border.top.width = px(4),
    table_body.border.bottom.width = px(4)) %>%
  cols_label(
    headshot_url = "Player",
    full_name = "",
    logo = "Team",
    targets = "Tgt",
    receptions = "Rec",
    receiving_yards = "Yds",
    receiving_yards_after_catch = "Yac",
    receiving_tds= "Td") 

title <- data.frame(x=1) %>% gt() %>% 
  tab_header(title="Sooners in the NFL",
             subtitle=glue("Week {week}")) %>% 
  tab_style(
    style = cell_text(size = px(125), font = "SFProDisplay-Regular",
                      align = "center"),
    locations=cells_title(group="title")) %>% 
  tab_style(
    style = cell_text(size = px(100), font = "SFProDisplay-Light",
                      align = "center"),
    locations=cells_title(group="subtitle")) %>% 
  tab_style(
    style=cell_text(
      color = "white",
      align = "center"),
    locations = cells_body(everything())) %>% 
  tab_style(
    style=cell_text(
      color = "white",
      align = "center"),
    locations = cells_column_labels(everything())) %>% 
  tab_options(table.width = px(1700),
              column_labels.border.top.style = "none",
              table.border.top.color = "white",
              table.border.bottom.color = "white",
              table_body.border.top.color = "white",
              table_body.border.bottom.color = "white",
              row_group.border.top.color = "white",
              row_group.border.bottom.color = "white",
              table_body.vlines.color = "white",
              column_labels.border.top.color = "white",
              column_labels.border.bottom.color = "white",
              heading.border.bottom.color = "white")

gtsave(qb_stats, 
       filename = glue("~/Desktop/NFL Reports/Partials/2021QBwk{week}.html"))

webshot(glue("~/Desktop/NFL Reports/Partials/2021QBwk{week}.html"),
        glue("~/Desktop/NFL Reports/Partials/2021QBwk{week}.png"),vwidth=1750)

gtsave(rb_stats, 
       filename = glue("~/Desktop/NFL Reports/Partials/2021RBwk{week}.html"))

webshot(glue("~/Desktop/NFL Reports/Partials/2021RBwk{week}.html"),
        glue("~/Desktop/NFL Reports/Partials/2021RBwk{week}.png"),vwidth=1750)

gtsave(wr_stats, 
       filename = glue("~/Desktop/NFL Reports/Partials/2021WRwk{week}.html"))

webshot(glue("~/Desktop/NFL Reports/Partials/2021WRwk{week}.html"),
        glue("~/Desktop/NFL Reports/Partials/2021WRwk{week}.png"),vwidth=1750)

gtsave(title, 
       filename = glue("~/Desktop/NFL Reports/Partials/2021Titlewk{week}.html"))

webshot(glue("~/Desktop/NFL Reports/Partials/2021Titlewk{week}.html"),
        glue("~/Desktop/NFL Reports/Partials/2021Titlewk{week}.png"),vwidth=1750)

qb <- image_read(glue("~/Desktop/NFL Reports/Partials/2021QBwk{week}.png"))
rb <- image_read(glue("~/Desktop/NFL Reports/Partials/2021RBwk{week}.png"))
wr <- image_read(glue("~/Desktop/NFL Reports/Partials/2021WRwk{week}.png"))
title <- image_read(glue(glue("~/Desktop/NFL Reports/Partials/2021Titlewk{week}.png"))) %>% 
  image_crop("1750x270")

logo <- image_read("~/Desktop/Projects/SoonerReport/Repo/SoonerReport/Images/Logo.png")

table <- image_append(c(title,qb,rb,wr),stack=T) %>%
  image_composite(image_scale(logo,"320"),offset="+10")

image_write(table, glue("~/Desktop/NFL Reports/Finals/2021Wk{week}.png"))

text <-  glue(
  "
  #Sooners in the NFL Week {week} Recap")

post_tweet(status = text,
           media = glue("~/Desktop/NFL Reports/Finals/2021Wk{week}.png"),
           token = token)
