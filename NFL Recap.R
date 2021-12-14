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
  add_pass_rating() %>% mutate(attempts = glue("{completions}/{attempts}")) %>% 
  arrange(desc(passing_yards)) %>% 
  left_join(active_players,by="player_name") %>% 
  left_join(logos,by="recent_team") %>% select(headshot_url, full_name, logo,
                                               attempts,avg,passing_yards:interceptions,
                                               rating) %>% 
  gt() %>% text_transform(locations = cells_body(columns = logo),
                          fn = function(x){web_image(x,height=120)}) %>% 
  text_transform(locations = cells_body(columns = headshot_url),
                 fn = function(x){web_image(x, height=170)}) %>% 
  tab_header(title=" Quarterbacks") %>% 
  tab_style(
    style = cell_text(size = px(100), font = "SFProDisplay-Regular", weight = "bold",
                      align = "left"),
    locations = cells_title(group = "title")) %>% 
  tab_style(
    style = cell_text(size = px(54), font = "SFProDisplay-Regular"),
    locations = cells_body(everything())) %>%
  tab_style(
    style = cell_text(
      size = px(42),
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
  fmt_number(columns = c(avg, rating), decimals=1) %>%
  cols_label(
    headshot_url = "Player",
    full_name = "",
    logo = "Team",
    attempts = "Comp/Att",
    passing_yards = "Yds",
    passing_tds = "TDs",
    interceptions = "INTs",
    avg = "YPA",
    rating = "Rating") %>% 
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
    heading.padding = px(30))

rb_stats <- stats %>% filter(player_name %in% active_rb) %>% 
  select(player_name, recent_team, carries, rushing_yards, rushing_tds, 
         receptions, receiving_yards, receiving_tds) %>% mutate(ypc = rushing_yards/carries,
                                                                ypr = receiving_yards/receptions) %>% 
  arrange(desc(rushing_yards)) %>% 
  left_join(active_players,by="player_name") %>% 
  left_join(logos,by="recent_team") %>% select(headshot_url, full_name, logo,
                                               carries, rushing_yards, ypc,
                                               rushing_tds:receiving_yards,ypr,receiving_tds) %>% 
  gt() %>% text_transform(locations = cells_body(columns = logo),
                          fn = function(x){web_image(x,height=120)}) %>% 
  text_transform(locations = cells_body(columns = headshot_url),
                 fn = function(x){web_image(x, height=170)}) %>% 
  tab_header(title=" Running Backs") %>% 
  tab_style(
    style = cell_text(size = px(100), font = "SFProDisplay-Regular", weight = "bold",
                      align = "left"),
    locations = cells_title(group = "title")) %>% 
  tab_style(
    style = cell_text(size = px(54), font = "SFProDisplay-Regular"),
    locations = cells_body(everything())) %>%
  tab_style(
    style = cell_text(
      size = px(42),
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
  fmt_number(columns = ypc, decimals=1) %>%
  cols_label(
    headshot_url = "Player",
    full_name = "",
    logo = "Team",
    carries = "Att",
    rushing_yards = "Yds",
    ypc = "Avg",
    rushing_tds = "TDs",
    receptions = "Rec",
    receiving_yards = "Yds",
    ypr = "Avg",
    receiving_tds = "TDs") %>% 
  tab_spanner(
    label = "Rushing",
    columns = c(carries, rushing_yards, ypc, rushing_tds)) %>% 
  tab_spanner(
    label = "Receiving",
    columns = c(receptions, receiving_yards, ypr, receiving_tds)) %>% 
  tab_style(
    style = cell_text(
      size = px(50),
      color = "#606066",
      font = "SFProDisplay-Regular"),
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
    heading.padding = px(0))

wr_stats <- stats %>% filter(player_name %in% active_wr) %>% 
  select(player_name, recent_team, targets, receptions, receiving_yards,
         receiving_yards_after_catch, receiving_tds) %>% 
  mutate(avg = receiving_yards/receptions) %>% 
  arrange(desc(receiving_yards)) %>% 
  left_join(active_players,by="player_name") %>% 
  left_join(logos,by="recent_team") %>% select(headshot_url, full_name, logo,
                                               targets:receiving_yards, avg,
                                               receiving_yards_after_catch,
                                               receiving_tds) %>% 
  gt() %>% text_transform(locations = cells_body(columns = logo),
                          fn = function(x){web_image(x,height=120)}) %>% 
  text_transform(locations = cells_body(columns = headshot_url),
                 fn = function(x){web_image(x, height=170)}) %>% 
  tab_header(title=" Receivers") %>% 
  tab_style(
    style = cell_text(size = px(100), font = "SFProDisplay-Regular", weight = "bold",
                      align = "left"),
    locations = cells_title(group = "title")) %>% 
  tab_style(
    style = cell_text(size = px(54), font = "SFProDisplay-Regular"),
    locations = cells_body(everything())) %>%
  tab_style(
    style = cell_text(
      size = px(42),
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
    heading.padding = px(30)) %>%
  cols_label(
    headshot_url = "Player",
    full_name = "",
    logo = "Team",
    targets = "Tgts",
    receptions = "Rec",
    receiving_yards = "Yds",
    receiving_yards_after_catch = "Yac",
    receiving_tds= "Tds") 

title <- data.frame(x=1) %>% gt() %>% 
  tab_header(title="                    Sooners in the NFL",
             subtitle=glue("   Week {week}")) %>% 
  tab_style(
    style = cell_text(size = px(125), font = "SFProDisplay-Regular", weight = "bold",
                      align = "center"),
    locations=cells_title(group="title")) %>% 
  tab_style(
    style = cell_text(size = px(100), font = "SFProDisplay-Regular",
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
       filename = glue("NFL Recaps/2021QBwk{week}.html"))

webshot(glue("NFL Recaps/2021QBwk{week}.html"),
        glue("NFL Recaps/2021QBwk{week}.png"),vwidth=1750)

gtsave(rb_stats, 
       filename = glue("NFL Recaps/2021RBwk{week}.html"))

webshot(glue("NFL Recaps/2021RBwk{week}.html"),
        glue("NFL Recaps/2021RBwk{week}.png"),vwidth=1750)

gtsave(wr_stats, 
       filename = glue("NFL Recaps/2021WRwk{week}.html"))

webshot(glue("NFL Recaps/2021WRwk{week}.html"),
                  glue("NFL Recaps/2021WRwk{week}.png"),vwidth=1750)

gtsave(title, 
       filename = glue("NFL Recaps/2021Titlewk{week}.html"))

webshot(glue("NFL Recaps/2021Titlewk{week}.html"),
        glue("NFL Recaps/2021Titlewk{week}.png"),vwidth=1750)

qb <- image_read(glue("NFL Recaps/2021QBwk{week}.png"))
rb <- image_read(glue("NFL Recaps/2021RBwk{week}.png"))
wr <- image_read(glue("NFL Recaps/2021WRwk{week}.png"))
title <- image_read(glue(glue("NFL Recaps/2021Titlewk{week}.png"))) %>% 
  image_crop("1750x270")

logo <- image_read("Images/Logo.png")

image_append(c(title,qb,rb,wr),stack=T) %>%
  image_composite(image_scale(logo,"320"),offset="+10")

