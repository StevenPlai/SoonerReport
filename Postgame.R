library(gt)
library(gtExtras)
library(stringi)
library(tidyverse)
library(glue)
library(fontawesome)
library(cfbfastR)
library(extrafont)
library(toOrdinal)
library(magick)
library(webshot2)
source("Functions.R")

cweek <- CFBWeek()

pbp <- read.csv("Data/AllPBP.csv")
fg <- read.csv("Data/BrkicFG.csv")
fgGames <- read.csv("Data/BrkicGames.csv") %>% mutate(rk = n()-rank(PtsAdded,ties.method = "last")+1)
logos <-   logos <- cfbd_team_info(only_fbs=T,year=2021) %>% select(school,logos) %>%
  separate(col=logos,sep=",",into=c("A","B"))
logos$A <- logos$A %>% stri_sub(from=4,to=nchar(logos$A)-1)
logos$B <- logos$B %>% stri_sub(from=3,to=nchar(logos$B)-2)

seasonFG <- fgGames %>% filter(season==2021)
weekFG <- fg %>% filter(wk==cweek-1,season==2021) %>% mutate(RowNumber = row_number())
kEPA <- sum(weekFG$PtsAdded)
kRk <- fgGames %>% filter(wk==cweek-1,season==2021) %>% select(rk) %>% as.numeric() %>% toOrdinal()
opp <- seasonFG %>% filter(wk==cweek-1) %>% select(opponent) %>%  as.character()
logo <- logos %>% filter(school==opp) %>% select(A) %>% as.character()

table <- weekFG %>% select(-wk,-fg_kicker_player_name,-season,-def_pos_team) %>% gt() %>% 
  tab_header(title="Field Goal Attempts",
             subtitle=html(glue("<div <span style='vertical-align:middle';>Week {cweek-1} vs. </span>
                                <img src='{logo}' style=height:40px;vertical-align:middle;margin-bottom:5px></div>"))) %>% 
  text_transform(
    locations = cells_body(columns=play_type),
    fn = function(x){
        ifelse(x == "Field Goal Good",
               gt::html(fontawesome::fa("check", fill = "#35b03b", height = "1em")),
               gt::html(fontawesome::fa("times", fill = "#DA2A2A", height = "1em")))
      }) %>% 
  text_transform(
    locations = cells_body(columns=yds_fg),
    fn = function(x){
      glue("{x} Yds")
    }) %>% 
  fmt_percent(fg_make_prob, decimals=0) %>% 
  fmt_number(xPts, decimals=1) %>% 
  fmt_number(PtsAdded, decimals=1) %>% 
  text_transform(
    locations=cells_body(columns=PtsAdded),
    fn = function(x){
      PtsAdded <- x
      choose_color <- function(x){
      if_else(x==0,as.character(html(glue("<span style='color:#000000;font-face:bold;font-size:18px;'>{x}</span>"))),
              if_else(x>0,as.character(html(glue("<span style='color:#35b03b;font-face:bold;font-size:18px;'>+{x}</span>"))),
                      as.character(html(glue("<span style='color:#DA2A2A;font-face:bold;font-size:18px;'>{x}</span>")))))}
      map(PtsAdded,choose_color)}) %>% 
  cols_label(yds_fg="Distance",fg_make_prob="Probability",play_type="Result",
             xPts="Expected Points", Pts="Actual Points",PtsAdded="Points Added") %>% 
  tab_style(
    style = cell_fill(color = "grey", alpha = .05),
    locations = cells_body(rows = (RowNumber %% 2) == 0)) %>% 
  cols_hide(columns="RowNumber") %>% 
  tab_style(
    style=cell_text(
      align = "center"),
    locations = cells_body(columns=c(yds_fg,fg_make_prob,play_type,xPts,Pts,PtsAdded))) %>%
  tab_style(
    style=cell_text(
      align = "center"),
    locations = cells_column_labels(columns=c(yds_fg,fg_make_prob,play_type,xPts,Pts,PtsAdded))) %>% 
  tab_style(
    style = cell_text(size = px(30), font = "SFProDisplay-Regular", weight = "bold",
                      align = "center"),
    locations = cells_title(group = "title")) %>% 
  tab_style(
    style = cell_text(size = px(25), font = "SFProDisplay-Regular",
                      align = "center", v_align = "middle"),
    locations = cells_title(group = "subtitle")) %>% 
  tab_style(
    style = cell_text(size = px(18), font = "SFProDisplay-Regular"),
    locations = cells_body(everything())) %>%
  tab_style(
    style = cell_text(
      size = px(14),
      color = "#606066",
      font = "SFProDisplay-Regular",
      transform = "uppercase"),
    locations = cells_column_labels(everything())) %>%
  tab_options(
    column_labels.border.top.style = "none",
    table.border.top.style = "none",
    table_body.border.top.style = "none",
    data_row.padding = px(5),
    source_notes.padding = px(12),
    table.align = "center",
    column_labels.border.bottom.width = 3,
    column_labels.border.bottom.color = "black",
    footnotes.border.bottom.style = "none",
    table.border.bottom.width = 0,
    heading.border.bottom.style = "none") %>% 
  tab_source_note(
    source_note = html(glue("With <b>{round(kEPA,1)}</b> points added, this performance ranks <b>{kRk}</b> of <b>{nrow(fgGames)}</b> games in Gabe Brkic's career"))) %>% 
  tab_style(
    style = cell_text(size = px(20), font = "SFProDisplay-Regular", align = "center"),
    locations = cells_source_notes()) 

gtsave(table, 
       filename = glue("Postgame Reports/Week {cweek-1}/FGA.html"))

webshot2::webshot(glue("Postgame Reports/Week {cweek-1}/FGA.html"),
                  glue("Postgame Reports/Week {cweek-1}/FGA.png"))

p <- image_read(glue("Postgame Reports/Week {cweek-1}/FGA.png"))
i <- image_crop(p, "+100")
i <- image_crop(i, "-100")

image_write(i, path = glue("Postgame Reports/Week {cweek-1}/FGA.png"), format = "png")

plot <- ggplot(data = seasonFG, aes(x=wk,)

id <- info %>% select(game_id) %>% as.integer()

adv <- cfbd_game_box_advanced(id, long = FALSE, verbose = FALSE)

