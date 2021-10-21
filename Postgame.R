library(gt)
library(gtExtras)
library(stringi)
library(tidyverse)
library(glue)
library(fontawesome)
library(cfbfastR)
library(grid)
library(extrafont)
library(ggimage)
library(ggthemes)
library(toOrdinal)
library(png)
library(egg)
library(magick)
library(webshot2)
library(ggtext)
source("Functions.R")

cweek <- CFBWeek()

pbp <- read.csv("Data/AllPBP.csv")
fg <- read.csv("Data/BrkicFG.csv")
fgGames <- read.csv("Data/BrkicGames.csv") %>% mutate(rk = n()-rank(PtsAdded,ties.method = "last")+1)
fgFBS <- read.csv("Data/FG2021.csv")
logos <-   logos <- cfbd_team_info(year=2021,only_fbs = F) %>% select(school,logos) %>%
  separate(col=logos,sep=",",into=c("A","B")) %>% select(-B)
logos$A <- logos$A %>% stri_sub(from=4,to=nchar(logos$A)-1)
SR <- image_read("Images/Logo.png")

seasonFG <- fgGames %>% filter(season==2021) %>% left_join(logos, by=c("opponent"="school"))
seasonFG[2,10] <- "Images/WCU.png"
weekFG <- fg %>% filter(wk==cweek-1,season==2021) %>% mutate(RowNumber = row_number())
kEPA <- sum(weekFG$PtsAdded)
kRk <- fgGames %>% filter(wk==cweek-1,season==2021) %>% select(rk) %>% as.numeric() %>% toOrdinal()
opp <- seasonFG %>% filter(wk==cweek-1) %>% select(opponent) %>%  as.character()
logo <- logos %>% filter(school==opp) %>% select(A) %>% as.character()
kEPA2 <- fgFBS %>% filter(last=="Brkic") %>% select(PtsAdded) %>% as.numeric()
kRk2 <- fgFBS %>% filter(last=="Brkic") %>% select(rk) %>% as.numeric() %>% toOrdinal()

table <- weekFG %>% select(-wk,-fg_kicker_player_name,-season,-def_pos_team,) %>% gt() %>% 
  tab_header(title="Field Goal Attempts",
             subtitle=html(glue("<div <span style='vertical-align:middle'>Week {cweek-1} vs.  </span>
                                <img src='{logo}' style=height:90px;vertical-align:middle;margin-bottom:10px></div>"))) %>% 
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
      if_else(x==0,as.character(html(glue("<span style='color:#000000;font-face:bold;font-size:45;'>{x}</span>"))),
              if_else(x>0,as.character(html(glue("<span style='color:#35b03b;font-face:bold;font-size:45;'>+{x}</span>"))),
                      as.character(html(glue("<span style='color:#DA2A2A;font-face:bold;font-size:45;'>{x}</span>")))))}
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
    style = cell_text(size = px(65), font = "SFProDisplay-Medium",
                      align = "center", v_align = "bottom"),
    locations = cells_title(group = "title")) %>% 
  tab_style(
    style = cell_text(size = px(55), font = "SFProDisplay-Regular",
                      align = "center", v_align = "middle"),
    locations = cells_title(group = "subtitle")) %>% 
  tab_style(
    style = cell_text(size = px(42), font = "SFProDisplay-Light"),
    locations = cells_body(everything())) %>%
  tab_style(
    style = cell_text(
      size = px(32),
      color = "#404040",
      font = "SFProDisplay-Regular",
      transform = "uppercase"),
    locations = cells_column_labels(everything())) %>%
  tab_options(
    column_labels.border.top.style = "none",
    heading.padding = px(5),
    table.border.top.style = "none",
    table_body.border.top.style = "none",
    data_row.padding = px(7.5),
    source_notes.padding = px(35),
    table.align = "center",
    table.width = px(1700),
    column_labels.border.bottom.width = 3,
    column_labels.border.bottom.color = "black",
    footnotes.border.bottom.style = "none",
    table.border.bottom.width = 0,
    table_body.border.bottom.width = 3,
    table_body.hlines.width = 1.5,
    heading.border.bottom.style = "none") %>% 
  tab_source_note(
    source_note = html(glue("With <span style=font-family:SFProDisplay-Semibold>{round(kEPA,1)}</span> points added, this performance ranks <span style=font-family:SFProDisplay-Semibold>{kRk}</span> of <span style=font-family:SFProDisplay-Semibold>{nrow(fgGames)}</span> games in Gabe Brkic's career"))) %>% 
  tab_style(
    style = cell_text(size = px(42), font = "SFProDisplay-Regular", align = "center"),
    locations = cells_source_notes()) 

gtsave(table, 
       filename = glue("Postgame Reports/Week {cweek-1}/FGA.html"),
       inline_css=T)

webshot2::webshot(glue("Postgame Reports/Week {cweek-1}/FGA.html"),
                  glue("Postgame Reports/Week {cweek-1}/FGA.png"),vwidth=1800)

i <- image_read(glue("Postgame Reports/Week {cweek-1}/FGA.png")) %>% 
  image_composite(image_scale(SR,"250"),offset="+25")

image_write(i, path = glue("Postgame Reports/Week {cweek-1}/FGA.png"), format = "png")

plot <- ggplot(data = seasonFG, aes(x=wk,y=PtsAdded)) +
  scale_y_continuous(limits = c(-6.5,5), labels=c("-5", "-2.5", "0", "2.5", "5"),
                     breaks=c(-5, -2.5, 0, 2.5, 5)) + 
  geom_hline(yintercept = 0, linetype="solid",size=1.5) + geom_image(aes(image=A),size=.09,y=-6,asp=7/5) +
  geom_line(size=3.5,color="#841617") +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(size = 24, hjust = .325, family = "SF Pro Display Medium", face = "plain", vjust = .7),
    plot.subtitle = element_text(size = 20, hjust = .37, family = "SF Pro Display Regular", face = "plain",lineheight = .2),
    plot.caption = element_markdown(size=16, family = "SF Pro Display Regular", face = "bold", hjust = -.6),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12, family = "SF Pro Display Regular", color = "black", angle=0,vjust=0.5610174),
    axis.text.y = element_text(size = 15, family = "SF Pro Display Light"),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(fill="white"),
    panel.background = element_rect(fill="white"),
    legend.position = "none") +
  labs(title = "Field Goal Performance",
       subtitle = "\nSeason To Date",
       y = "Points Added",
       caption=glue("With <span style=font-family:SFProDisplay-Semibold>{round(kEPA2,1)}</span> points added, Gabe Brkic ranks <span style=font-family:SFProDisplay-Semibold>{kRk2}</span> among <span style=font-family:SFProDisplay-Semibold>{nrow(fgFBS)}</span> FBS kickers")) +
  annotation_custom(rasterGrob(readPNG(seasonFG[2,10])), 
                    ymax=0, ymin=-2, xmin=seasonFG[2,2]-.25, xmax =seasonFG[2,2]-25)

ggsave(glue("Postgame Reports/Week {cweek-1}/FGP.png"), 
       plot = egg::set_panel_size(p=plot, width=unit(7, "in"), height=unit(5, "in")),
       height = 7, width = 11)

i <- image_read(glue("Postgame Reports/Week {cweek-1}/FGP.png")) %>% 
  image_crop("2675x1925+325+97") %>% 
  image_composite(image_scale(SR,"390"),offset="+25")

image_write(i, path = glue("Postgame Reports/Week {cweek-1}/FGP.png"), format = "png")



