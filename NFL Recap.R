library(tidyverse)
library(nflfastR)

active_players <- c("M.Andrews","B.Bell","M.Brown","J.Hurts",
                    "C.Lamb", "B.Mayfield", "J.Mixon", "K.Murray",
                    "S.Perine", "S.Shepard", "R.Stevenson", "K.Stills",
                    "D.Westbrook", "D.Williams")

active_qb <- c("J.Hurts","B.Mayfield","K.Murray")

active_rb <- c("J.Mixon","S.Perine","R.Stevenson","D.Williams")

stats <- load_player_stats(2021) %>% 
  filter(player_name %in% active_players, player_id!="00-0031806", week == max(week))

qb_stats <- stats %>% filter(player_name %in% active_qb) %>% 
  select(player_name, recent_team, completions, attempts, passing_yards, 
         passing_tds, interceptions) %>% mutate(avg = passing_yards/attempts) %>% 
  add_pass_rating()

rb_stats <- stats %>% filter(player_name %in% active_rb)


