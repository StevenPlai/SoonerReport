library(rtweet)
library(tidyverse)

token <- create_token(
  app = "SoonerBot",
  "rv9GhbZ7h78cBJqToYNZbxZTP",
  "Tv604bevC3uXaobOq7bihCkA21PjUOkvB5ObhxYDYgKVtFWzVk",
  access_token = "1105957203020861440-Y9UMc33oNBp2fREzJYGzD42XEixDA4",
  access_secret = "iBVBB8Lvq4qfXItouUGi6oCrL1hhWD17gl0v3Sacl2K1B",
  set_renv = TRUE
)

last_tweet <- get_timeline("SoonerReport",n = 1, token = token) %>% select(status_id)

post_tweet(
  status = "TEST2",
  media = NULL,
  token = token,
  in_reply_to_status_id = last_tweet
)
