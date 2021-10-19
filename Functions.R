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
        



