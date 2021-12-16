library(sf)
library(fs)
library(tidyverse)
library(RODBC)
library(DBI)
library(assertr)
library(skimr)
library(lubridate)
library(mapview)
options(scipen = 10)
Sys.setlocale(locale = "hebrew")
paths <- dir_info("../Google_data_2015_2020/Original_data/jumbomail/") %>% 
  pull(path) %>% 
  as.character() 
dbnames <- paths %>% str_split("/") %>% map_chr(~.x[[length(.x)]]) %>% str_sub(end = -7) 
df_db = data.frame(dbname = dbnames, path = paths) %>% 
  filter(!str_detect(dbname,"\\."))
# googletimes <- map(df_db$dbname,function(x){
#   odbcCloseAll()
#   gc()
#   path <- df_db[df_db$dbname == x,"path"]
#   con <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",path))
#   googletime <- sqlQuery(con, "select * from googletimes")
# })
# united_googletimes <- googletimes %>% 
#   bind_rows()
# gc()
# beinironi <- united_googletimes %>% 
#   filter(project == "beinironi") %>% 
#   select(-ID) %>% 
#   distinct() 
# rm(united_googletimes)
# write_csv(beinironi,"beinironi.csv")
# rm(googletimes)
beinironi <- read_csv("beinironi.csv")
beinironi %>% 
  arrange(timestamp) %>% 
  ggplot(aes(x=maslulid)) + 
  geom_histogram()
direction_1 <- st_read("D:/OneDrive_2021_11_10/Google_data_2015_2020/Original_data/gis files/shp/בין_עירוני-_כיוון_1.shp")
direction_2 <- st_read("D:/OneDrive_2021_11_10/Google_data_2015_2020/Original_data/gis files/shp/בין_עירוני-_כיוון_2.shp")
shp <- bind_rows(direction_1,direction_2) %>% arrange(LINKID)
path <- "D:/OneDrive_2021_11_10/Google_data_2015_2020/Original_data/jumbomail/allprojects11.accdb"
con <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",path),DBMSencoding = "windows-1255")
trips_all <- sqlQuery(con, "select * from trips_all")
meta <- trips_all %>% 
  filter(`פרוייקט` == "beinironi")
shp %>% 
  left_join(meta,by = c("LINKID" = "maslulid")) %>% 
  mapview(zcol = "LINKID")
plots <- beinironi %>% 
  mutate(houra = hour(timestamp),
         weekday = wday(timestamp)) %>% 
  filter(sugyom == 1) %>% 
  group_by(maslulid,houra,weekday) %>% 
  summarise(time = sum(time,na.rm = T),length = sum(length,na.rm = T),speed = 3.6*length/time) %>% 
  ungroup() %>% 
  group_by(maslulid) %>% 
  nest() %>% 
  mutate(ggpl = map(data, ~.x %>% ggplot(aes(houra,speed)) + geom_col() + facet_wrap(~weekday)))
plots$ggpl[[201]]

