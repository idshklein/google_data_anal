library(sf)
library(fs)
library(tidyverse)
library(RODBC)
library(DBI)
library(assertr)
library(skimr)
library(lubridate)
library(mapview)
library(patchwork)
library(leafpop)
library(randomcoloR)
library(dbscan)
library(hms)
library(leaflet)
options(scipen = 10)
Sys.setlocale(locale = "hebrew")
# paths <- dir_info("../Google_data_2015_2020/Original_data/jumbomail/") %>% 
#   pull(path) %>% 
#   as.character() 
# dbnames <- paths %>% str_split("/") %>% map_chr(~.x[[length(.x)]]) %>% str_sub(end = -7) 
# df_db = data.frame(dbname = dbnames, path = paths) %>% 
#   filter(!str_detect(dbname,"\\."))
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
# beinironi <- beinironi %>% 
#   arrange(timestamp)
# 
# rm(united_googletimes)
# write_csv(beinironi,"beinironi.csv")
# rm(googletimes)
beinironi <- read_csv("beinironi.csv")
beinironi %>% 
  mutate(segmentid = ifelse(maslulid > 200, maslulid - 200,maslulid),
         direction = ifelse(maslulid > 200, 1,2)) %>% 
  select(segmentid,direction,timestamp,time,length) %>% 
  write_csv("travel_times.csv")

beinironi %>% 
  arrange(timestamp) %>% 
  ggplot(aes(x=timestamp)) + 
  geom_histogram()

# create segments.shp
direction_1 <- st_read("C:/idos_shit/google_data_anal/shp/dir1.shp")
# direction_2 <- st_read("C:/idos_shit/google_data_anal/shp/dir2.shp")
# shp <- bind_rows(direction_1,direction_2) %>% arrange(LINKID)
path <- "C:/Users/yehuda/Downloads/New folder/allprojects11.accdb"
con <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",path),DBMSencoding = "Windows-1255")
trips_all <- sqlQuery(con, "select * from trips_all")
meta <- trips_all %>% 
  filter(`פרוייקט` == "beinironi")

shp <- direction_1 %>% arrange(LINKID) %>% 
  left_join(meta,by = c("LINKID" = "maslulid" ))
st_bbox(shp)


marcos_manual_labor <- st_read("C:/idos_shit/google_data_anal/segments_cl/segments.shp",crs = 4326, options = "ENCODING=WINDOWS-1255") %>% 
  st_drop_geometry() %>% 
  select(SEGMNTD,FROM,TO,ROAD)
shp %>% select(segmentid = LINKID, 
               length = LENGTH, 
               origin = `מוצא`,
               destination = `יעד`) %>% 
  separate(origin,into = c("origin_y","origin_x"),sep = ",") %>% 
  separate(destination,into = c("destination_y","destination_x"),sep = ",") %>% 
  mutate(segmentid = ifelse(segmentid > 200, segmentid - 200,segmentid),
         origin_x = as.numeric(origin_x),
         origin_y = as.numeric(origin_y),
         destination_x = as.numeric(destination_x),
         destination_y = as.numeric(destination_y)) %>% 
  left_join(marcos_manual_labor,by = c("segmentid" = "SEGMNTD")) %>% 
  select(segmntd = segmentid,length,orign = FROM,dstntn = TO, road = ROAD,
         orign_x = origin_x,orign_y = origin_y,dstntn_x =destination_x,
         dstntn_y =destination_y) %>% 
  st_write("segments.shp",delete_dsn =T,layer_options = "ENCODING=UTF-8")
segments <- st_read("segments.shp")

# create calendar.csv - v0.1, canceled v0.2
# path <- "D:/OneDrive_2021_11_10/Google_data_2015_2020/Original_data/jumbomail/allprojects11.accdb"
# con <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",path),DBMSencoding = "windows-1255")
# waiting_1 <- sqlQuery(con, "select * from waiting_time") %>% 
#   select(date = day,sugyom) %>% 
#   distinct()
# path <- "D:/OneDrive_2021_11_10/Google_data_2015_2020/Original_data/jumbomail/new_OD_line.accdb"
# con <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",path),DBMSencoding = "windows-1255")
# waiting_2 <- sqlQuery(con, "select * from waiting_time") %>% 
#   select(date = day,sugyom) %>% 
#   distinct()
# calender <- bind_rows(waiting_1,waiting_2) %>% 
#   distinct() %>% 
#   # there are some duplicated days. I choose the higher sugyom, due to the fact that 1 is default. 
#   group_by(date) %>% 
#   filter(sugyom == max(sugyom)) %>% 
#   ungroup() %>% 
#   mutate(date = as_date(date))



check1 <- beinironi %>% 
  select(-sugyom,-project) %>% 
  rename(segmntd=maslulid) %>%
  mutate(directn = ifelse(segmntd > 200,2,1),
         segmntd = ifelse(segmntd > 200,segmntd-200,segmntd)) %>% 
  group_by(segmntd,directn, length) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(segmntd,directn) %>% 
  add_count() %>% 
  left_join(segments %>% st_drop_geometry() %>% select(segmntd,directn,orig_length =length,dscrptn),by = c("segmntd","directn")) 
cluses <- check1 %>% 
  # filter(masluld==1,directn==1) %>%
  # pull(length) %>%
  # matrix() %>%
  # dbscan(50,2) %>%
  # `$`(cluster)
  mutate(orig_length = orig_length*1000,
         ratio = length / orig_length,
         diff = length - orig_length,
         perc = n/sum(n),
         cluster = dbscan(matrix(length),0.01*unique(orig_length),2)$cluster,
         recluster = ifelse(cluster == 0, max(cluster) + row_number(),cluster) %>% as.factor() %>% as.numeric(),
         nclus = max(recluster)) #%>% 
  # filter(perc > 0.05,perc < 0.5, abs(diff) > 500) %>%
  # view()
plots1 <- beinironi %>% 
  select(-sugyom,-project) %>% 
  rename(segmntd=maslulid) %>% 
  mutate(directn = ifelse(segmntd > 200,2,1),
         segmntd = ifelse(segmntd > 200,segmntd-200,segmntd)) %>% 
  left_join(cluses, by = c("segmntd","directn","length")) %>% 
  mutate(dailytime = as_hms(timestamp),
         recluster = as.factor(recluster)) %>% 
  select(segmntd,directn,dscrptn, length,timestamp,dailytime,recluster,orig_length) %>% 
  group_by(segmntd,directn,dscrptn) %>% 
  nest() %>% 
  mutate(directn = paste0("d",directn)) %>% 
  pivot_wider(id_cols = segmntd,names_from = directn, values_from = c(dscrptn,data)) %>% 
  mutate(plot = pmap(list(dscrptn_d1,dscrptn_d2,data_d1,data_d2), function(x,y,z,w){
    p1 <- ggplot(z,aes(x = dailytime,y = length,color = recluster)) + 
      geom_point() + 
      geom_hline(mapping = aes(yintercept = orig_length)) +
      ggtitle(paste0(x,"1"))
    p2 <- ggplot(w,aes(x = dailytime,y = length,color = recluster)) + 
      geom_point() + 
      geom_hline(mapping = aes(yintercept = orig_length)) +
      ggtitle(paste0(y,"2"))
    p3 <- ggplot(z,aes(x = timestamp,y = length,color = recluster)) + 
      geom_point() + 
      geom_hline(mapping = aes(yintercept = orig_length)) +
      ggtitle(paste0(x,"1"))
    p4 <- ggplot(w,aes(x = timestamp,y = length,color = recluster)) + 
      geom_point() + 
      geom_hline(mapping = aes(yintercept = orig_length)) +
      ggtitle(paste0(y,"2"))
    (p1 + p2) / (p3 + p4)
  }))
plots1$plot[[1]]
map2(plots1$plot,plots1$segmntd,~ggsave(filename = paste0(.y,".png"),plot = .x,width = 20,height = 10))
popupOptions = popupOptions(maxWidth = 1000)
cluses %>% 
  summarise(metr = max(perc)) %>% 
  left_join(segments, by = c("segmntd","directn")) %>% 
  st_sf() %>% 
  select(segmntd,directn,metr) %>% 
  filter(directn == 2) %>% 
  # mapview(zcol = "metr",popup = popupGraph(plots1$plot))
  mapview(zcol = "metr",popup = paste0('<style> div.leaflet-popup-content {width:auto !important;}</style>',
                                       popupGraph(plots1$plot,type = "png",width = 1200,height = 620)))
# mapview(segments)

# segments %>% filter(masluld==36)


# daily 

# weekly
# yearly










# plots <- beinironi %>% 
#   mutate(houra = hour(timestamp),
#          weekday = wday(timestamp,label = T)) %>% 
#   filter(sugyom == 1,wday(timestamp) < 6) %>% 
#   group_by(maslulid,houra,weekday) %>% 
#   summarise(time = sum(time,na.rm = T),length = sum(length,na.rm = T),speed = 3.6*length/time) %>% 
#   ungroup() %>% 
#   group_by(maslulid) %>% 
#   nest() %>% 
#   mutate(ggpl = map(data, ~.x %>% ggplot(aes(houra,speed)) + geom_col() + facet_wrap(~weekday,nrow=1)))
# 
# 
# 
# direction_1 %>% 
#   st_transform(2039) %>% 
#   mutate(linkid2 = LINKID + 200) %>% 
#   as_tibble() %>% 
#   left_join(direction_2 %>% st_transform(2039) %>% as_tibble(), by = c("linkid2" = "LINKID")) %>% 
#   mutate(dist = map2_dbl(geometry.x,geometry.y,~st_difference(.x,.y) %>% st_length())) %>% 
#   select(LINKID,dist) %>% 
#   View()
# a <- direction_1 %>% 
#   filter(LINKID==4) %>% 
#   st_transform(2039)
# a %>% 
#   st_coordinates()
# b <- direction_2 %>% 
#   filter(LINKID==204) %>% 
#   st_transform(2039)
# b %>% 
#   st_coordinates()
# st_difference(a,b)
# end <- direction_1 %>% 
#   st_transform(2039) %>% 
#   mutate(linkid2 = LINKID + 200) %>% 
#   left_join(meta %>% select(maslulid,text = `מסלול`), by  = c("LINKID" = "maslulid") ) %>% 
#   left_join(meta %>% select(maslulid,text = `מסלול`), by  = c("linkid2" = "maslulid") ) %>% 
#   left_join(plots %>% select(-data), by  = c("LINKID" = "maslulid") ) %>% 
#   left_join(plots %>% select(-data), by  = c("linkid2" = "maslulid") ) %>% 
#   mutate(ggpl.x = map2(ggpl.x, text.x, ~.x + ggtitle(.y)),
#          ggpl.y = map2(ggpl.y, text.y, ~.x + ggtitle(.y)),
#          plot = map2(ggpl.x,ggpl.y,~.x/.y))
#   
# mapviewOptions()
# mapviewOptions(fgb = FALSE,vector.palette = randomColor(200))
# end %>% 
#   select(LINKID) %>% 
#   mapview(zcol = "LINKID",popup = popupGraph(end$plot))




