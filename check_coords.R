library(sf)
library(lwgeom)
library(tidyverse)
Sys.setlocale(locale = "hebrew")
shp <- st_read("D:/Downloads/segments_cl (1)/segments.shp",crs = 4326)
shp1 <- st_read("segments.shp")
shp %>% 
  mutate(start = st_startpoint(geometry),
         end = st_endpoint(geometry),
         start_y = map_dbl(start,~st_coordinates(.x)[1]),
         start_x = map_dbl(start,~st_coordinates(.x)[2]),
         end_y = map_dbl(end,~st_coordinates(.x)[1]),
         end_x = map_dbl(end,~st_coordinates(.x)[2]),
         tst1 = start_x - ORIGN_X,
         tst2 = start_y - ORIGN_Y,
         tst3 = end_x - DSTNTN_X,
         tst4 = end_y - DSTNTN_Y) %>% 
  st_drop_geometry() %>% 
  select(SEGMNTD,starts_with("tst")) %>% 
  gather(tst,delta,-SEGMNTD) %>% 
  View()

shp[36,] %>% mapview::mapView()
shp[36,"ORIGN_X"]
shp1[36,"orign_x"]
shp1 %>% 
  mutate(start = st_startpoint(geometry),
         end = st_endpoint(geometry),
         start_x = map_dbl(start,~st_coordinates(.x)[1]),
         start_y = map_dbl(start,~st_coordinates(.x)[2]),
         end_x = map_dbl(end,~st_coordinates(.x)[1]),
         end_y = map_dbl(end,~st_coordinates(.x)[2]),
         tst1 = start_x - orign_x,
         tst2 = start_y - orign_y,
         tst3 = end_x - dstntn_x,
         tst4 = end_y - dstntn_y) %>% 
  st_drop_geometry() %>% 
  select(segmntd ,starts_with("tst")) %>% 
  gather(tst,delta,-segmntd) %>% 
  View()
shp1
# from node 36 is faulty, so is 87,so is 100,
# to node 35 is faulty,so is 84, 
# could be because it is taken from allprojects11.accdb which is old
