library(sf)
library(fs)
library(tidyverse)
library(RODBC)
library(DBI)
library(assertr)
library(skimr)
library(lubridate)
options(scipen = 10)
Sys.setlocale(locale = "hebrew")
paths <- fs::dir_info("D:/OneDrive_2021_11_10/Google_data_2015_2020/Original_data/access_files2/") %>% 
  pull(path) %>% 
  as.character() 
dbnames <- paths %>% str_split("/") %>% map_chr(~.x[[length(.x)]]) %>% str_sub(end = -7) 
df_db = data.frame(dbname = dbnames, path = paths) %>% 
  filter(!str_detect(dbname,"\\."))
# df_db %>% 
  # left_join(fs::dir_info("D:/OneDrive_2021_11_10/Google_data_2015_2020/Original_data/access_files2/"), by = "path") %>% 
  # select(dbname,size) %>% 
  # write_csv("jordan.csv")
cons <- map(df_db$path,~odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",.x)))

# creation of googletimes files - ran only once, computer has only 16 giga ram

# googletimes <- map(cons,~sqlQuery(.x, "select * from googletimes"))
# googletimes_table1 <- map2(googletimes,dbnames,~.x %>% mutate(dbname = .y)) 
# googletimes_table <- googletimes_table1[googletimes_table1 %>% map(~nrow(.x)!=0) %>% as.logical()] %>% 
# bind_rows()
# nr <- nrow(googletimes_table)
# map(0:61,~googletimes_table[(1 + .x*1000000):min(1000000 + .x*1000000,nr),] %>% 
# write_csv(paste0("google_times",.x,".csv")))

# unifying of googletimes files - ran only once
# googletimes_table <- googletimes %>% bind_rows()
# lst <- dir_info() %>% 
#   filter(str_detect(path,".csv")) %>% 
#   pull(path) %>% 
#   as.character() %>% 
#   map(read_csv)
# googletimes_table <- lst %>% 
#   bind_rows() 
# rm(lst)
# gc()
# write_csv(googletimes_table,"googletimes.csv")
# almost 5 giga, much less than before (around 12)
# saved google data
googletimes<- read_csv("D:/OneDrive_2021_11_10/google_data_anal/googletimes.csv")
# add relevant columns
googletimes$date <- as_date(googletimes$timestamp)
googletimes$from_epoch <- as.numeric(googletimes$timestamp)
# change relevant columns
googletimes$project <-  as.factor(googletimes$project)
googletimes$dbname <-  as.factor(googletimes$dbname)
head(googletimes)
gc()
# which project has most calls? 232 projects
largest_proj <- googletimes %>% 
  count(project) %>% 
  arrange(-n)
# beinironi is the largest. 9 projects with more than a million records, na for almost 4 million records
largest_proj %>% 
  filter(n>1000000) %>% 
  mutate(project = fct_inorder(as.character(project))) %>% 
  ggplot(aes(x= project,y= n)) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90))
# largest projects distribution over time.
# we have a huge hole between april 2018 to may 2019. this might have to do with the corrupted file
googletimes %>% 
  filter(project %in% largest_proj$project[1:10]) %>% 
  count(project,date) %>% 
  ggplot(aes(x=date,y=n)) + 
  geom_col() + 
  facet_grid(project~.)
# unique identifiers: 
# ID is not unique. in each access file, there was a different counter. some of the dbs are suplicated
googletimes %>% 
  filter(project == "beinironi",ID == 1)
# there are a lot of beinironi project rows in different dbs
googletimes %>% 
  filter(project == "beinironi") %>% 
  count(dbname) %>% 
  arrange(-n)
# trying to get distinct values by everything else that id and dbname - got 44 milion. meaning - at least 17 million are duplicated. 
gc()
distincts <- googletimes %>% 
  select(-ID,-dbname) %>%
  distinct()
# reevalute which project has most calls - only 6 named projects with over a million, 3 million querys have no project.
largest_proj2 <- distincts %>% 
  count(project) %>% 
  arrange(-n)
largest_proj2%>% 
  filter(n>1000000) %>% 
  mutate(project = fct_inorder(as.character(project))) %>% 
  ggplot(aes(x= project,y= n)) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90))
# 10 largest projects - what's going on? looks like we dodged the duplicated values problem. 
distincts %>% 
  filter(project %in% largest_proj$project[1:10]) %>% 
  count(project,date) %>% 
  ggplot(aes(x=date,y=n)) + 
  geom_col() + 
  facet_grid(project~.)
# end result of this stage - got the data to to be not duplicated. problem - huge missing period. 

# table status - what's going on in other tables?

table_status <- map(cons,~sqlTables(.x, tableType = "TABLE")) %>% 
  bind_rows() %>% 
  left_join(df_db, by = c("TABLE_CAT" = "path")) %>% 
  select(dbname,TABLE_NAME) %>% 
  mutate(num = 1) %>% 
  spread(TABLE_NAME,num)
# get all calemders, see whether there are problems
calendar <- map(cons,~sqlQuery(.x, "select * from calendar"))
has_calendar <- map(calendar,~class(.x) =="data.frame") %>% unlist()
calendar_table1 <- map2(calendar[has_calendar],df_db$dbname[has_calendar],function(x,y){x %>% mutate(dbname = y)})
calendar_table <- calendar_table1[calendar_table1 %>% map(~nrow(.x)!=0) %>% as.logical()] %>%
  bind_rows()
gc()
# got a distinct calender. only for 2016-2017
dis_calender <- calendar_table %>% 
  select(-dbname) %>% 
  distinct()
# get all newroutes see whether there are problems
newroutes <- map(cons,~sqlQuery(.x, "select * from newroutes"))
has_newroutes <- map(newroutes,~class(.x) =="data.frame") %>% unlist()
newroutes_table1 <- map2(newroutes[has_newroutes],df_db$dbname[has_newroutes],function(x,y){x %>% mutate(dbname = y)})
# map(newroutes_table1,~.x$מסלול %>% class())
newroutes_table1[[26]]$מסלול <- as.character(newroutes_table1[[26]]$מסלול)
newroutes_table <- newroutes_table1[newroutes_table1 %>% map(~nrow(.x)!=0) %>% as.logical()] %>%
  bind_rows()
# got distinct newroutes, however they are lacking, most likely to be found in "maslulim"
dis_newroutes <- newroutes_table %>% 
  select(-dbname) %>% 
  distinct()

# testrips - like googletimes, however meant to test the db
testrips  <- map(cons,~sqlQuery(.x, "select * from testrips"))
has_testrips<- map(testrips,~class(.x) =="data.frame") %>% unlist()
testrips_table1 <- map2(testrips[has_testrips],df_db$dbname[has_testrips],function(x,y){x %>% mutate(dbname = y)})

testrips_table <- testrips_table1[testrips_table1 %>% map(~nrow(.x)!=0) %>% as.logical()] %>%
  bind_rows()
# got distinct testrips, don't know what to do with them
dis_testrips  <- testrips_table %>% 
  select(-dbname) %>% 
  distinct()

# trips - like newroutes
trips  <- map(cons,~sqlQuery(.x, "select * from trips"))
has_trips <- map(trips,~class(.x) =="data.frame") %>% unlist()
trips_table1 <- map2(trips[has_trips],df_db$dbname[has_trips],function(x,y){x %>% mutate(dbname = y)})
# map(trips_table1,~.x$מסלול %>% class())
trips_table1[[22]]$מסלול <- as.character(trips_table1[[22]]$מסלול)
trips_table <- trips_table1[trips_table1 %>% map(~nrow(.x)!=0) %>% as.logical()] %>%
  bind_rows()
# got distinct trips, they are like newroutes. however, querying a matrix is differnt than a simple api call, need to split
dis_trips <- trips_table %>% 
  select(-dbname) %>% 
  distinct()
not_all_na <- function(x) any(!is.na(x))
matrices <- dis_trips %>% 
  filter(!is.na(project)) %>% 
  select_if(not_all_na) 
dis_trips_not_matrices <- dis_trips %>% 
  filter(is.na(project)) %>% 
  select_if(not_all_na) 


# trips_all - like newroutes
trips_all <- map(cons,~sqlQuery(.x, "select * from trips_all"))
has_trips_all <- map(trips_all,~class(.x) =="data.frame") %>% unlist()
trips_all_table1 <- map2(trips_all[has_trips_all],df_db$dbname[has_trips_all],function(x,y){x %>% mutate(dbname = y)})

map(trips_all_table1,~.x$מסלול %>% class())
trips_all_table1[[26]]$מסלול <- as.character(trips_all_table1[[26]]$מסלול)
trips_all_table <- trips_all_table1[trips_all_table1 %>% map(~nrow(.x)!=0) %>% as.logical()] %>%
  bind_rows()
# got distinct trips, they are like newroutes. however, querying a matrix is differnt than a simple api call, need to split
dis_trips_all <- trips_all_table %>% 
  select(-dbname) %>% 
  distinct()


# explore trips1, wait and waiting times
trips1 <- map(cons,~sqlQuery(.x, "select * from trips1"))
has_trips1 <- map(trips1,~class(.x) =="data.frame") %>% unlist()
trips1_table1 <- map2(trips1[has_trips1],df_db$dbname[has_trips1],function(x,y){x %>% mutate(dbname = y)})
trips1_table <- trips1_table1[trips1_table1 %>% map(~nrow(.x)!=0) %>% as.logical()] %>%
  bind_rows()
dis_trips1 <- trips1_table %>% 
  select(-dbname) %>% 
  distinct()
# have only 110 projects, out of 232 with data. concerning
bind_rows(dis_trips_not_matrices,dis_newroutes,dis_trips_all,dis_trips1) %>% 
  select_if(not_all_na) %>% 
  select(`פרוייקט`,maslulid) %>% 
  distinct() %>% 
  count(`פרוייקט`)
meta_data_proj <- bind_rows(dis_trips_not_matrices,dis_newroutes,dis_trips_all,dis_trips1) %>% 
  select_if(not_all_na) %>% 
  select(`פרוייקט`,maslulid) %>% 
  distinct() %>% 
  count(`פרוייקט`) %>% 
  arrange(-n)

meta_data_proj
largest_proj2 %>% 
  filter(project %in% meta_data_proj$פרוייקט)
odbcCloseAll()
# rm(cons)

cnts1 <- trips_table %>% 
  filter(is.na(project)) %>% 
  select_if(not_all_na) %>% 
  select(project = `פרוייקט`,maslulid,dbname) %>% 
  distinct()
cnts <- googletimes %>% 
  distinct(project,maslulid,dbname) 

cnts %>% 
  mutate(n = 1) %>% 
  left_join(cnts1 %>% mutate(n=2),by = c("project","maslulid","dbname")) %>% 
  filter(project != "beinironi",
         is.na(n.y),
         dbname != "allnew_lines",
         dbname != "allnew_lines_Backup",
         dbname != "allprojects",
         dbname != "allprojects_Backup",
         dbname != "allprojects1",
         dbname != "allprojects1_Backup",
         dbname != "all_lines_OD_Backup",)
