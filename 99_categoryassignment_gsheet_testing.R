library(tidyverse)
library(lubridate)
library(janitor)
library(glue)
library(googlesheets)
library(kableExtra)
library(readxl)
library(writexl)

# to authenticate the first time:
# gs_ls()

# import data from Google Sheet 

#register DW's 2020 google sheet using stored sheet id key 
# mykey <- Sys.getenv("DW2020_KEY")
# dw2020 <- gs_key(mykey)

mykey <- Sys.getenv("TRUMPCLAIMS_KEY")
trumpsheet <- gs_key(mykey)

#see worksheets
gs_ws_ls(trumpsheet) 

#read in all the data in the trump false claims tab
falseclaims <- trumpsheet %>% 
  gs_read(ws = "falseclaims") 

glimpse(falseclaims)

#clean column names
fcheck <- falseclaims %>%
  clean_names()

names(fcheck)

# filter out missing dates 
fcheck <- fcheck %>% 
  filter(!is.na(date))

#set date to date format
fcheck$date <- mdy(fcheck$date) 
year(fcheck$date)

#set formatting of certain columns and break out dates into own columns
fcheck <- fcheck %>%
  mutate(
    kind_of_forum = str_trim(str_to_upper(kind_of_forum)),
    location = str_trim(str_to_upper(location)),
    state = str_trim(str_to_upper(state)),
    code_name = str_trim(str_to_upper(code_name)),
    category_tags_combined = str_trim(str_to_upper(category_tags_combined)),
    year = year(date),
    month = month(date),
    day = day(date),
    day_of_week = weekdays(date, abbreviate = T),
    isoweek = isoweek(date) #isoweek starts on MONDAYS
  )



aaa <- fcheck %>% 
  select(category_tags_combined) %>% 
  separate_rows(category_tags_combined, sep = ";") %>% 
  mutate(
    category_tags_combined = str_trim(category_tags_combined)
  )






### TESTING WITH SAMPLE SHEET #### ----
trumpsheet %>% 
  gs_ws_new(ws_title = "mtcars_new", input = head(mtcars),
          trim = TRUE, verbose = FALSE)

#re-register?
trumpsheet <- gs_key(mykey)
#see worksheets
gs_ws_ls(trumpsheet) 

cars <- trumpsheet %>% 
  gs_read(ws = "mtcars_new")


##google sheets index/match formula combo
# https://www.youtube.com/watch?v=vOwUXD0N4-o





# remove NAs from category columns 
fcheck$category_tag1 <- fcheck$category_tag1 %>% replace_na("")
fcheck$category_tag2 <- fcheck$category_tag2 %>% replace_na("")
fcheck$category_tag3 <- fcheck$category_tag3 %>% replace_na("")
fcheck$category_tag4 <- fcheck$category_tag4 %>% replace_na("")
fcheck$category_tag5 <- fcheck$category_tag5 %>% replace_na("")
fcheck$code_name <- fcheck$code_name %>% replace_na("")

#create combined column with all category tags together in one
fcheck$category_combined <- paste0(fcheck$category_tag1, " ", fcheck$category_tag2, " ", fcheck$category_tag3, " ", fcheck$category_tag4, " ", fcheck$category_tag5)

# remove NAs from link column 
fcheck$source_article <- fcheck$source_article %>% replace_na("")

#convert link to html hyperlink format
fcheck$source_article <- paste0("<a href='", fcheck$source_article, "' target='_blank'>", fcheck$source_article, "</a>")

#clean up empty hyperlinks
fcheck$source_article <- str_remove(fcheck$source_article, "<a href='' target='_blank'></a>")




#save copy in case needed
write_xlsx(fcheck, "saved_versions/fcheck_temp.xlsx")
