library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)
library(glue)



raw_data <- read_excel("TRUMP FACT-CHECKS.xlsx", 
                       sheet = "ak", col_types = c("date", "text", 
                                                   "text", "text", "text", "text", "text", 
                                                   "text", "text", "text", "text", "text", 
                                                   "text", "text"))

glimpse(raw_data)

#clean column names
fcheck <- raw_data %>%
  clean_names()

names(fcheck)

#set formatting of certain columns and break out dates into own columns
fcheck <- fcheck %>%
  mutate(
    kind_of_forum = str_trim(str_to_upper(kind_of_forum)),
    location = str_trim(str_to_upper(location)),
    state = str_trim(str_to_upper(state)),
    code_name = str_trim(str_to_upper(code_name)),
    category_tag1 = str_trim(str_to_upper(category_tag1)),
    category_tag2 = str_trim(str_to_upper(category_tag2)),
    category_tag3 = str_trim(str_to_upper(category_tag3)),
    category_tag4 = str_trim(str_to_upper(category_tag4)),
    category_tag5 = str_trim(str_to_upper(category_tag5)),
    year = year(date),
    month = month(date),
    day = day(date),
    day_of_week = weekdays(date, abbreviate = T),
    isoweek = isoweek(date) #isoweek starts on MONDAYS
  )


# any dates missing?
fcheck %>% 
  filter(is.na(date))


# initial count by week
fcheck %>% 
  count(isoweek)

# order by week for checking manually
fcheck %>% 
  select(
    date,
    day_of_week,
    isoweek,
    everything()
  ) %>% 
  arrange(date) %>% 
  writexl::write_xlsx("output/datecheck_file.xlsx")






#create table of JUST THE MOST RECENT MON-SUN WEEK's checks #### ------------------
dates <- today()

weekdays(dates)

wdays <- setNames(0:6, c("Monday", "Tuesday", "Wednesday",
                         "Thursday", "Friday", "Saturday", "Sunday"))

weekdays(dates - wdays[weekdays(dates)])

#find previous SUN date
most_recent_sunday <- dates - match(weekdays(dates), c("Monday", "Tuesday", "Wednesday", 
                                                       "Thursday", "Friday", "Saturday", "Sunday"))

#calculate Monday before that Sunday
monday_before_recent_sunday <- most_recent_sunday - 6

#now let's try to use this to filter our dates from the table
fcheck_PREVWEEK <- fcheck %>%
  filter(date <= most_recent_sunday,
         date >= monday_before_recent_sunday) 

#formatted dates for text output string
monday_formatted <- format(monday_before_recent_sunday, "%a %b %d")
sunday_formatted <- format(most_recent_sunday, "%a %b %d")

week_range_string <- glue("week of {monday_formatted} to {sunday_formatted}")


