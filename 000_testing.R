library(tidyverse)
library(lubridate)
library(janitor)
library(googlesheets)
library(gt)
library(ggmap)
library(plotly)
library(RColorBrewer)
library(readxl)

raw_data <- read_excel("TRUMP FACT-CHECKS.xlsx", 
                                sheet = "ak", col_types = c("date", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "numeric", 
                                                            "text"))


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
    category_tag1 = str_trim(str_to_upper(category_tag1)),
    category_tag2 = str_trim(str_to_upper(category_tag2)),
    category_tag3 = str_trim(str_to_upper(category_tag3)),
    category_tag4 = str_trim(str_to_upper(category_tag4)),
    year = year(date),
    month = month(date),
    day = day(date)
  )


#save version to file for later use
saveRDS(fcheck, "saved_versions/fcheck_saved.rds")

fcheck

t <- events %>%
  filter(date > today("EST"))


fcheck %>%
  filter(date <= today("EST"),
         date >= today("EST")-7) %>% 
  count(date)


# EXPLORATORY ANALYSIS #### ---------------------------------

names(fcheck)

#some exploring 
fcheck %>% 
  count(forum) %>% 
  arrange(desc(n)) 

fcheck %>% 
  count(kind_of_forum) %>% 
  arrange(desc(n))

fcheck %>% 
  count(location) %>% 
  arrange(desc(n)) 

fcheck %>% 
  count(category_tag1) %>% 
  arrange(desc(n)) 

fcheck %>% 
  count(spectrum_of_truth, candidate) %>% 
  arrange(spectrum_of_truth, desc(n)) 


# 
# # build gt tables ####
# 
# # https://gt.rstudio.com/reference/index.html
# fcheck_cnn_bycand_truth %>%
#   gt() %>%
#   tab_header(
#     title = "Truthfulness by Candidate"
#   ) %>% 
#   # tab_spanner(
#   #   label = "location",
#   #   columns = vars(
#   #     city, state)
#   # ) %>%
#   tab_row_group(
#     group = "False",
#     rows = spectrum_of_truth == "FALSE"
#   ) %>%
#   tab_row_group(
#     group = "Half True",
#     rows = spectrum_of_truth == "HALF TRUE"
#   )
# 
# 
# 
# events_selectcols %>%
#   gt() %>%
#   tab_header(
#     title = "Campaign Events"
#   ) %>%
#   # tab_spanner(
#   #   label = "location",
#   #   columns = vars(
#   #     city, state)
#   # ) %>%
#   tab_row_group(
#     group = "Califorina Trips",
#     rows = state == "California"
#   ) %>%
#   tab_row_group(
#     group = "Non California Trips",
#     rows = state != "California"
#   )
# 
# 
# 
# ## working space ####
# 
# 
# datatable(events,
#           rownames = FALSE,
#           options = list(bPaginate = FALSE,
#                          searching = FALSE,
#                          ordering = FALSE
#           )) %>%
#   formatDate('date', 'toDateString')
# 
# 
# 
# events <- events %>%
#   mutate(
#     date = mdy(date),
#     thru_date = mdy(thru_date),
#     cand_fullname = as.factor(cand_fullname),
#     state = as.factor(state),
#     cd_if_known = as.factor(cd_if_known),
#     event_type = as.factor(event_type)
#          )
# 
# 
# events %>%
#   filter(date > Sys.Date(),
#          date < (Sys.Date()+7))
# 
# 
# 
# 
# #geocoding
# # https://www.jessesadler.com/post/geocoding-with-r/
# # https://community.rstudio.com/t/how-to-add-my-api-key-into-get-map/15992
# 
# locs <- events %>%
#   filter(!is.na(city)) %>%
#   mutate(location = paste0(city, ", ", state)) %>%
#   select(location) %>%
#   unique() %>%
#   head()
# 
# 
# locations_df <- mutate_geocode(locs, location)
# 
# 
# #########################################
# ### KEY REMARKS AND INVERVIEWS ##########
# 
# #read in all the data in the interviews/remarks tab
# keyremarks <- fc2020 %>%
#   gs_read(ws = "Key Interviews/Remarks") %>%
#   clean_names()
# 
# keyremarks %>%
#   count(candidate) %>%
#   arrange(desc(n))
# 
# keyremarks %>%
#   count(venue) %>%
#   arrange(desc(n))
# 
# 
# 
# 
# #### FACET CHARTS FOR CANDIDATES BY STATE ####
# 
# 
# #by state counts
# by_cand_and_state <- events %>%
#   filter(date < Sys.Date(),
#          state != "INTL") %>%
#   count(cand_lastname, state) %>%
#   arrange(cand_lastname, desc(n))
# 
# #reorder factors to order bars descending
# bystate <- bystate %>%
#   mutate(
#     state = as.factor(state),
#     state = fct_reorder(state, desc(n))
#   )
# 
# 
# 
# colourCount = length(unique(by_cand_and_state$state))
# getPalette = colorRampPalette(brewer.pal(9, "Set2"))
# 
# 
# d <- ggplot(data = by_cand_and_state, aes(x = state, y = n, fill = state)) +
#   geom_col() +
#   # coord_flip() +
#   theme_minimal() +
#   scale_fill_manual(values = getPalette(colourCount))
#   # scale_fill_brewer(palette="Set3")
# 
# d
# 
# d2 <- d + labs(x ="", y = "") +
#   theme(legend.title=element_blank()) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# 
# d3 <- d2 + facet_wrap(~cand_lastname)
# 
# d3
# 
# dd <- ggplotly(d3)
# 
# dd
# 
# dd_nomenu <- dd %>% config(displayModeBar = FALSE)
# dd_nomenu
# 
# 
# 
# # https://www.r-bloggers.com/how-to-expand-color-palette-with-ggplot-and-rcolorbrewer/
# 
# 
