# NOTE: run all chunks from the dashboard file (01) first populate environment with the variables


#top forum types 
fcheck %>%
  count(kind_of_forum) %>%
  arrange(desc(n)) %>%
  head(5) %>%
  ungroup() %>% 
  write_csv("forgfx/top_forumtype.csv")


#top individual forums  
fcheck %>%
  filter(forum != "") %>% 
  count(forum, date) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  ungroup() %>% 
  write_csv("forgfx/top_events_forums.csv")


#top subject categories  
category_tag_count %>% 
  filter(tag != "") %>% 
  head(10) %>% 
  write_csv("forgfx/top_subjectcategories.csv")


#top days
fcheck %>%
  count(date) %>% 
  arrange(desc(n)) %>%
  head(10) %>% 
  ungroup() %>% 
  write_csv("forgfx/top_days.csv")
