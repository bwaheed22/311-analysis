library(dplyr)

# read in data
calls_df <- readr::read_csv("data/311_cleaned.csv")

# aggregate to daily based on compliant_type --> could also do it based on agency
calls_daily <- calls_df %>%  
  mutate(date = lubridate::date(created_datetime)) %>% 
  select(date, complaint_type) %>% 
  group_by(date, complaint_type) %>% 
  tally() %>% 
  ungroup()

# create misc category for complaint_types under 1000 calls
top_types <- calls_daily %>% 
  group_by(complaint_type) %>% 
  summarise(n = sum(n)) %>% 
  filter(n >= 10000) %>% 
  pull(complaint_type)

# for non top types, recode as misc.
calls_daily$complaint_type <- if_else(calls_daily$complaint_type %in% top_types,
                                      calls_daily$complaint_type,
                                      "Misc")
# aggregate the Misc type
calls_daily <- calls_daily %>% 
  group_by(date, complaint_type) %>% 
  summarize(n = sum(n),
            .groups = 'drop')

# clean up names
calls_daily$complaint_type <- stringr::str_to_lower(calls_daily$complaint_type)

# write out
readr::write_csv(calls_daily, 'data/311_cleaned_daily.csv')
