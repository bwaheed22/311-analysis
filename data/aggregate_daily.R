library(dplyr)
library(tidyverse)

# Read in data
calls_df <- readr::read_csv("data/311_cleaned.csv")

# clean up names
calls_df$complaint_type <- stringr::str_to_lower(calls_df$complaint_type)

# select top agencies with > 50,000 calls (~17 calls per day)
top_agencies <- calls_df %>% 
  group_by(agency) %>% 
  tally() %>% 
  arrange(desc(n)) %>%
  filter(n > 50000) %>% 
  pull(agency)

# aggregate to daily based on compliant_type --> could also do it based on agency
calls_daily <- calls_df %>% 
  filter(agency %in% top_agencies) %>% 
  mutate(date = lubridate::date(created_datetime)) %>% 
  select(date, agency, complaint_type) %>% 
  group_by(date, agency, complaint_type) %>% 
  tally() %>% 
  arrange(agency, desc(n)) %>% 
  ungroup()

# look at low frequency complaint types within each agency:
low_complaints <- calls_daily %>% 
  group_by(agency, complaint_type) %>% 
  summarise(n = sum(n)) %>% 
  filter(n < 14000) %>% select(-n) %>% 
  mutate(is_low = TRUE)

# re-label  low complaint types as other, and then aggregate by day:
calls_daily <- calls_daily %>% 
  left_join(low_complaints, by = c('agency', 'complaint_type')) %>%
  mutate(is_low = ifelse(is.na(is_low),FALSE,TRUE),
         complaint_type = ifelse(is_low == TRUE, 'other', complaint_type)) %>% 
  group_by(date, agency, complaint_type) %>% 
  summarise(n = sum(n)) %>% 
  ungroup()


calls_daily %>% filter(agency == 'NYPD')s %>% 
  ggplot(., aes(x = date, y = n)) +
  facet_wrap(agency~complaint_type, scales = "free") + 
  geom_line() + 
  theme_bw() +
  labs(title = "NYPD Calls by Type",
       subtitle = "July 10, 2017 - July 10, 2021",
       x = "Date",
       y = "Number of Calls") + 
  scale_y_continuous(labels = scales::comma)


# write out
readr::write_csv(calls_daily, 'data/311_cleaned_daily.csv')
