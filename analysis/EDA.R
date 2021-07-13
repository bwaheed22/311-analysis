library(tidyverse)
library(forecast)

# read data:
nyc311 <- readr::read_csv('data/311_cleaned.csv')

# create day, week, month, etc. columns:
nyc311 <- nyc311 %>%  mutate(date = lubridate::date(created_datetime),
                             hour = lubridate::hour(created_datetime),
                             day = lubridate::day(created_datetime),
                             week = lubridate::week(created_datetime),
                             month = lubridate::month(created_datetime),
                             year = lubridate::year(created_datetime))


# busiest hours of the day:
calls_by_hour <- nyc311 %>% group_by(hour) %>% tally() %>% ggplot(., aes(x = hour, y = n)) + 
  geom_col() + 
  theme_bw() + 
  labs(title = "Total NYC 311 Calls by Hour of Day",
       subtitle = "July 10, 2017 - July 10, 2021",
       x = "Hour of Day",
       y = "Number of Calls") + 
  scale_y_continuous(labels = scales::comma)

ggsave(calls_by_hour, filename = 'analysis/plots/calls_by_hour.png')

# calls by boro:
calls_boro <- nyc311 %>% group_by(borough) %>%
  tally() %>% 
  ggplot(., aes(x = reorder(borough,-n), y = n)) +
  geom_col() +
  theme_bw() +
  labs(title = "Total NYC 311 Calls by Borough",
       subtitle = "July 10, 2017 - July 10, 2021",
       x = "NYC Borough",
       y = "Number of Calls") + 
  scale_y_continuous(labels = scales::comma)

ggsave(calls_boro, filename = 'analysis/plots/calls_boro.png')

# monthly calls overtime by boro:
calls_boro_ts <-  nyc311 %>% filter(borough != 'Unspecified' | borough != 'NA') %>% 
  group_by(date = lubridate::floor_date(created_datetime, 'month'), borough) %>%
  tally() %>% ggplot(., aes(x = date, y = n)) +
  geom_line() +
  facet_wrap(~borough) + 
  theme_bw() +
  labs(title = "Monthly NYC 311 Calls by Borough",
       subtitle = "July 10, 2017 - July 10, 2021",
       x = "Date",
       y = "Number of Calls") + 
  scale_y_continuous(labels = scales::comma)

ggsave(calls_boro_ts, filename = 'analysis/plots/calls_boro_ts.png')

# call types by boro (need to clean names of call types):
calls_bytype <- nyc311 %>% group_by(borough, complaint_type) %>% 
  tally() %>% 
  arrange(desc(n), .by_group = T) %>% slice_max(order_by = n , n = 10) %>% 
  ggplot(., aes(x = reorder(complaint_type,-n), y = n)) + 
  geom_col() +
  theme_bw() +
  facet_wrap(~borough, scales = 'free') + 
  scale_y_continuous(labels = scales::comma) + 
  theme(axis.text.x = element_text(size = 8, angle = 60, hjust=1)) + 
  labs(title = 'Top 10 Call Types by Borough',
       subtitle = "July 10, 2017 - July 10, 2021",
       x = "Complaint Type",
       y = "Number of Calls")

ggsave(calls_bytype, filename = 'analysis/plots/calls_bytype.png')

