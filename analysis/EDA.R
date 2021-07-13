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