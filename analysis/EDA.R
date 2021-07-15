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

# lowercase character columns: 
cols_to_lower <- c("agency", "complaint_type", "descriptor", "status", "city", "resolution_description", "borough")

nyc311 <- nyc311 %>% mutate_at(.vars = cols_to_lower, .funs = tolower)

# --- Clean complaint type names and group similar complaints into single categories---
nyc311df <- nyc311

# list of complaint types and counts:
complaints <- data.frame(table(nyc311df$complaint_type)) %>% arrange(Freq)

# complaints to remove:
complaints_rm <- complaints[c(1:18, 22, 26:27, 30:31, 33:53, 56, 62, 68),1]
nyc311df$complaint_type <- ifelse(nyc311df$complaint_type %in% complaints_rm, "NA", nyc311df$complaint_type)

# check new list of complaints:
complaints_new <- data.frame(table(nyc311df$complaint_type)) %>% arrange(Freq)

# group noise complaints into single 'noise' category:
noise_types <- nyc311df %>% 
  filter(stringr::str_detect(complaint_type, 'noise')) %>% 
  select(complaint_type) %>% unique() %>% pull()

nyc311df$complaint_type <- ifelse(nyc311df$complaint_type %in% noise_types, 'noise', nyc311df$complaint_type)

# ---- EDA ----

# busiest hours of the day each year:
calls_by_hour <- nyc311df %>% group_by(hour, year) %>% tally() %>% ggplot(., aes(x = hour, y = n)) + 
  geom_col() + 
  theme_bw() +
  facet_wrap(~year) +
  labs(title = "Total NYC 311 Calls by Hour of Day",
       subtitle = "July 10, 2017 - July 10, 2021",
       x = "Hour of Day",
       y = "Number of Calls") + 
  scale_y_continuous(labels = scales::comma)

ggsave(calls_by_hour, filename = 'analysis/plots/calls_by_hour.png')

# calls by boro:
calls_boro <- nyc311df %>% group_by(borough) %>%
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

# monthly calls over time by boro:
calls_boro_ts <-  nyc311df %>% filter(borough != 'Unspecified' | borough != 'NA') %>% 
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

# call types by boro:
calls_bytype <- nyc311df %>% filter(borough != "unspecified" & borough != "na") %>% 
  group_by(borough, complaint_type) %>% 
  tally() %>% 
  arrange(n, .by_group = T) %>% slice_max(order_by = n , n = 10) %>% 
  ggplot(., aes(x = reorder(complaint_type,n), y = n)) + 
  geom_col() +
  theme_bw() +
  coord_flip() +
  facet_wrap(~borough, scales = 'free') + 
  scale_y_continuous(labels = scales::comma) + 
  theme(axis.text.x = element_text(size = 7, angle = 60, hjust=1)) + 
  labs(title = 'Top 10 Call Types by Borough',
       subtitle = "July 10, 2017 - July 10, 2021",
       x = "Complaint Type",
       y = "Number of Calls")

ggsave(calls_bytype, filename = 'analysis/plots/calls_bytype.png')

# times series each year, superimposed:

# daily calls over time by year:
calls_boro_ts <-  nyc311df %>% filter(borough != 'unspecified' & borough != 'na') %>% 
  group_by(date = lubridate::floor_date(created_datetime, 'day'), year) %>%
  tally() %>% ggplot(., aes(x = date, y = n)) +
  geom_line() +
  facet_wrap(~year, scales = 'free') + 
  theme_bw() +
  labs(title = "Monthly NYC 311 Calls by Borough",
       subtitle = "July 10, 2017 - July 10, 2021",
       x = "Date",
       y = "Number of Calls") + 
  scale_y_continuous(labels = scales::comma)

