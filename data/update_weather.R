setwd('/Users/joemarlo/Dropbox/Data/Projects/311-analysis')
source('helper_functions/helpers_weather.R')
source('helper_functions/secrets.R')
library(dplyr)

# set current date
date_current <- Sys.Date()


# instantiate weather data ------------------------------------------------

# # get all the dates
# dates_historical <- calls_daily <- readr::read_csv('data/311_cleaned_daily.csv')
# dates_historical <- seq(min(dates_historical$date), date_current, by = '1 day')
# 
# # split up dates due to API restrictions
# dates_cut <- split(dates_historical, 
#                    paste0(lubridate::year(dates_historical), 
#                           ceiling(lubridate::month(dates_historical) / 6) * 6))
# 
# # get the weather data
# weather <- purrr::map_dfr(dates_cut, get_weather)
# 
# # write out
# readr::write_csv(weather, 'data/weather.csv')


# daily update ------------------------------------------------------------

# get previous weather data
weather_previous <- readr::read_csv("data/weather.csv")

# get current weather data
dates_new <- seq(date_current - 10, date_current + 7, by = '1 day')
weather_current <- get_weather(dates_new)

# overwrite old data with latest data
weather_new <- weather_previous %>% 
  bind_rows(weather_current) %>% 
  arrange(date, source) %>% 
  group_by(date) %>% 
  filter(row_number() == last(row_number())) %>% 
  ungroup() 

# handle missing wind data by STL interpolation
# not that best method but ¯\_(ツ)_/¯
# skimr::skim(weather_new)
wind_ts <- ts(weather_new$wind, 
           start = weather_new$date[1],
           frequency = 365)
wind_ts <- forecast::na.interp(wind_ts)
weather_new$wind <- wind_ts

# write out
readr::write_csv(weather_new, 'data/weather.csv')
