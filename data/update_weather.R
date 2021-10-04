wd <- '~/Dropbox/Data/Projects/311-analysis/'
library(dplyr)
library(simpleweather)
# source(paste0(wd, 'helper_functions/secrets.R'))

# set current date
date_current <- Sys.Date()

# lat long for Central Park
lat <- 40.7812
long <- -73.9665

# instantiate weather data ------------------------------------------------

# # get all the dates
# dates_historical <- readr::read_csv('data/311_cleaned_daily.csv')
# dates_historical <- seq(min(dates_historical$date), date_current, by = '1 day')
# 
# # get the weather for Central Park
# weather <- get_weather(dates_historical, lat, long)
# 
# # write out
# readr::write_csv(weather, paste0(wd, 'data/weather.csv'))


# daily update ------------------------------------------------------------

# get previous weather data
weather_previous <- readr::read_csv(paste0(wd, "data/weather.csv"))

# get current weather data
dates_new <- seq(date_current - 10, date_current + 7, by = '1 day')
weather_current <- get_weather(dates_new, lat, long)

# overwrite old data with latest data
weather_new <- weather_previous %>% 
  bind_rows(weather_current) %>%
  arrange(date, source, desc(is_forecast)) %>% 
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
readr::write_csv(weather_new, paste0(wd, 'data/weather.csv'))
