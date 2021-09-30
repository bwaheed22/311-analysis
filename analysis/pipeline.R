# Daily workflow script to pull most recent data, clean it, and apply optimal forecast 
# models to create 1-step ahead forecast with confidence intervals:

# This script should be run daily.

library(dplyr)
library(RSocrata)
library(magrittr)
source("helper_functions/helper_functions.R")

# get today's date
date_today <- Sys.Date()

# Pull yesterday's data:
url <-"https://data.cityofnewyork.us/resource/erm2-nwe9.json?"
query <- paste0("$where=created_date between '", 
                date_today-3, "T00:00:00.000' and '", 
                date_today, "T00:00:00.000'", 
                collapse = "")

yesterday_data <- RSocrata::read.socrata(paste0(url, query, collapse = ""))
yesterday_data$agency <- iconv(yesterday_data$agency, from = "UTF-8", to = "ASCII", sub = "")

# Write-out yesterday's data for mapping in shiny app (currently 2 days prior):
readr::write_csv(yesterday_data, 'data/yesterday_data.csv')

# Clean data 
yesterday_data <- clean_data(yesterday_data)

# Aggregate data:
yesterday_data <- aggregate_daily(yesterday_data)

# Read in historical aggregated data:
historical_data <- readr::read_csv('data/311_cleaned_daily.csv')

# Append yesterday's data to historical:

# TODO: we seem to be double counting some data and yesterday's data is underestimated due to reporting lag
calls_daily <- dplyr::bind_rows(historical_data, yesterday_data)


# Create time series object:
calls_daily <- calls_daily %>% 
  tsibble::as_tsibble(key = c('agency', 'complaint_type'), 
                      index = 'date') %>% 
  tsibble::fill_gaps() %>% 
  tidyr::fill(n, .direction = "down")

# add in exogenous variables data
calls_daily$is_holiday <- is_holiday(calls_daily$date)
weather <- readr::read_csv('data/weather.csv')
calls_daily <- left_join(calls_daily, weather, by = 'date')

# read in the dataframe denoting the optimal models
best_models <- readr::read_csv('analysis/backtest/best_models.csv')

# make the forecasts
new_data <- calls_daily %>% 
  distinct(agency, complaint_type) %>% 
  tidyr::crossing(tibble(date = date_today + 0:6)) %>% 
  mutate(is_holiday = is_holiday(date)) %>% 
  left_join(weather, by = 'date') %>% 
  tsibble::as_tsibble(key = c('agency', 'complaint_type'), 
                      index = 'date')
forecasts <- make_forecasts(calls_daily, best_models, new_data = new_data)


# TODO: we need a way to manage the historical data + daily data (e.g. a running csv)

# combine historical last 3 weeks with one week forecast
last_four_weeks <- forecasts %>% 
  select(date, agency, complaint_type, .mean, n) %>% 
  tsibble::as_tsibble() %>% 
  bind_rows(select(calls_daily, date, agency, complaint_type, .mean = n)) %>% 
  filter(date >= (Sys.Date() - 21))

# extract prediction intervals and set lower bound to 0
last_four_weeks <- last_four_weeks %>% 
  fabletools::as_fable(response = '.mean', distribution = 'n') %>% 
  hilo(level = 80) %>%
  mutate(lower_80 = `80%`$lower,
         upper_80 = `80%`$upper) %>% 
  select(-`80%`, -n) %>% 
  mutate(.mean = if_else(.mean < 0, 0, .mean),
         lower_80 = if_else(lower_80 < 0, 0, lower_80),
         upper_80 = if_else(upper_80 < 0, 0, upper_80))


# write out data to be included with shiny app
readr::write_csv(last_four_weeks, 'data/forecasts_daily.csv')

# TODO: need a way to deal with yesterday's lack of data: 
# use previous forecast? and then update the following day?
# interpolate using STL decomposition?

# plot it
plot_ts(last_four_weeks, .agency = 'TLC', .complaint_type = 'taxi complaint')
