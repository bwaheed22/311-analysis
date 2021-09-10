# Daily workflow script to pull most recent data, clean it, and apply optimal forecast 
# models to create 1-step ahead forecast with confidence intervals:

# This script should be run daily.

library(dplyr)
library(RSocrata)
library(magrittr)
source("helper_functions/helper_functions.R")

# Pull yesterday's data:
url <-"https://data.cityofnewyork.us/resource/erm2-nwe9.json?"
query <- paste0("$where=created_date between '", 
                Sys.Date()-2,"T00:00:00.000' and '", 
                Sys.Date()-1, "T00:00:00.000'", collapse = "")

yesterday_data <- RSocrata::read.socrata(paste0(url, query, collapse = ""))

# Clean data 
yesterday_data <- clean_data(yesterday_data) #!! need to change column types in order for bind_rows to work

# Read in historical clean data:
# TODO: do we want to read all the data or just the aggregated data?
#   all the data will be slow especially if we move this to a raspberry pi
historical_data <- readr::read_csv('data/311_cleaned.csv') %>% 
  select(created_datetime, agency, complaint_type)

# Append yesterday's data to historical:
calls_full <- dplyr::bind_rows(historical_data, yesterday_data)

# Aggregate data to daily:
calls_daily <- aggregate_daily(calls_full)
calls_daily <- calls_daily %>% 
  tsibble::as_tsibble(key = c('agency', 'complaint_type'), 
                      index = 'date') %>% 
  tsibble::fill_gaps() %>% 
  tidyr::fill(n, .direction = "down")

# read in the dataframe denoting the optimal models
best_models <- readr::read_csv('analysis/backtest/best_models.csv')

# make the forecasts
forecasts <- make_forecasts(calls_daily, best_models, h = 7)
forecasts %>% 
  hilo() %>% 
  select(date, agency, complaint_type, .mean, ci_80 = `80%`, ci_95 = `95%`)

# TODO: we need a way to manage the historical data + daily data (e.g. a running csv)

# combine historical last 3 weeks with one week forecast
last_four_weeks <- forecasts %>% 
  select(date, agency, complaint_type, .mean, n) %>% 
  tsibble::as_tsibble() %>% 
  bind_rows(rename(calls_daily, .mean = n)) %>% 
  filter(date >= (Sys.Date() - 21))

# write out data to be included with shiny app
# readr::write_csv(last_four_weeks, 'data/forecasts_daily.csv')

# TODO: need a way to deal with yesterday's lack of data: use previous forecast? and then update the following day?

# plot it
last_four_weeks %>% 
  fabletools::as_fable(response = '.mean', distribution = 'n') %>% 
  filter(agency == 'DHS') %>% 
  autoplot(filter(last_four_weeks, date <= Sys.Date()))

           