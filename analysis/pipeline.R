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
historical_data <- readr::read_csv('data/311_cleaned.csv')

# Append yesterday's data to historical:
calls_full <- dplyr::bind_rows(historical_data, yesterday_data)

# Aggregate data to daily:
calls_daily <- aggregate_daily(calls_full)

# Backtest and forecast based on optimal models:
forecasts <- backtest_forecast(calls_full)


# Plot: ----






