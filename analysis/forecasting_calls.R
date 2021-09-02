# Forecast next-day number of calls for all complaint types within each agency
# using ARIMA time series analysis:

library(tidyverse)
library(forecast)
library(lubridate)

# Read cleaned daily data: 
calls <- read_csv("data/311_cleaned_daily.csv")

# Store agency names:
agencies <- unique(calls$agency)

# Make a function to forecast calls for a specific agency, 
# and all of the complaint types within it, stored in a list:

calls_forecast <- function(data, agency_name) {
  
  # filter data to specific agency:
  data <- data %>% filter(agency == agency_name)
  
  # pull all unique complaint types within specific agency:
  complaint_names <- unique(data$complaint_type)
  
  # create dummy daily df (to make all missing days 0):
  x <- data.frame(date = seq(from = as.Date(min(data$date)),
                             to = as.Date(max(data$date)), by = 'day'))
  
  # cycle through complaint types and create point forecasts:
  forecasts = list()
  
  for (i in 1:length(complaint_names)) {
    
  df <- data %>% filter(complaint_type == complaint_names[i])
  df <- x %>% left_join(df, by = 'date') %>% select(date, n)
  df$n <- ifelse(is.na(df$n), 0, df$n)
  
  fit <- auto.arima(df$n)
  f1 <- forecast(fit, h = 1, level = 95)[[4]][1]
  
  # store forecasts in a list, and label each with complaint name:
  forecasts[i] <- f1
  names(forecasts)[i] <- complaint_names[i]
  
  }
  
  return(forecasts)
}

# Use function to create point forecasts for all agencies:

forecasts_all <- list()

for (i in 1:length(agencies)) {
  forecasts_all[[i]] <- calls_forecast(calls, agencies[i])
  names(forecasts_all)[i] <- agencies[i]
}

# Store all results in a data frame:
forecasts_all_df <- list()

for (i in 1: length(agencies)) {
  forecasts_all_df[[i]] <- data.frame("agency" = agencies[i], 
                                      "complaint_type" = rownames(do.call(rbind, forecasts_all[[i]])),
                                      "forecast" = round(do.call(rbind, forecasts_all[[i]]),3))
}

forecasts_all_df <- do.call(rbind, forecasts_all_df)
rownames(forecasts_all_df) <- seq(1:nrow(forecasts_all_df))


# Write to csv:
readr::write_csv(forecasts_all_df, 'data/311_forecasts_all_agencies.csv')
