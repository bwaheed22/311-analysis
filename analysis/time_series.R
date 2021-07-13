# Time series analyis NYC 311 data:

library(tidyverse)
library(forecast)
library(lubridate)

# read data: 
nyc311 <- readr::read_csv('data/311_cleaned.csv')

# create day, week, month columns:
nyc311 <- nyc311 %>%  mutate(date = lubridate::date(created_datetime),
                             hour = lubridate::hour(created_datetime),
                             day = lubridate::day(created_datetime),
                             week = lubridate::week(created_datetime),
                             month = lubridate::month(created_datetime),
                             year = lubridate::year(created_datetime))

# create hourly, daily, weekly, and monthly time series data:

hourly <- nyc311 %>%
  group_by(date = lubridate::floor_date(created_datetime, 'hour')) %>% 
  summarize(num_calls = n())

daily <- nyc311 %>% group_by(date = lubridate::floor_date(created_datetime, 'day')) %>% 
  summarize(num_calls = n())

weekly <- nyc311 %>% group_by(date = lubridate::floor_date(created_datetime, 'week')) %>% 
  summarize(num_calls = n())

monthly <- nyc311 %>% group_by(date = lubridate::floor_date(created_datetime, 'month')) %>% 
  summarize(num_calls = n())

yearly <- nyc311 %>% group_by(year) %>% summarize(num_calls = n())

# plot each time series and respective ACF/PACF functions:

tsplots <- function(data, unit) {
  
  tsplot <- ggplot(data, aes(x = date, y = num_calls)) +
    geom_line() +
    labs(title = paste0(unit, " NYC 311 Service Requests"),
      subtitle = "July 10, 2017 - July 10, 2021",
      x = 'Time',
      y = "Number of Calls") +
    scale_y_continuous(labels = scales::comma) + theme_bw()
  
  acf <- forecast::Acf(data$num_calls, main = paste0("ACF Plot - ", unit))
  pacf <- forecast::Pacf(data$num_calls, main = paste0("PACF Plot - ", unit))
  
  print(tsplot)
  
  return(list(tsplot, acf, pacf))
}

p_hourly <- tsplots(hourly, 'Hourly')
p_daily <- tsplots(daily, 'Daily')
p_weekly <- tsplots(weekly, 'Weekly')
p_monthly <- tsplots(monthly, 'Monthly')

ggsave(p_hourly[[1]], filename = 'analysis/plots/ts_hourly.png')
ggsave(p_daily[[1]], filename = 'analysis/plots/ts_daily.png')
ggsave(p_weekly[[1]], filename = 'analysis/plots/ts_weekly.png')
ggsave(p_monthly[[1]], filename = 'analysis/plots/ts_monthly.png')


# take differences to make series stationary and plot time series and ACF/PACF:

diffhourly <- c(NA, diff(hourly$num_calls))
diffdaily <- c(NA, diff(daily$num_calls))
diffweekly <- c(NA, diff(weekly$num_calls))
diffmonthly <- c(NA, diff(monthly$num_calls))

ggtsdisplay(diffhourly)
ggtsdisplay(diffdaily)
ggtsdisplay(diffdaily)
ggtsdisplay(diffdaily)
                            

