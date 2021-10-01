library(dplyr)
library(forecast)
library(fable)
library(fabletools)
library(fable.prophet)
library(furrr)
source('helper_functions/helper_functions.R')
plan(multisession, workers = 4)

# read in cleaned calls data
calls_daily <- readr::read_csv('data/311_cleaned_daily.csv')

# create time series tibble; make NAs explicit and fill with last value
calls_daily_ts <- calls_daily %>% 
  tsibble::as_tsibble(key = c('agency', 'complaint_type'), 
                      index = 'date') %>% 
  tsibble::fill_gaps() %>% 
  tidyr::fill(n, .direction = "down")

# add holidays
calls_daily_ts$is_holiday <- is_holiday(calls_daily_ts$date)

# add weather
weather <- readr::read_csv('data/weather.csv')
calls_daily_ts <- left_join(calls_daily_ts, 
                            select(weather, date, temperature, precipitation, wind), 
                            by = 'date')


# backtest by fitting each model on each agency:complaint_type pair -------

# set cutoff dates for end of the moving window
# only chose dates that have days of forecast for each agency:complaint_type pair
candidate_dates <- sort(unique(calls_daily_ts$date))
contains_data <- sapply(candidate_dates, function(cutoff_date){
  leading_dates <- cutoff_date + 1:7
  calls_daily_ts %>% 
    as_tibble() %>% 
    group_by(agency, complaint_type) %>% 
    summarize(contains_data = leading_dates %in% date,
              .groups = 'drop') %>% 
    summarize(contains_data = all(contains_data))
})
candidate_dates <- candidate_dates[unlist(contains_data)] # they're all 2021 dates

# choose 8 evenly spaced dates
cutoff_dates <- candidate_dates[round(seq(1, length(candidate_dates), length.out = 8))]
# hist(lubridate::day(cutoff_dates))

# visualize the cross validation methodology
# https://otexts.com/fpp3/tscv.html
# purrr::map_dfr(cutoff_dates, function(end){
#   tibble(cutoff = which(cutoff_dates == end),
#          date = seq(min(calls_daily$date), end + 7, by = '1 day')) %>%
#     mutate(forecast_test_dates = if_else(date > end, 'Test', 'Train'))
# }) %>%
#   ggplot(aes(x = date, y = cutoff, color = forecast_test_dates)) +
#   # geom_tile() +
#   geom_point() +
#   scale_y_continuous(breaks = seq_along(cutoff_dates)) +
#   labs(title = 'Cross-validation methodology',
#        x = NULL,
#        y = 'Training/test set',
#        color = NULL) +
#   theme(legend.position = 'bottom')

# run moving window backtest
backtest_forecasts <- future_map_dfr(cutoff_dates, function(cutoff_date){
  
  # filter data to prior to cutoff date and fit models
  fit <- calls_daily_ts %>% 
    filter(date <= cutoff_date) %>%
    model(
      mean = MEAN(n),
      naive = NAIVE(n),
      snaive = SNAIVE(n),
      drift = RW(n ~ drift()),
      ets = ETS(n ~ trend() + season()),
      arima = ARIMA(n ~ is_holiday + temperature + precipitation + wind),
      # nnts = NNETAR(n ~ is_holiday + temperature + precipitation + wind, lambda = 0),
      prophet = prophet(n ~ is_holiday + temperature + precipitation + wind +
                          growth('linear') +
                          season('week', type = 'additive') +
                          season('year', type = 'additive'))
    )
  
  # some dates don't have test data so they fail
  metrics <- tryCatch({
    # forecast out one week
    new_data <- filter(calls_daily_ts, date %in% (cutoff_date + 1:7))
    fc <- fabletools::forecast(fit, new_data = new_data)
    
    # calculate accuracy metrics on the forecasts
    metrics <- accuracy(fc, calls_daily_ts)
    
    # add cutoff date to dataframe so it can be used to identified the run
    metrics$cutoff_date <- cutoff_date
    
    return(metrics)
  },
    error = function(e) return(NULL)
  )
  
  return(metrics)
})

# close the parallel processes
plan(sequential)

# pull the best model by averaging metrics over cutoff dates and taking the lowest mean RMSE
best_models <- backtest_forecasts %>% 
  group_by(agency, complaint_type, .model) %>% 
  summarize(RMSE_mean = mean(RMSE, na.rm = TRUE)) %>% 
  group_by(agency, complaint_type) %>% 
  summarize(best_model = .model[which.min(RMSE_mean)],
            .groups = 'drop')

# if any models didn't fit, then add in and use the mean model
best_models <- calls_daily %>% 
  distinct(agency, complaint_type) %>% 
  left_join(best_models, by = c('agency', 'complaint_type')) %>% 
  mutate(best_model = if_else(is.na(best_model), 'mean', best_model))
# best_models %>%
#   group_by(best_model) %>%
#   tally() %>%
#   full_join(tibble(best_model = c('mean', 'naive', 'snaive', 'drift', 'ets', 'arima', 'prophet')),
#             by = 'best_model') %>% 
#   ggplot(aes(x = reorder(best_model, -n), y = n)) +
#   geom_col() +
#   labs(title = 'Exponential smoothing and ARIMA are by far the most common',
#        subtitle = 'Frequency of each model',
#        caption = 'Tests for trends and seasonal components where applicable') +
#   theme(plot.caption = element_text(face = 'italic', size = 6))
# ggsave('analysis/plots/best_models.png', width = 7, height = 3.5)

# write out the best model per agency:complaint_type pair
readr::write_csv(best_models, '311_calls/data/best_models.csv')
