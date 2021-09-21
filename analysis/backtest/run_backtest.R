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
# calls_daily_ts$temp <- weather_temperature()
# calls_daily_ts$precip <- weather_precipitation()
# calls_daily_ts$wind <- weather_wind()


# backtest by fitting each model on each agency:complaint_type pair -------

# set cutoff dates for end of the moving window
cutoff_dates <- seq(from = min(calls_daily$date) + (24 * 30), 
                    to = max(calls_daily$date) - 31, 
                    length.out = 12)
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
      arima = ARIMA(n ~ is_holiday),
      # nnts = NNETAR(n ~ is_holiday, lambda = 0),
      prophet = prophet(n ~ is_holiday + growth('linear') + season('week', type = 'additive') + season('year', type = 'additive'))
    )
  
  # forecast out one week
  fc <- forecast(fit, h = 7)

  # calculate accuracy metrics on the forecasts
  metrics <- accuracy(fc, calls_daily_ts)
  
  # add cutoff date to dataframe so it can be used to identified the run
  metrics$cutoff_date <- cutoff_date
  
  return(metrics)
})

# close the parallel processes
plan(sequential)

# pull the best model by averaging metrics over cutoff dates and taking the lowest MAPE
# throw out outliers first
best_models <- backtest_forecasts %>% 
  group_by(agency, complaint_type, .model) %>% 
  filter(MAPE > quantile(MAPE, 0.01),
         MAPE < quantile(MAPE, 0.99)) %>% 
  summarize(MAPE_mean = mean(MAPE, na.rm = TRUE)) %>% 
  group_by(agency, complaint_type) %>% 
  summarize(best_model = .model[which.min(MAPE_mean)],
            .groups = 'drop')
# barplot(table(best_models$best_model))

# if any models didn't fit, then add in and use the mean model
best_models <- calls_daily %>% 
  distinct(agency, complaint_type) %>% 
  left_join(best_models, by = c('agency', 'complaint_type')) %>% 
  mutate(best_model = if_else(is.na(best_model), 'mean', best_model))

# write out the best model per agency:complaint_type pair
readr::write_csv(best_models, 'analysis/backtest/best_models.csv')


# example workflow to fit the models --------------------------------------

# function to return a model based on an input string
best_model_as_fn <- function(model){
  if (!(model %in% c('mean', 'naive', 'snaive', 'drift', 'ets', 'arima', 'prophet'))){
    stop("model must be one of c('mean', 'naive', 'snaive', 'drift', 'ets', 'arima', 'prophet')")
  }
  model_fn <- switch(
    model,
    mean = "MEAN(n)",
    naive = "NAIVE(n)",
    snaive = "SNAIVE(n)",
    drift = "RW(n ~ drift())",
    ets = "ETS(n ~ trend() + season())",
    arima = "ARIMA(n)",
    prophet = "prophet(n ~ growth('linear') + season('week', type = 'additive') + season('year', type = 'additive'))"
  )
  return(model_fn)
}

# fit the models and predict the latest week
fcsts <- calls_daily_ts %>%
  filter(date <= (max(date) - 7)) %>% 
  left_join(best_models, by = c('agency', 'complaint_type')) %>% 
  group_by(agency, complaint_type) %>% 
  group_split() %>% 
  purrr::map_dfr(function(tbl_group){
    
    # convert the string denoting the best model into a function call
    best_model_fn <- best_model_as_fn(tbl_group$best_model[[1]])

    # fit the model
    fit <- model(tbl_group, model = eval(parse(text = best_model_fn)))
    
    # forecast one week
    fc <- forecast(fit, h = 7)
    
    return(fc)
  })

# plot the results
fcsts %>% 
  filter(agency == 'DOB') %>% 
  autoplot(tsibble::filter_index(calls_daily_ts, '2021'))
