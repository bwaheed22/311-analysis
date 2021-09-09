library(dplyr)
library(forecast)
library(fable)
library(fabletools)
library(fable.prophet)
library(furrr)
plan(multisession, workers = 4)

# read in cleaned calls data
calls_daily <- readr::read_csv('data/311_cleaned_daily.csv')

# create time series tibble; make NAs explicit and fill with last value
calls_daily_ts <- calls_daily %>% 
  tsibble::as_tsibble(key = c('agency', 'complaint_type'), 
                      index = 'date') %>% 
  tsibble::fill_gaps() %>% 
  tidyr::fill(n, .direction = "down")
  


# backtest by fitting each model on each agency:complaint_type pair -------

# set cutoff dates for end of the moving window
cutoff_dates <- seq(from = min(calls_daily$date) + 31, 
                    to = max(calls_daily$date), 
                    length.out = 12)
# hist(lubridate::day(cutoff_dates))

# run moving window backtest
backtest_forecasts <- future_map_dfr(cutoff_dates, function(cutoff_date){
  
  # filter data to prior to cutoff date and fit models
  fit <- calls_daily_ts %>% 
    filter(date <= cutoff_date) %>% 
    model(
      mean = MEAN(n),
      naive = NAIVE(n),
      snaive = SNAIVE(n),
      ets = ETS(n),
      arima = ARIMA(n),
      # nnts = NNETAR(n),
      prophet = prophet(n ~ growth("linear") + season('week', type = 'additive') + season("year", type = 'additive'))
    )
  
  # forecast out one week
  fc <- fit %>% forecast(h = 7)

  # calculate accuracy metrics on the forecasts
  metrics <- accuracy(fc, calls_daily_ts)
  
  # add cutoff date to dataframe so it can be used to identified the run
  metrics$cutoff_date <- cutoff_date
  
  return(metrics)
})

# pull the best model by averaging metrics over cutoff dates and taking the lowest MAPE
best_models <- backtest_forecasts %>% 
  group_by(agency, complaint_type) %>% 
  summarize(best_model = .model[which.min(MAPE)],
            .groups = 'drop')
barplot(table(best_models$best_model))

# write out the best model per agency:complaint_type pair
readr::write_csv(best_models, 'analysis/backtest/best_models.csv')
