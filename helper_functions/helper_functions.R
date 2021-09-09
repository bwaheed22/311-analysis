# Helper functions to clean and aggregrate data daily:

library(dplyr)
library(lubridate)
library(forecast)
library(fable)
library(fabletools)
library(fable.prophet)
library(furrr)


# ----- FUNCTION TO CLEAN RAW DATA: ----------

clean_data <- function(rawdata) {
  # clean up column names
  colnames(rawdata) <- janitor::make_clean_names(colnames(rawdata))
  
  # trim columns and filter to last four years
  clean_data <- rawdata %>%
    mutate(
      created_datetime = lubridate::as_datetime(created_date, tz = "America/New_York"),
      closed_datetime = lubridate::as_datetime(closed_date, tz = "America/New_York")
    ) %>%
    select(
      unique_key,
      created_datetime,
      closed_datetime,
      agency,
      complaint_type,
      descriptor,
      status,
      resolution_description,
      incident_zip,
      city,
      borough,
      latitude,
      longitude
    ) %>%
    filter(created_datetime >= max(created_datetime) - lubridate::years(4))
  
  return(clean_data)
  
}

# ----- FUNCTION TO AGGREGATE DATA TO DAILY: ----------

aggregate_daily <- function(clean_data) {
  
  # clean up names:
  clean_data$complaint_type <- stringr::str_to_lower(clean_data$complaint_type)
  
  # select top agencies with > 50,000 calls (~17 calls per day):
  top_agencies <- clean_data %>% 
    group_by(agency) %>% 
    tally() %>% 
    arrange(desc(n)) %>%
    filter(n > 50000) %>% 
    pull(agency)
  
  # aggregate to daily based on compliant_type:
  calls_daily <- clean_data %>% 
    filter(agency %in% top_agencies) %>% 
    mutate(date = lubridate::date(created_datetime)) %>% 
    select(date, agency, complaint_type) %>% 
    group_by(date, agency, complaint_type) %>% 
    tally() %>% 
    arrange(agency, desc(n)) %>% 
    ungroup()
  
  # look at low frequency complaint types within each agency:
  low_complaints <- calls_daily %>% 
    group_by(agency, complaint_type) %>% 
    summarise(n = sum(n)) %>% 
    filter(n < 14000) %>% 
    select(-n) %>% 
    mutate(is_low = TRUE)
  
  # re-label  low complaint types as other, and then aggregate by day:
  calls_daily <- calls_daily %>% 
    left_join(low_complaints, by = c('agency', 'complaint_type')) %>%
    mutate(is_low = ifelse(is.na(is_low), FALSE, TRUE), 
           complaint_type = ifelse(is_low == TRUE, 'other', complaint_type)) %>% 
    group_by(date, agency, complaint_type) %>% 
    summarise(n = sum(n)) %>% 
    ungroup()
  
  # remove agency:complaint type pairs that do not have at least one call in the last month
  calls_daily <- calls_daily %>% 
    group_by(agency, complaint_type) %>% 
    filter(max(date) >= (max(calls_daily$date) - 31)) %>% 
    ungroup()
  
  return(calls_daily)
  
}

# ----- FUNCTION TO BACKTEST MODELS, SELECT OPTIMAL MODEL, AND CREATE FORECAST : ----------

backtest_forecast <- function(calls_daily) {
  
  plan(multisession, workers = 4)
  
  # create time series tibble; make NAs explicit and fill with last value
  calls_daily_ts <- calls_daily %>% 
    tsibble::as_tsibble(key = c('agency', 'complaint_type'), 
                        index = 'date') %>% 
    tsibble::fill_gaps() %>% 
    tidyr::fill(n, .direction = "down")
  
  
  # backtest by fitting each model on each agency:complaint_type pair -------
  
  # set cutoff dates for end of the moving window
  cutoff_dates <- seq(from = min(calls_daily$date) + (24 * 30), 
                      to = max(calls_daily$date) - 31, 
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
        drift = RW(n ~ drift()),
        ets = ETS(n ~ trend() + season()),
        arima = ARIMA(n),
        # nnts = NNETAR(n),
        prophet = prophet(n ~ growth('linear') + season('week', type = 'additive') + season('year', type = 'additive'))
      )
    
    # forecast out one week
    fc <- fit %>% forecast(h = 7)
    
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
  
  # fit the models to all the data and forecast one week
  fcsts <- calls_daily_ts %>%
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
  
  return(fcsts)
}

