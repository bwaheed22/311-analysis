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
  
  # trim to only the required columns
  clean_data <- clean_data %>% 
    select(created_datetime, agency, complaint_type) %>% 
    as_tibble()
  
  return(clean_data)
  
}

# ----- FUNCTION TO AGGREGATE DATA TO DAILY: ----------

aggregate_daily <- function(clean_data) {
  
  # clean up names:
  clean_data$complaint_type <- stringr::str_to_lower(clean_data$complaint_type)
  
  # select top agencies with > 50,000 calls (~17 calls per day):
  # TODO: do we want to reset the agency:complaint_type pairs every day or just reuse the originals?
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
make_forecasts <- function(data, best_models, h = 7){
  
  # make forecasts for each agency:complaint_type pair
  fcsts <- data %>%
    left_join(best_models, by = c('agency', 'complaint_type')) %>% 
    group_by(agency, complaint_type) %>% 
    group_split() %>% 
    purrr::map_dfr(function(tbl_group){
      
      # convert the string denoting the best model into a function call
      best_model_fn <- best_model_as_fn(tbl_group$best_model[[1]])
      
      # fit the model
      fit <- model(tbl_group, model = eval(parse(text = best_model_fn)))
      
      # forecast one week
      fc <- forecast(fit, h = h)
      
      return(fc)
    })
  
  return(fcsts)
}


#' Plot a time-series with prediction interval
#'
#' @param .data a dataframe containing columns c('date', 'agency', 'complaint_type', 'n', 'lower_80', 'upper_80')
#' @param .agency the agency to filter to
#' @param .complaint_type the complaint type to filter to
#'
#' @return a ggplot2 object
#' @export
#' 
#' @import ggplot2
#'
#' @examples
plot_ts <- function(.data, .agency, .complaint_type){
  
  .title <- paste0('Daily calls for "', .complaint_type, '" at ', .agency)
  
  .data %>% 
    filter(agency == .agency, complaint_type == .complaint_type) %>% 
    ggplot(aes(x = date)) +
    geom_ribbon(aes(ymin = lower_80, ymax = upper_80),
                         fill = 'grey 80') +
    geom_line(aes(y = .mean), color = 'grey20') +
    geom_point(aes(y = .mean), fill = 'grey20') +
    scale_x_date(date_breaks = '1 week', date_labels = '%b %d') +
    labs(title = .title,
         x = NULL,
         y = 'Count of daily calls')
}

