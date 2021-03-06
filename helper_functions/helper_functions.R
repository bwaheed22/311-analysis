# Helper functions to clean and aggregrate data daily:

library(dplyr)
library(lubridate)
library(forecast)
library(fable)
library(fabletools)
library(fable.prophet)
# library(furrr)


# ----- FUNCTION TO CLEAN RAW DATA: ----------

clean_data <- function(rawdata) {
  # clean up column names and agency names
  colnames(rawdata) <- janitor::make_clean_names(colnames(rawdata))
  rawdata$agency <- iconv(rawdata$agency, from = "UTF-8", to = "ASCII", sub = '')
  
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
    )
  
  # trim to only the required columns
  clean_data <- clean_data %>% 
    select(created_datetime, agency, complaint_type) %>% 
    as_tibble()
  
  # clean up complaint_type names:
  clean_data$complaint_type <- stringr::str_to_lower(clean_data$complaint_type)
  
  return(clean_data)
  
}

# ----FUNCTION TO CLEAN YESTERDAY DATA FOR MAPPING IN DASHBOARD ---------

clean_yest_data <- function(rawdata) {
  
  # read in selected agency-complaint pairs:
  agencies_cmplts <- readr::read_csv("data/selected_agencies_complaints.csv")
  
  # clean up column names and agency names
  colnames(rawdata) <- janitor::make_clean_names(colnames(rawdata))
  rawdata$agency <- iconv(rawdata$agency, from = "UTF-8", to = "ASCII", sub = '')
  
  # trim columns and filter to last four years
  clean_yest_data <- rawdata %>%
    mutate(
      created_date = lubridate::as_datetime(created_date, tz = "America/New_York"),
      closed_date = lubridate::as_datetime(closed_date, tz = "America/New_York")
    ) %>%
    select(
      unique_key,
      created_date,
      closed_date,
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
    )
  
  # clean up complaint_type names:
  clean_yest_data$complaint_type <- stringr::str_to_lower(clean_yest_data$complaint_type)
  
  clean_yest_data <- clean_yest_data %>% 
    filter(agency %in% agencies_cmplts$agency) %>% 
    mutate(complaint_type = ifelse(complaint_type %in% agencies_cmplts$complaint_type, complaint_type, 'other'))
  
  return(clean_yest_data)
  
}


# ----- FUNCTION TO AGGREGATE DATA TO DAILY: ----------

#' Aggregate raw calls into daily data
#' 
#' 
#'
#' @param clean_data 
#'
#' @return
#' @export
#'
#' @examples
#' 
aggregate_daily <- function(clean_data) {
  
  # read in selected agencies and complaints
  agencies_cmplts <- readr::read_csv("data/selected_agencies_complaints.csv")
  
  clean_data <- clean_data %>% 
    mutate(date = lubridate::date(created_datetime)) %>% 
    select(-created_datetime)
  
  # exclude agencies that aren't in selected list:
  clean_data <- clean_data %>% 
    filter(agency %in% agencies_cmplts$agency)
  
  # Check if these selected agencies/complaint type pairs have data in the last month:
  agencies_cmplts <- clean_data %>% 
    inner_join(agencies_cmplts, by = c('agency', 'complaint_type')) %>% 
    group_by(date, agency, complaint_type) %>% 
    tally() %>%
    group_by(agency, complaint_type) %>% 
    filter(max(date) >= (max(clean_data$date) - 31)) %>% 
    ungroup() %>% 
    distinct(agency, complaint_type)
  
  # Replace those that are not in the above list as other:
  others <- clean_data %>% 
    anti_join(agencies_cmplts, by = c('agency', 'complaint_type')) %>%
    mutate(complaint_type = 'other') %>% 
    group_by(date, agency, complaint_type) %>% 
    tally() %>% 
    ungroup()
  
  not_other <- clean_data %>% 
    inner_join(agencies_cmplts, by = c('agency', 'complaint_type')) %>% 
    group_by(date, agency, complaint_type) %>% 
    tally() %>% 
    ungroup()
  
  calls_daily <- dplyr::bind_rows(others, not_other) %>% 
    arrange(date, agency, desc(n))
  
  return(calls_daily)
  
}

#' Convert a string representing a model into a function call
#'
#' @param model one of c('mean', 'naive', 'snaive', 'drift', 'ets', 'arima', 'prophet')
#'
#' @return a string to be used with `eval(parse(test = [string]))`
#' @export
#'
#' @examples
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
    arima = "ARIMA(n ~ is_holiday + temperature + precipitation + wind)",
    prophet = "prophet(n ~ is_holiday + temperature + precipitation + wind + growth('linear') + season('week', type = 'additive') + season('year', type = 'additive'))"
  )
  return(model_fn)
}


#' Forecast using a list of models
#'
#' Create forecasts for each agency and complaint type pair using their specific model type. Model is fit to all historical data first.
#'
#' @param data a tsibble containing historical data 
#' @param best_models a dataframe denoting the best models
#' @param new_data a tsibble containing the forecast dates and exogenous variables
#'
#' @return a dataframe of forecasts
#' @export
#'
#' @examples
make_forecasts <- function(data, best_models, new_data){
  
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
      fc <- forecast(fit, new_data = new_data)
      
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
  data <- filter(.data, agency == .agency, complaint_type == .complaint_type)
    
  ggplot(data, aes(x = date)) +
    geom_ribbon(aes(ymin = lower_80, ymax = upper_80),
                         fill = 'grey 80') +
    geom_line(aes(y = .mean), color = 'grey20') +
    geom_point(aes(y = .mean), fill = 'grey20') +
    scale_x_date(date_breaks = '1 day', date_labels = '%b %d') +
    labs(title = .title,
         x = NULL,
         y = 'Count of daily calls') +
    theme(axis.text.x = element_text(angle = -40, hjust = 0, size = 6))
}


#' Check if date is weekend or NYSE holiday
#'
#' @param .date a Date object or string coercible to a Date object
#'
#' @return
#' @export
#'
#' @examples
#' is_holiday(c(Sys.Date(), Sys.Date()+1))
is_holiday <- function(.date){
  date <- timeDate::as.timeDate(.date)
  is_holiday <- timeDate::isHoliday(date, holidays = timeDate::holidayNYSE(), wday = 1:5)
  return(is_holiday)
}
