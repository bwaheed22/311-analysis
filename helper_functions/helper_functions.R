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


source('helper_functions/secrets.R')
#' Retrieve historical weather data from the NOAA API
#' 
#' Returns daily temperature (F), precipitation (T/F), or wind (mph) data for Central Park, NY using the NOAA API. Must set API token via Sys.setenv(token_noaa = '<token>') prior to use. Request token here get token here https://www.ncdc.noaa.gov/cdo-web/token.
#'
#' @param date_start the starting date to retrieve data for
#' @param date_end the end date to retrieve data for
#'
#' @return a data frame
#' @export
#' 
#' @references https://www.ncdc.noaa.gov/cdo-web/webservices/v2
#'
#' @examples
#' #Sys.setenv(token_noaa = '<token>')
#' get_noaa('2021-09-10', '2021-09-22')
get_noaa <- function(.date_start, .date_end){

  if (!inherits(as.Date(.date_start), 'Date') | !inherits(as.Date(.date_end), 'Date')) stop('date_start & date_end must be coercible to dates')

  # construct call to NOAA API
  token <- Sys.getenv('token_noaa')
  date_start <- paste0('startdate=', .date_start)
  date_end <- paste0('enddate=', .date_end)
  station <- 'stationid=GHCND:USW00094728' # Central Park
  dataset <- 'datasetid=GHCND' #
  datatype <- 'datatypeid=TMAX,PRCP,WSF2'
  units <- 'units=standard'
  limit <- 'limit=1000' # 1000 is the max
  args <- paste(dataset, datatype, station, date_start, date_end, units, limit, sep = '&')
  url_base <- 'https://www.ncdc.noaa.gov/cdo-web/api/v2/data?'
  url_complete <- paste0(url_base, args)
  
  # make the GET request and flatten the response into a dataframe
  resp <- httr::GET(url_complete, httr::add_headers("token" = token))
  resp_content <- httr::content(resp)$results
  resp_df <- purrr::map_dfr(resp_content, function(item) as_tibble(item))
  
  # clean up dataframe
  resp_df <- resp_df %>% 
    transmute(date = as.Date(date), datatype = datatype, value = value) %>% 
    tidyr::pivot_wider(names_from = datatype) %>% 
    select(date, temperature = TMAX, precipitation = PRCP, wind = WSF2) %>% 
    mutate(precipitation = precipitation > 0.1,
           is_forecast = FALSE,
           source = 'NOAA')
  
  return(resp_df)
}

#' Retrieve forecasted weather data from the OpenWeather API
#'
#' Returns the 7-day temperature (F), precipitation (T/F), and wind speed (mph) forecast for Central Park, NY. Precipitation defined as >= 0.35 forecasted probability of rain. Must set API token via Sys.setenv(token_openweather = '<token>') prior to use. Request token here get token here https://openweathermap.org/full-price#current.
#'
#' @return
#' @export
#'
#' @references https://openweathermap.org/api/one-call-api
#'
#' @examples
#' get_openweather()
get_openweather <- function(){

  # construct call to OpenWeather API
  token <- Sys.getenv('token_openweather')
  token <- paste0('appid=', token)
  lat <- 'lat=40.7812'
  long <- 'lon=-73.9665'
  exclude <- 'exclude=current,minutely,hourly,alerts'
  units <- 'units=imperial'
  args <- paste(lat, long, exclude, units, token, sep = '&')
  url_base <- 'https://api.openweathermap.org/data/2.5/onecall?'
  url_complete <- paste0(url_base, args)
  
  # make the GET request and flatten the response into a dataframe
  resp <- httr::GET(url_complete)
  resp_content <- httr::content(resp)$daily
  resp_df <- purrr::map_dfr(resp_content, function(item){
    
    # extract data and put in a dataframe
    date <- as.Date(as.POSIXct(item$dt, origin = "1970-01-01"))
    temp <- item$temp$max
    precip <- item$pop >= 0.3
    wind <- item$wind_speed
    tibble(date = date,
           temperature = temp,
           precipitation = precip,
           wind = wind,
           is_forecast = TRUE,
           source = 'OpenWeather')
  })
  
  return(resp_df)
}

#' Retrieve last 5 days weather data from the OpenWeather API
#'
#' Returns the last 5 days temperature (F), precipitation (T/F), and wind speed (mph) for Central Park, NY. Must set API token via Sys.setenv(token_openweather = '<token>') prior to use. Request token here get token here https://openweathermap.org/full-price#current.
#'
#' @return
#' @export
#'
#' @examples
#' get_openweather_historical()
get_openweather_historical <- function(){
  
  # construct call to OpenWeather API
  token <- Sys.getenv('token_openweather')
  token <- paste0('appid=', token)
  lat <- 'lat=40.7812'
  long <- 'lon=-73.9665'
  units <- 'units=imperial'
  
  # make a call for each of the last 5 days
  dates <- as.numeric(as.POSIXct(Sys.Date()-1:5))
  resp_df <- purrr::map_dfr(dates, function(dt){
    
    # finish API construction
    dt <- paste0('dt=', dt)
    args <- paste(lat, long, units, dt, token, sep = '&')
    url_base <- 'https://api.openweathermap.org/data/2.5/onecall/timemachine?'
    url_complete <- paste0(url_base, args)
    
    # make the GET request and flatten the response into a dataframe
    resp <- httr::GET(url_complete)
    resp_content <- httr::content(resp)$current
    resp_df <- tibble(date = as.Date(as.POSIXct(resp_content$dt, origin = "1970-01-01")),
                      temperature = resp_content$temp,
                      precipitation = resp_content$weather[[1]]$main == 'Rain', # not a perfect solution but seems to work
                      wind = resp_content$wind_speed,
                      is_forecast = FALSE,
                      source = 'OpenWeather')
    
    return(resp_df)
  })
  
  return(resp_df)
}

#' Retrieve historical or forecasted weather
#'
#' Returns historical or forecasted temperature (F), precipitation (T/F), and wind speed (mph) for Central Park, NY.
#'
#' @param .dates a vector of dates
#'
#' @return a dataframe with nrows == length(.dates)
#' @export
#'
#' @examples
#' dates <- seq(Sys.Date() - 10, Sys.Date() + 5, by = 'day')
#' get_weather(.dates = dates)
get_weather <- function(.dates){
  
  # figure out which dates require which API
  current_date <- Sys.Date()
  dates <- sort(unique(.dates))
  sources <- case_when(
    dates >= current_date ~ 'OpenWeather_forecast',
    dates >= (current_date - 5) ~ 'OpenWeather_historical',
    TRUE ~ 'NOAA'
  )
  
  # call the APIs and get the data
  OpenWeather_forecast <- NULL
  OpenWeather_historical <- NULL
  if ('OpenWeather_forecast' %in% sources) OpenWeather_forecast <- get_openweather()
  if ('OpenWeather_historical' %in% sources) OpenWeather_historical <- get_openweather_historical()
  if ('NOAA' %in% sources){
    # TODO: split into chunks of 1000 because that is the limit
    date_start <- min(dates)
    date_end <- max(dates[sources == 'NOAA'])
    NOAA <- get_noaa(.date_start = date_start, .date_end = date_end)
  }
  
  # construct dataframe and ensure its the same order as the original vector
  weather_data <- dplyr::bind_rows(OpenWeather_forecast, OpenWeather_historical, NOAA)
  weather_data <- left_join(tibble(date = .dates), weather_data, by = 'date')
  
  return(weather_data)
}
