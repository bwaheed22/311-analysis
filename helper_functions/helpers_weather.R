# source('helper_functions/secrets.R')

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
  if (.date_end - .date_start > 365) stop('Date range must be less than a year due to API restrictions')
  
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
#' #Sys.setenv(token_openweather = '<token>')
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
#' @references https://openweathermap.org/api/one-call-api
#'
#' @examples
#' #Sys.setenv(token_openweather = '<token>')
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
#' #Sys.setenv(token_noaa = '<token>')
#' #Sys.setenv(token_openweather = '<token>')
#' dates <- seq(Sys.Date() - 10, Sys.Date() + 5, by = 'day')
#' get_weather(.dates = dates)
get_weather <- function(.dates){
  
  if (!inherits(as.Date(.dates), 'Date')) stop('.dates must be coercible to date format')
  
  # figure out which dates require which API
  current_date <- Sys.Date()
  dates <- sort(unique(.dates))
  sources <- dplyr::case_when(
    dates >= current_date ~ 'OpenWeather_forecast',
    dates >= (current_date - 5) ~ 'OpenWeather_historical',
    TRUE ~ 'NOAA'
  )
  
  if (length(dates) > 1000) stop('.dates vector exceeds 1000 days between min and max date. API limited to 1000 days.')
  
  # call the APIs and get the data
  OpenWeather_forecast <- NULL
  OpenWeather_historical <- NULL
  NOAA <- NULL
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
  weather_data <- dplyr::left_join(tibble(date = .dates), weather_data, by = 'date')
  
  return(weather_data)
}
