library(dplyr)
library(fable)

# Calculate forecasts (1-day ahead) for each agency/complaint type pair 
# using optimal forecast method

best_models <- readr::read_csv("analysis/backtest/best_models.csv")
calls_daily <- readr::read_csv('data/311_cleaned_daily.csv')

calls_forecast <- function(data, agency_name) {
  
  # create proper time series dataframe, filling in NAs and gaps
  # with last value carried forward
  
  data <- data %>% 
    tsibble::as_tsibble(key = c('agency', 'complaint_type'), 
                        index = 'date') %>% 
    tsibble::fill_gaps() %>% 
    tidyr::fill(n, .direction = "down")
  
  # filter data to specific agency:
  data <- data %>% filter(agency == agency_name)
  
  # pull all unique complaint types within specific agency:
  complaint_names <- unique(data$complaint_type)
  
  # cycle through complaint types, select optimal model,
  # and create point forecasts:
  
  forecasts = list()
  
  for (i in 1:length(complaint_names)) {
    
    df <- data %>% 
      filter(complaint_type == complaint_names[i])
    
    best_model <- best_models %>% 
      filter(agency == agency_name, complaint_type == complaint_names[i]) %>% 
      select(best_model) %>% 
      pull()
    
    best_model <- switch(best_model, "mean" = "MEAN(n)",
                         "naive" = "NAIVE(n)",
                         "snaive" = "SNAIVE(n)",
                         "ets" = "ETS(n)",
                         "arima" = "ARIMA(n)",
                         # nnts = NNETAR(n),
                         "prophet" = "prophet(n ~ growth('linear') + season('week', type = 'additive') + season('year', type = 'additive'))")
    
    fit <- model(df, eval(parse(text = best_model)))
    
    f1 <- forecast(fit, h = 1, level = 95)
    
    # store forecasts in a list, and label each with complaint name:
    forecasts[[i]] <- f1
    names(forecasts)[i] <- complaint_names[i]
    
  }
  
  return(forecasts)
}