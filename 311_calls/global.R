library(shiny)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(leaflet)
library(ggthemes)

theme_set(theme_fivethirtyeight(base_size = 10,
                                base_family = 'arial'))

# Read in daily forecasts, best models, and yesterday's actuals data frames:
forecasts_daily <- readr::read_csv('data/forecasts_daily.csv', col_types = list(lower_80=col_double(),
                                                                                upper_80=col_double()))
best_models<- readr::read_csv('data/best_models.csv')
yest_data <- readr::read_csv('data/yesterday_data.csv') %>% 
  mutate(complaint_type = tolower(complaint_type))

# Get unique agency names and complaint types:
agency_names <- unique(forecasts_daily$agency)
complaint_types <- unique(forecasts_daily$complaint_type)

# Define function for base map:
base_map <- leaflet(yest_data) %>% 
  addProviderTiles(providers$CartoDB.Voyager,
                   options = providerTileOptions(noWrap = TRUE)) %>%
  setView(lng = -73.98928, lat = 40.75042, zoom = 10)
