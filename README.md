# Forecasting 311 calls

Forecasting daily 311 call volume by agency and complaint types

<p align="center">
<img src="screenshot.png" width=80%>
</p>


### Folder structure
    .
    ├── 311_calls         # Shiny app
    ├── analysis          # Analysis scripts
    │   ├── backtest      # Time-series cross validation
    │   └── plots         # Generated plots
    ├── data              # Cleaned data, cleaning scripts, daily scripts
    ├── discard           # Deprecated scripts
    ├── inputs            # Raw input data
    └── README.md
    
    
### Modeling process

Forecast models are selected independently for each agency and complaint type. There are seven candidate time-series model families and each are evaluated via time-series cross validation using RMSE. The seven models are mean, naive, seasonal naive, drift, exponential smoothing, ARIMA, and Prophet. ARIMA model selected via stepwise process. Exogenous variables include holidays, temperature, precipitation, and wind. 

<p align="center">
<img src="analysis/plots/best_models.png" width=80%>
</p>


### Reproducibility

Run the scripts in the following order:
1) `data/clean_data.R`
2) `data/aggregated_daily.R`
3) `data/update_weather.R`
4) `analysis/backtest/run_backtest.R`
5) `analysis/pipeline.R`

Original data comes from [NYC Data](https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9). Weather data requires tokens from [NOAA API](https://www.ncdc.noaa.gov/cdo-web/webservices/v2) and [OpenWeather](https://openweathermap.org/api/one-call-api). For daily updates, `update_weather.R` and `pipeline.R` should be set up as cron jobs.
