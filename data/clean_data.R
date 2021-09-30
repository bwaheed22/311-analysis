library(dplyr)

# read in the raw data -- 15gb
calls_raw <- readr::read_csv('inputs/311_Service_Requests_from_2010_to_Present.csv')

# write-out last 5 days of raw data for checking duplicates in pipeline:
last_five_days <- calls_raw %>% 
  filter(created_date >= max(created_date) - lubridate::days(5))
readr::write_csv(last_five_days, 'data/last_five_days_raw.csv')

# clean up column names
colnames(calls_raw) <- janitor::make_clean_names(colnames(calls_raw))

# trim columns and filter to last four years
datetime_format <- '%m/%d/%Y %I:%M:%S %p'
calls_trimmed <- calls_raw %>% 
  mutate(created_datetime = lubridate::as_datetime(created_date, format = datetime_format),
         closed_datetime = lubridate::as_datetime(closed_date, format = datetime_format)) %>% 
  select(unique_key, created_datetime, closed_datetime, agency, complaint_type, 
         descriptor, status, resolution_description, 
         incident_zip, city, borough, latitude, longitude) %>% 
  filter(created_datetime >= max(created_datetime) - lubridate::years(4))


calls_raw$agency <- iconv(calls_raw$agency, from = "UTF-8", to = "ASCII", sub = '')
calls_raw$complaint_type <- stringr::str_to_lower(calls_raw$complaint_type)

# write out
readr::write_csv(calls_trimmed, 'data/311_cleaned.csv')
