library(dplyr)

# This script should be run *once* - when we choose to start running the app:
# We feed it the base, cleaned data that we downloaded from the OpenData website:

# read in the raw data -- 15gb
calls_raw <- readr::read_csv('inputs/311_Service_Requests_from_2010_to_Present.csv')

# clean up column names
colnames(calls_raw) <- janitor::make_clean_names(colnames(calls_raw))

# clean up agency and complain names
calls_raw$agency <- iconv(calls_raw$agency, from = "UTF-8", to = "ASCII", sub = '')
calls_raw$complaint_type <- stringr::str_to_lower(calls_raw$complaint_type)

# trim columns and filter to last four years
datetime_format <- '%m/%d/%Y %I:%M:%S %p'
calls_trimmed <- calls_raw %>% 
  mutate(created_datetime = lubridate::as_datetime(created_date, format = datetime_format),
         closed_datetime = lubridate::as_datetime(closed_date, format = datetime_format)) %>% 
  select(unique_key, created_datetime, closed_datetime, agency, complaint_type, 
         descriptor, status, resolution_description, 
         incident_zip, city, borough, latitude, longitude) %>% 
  filter(created_datetime >= max(created_datetime) - lubridate::years(4))

# write-out last 5 days of raw data for checking duplicates in pipeline:
calls_trimmed %>%
  transmute(unique_key, created_date = lubridate::as_date(created_datetime)) %>% 
  filter(created_date >= max(created_date) - lubridate::days(5)) %>% 
  readr::write_csv('data/last_five_days_raw.csv')

# write out
readr::write_csv(calls_trimmed, 'data/311_cleaned.csv')
