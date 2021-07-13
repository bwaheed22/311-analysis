library(dplyr)

# read in the raw data -- 15gb
calls_raw <- readr::read_csv('inputs/311_Service_Requests_from_2010_to_Present.csv')

# clean up column names
colnames(calls_raw) <- janitor::make_clean_names(colnames(calls_raw))

# trim columns and filter to last four years
datetime_format <- '%m/%d/%Y %I:%M:%S %p'
calls_trimmed <- calls_raw %>% 
  mutate(created_datetime = lubridate::as_datetime(created_date, format = datetime_format),
         closed_datatime = lubridate::as_datetime(closed_date, format = datetime_format)) %>% 
  select(unique_key, created_datetime, closed_datatime, agency, complaint_type, 
         descriptor, status, resolution_description, 
         incident_zip, city, borough, latitude, longitude) %>% 
  filter(created_datetime >= max(created_datetime) - lubridate::years(4))

# write out
readr::write_csv(calls_trimmed, 'data/311_cleaned.csv')
