library(RSocrata)
wd <- "~/Dropbox/Data/Projects/311-analysis/"

# get today's date
date_today <- Sys.Date()
time_now <- Sys.time()

# construct API query
url <-"https://data.cityofnewyork.us/resource/erm2-nwe9.json?"
args <- paste0("$where=created_date between '", 
               "2021-09-22", "T00:00:00.000' and '", 
                date_today, "T00:00:00.000'", 
                collapse = "")
query <- paste0(url, args, collapse = "")

# call the API
previous_data <- RSocrata::read.socrata(query)
previous_data$agency <- iconv(previous_data$agency, from = "UTF-8", to = "ASCII", sub = "")

# write out the data
file_name <- paste0(wd, 'data/api_test/pulled_data_', gsub("-|:| ", "", time_now), '.csv')
readr::write_csv(previous_data, file = file_name)

# files <- list.files('data/api_test', '*.csv')
# df <- purrr::map_dfr(files, function(file){
#   readr::read_csv(file.path('data', 'api_test', file),
#                   col_types = list(incident_zip = readr::col_character())) %>%
#     mutate(file = file)
# }) %>% 
#   mutate(file_time = stringr::str_extract(file, '[0-9]{14}'),
#          file_year = stringr::str_sub(file_time, 1, 4),
#          file_month = stringr::str_sub(file_time, 5, 6),
#          file_day = stringr::str_sub(file_time, 7, 8),
#          file_hour = stringr::str_sub(file_time, 9, 10),
#          file_minute = stringr::str_sub(file_time, 11, 12),
#          file_second = stringr::str_sub(file_time, 13, 14),
#          across(c(file_year, file_month, file_day, file_hour, file_minute, file_second), as.integer),
#          file_datetime = lubridate::make_datetime(file_year, file_month, file_day, file_hour, file_minute, file_second))
# 
# df %>%
#   group_by(file_datetime) %>%
#   tally() %>%
#   ggplot(aes(x = file_datetime, y = n)) +
#   geom_line() +
#   geom_point() +
#   scale_x_datetime(date_breaks = '12 hours', date_labels = '%I %p') +
#   scale_y_continuous(labels = scales::comma_format()) +
#   labs(title = 'Data updates occur roughly around 10am-1pm every day but is not guaranteed',
#        x = 'Time of day',
#        y = 'Number of records')
# 
# df %>% 
#   group_by(file_datetime,
#            date = lubridate::date(created_date)) %>% 
#   tally() %>%
#   ungroup() %>% 
#   distinct(date, n, .keep_all = TRUE) %>% 
#   ggplot(aes(x = date, y = n)) +
#   geom_col() + 
#   facet_wrap(~as.character(file_datetime), ncol = 1) +
#   labs(title = 'Number of records by date by the time the API was called',
#        subtitle = "Only 'changepoints' shown",
#        x = NULL,
#        y = 'Number of records')