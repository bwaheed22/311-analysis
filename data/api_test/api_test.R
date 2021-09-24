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
#   readr::read_csv(file.path('data', 'api_test', file)) %>% 
#     mutate(file = file)
# })
