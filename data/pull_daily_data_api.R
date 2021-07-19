# Pulling dauly data using Socrata API:
# Documentation: https://dev.socrata.com/foundry/data.cityofnewyork.us/erm2-nwe9

# Load RSocrata package (easy interaction with Socrata open data portals for R)
# https://cran.r-project.org/web/packages/RSocrata/index.html

# install.packages("RSocrata")
library(RSocrata)

# Set URL and query - query should be run everyday to pull all of yesterday's data:
url <-"https://data.cityofnewyork.us/resource/erm2-nwe9.json?"
query <- paste0("$where=created_date between '", 
                Sys.Date()-1,"T00:00:00.000' and '", 
                Sys.Date(), "T00:00:00.000'", collapse = "")

# Pull data:
todays_data <- RSocrata::read.socrata(paste0(url, query, collapse = ""))
