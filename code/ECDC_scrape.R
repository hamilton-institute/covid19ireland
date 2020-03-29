# Scrape data from the European centre for disease control 
# taken from https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

library(readxl)
library(httr)
library(tidyverse)

#create the URL where the dataset is stored with automatic updates every day

url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")

# Just in case you need to run it before they've updated that day - put the correct date in below
#url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "2020-03-28"), ".xlsx", sep = "")


#download the dataset from the website to a local temporary file
GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))

#read the Dataset sheet into “R”
data <- read_excel(tf)

# Output to the scrape folder
saveRDS(data, file = paste0('data/scraped/ECDC_data_',format(Sys.time(), "%Y%m%d"),'.rds'))
saveRDS(data, file = paste0('shiny/covid19_hamilton/ECDC_data_current.rds'))
saveRDS(data, file = paste0('shiny/covid19_hamilton_v2/ECDC_data_current.rds'))
