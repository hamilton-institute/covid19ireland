# Script to try and download all the data in one script. 
# It aims to 
# Download each of the sources as best as possible in a neat and well-named format
# Creae a file or note of when each file was last updated so as to provide better updates

# This file should always be run with the working directory set in the shiny folder
#setwd("~/GitHub/covid19ireland/shiny/covid19_hamilton")

# First clear the workspace and load in packages
rm(list = ls())
library(tidyverse)

# Create a holder to determine when the data were updated
# last_updated = tibble(
#   data_set = c("ECDC", "GOV_IE"),
#   dates = as.Date("2020/4/1")
# )
last_updated = readRDS("last_updated.rds")

# ECDC Scrape -------------------------------------------------------------

source("scrape_scripts/ECDC_scrape.R")

# Compare this data file to the one that's already there
ecdc_old = readRDS(file = 'ECDC_data_current.rds')

# If not identical update the saved file and update the latest data set
if(!identical(ecdc_data, ecdc_old)) {
  
  last_updated$dates[1] = as_datetime(Sys.time(), tz = "Europe/Dublin")
  # Output to the scrape folder
  saveRDS(data, file = paste0('ECDC_data_current.rds'))
}


# GOV_IE ------------------------------------------------------------------

source("scrape_scripts/gov_ie_data.R")

# Compare this data file to the one that's already there
gov_ie_old = readRDS(file = 'gov_ie_current.rds')

# Sort this data out into a neater tibble - Must be a better way to do this - Bruna?
# First list is for daily cases/hospitalised/ICU for Ireland
totals = gov_ie_data %>% 
  # Extract the totals
  map(.,  "totals") %>% 
  # Convert to characters as formats all over the place
  map(~mutate(., no_of_cases = as.character(`Number of Cases`))) %>%
  # Add the dates 
  map(~select(., Totals, no_of_cases)) %>% # Select only the useful data
  # Add the dates 
  map2_df(., map(gov_ie_data, "published"), ~ mutate(.x, Date = .y)) %>% 
  # Turn it into a tibble
  bind_rows %>% 
  # Separate out columns
  pivot_wider(names_from = Totals, values_from = no_of_cases) %>% 
  # Coalesce the weirdly-named variables
  mutate(`Total number of deaths` = coalesce(`Total number of deaths`,
                                             `Total number deaths`) ,
         `Total number of healthcare workers` = 
           coalesce(`Total number of healthcare workers`,
                    `Total number healthcare workers`)) %>% 
  # Remove the old ones
  select(-`Total number deaths`, -`Total number healthcare workers`) %>%  
  # Fix the date variable
  mutate(Date = as.Date(Date, "%d %B %Y")) %>% 
  # Remove the commas and turn back to numeric
  mutate_at(vars(-Date), ~ gsub(",", "", .)) %>% 
  mutate_at(vars(-Date), ~as.numeric(.))

total_by_county = gov_ie_data %>% 
  # Extract the totals
  map(.,  "counties") %>% 
  # Convert everything to character
  map(. ~mutate_all(as.character))
  # Add the dates 
  map2_df(., map(gov_ie_data, "published"), ~ mutate(.x, Date = .y)) %>% 
  # Fix the date variable
  mutate(Date = as.Date(Date, "%d %B %Y"))


total_by_age = gov_ie_data %>% 
  # Extract standard age 
  map(., "age") %>% 
  # Add in the dates
  map2_df(., map(gov_ie_data, "published"), ~mutate(.x, Date = .y)) %>% 
  # Remove total rows
  filter(Age != "Total") %>% 
  # Fix the date variable
  mutate(Date = as.Date(Date, "%d %B %Y"))

# Create hospitalised data
total_by_age_hopitalised = gov_ie_data %>% 
  # Extract standard age 
  map(., "age_hospitalised") %>% 
  # Remove the two null elements at the end
  purrr::discard(is.null) %>% 




%>% 
  # map2_df(., 
  #         , 
  #                     ~ mutate(.x, Date = .y))              
  #           
  #             
  #           
  #           map(~mutate(., no_of_cases = as.character(`Number of Cases`))) %>% 
  #             bind_rows(),
  #           by = )
  

  
# Collect only those variables where age_hospitalised is found



map2_df(., map(gov_ie_data, "published"), ~ mutate(.x, Date = .y)) %>% 
  filter(Age != "Total")
  


# If not identical update the saved file and update the latest data set
if(!identical(gov_ie_data, gov_ie_old)) {
  
  last_updated$dates[2] = as_datetime(Sys.time(), tz = "Europe/Dublin")
  # Output to the scrape folder
  saveRDS(gov_ie_data, file = paste0('gov_ie_current.rds'))
}

# Save last_updated -------------------------------------------------------

saveRDS(last_updated, "last_updated.rds")
