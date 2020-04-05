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
#   dates = as_datetime(Sys.time(), tz = "Europe/Dublin")
# )
last_updated = read_csv('last_updated.csv')

# ECDC Scrape -------------------------------------------------------------

cat('Scraping ECDC...\n')
source("scrape_scripts/ECDC_scrape.R")

# See if it failed - if not keep going
if(any(class(ecdc_data) != "try-error")) {
  ecdc_old = readRDS(file = 'latest_ECDC_data.rds')
  
  # If not identical update the saved file and update the latest data set
  if(!identical(ecdc_data, ecdc_old)) {
    # Update scraped data
    last_updated$dates[1] = as_datetime(Sys.time(), tz = "Europe/Dublin")
    # Output to the scrape folder
    saveRDS(ecdc_data, file = paste0('latest_ECDC_data.rds'))
    # Keep an old record in case things braek
    saveRDS(ecdc_data, file = paste0('old_data/old_ECDC_data.rds'))
  }
}


# GOV_IE ------------------------------------------------------------------

cat('Scraping Irish government data...\n')
source("scrape_scripts/gov_ie_data.R")

# Compare this data file to the one that's already there
old_irish_data = readRDS(file = 'latest_irish_data.rds')

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
  map(., ~mutate_all(., as.character)) %>% 
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
  # Add in date - but only for ones where it's not NULL
  map2(., map(gov_ie_data, "published") %>%  
            purrr::discard(., 
                           .p = gov_ie_data %>% 
                             map("age_hospitalised") %>% 
                             map_lgl(is.null)), 
          ~mutate(.x, Date = .y)) %>%
  # Turn everything into character
  map(., ~mutate_all(., as.character)) %>% 
  # Bind into rows
  bind_rows() %>% 
  # Convert date
  mutate(Date = as.Date(Date, "%d %B %Y")) %>% 
  # Get rid of totals
  rename('Age' = "Hospitalised Age") %>% 
  filter(Age != "Total")

# Join age and age_hospitalised together
total_by_age_all = full_join(total_by_age,
                             total_by_age_hopitalised,
                             by = c("Age", 
                                    "Date")) %>% 
  rename(`Number of cases` = `Number of Cases.x`,
         `Number of hospitalised cases` = `Number of Cases.y`) %>% 
  select(Date, Age, `Number of cases`, `Number of hospitalised cases`) %>% 
  arrange(desc(Date), Age)

# Totals by gender
total_by_gender = gov_ie_data %>% 
  # Extract standard age 
  map(., "gender") %>% 
  # Remove the null elements at the end
  purrr::discard(is.null) %>% 
  # Add in dates for non-NULL 
  map2(., map(gov_ie_data, "published") %>%  
         purrr::discard(., 
                        .p = gov_ie_data %>% 
                          map("gender") %>% 
                          map_lgl(is.null)), 
       ~mutate(.x, Date = .y)) %>% 
  # Convert all to character
  map(., ~mutate_all(., as.character)) %>% 
  # Bind together
  bind_rows %>% 
  # Remove total rows
  filter(Gender != "Total") %>% 
  # Fix the date variable
  mutate(Date = as.Date(Date, "%d %B %Y")) %>% 
  mutate_at("Number of Cases", as.numeric)

# Totals by transmission
total_by_transmission = gov_ie_data %>% 
  # Extract standard age 
  map(., "transmission") %>% 
  # Remove the null elements at the end
  purrr::discard(is.null) %>% 
  # Add in the dates - removing the missing rowss
  map2(., map(gov_ie_data, "published") %>%  
         purrr::discard(., 
                        .p = gov_ie_data %>% 
                          map("transmission") %>% 
                          map_lgl(is.null)), 
       ~mutate(.x, Date = .y)) %>% 
  # Remove all the pc signs 
  map(., ~mutate_all(., as.character)) %>% 
  # Bind together
  bind_rows() %>% 
  # Fix the date variable
  mutate(Date = as.Date(Date, "%d %B %Y")) %>%
  # Convert number of cases to number
  mutate_at("Cases", parse_number)

latest_irish_data = list(
  totals = totals,
  total_by_county = total_by_county,
  total_by_age_all = total_by_age_all  ,
  total_by_gender = total_by_gender,
  total_by_transmission = total_by_transmission
)

# If not identical update the saved file and update the latest data set
if(!identical(latest_irish_data, old_irish_data)) {
  
  last_updated$dates[2] = as_datetime(Sys.time(), tz = "Europe/Dublin")
  # Output to the scrape folder
  saveRDS(latest_irish_data, file = paste0('latest_irish_data.rds'))
  # Keep an old copy
  saveRDS(latest_irish_data, file = paste0('old_data/old_irish_data.rds'))
}


# Save last_updated -------------------------------------------------------

write_csv(last_updated, path = 'last_updated.csv')

cat('Completed!\n')