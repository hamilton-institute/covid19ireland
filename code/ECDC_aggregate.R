# -------------------------------
# A simple scrip to aggregate worldwide cases
# -------------------------------
library(tidyverse)

# The file name should be in a format like:
current_date <- Sys.Date()
month <- lubridate::month(current_date)
day <- lubridate::day(current_date)
file_name <- paste0("ECDC_data_20200", month, day, '.rds')

update_aggregated_data <- function(file_name = file_name){
  path <- paste0("data/scraped/", file_name)
  df <- read_rds(path)

  df %>% 
    group_by(DateRep) %>% 
    summarise(new_cases = sum(Cases)) %>% 
    mutate(total_cases = cumsum(new_cases)) %>% 
    arrange(desc(DateRep))

}

saveRDS(
  update_aggregated_data(file_name),
  file = paste0("data/scraped/",
    sub(x = file_name, "\\.rds", replacement = ""), 
    "_aggregated.rds"))
  