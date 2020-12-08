rm(list = ls(all = TRUE))
library(tidyverse)
library(lubridate)
library(tidycovid19)
library(ranger) # For making predictions 

latest <- download_merged_data(silent = TRUE, cached = TRUE)

find_data <- function(latest_data = latest){
  date_max <- Sys.Date()
  seq_dates <- seq.Date(date_max - 45, date_max,  by = 1)
  
  all_data <- function(date_maxx){
  latest_data %>% 
    dplyr::mutate(cum_cases = ecdc_cases,
                  cases = c(cum_cases[1], diff(ecdc_cases))) %>% 
    dplyr::select(date, cases, country) %>% 
    dplyr::filter(date >= date_maxx - 21, date <= date_maxx) %>% 
    na.omit() %>% 
    dplyr::group_by(country) %>% 
    dplyr::mutate(
      n_ind = 1:n(), 
      R_name = paste0("R", n_ind)) %>% 
    dplyr::select(-date) %>% 
    dplyr::arrange(country) %>% 
    dplyr::ungroup() %>% 
    dplyr::as_tibble() %>% 
    tidyr::complete(R_name, fill = list(cases = NA)) %>% 
    dplyr::group_by(country) %>% 
    dplyr::arrange(country, n_ind) %>% 
    tidyr::fill(cases, .direction = "down") %>% 
    dplyr::select(-n_ind) %>% 
    tidyr::spread(R_name, cases) %>% 
    dplyr::ungroup() 
}
  
  data_seq_dates <- purrr:::map(seq_dates, all_data) %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate_if(is.numeric, scale) %>% 
    na.omit()
  data_seq_dates
  
}

countries <- latest %>% 
  split(.$country)

all_countries_data  <- countries %>% 
  purrr:::map(safely(find_data))

df <- all_countries_data %>% 
  purrr::map("result") %>% 
  dplyr::bind_rows() 


# This model uses the last 21 days of R 
model <-  read_rds("est_R0_final_model_comp.rds")
pred_country <- function(data, rf_model = model){
  pred.R <- predict(rf_model, data = data,
                    type = 'quantiles')
  df <- data.frame(
    low = pred.R$predictions[,1],
    upp = pred.R$predictions[,3],
    pred = pred.R$predictions[,2]
  ) %>% 
    dplyr::bind_cols(data)
  df
}


predictions <- pred_country(df)

saveRDS(predictions, paste0("r0_predictions_", 
                            str_replace_all(Sys.Date(), "-", "_"), ".rds"))