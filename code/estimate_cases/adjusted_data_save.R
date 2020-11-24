all_adjusted_case_data <- getAdjustedCaseDataNational()

plot_3_dat <- all_adjusted_case_data %>%
  dplyr::group_by(country) %>%
  dplyr::filter(country %in% "Ireland") %>% 
  dplyr::filter(new_cases > 1 & new_cases_adjusted_smooth_mid > 1 & new_cases_adjusted_smooth_low > 1 & new_cases_adjusted_smooth_high > 1)

saveRDS(plot_3_dat, "adjusted_cases.rds")
