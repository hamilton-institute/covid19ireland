# ------- script to make all the figures ------- #
cfr_age <- c(0.002299908, 0.009705793, 0.15, 0.314009662)*100

df_age <- data.frame(
  age_props  = c(2.38 + 5.73 + 17.34 + 16.94 + 15.69, 
                 15.27 + 10.98, 5.83 + 5.17, 4.62),
  cats  = c("0-44", "45-64", "65-84", "85+"))

library(tidyverse)

# ------- setting working directory -------#
setwd("estimate_cases")
source("data_ages.R")

#----------- making temporal variation under-reporting and testing effort figure -------------#

countries_of_interest <- c("Ireland")

files <- c("results_IRL_age1.rds", "results_IRL_age2.rds", 
           "results_IRL_age3.rds", "results_IRL_age4.rds")

prop_cases <- df_age$age_props/100

under_reporting_data_all <- map2_df(.x = files, .y = prop_cases, 
                                    getFigure1UnderReportingData)
iso_codes_of_interest <- c("IRL")

plot_dat_under_reporting <- under_reporting_data_all %>%
  filter(date < "2020-08-31") %>% 
  dplyr::filter(iso_code == iso_codes_of_interest) %>%
  dplyr::mutate(estimate = estimate*100,
                lower = lower*100,
                upper = upper*100) %>% 
  tidyr::drop_na() %>% 
  mutate(file = as.factor(file))

levels(plot_dat_under_reporting$file) <- 
  paste0('Age group: ', df_age$cats, ",\n CFR: ", round(cfr_age, 2), "%", 
         ", Case proportion: ", round(df_age$age_props, 1), "%")


plot_dat_under_reporting %>% 
  ggplot(aes(x = date, y = estimate)) +
  geom_ribbon(
    aes(ymin = lower, 
        ymax = upper), fill = "dodgerblue", alpha = 0.6) + 
  geom_line(aes(y = estimate), 
            linetype = 2, colour = "black", size = 0.8) +
  facet_wrap(~file) +
  ggplot2::scale_x_date(date_breaks = "1 month", 
                        labels = scales::date_format("%d-%b"),
                        limits = as.Date(c('2020-03-15','2020-08-31'))) + 
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  theme_bw(17) +
  theme(strip.background =element_rect(fill="white")) +
  ggplot2::labs(
    y = "Estimate of symptomatic \n cases reported (%)", 
    x = "Date") 

ggplot2::ggsave("presentation/figure_3_Ireland_ages_filter.png",
                #plot = figure2, 
                width = 12,
                height = 8,
                units = "in")

#-------------------------------------------------------------------------


files <- c("results_IRL_age1.rds", "results_IRL_age2.rds", 
           "results_IRL_age3.rds", "results_IRL_age4.rds")

prop_cases <- df_age$age_props/100

all_adjusted_case_data <- map2_df(.x = files, .y = prop_cases, 
                                    getAdjustedCaseDataNational)

  
options(scipen = 999)

plot_3_dat <- all_adjusted_case_data %>%
  dplyr::group_by(country) %>%
  dplyr::filter(country %in% "Ireland") %>% 
  dplyr::filter(new_cases > 1 & new_cases_adjusted_smooth_mid > 1 & new_cases_adjusted_smooth_low > 1 & new_cases_adjusted_smooth_high > 1) %>% 
  mutate(file = as.factor(file))


levels(plot_3_dat$file) <- 
  paste0('Age group: ', df_age$cats, ",\n CFR: ", round(cfr_age, 2), "%", 
         ", Case proportion: ", round(df_age$age_props, 1), "%")

plot_3_dat %>%
  ggplot2::ggplot() + 
  ggplot2::geom_line(ggplot2::aes(x = date, y = new_cases_smoothed)) + 
  ggplot2::geom_ribbon(ggplot2::aes(x = date,
                                    ymin = new_cases_adjusted_smooth_low, 
                                    ymax = new_cases_adjusted_smooth_high), fill = "dodgerblue", alpha = 0.6) + 
  ggplot2::geom_line(ggplot2::aes(x = date, y = new_cases_adjusted_smooth_mid), linetype = 2, colour = "royalblue", size = 0.5) + 
  ggplot2::labs(x = "", y = "") + 
  facet_wrap(~file, scales = 'free') +
  ggplot2::scale_y_log10(breaks = c(
    1, 10, 30, 100, 250, 500, 1000, 2000, 5000, 10000)) +
  ggplot2::scale_x_date(date_breaks = "4 weeks", 
                          labels = scales::date_format("%d-%b"),
                          limits = as.Date(c('2020-03-15','2020-08-28'))) + 
    theme_bw(18) + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 15)) + 
    ggplot2::theme(legend.position="none",
                   plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")) + 
    ggplot2::labs(
      #tag = "C",
      x = "", y = "Cases by date of confirmation") + 
  theme(strip.background =element_rect(fill = "white")) +
    viridis::scale_color_viridis(discrete = TRUE, begin = 0, end = 0.82) 

ggplot2::ggsave("presentation/figure_2_Ireland_ages_filter.png",
                #plot = figure2, 
                width = 12,
                height = 8.5,
                units = "in")


#---------------------------------------------------------------------------
