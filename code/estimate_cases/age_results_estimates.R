# Set parameters
cfr_age <- c(0.002299908, 0.009705793, 0.009705793, 0.15, 0.314009662)*100
df_age <- data.frame(
  age_props  = c(2.38 + 5.73 + 17.34 + 16.94 + 15.69,
                 15.27 + 10.98, 5.83 + 5.17, 4.62),
  cats  = c("0-44", "45-64", "65-84", "85+"))

# First age
i = 1
# setting baseline level CFR
#CFRRow <- adjustedCFR %>% filter(iso3c == dataKey)
CFRBaseline <- cfr_age[i]
CFREstimateRange <- c( 0.8444831, 1.1940137)*CFRBaseline
# Set parameters
mean <- 13
median <- 9.1
mu <- log(median)
sigma <- sqrt(2*(log(mean) - mu))
# Hospitalisation to death distribution
hospitalisation_to_death_truncated <- function(x) {
  plnorm(x + 1, mu, sigma) - plnorm(x, mu, sigma)
}
plot_country_name <- someDat$country %>% unique()
someDat <- allDat %>% filter(country_code == dataKey)
# how to correct by case fatality rate?
#someDat$new_deaths <- round(someDat$new_cases * cfr_age[i]/100)
someDat$new_cases <- round(someDat$new_cases * df_age$age_props[i]/100)
cf_rate <- CFRRow %>% select(cfr_mid) %>% as.numeric()
someDat$new_deaths <- round(someDat$new_deaths * cfr_age[i]/cf_rate)

plot_data <- get_plot_data(country_name = plot_country_name,
                           data = someDat, CFRBaseline = CFRBaseline)
# Filter until 2020-08-31
plot_data <-  plot_data %>% filter(date < "2020-08-31")

prediction <- run_bayesian_model(data = plot_data)
i = 1
saveRDS(prediction, paste0("results_IRL_age", i, ".rds"))
#------------------------------------------------------------------------
# Second age
i = 2
# setting baseline level CFR
#CFRRow <- adjustedCFR %>% filter(iso3c == dataKey)
CFRBaseline <- cfr_age[i]
CFREstimateRange <- c( 0.8444831, 1.1940137)*CFRBaseline
# Set parameters
mean <- 13
median <- 9.1
mu <- log(median)
sigma <- sqrt(2*(log(mean) - mu))
# Hospitalisation to death distribution
hospitalisation_to_death_truncated <- function(x) {
  plnorm(x + 1, mu, sigma) - plnorm(x, mu, sigma)
}
plot_country_name <- someDat$country %>% unique()
someDat <- allDat %>% filter(country_code == dataKey)
# how to correct by case fatality rate?
#someDat$new_deaths <- round(someDat$new_cases * cfr_age[i]/100)
someDat$new_cases <- round(someDat$new_cases * df_age$age_props[i]/100)
cf_rate <- CFRRow %>% select(cfr_mid) %>% as.numeric()
someDat$new_deaths <- round(someDat$new_deaths * cfr_age[i]/cf_rate)

plot_data <- get_plot_data(country_name = plot_country_name,
                           data = someDat, CFRBaseline = CFRBaseline)
# Filter until 2020-08-31
plot_data <-  plot_data %>% filter(date < "2020-08-31")

prediction <- run_bayesian_model(data = plot_data)
i = 2
saveRDS(prediction, paste0("results_IRL_age", i, ".rds"))

#------------------------------------------------------------------------
# Third age
i = 3
# setting baseline level CFR
#CFRRow <- adjustedCFR %>% filter(iso3c == dataKey)
CFRBaseline <- cfr_age[i]
CFREstimateRange <- c( 0.8444831, 1.1940137)*CFRBaseline
# Set parameters
mean <- 13
median <- 9.1
mu <- log(median)
sigma <- sqrt(2*(log(mean) - mu))
# Hospitalisation to death distribution
hospitalisation_to_death_truncated <- function(x) {
  plnorm(x + 1, mu, sigma) - plnorm(x, mu, sigma)
}
plot_country_name <- someDat$country %>% unique()
someDat <- allDat %>% filter(country_code == dataKey)
# how to correct by case fatality rate?
#someDat$new_deaths <- round(someDat$new_cases * cfr_age[i]/100)
someDat$new_cases <- round(someDat$new_cases * df_age$age_props[i]/100)
cf_rate <- CFRRow %>% select(cfr_mid) %>% as.numeric()
someDat$new_deaths <- round(someDat$new_deaths * cfr_age[i]/cf_rate)

plot_data <- get_plot_data(country_name = plot_country_name,
                           data = someDat, CFRBaseline = CFRBaseline)
# Filter until 2020-08-31
plot_data <-  plot_data %>% filter(date < "2020-08-31")

prediction <- run_bayesian_model(data = plot_data)
i = 3
saveRDS(prediction, paste0("results_IRL_age", i, ".rds"))

#------------------------------------------------------------------------
# Fourth age
i = 4
# setting baseline level CFR
#CFRRow <- adjustedCFR %>% filter(iso3c == dataKey)
CFRBaseline <- cfr_age[i]
CFREstimateRange <- c( 0.8444831, 1.1940137)*CFRBaseline
# Set parameters
mean <- 13
median <- 9.1
mu <- log(median)
sigma <- sqrt(2*(log(mean) - mu))
# Hospitalisation to death distribution
hospitalisation_to_death_truncated <- function(x) {
  plnorm(x + 1, mu, sigma) - plnorm(x, mu, sigma)
}
plot_country_name <- someDat$country %>% unique()
someDat <- allDat %>% filter(country_code == dataKey)
# how to correct by case fatality rate?
#someDat$new_deaths <- round(someDat$new_cases * cfr_age[i]/100)
someDat$new_cases <- round(someDat$new_cases * df_age$age_props[i]/100)
cf_rate <- CFRRow %>% select(cfr_mid) %>% as.numeric()
someDat$new_deaths <- round(someDat$new_deaths * cfr_age[i]/cf_rate)

plot_data <- get_plot_data(country_name = plot_country_name,
                           data = someDat, CFRBaseline = CFRBaseline)
# Filter until 2020-08-31
plot_data <-  plot_data %>% filter(date < "2020-08-31")

prediction <- run_bayesian_model(data = plot_data)
i = 4
saveRDS(prediction, paste0("results_IRL_age", i, ".rds"))
