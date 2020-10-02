library(tidyverse)
library(TSclust)
library(ape)
library(ggrepel)
library(DescTools)
library(directlabels)
#=======================================================================
# Forecast
#=======================================================================
load("shiny_data.RData")
shiny_data$Country <- as.factor(gsub(pattern = "_", replacement = " ",
                                     shiny_data$Country))
shiny_data$Indicator <- as.factor(shiny_data$Indicator)
levels(shiny_data$Indicator) <- c("Observation", "Forecast")
#=======================================================================
# Dendrogram
#=======================================================================
load("fitted_all.RData")
fitted_values$country <- as.factor(fitted_values$country)
fitted_values$country <- as.factor(gsub(pattern = "_", replacement = " ",
                                        fitted_values$country))
last_60 <- fitted_values %>%
  dplyr::filter(as.numeric(day) -
                  max(as.numeric(fitted_values$day)) + 61 > 0) %>%
      dplyr::select(day, country, ar) %>%
      pivot_wider(id_cols = 1:2,
                  names_from = "country",
                  values_from = "ar") %>%
      dplyr::select(-day)
tsdist <- diss(t(last_60), "DTWARP")
names(tsdist) <- colnames(last_60)
hc <- hclust(tsdist, "ward.D2")
#=======================================================================
# data_forecast
#=======================================================================
load("data_forecast3.RData")
data_forecast1 <- data_forecast
data_forecast1$Type  <- "First Validation"
data_forecast1$day2 <- as.factor(data_forecast1$day)
levels(data_forecast1$day2) <- paste(1:7, "ahead")
levels(data_forecast1$country)[c(176, 196, 198)] <-
  c("South Korea", "UK", "USA")
#-----------------------------------------------------------------------
load("data_forecast2.RData")
data_forecast2 <- data_forecast
data_forecast2$Type  <- "Second Validation"
data_forecast2$day2 <- as.factor(data_forecast2$day)
levels(data_forecast2$day2) <- paste(1:7, "ahead")
levels(data_forecast2$country)[c(177, 198, 200)] <-
  c("South Korea", "UK", "USA")
#-----------------------------------------------------------------------
load("data_forecast1.RData")
data_forecast3 <- data_forecast
data_forecast3$Type  <- "Third Validation"
data_forecast3$day2 <- as.factor(data_forecast3$day)
levels(data_forecast3$day2) <- paste(1:7, "ahead")
levels(data_forecast3$country)[c(178, 199, 201)] <-
  c("South Korea", "UK", "USA")
#-----------------------------------------------------------------------
data_forecast <- rbind(data_forecast1, data_forecast2, data_forecast3)
#-----------------------------------------------------------------------
forecast_summary <- data_forecast %>%
  group_by(day) %>%
  summarise(correlation = cor(Y_obs, Y_forecast),
            concordance = CCC(Y_obs, Y_forecast)$rho.c[1]$est,
            accuracy = CCC(Y_obs, Y_forecast)$rho.c[1]$est/
                                            cor(Y_obs, Y_forecast),
            log10_rss = log10(sum((Y_obs - Y_forecast)^2)))

data_ccc <- forecast_summary %>%
  gather("Type",  "Value",  correlation, concordance, accuracy)
data_ccc$Method <- as.factor(data_ccc$Type)
levels(data_ccc$Type) <- c("CCC", "r", "Cb")
data_ccc$day2 <- rep(seq(1, 7, 1), 3 * 3)
data_ccc$Type <- rep(gl(3, 7, labels = c("First Validation",
                        "Second Validation", "Third Validation")),3)
#=======================================================================
