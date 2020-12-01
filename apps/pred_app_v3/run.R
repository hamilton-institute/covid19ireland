source("model.R")
source("global.R")
source("ui.R")
source("server.R")
shinyApp(ui = ui, server = server)

rm(list = ls())
library(rsconnect)
rsconnect::setAccountInfo(name='prof-thiagooliveira',
                          token='9C09A77AA05807E9D0841159C9CD3893',
                          secret='aNT3uvEuv7ssNNjc8jOhZXBTkMRQE+f64wvUFrII')
options(encoding = "UTF-8")
rsconnect::deployApp(appDir = getwd(),
                     account = "prof-thiagooliveira",
                     appName = "COVIDForecast",
                     server = "shinyapps.io",
                     upload = TRUE,
                     appFiles = c("global.R",
                                  "server.R",
                                  "ui.R",
                                  "shiny_data.RData",
                                  "fitted_all.RData",
                                  "data_forecast3.RData",
                                  "data_forecast2.RData",
                                  "data_forecast1.RData",
                                  "tsdist.RData",
                                  "last_60.RData"))
Y
