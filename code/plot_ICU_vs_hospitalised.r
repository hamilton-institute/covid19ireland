library(plotly)
library(tidyverse)
library(lubridate)



all_tables <- readRDS("~/OneDrive - Maynooth University/github-projects/covid19ireland/shiny/covid19_hamilton/all_tables_current.rds")






hosp.data <- read.csv('data/scraped/gov_hospital_data.csv')
hosp.data$dates <- dmy(hosp.data$dates)


fig <- plot_ly(x = ~ hosp.data$dates) %>% 
    add_lines(y = ~ hosp.data$Total.number.hospitalised, , text = paste(hosp.data$Total.number.hospitalised, " patients in hospital"), name = "Hospitalised Patients") %>%
    add_lines(y = ~ hosp.data$Total.number.admitted.to.ICU, , text = paste(hosp.data$Total.number.admitted.to.ICU, " patients in ICU"), name = "ICU Patients") 
     

fig <- fig %>% layout(title = 'Hospitalised Patients', yaxis = list(title = "Count"), xaxis = list(title = "Date"))
fig








