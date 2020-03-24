library(plotly)
library(tidyverse)
library(lubridate)

age.hosp.data <- read.csv('data/scraped/hospitalised_by_age.csv')

x<-as.character(age.hosp.data$age)



y<-as.numeric(age.hosp.data$number.hospitalised)

text<- paste0(age.hosp.data$number.hospitalised, ' patients')


data <- data.frame(x, y, text)
data$x <- factor(data$x, levels = as.character(age.hosp.data$age))


fig <- plot_ly(data, x = ~x, y = ~y, type = 'bar', text = text,
               marker = list(color = 'rgb(158,202,225)',
                             line = list(color = 'rgb(8,48,107)',
                                         width = 1.5)))
fig <- fig %>% layout(title = "Number of Patients Hospitalised by Age",
                      xaxis = list(title = "Age Range"),
                      yaxis = list(title = "Count"))

fig
