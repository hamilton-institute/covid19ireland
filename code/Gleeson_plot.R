# Create a plot of daily cases where axis is days from 29th Feb and daily deaths using the scraped Irish data
# Axis needs to run from 1 to 100 on the horizontal and 0 to 800 on vetical

# Clear workspace and load in packages
rm(list = ls())
library(tidyverse)
library(readxl)

# Load Irish data
latest_irish_data <- read_excel("latest_irish_data.xlsx", sheet = 'totals') %>% 
  mutate(days_from_28_2 = as.Date(Date) - as.Date("2020/02/28")) %>% 
  filter(Status == 'Confirmed')

# Plot
ggplot(latest_irish_data, aes(x = days_from_28_2, y = `Daily cases`)) + 
  geom_point() + 
  theme_bw() + 
  scale_x_continuous(limits = c(0, 100)) + 
  scale_y_continuous(limits = c(0 ,800))
