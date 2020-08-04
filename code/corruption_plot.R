# In this script I'm going to download the latest data, calculate the CFR, and then plot against the corruption index from:
# https://www.transparency.org/en/cpi/2019/results

# Clear workspace and load in packages
rm(list = ls())
library(tidyverse)
library(tidycovid19)
library(readxl)
library(plotly)

# Load in the tidy covid data
#data = download_jhu_csse_covid19_data(silent = TRUE)
#data = download_ecdc_covid19_data(silent = TRUE)
data = download_merged_data(silent = TRUE, cached = TRUE)

# Load in the CPI data
cpi_data = read_excel('data/CPI2019.xlsx', skip = 2)

# Just get the latest cumulative totals
data_use = data %>% 
  filter(date == max(date)) %>% 
  left_join(cpi_data %>% select(Country, `CPI score 2019`),
            by = c("country" = "Country")) %>% 
  mutate(CFR = round(100*ecdc_deaths/ecdc_cases,1)) %>% 
  filter(ecdc_cases > 10000)

# Now create a plot
p = ggplot(data_use, aes(x = CFR, y = `CPI score 2019`, label = country,
                         colour = region)) + 
  geom_text() + 
  scale_x_log10() + 
  labs(y = 'Corruption Perception Index 2019 (higher = less corrupt)',
       x = 'Case fatality rate %',
       title = 'All countries with more than 10k cases') + 
  theme_bw()
ggplotly(p)

cor(data_use$CFR, data_use$`CPI score 2019`, use = 'pairwise.complete.obs')
cor(data_use$CFR[-c(67,73)], data_use$`CPI score 2019`[-c(67,73)], use = 'pairwise.complete.obs') # remove Singapore and Qatar
  