# In this script I'm going to download the latest data, calculate the CFR, and then plot against the corruption index from:
# https://www.transparency.org/en/cpi/2019/results

# Clear workspace and load in packages
rm(list = ls())
library(tidyverse)
library(remotes)
remotes::install_github('joachim-gassen/tidycovid19')
library(tidycovid19)
library(readxl)
library(plotly)
library(htmltab)

# Load in the tidy covid data
#data = download_jhu_csse_covid19_data(silent = TRUE)
#data = download_ecdc_covid19_data(silent = TRUE)
data = download_merged_data()

# Load in the CPI data
cpi_data = read_excel('data/CPI2019.xlsx', skip = 2) %>% 
  mutate(Country=recode(Country, 
                        "United States of America"="United States"))

# Just get the latest cumulative totals
data_use = data %>% 
  filter(date == max(date)) %>% 
  left_join(cpi_data %>% select(Country, `CPI score 2019`),
            by = c("country" = "Country")) %>% 
  mutate(CFR = round(100*ecdc_deaths/ecdc_cases,1),
         `Deaths/confirmed` = paste(ecdc_deaths, '/',ecdc_cases)) %>% 
  filter(ecdc_cases > 10000)

# Now create a plot
p = ggplot(data_use, aes(x = CFR, y = `CPI score 2019`,
                         colour = region, label2 = `Deaths/confirmed`)) + 
  geom_text(aes(label = country)) + 
  scale_x_log10() + 
  labs(y = 'Corruption Perception Index 2019 (higher = less corrupt)',
       x = 'Case fatality rate %',
       title = 'Corruption vs CFR: All countries with more than 10k cases') + 
  theme_bw()
ggplotly(p)

cor(data_use$CFR, data_use$`CPI score 2019`, use = 'pairwise.complete.obs')
cor(data_use$CFR[-c(68,74)], data_use$`CPI score 2019`[-c(68,74)], use = 'pairwise.complete.obs') # remove Singapore and Qatar
  
# try it with Health system performance - THESE ARE RANKINGS - BROKEN PLOTS
data_hc = htmltab("https://en.wikipedia.org/wiki/World_Health_Organization_ranking_of_health_systems_in_2000",1)

data_use2 = data_use %>% 
  left_join(data_hc %>% select(Country, `Performance / Overall health system performance`,
                               `Health expenditure per capita in international dollars`),
            by = c("country" = "Country")) %>% 
  mutate("Health system score" = parse_number(`Performance / Overall health system performance`),
         "Health expenditure ($)" = parse_numsber(`Health expenditure per capita in international dollars`))
p2 = ggplot(data_use2, aes(x = CFR, y = `Health expenditure ($)`, label = country,
                         colour = region)) + 
  geom_text() + 
  scale_x_log10() + 
  labs(x = 'Case fatality rate %',
       title = 'Health expenditure vs CFR: All countries with more than 10k cases',
       y = 'Health expenditure per capita (rank)') + 
  theme_bw()
ggplotly(p2)

# Now try plotting the CFR against the (estimated) proportion of the population who have been tested
data_use3 = data %>% 
  drop_na(total_tests) %>% 
  group_by(country) %>% 
  filter(confirmed > 10000,
         region == 'Europe & Central Asia') %>% 
  ungroup() %>% 
  mutate(CFR = round(100*deaths / confirmed, 1),
         pop_tested = total_tests / population,
         `Deaths/confirmed` = paste(deaths, '/',confirmed))
p3 = ggplot(data_use3, aes(x = date, y = CFR, colour = country, label = `Deaths/confirmed`)) +
  geom_line() + 
  labs(title = "Case fatality rate over time in Europe & Central Asia",
       y = "CFR (%)") + 
  theme_bw()
ggplotly(p3)

