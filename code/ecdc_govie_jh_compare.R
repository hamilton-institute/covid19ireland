# Create some plots of daily cases between the Irish government, the ECDC, and the John Hopkins data for Ireland to see if any agree

# Clear the workspace
rm(list = ls())
library(tidyverse)
library(tidycovid19)
library(readxl)
library(plotly)

# Read in the ECDC data and filter for Ireland
ecdc_data = readRDS(file = 'latest_ECDC_data.rds') %>% 
  filter(countriesAndTerritories == 'Ireland') %>% 
  rename(Date = dateRep) %>% 
  arrange(Date) %>% 
  mutate(Date = as_date(Date),
         ecdc_cases = cumsum(cases),
         ecdc_deaths = cumsum(deaths)) %>% 
  select(Date, ecdc_cases, ecdc_deaths)

# Read in the Irish gov data
gov_data = read_excel("latest_irish_data.xlsx", sheet = 'totals') %>% 
  rename(gov_cases =`Total number of cases`,
         gov_deaths = `Total number of deaths`) %>% 
  mutate(Date = as_date(Date),
         gov_deaths = parse_number(gov_deaths)) %>% 
  select(Date, gov_cases, gov_deaths)

# Read in the JH data
jh_data = download_jhu_csse_covid19_data(silent = TRUE, cached = TRUE) %>% 
  filter(country == 'Ireland') %>% 
  rename(Date = date,
         jh_cases = confirmed,
         jh_deaths = deaths) %>% 
  select(Date, jh_cases, jh_deaths)

# Merge them together
full_data = list(ecdc_data, gov_data, jh_data) %>%
  reduce(full_join, by = "Date") %>% 
  pivot_longer(names_to = 'Type', values_to = "Cases", -Date)

# Plot the cases
p_cases = full_data %>% 
  filter(str_detect(Type, 'cases'),
         Date > "2020-03-10") %>% 
  ggplot(aes(x = Date, y = Cases, colour = Type)) + 
  geom_line() + 
  labs(x = "", y = "Confirmed cases") + 
  scale_x_date(breaks = breaks_pretty(n=10)) + 
  # scale_y_continuous(breaks = breaks_log(n=10, base = 10), 
  #                    trans = 'log10') +
  scale_y_continuous(breaks = breaks_pretty(n=10)) +
  theme_bw() +
  theme(legend.title = element_blank())
ggplotly(p_cases)  

# Plot the cases
p_deaths = full_data %>% 
  filter(str_detect(Type, 'deaths'),
         Date > "2020-03-10") %>% 
  ggplot(aes(x = Date, y = Cases, colour = Type)) + 
  scale_x_date(breaks = breaks_pretty(n=10)) + 
  # scale_y_continuous(breaks = breaks_log(n=10, base = 10), 
  #                    trans = 'log10') +
  scale_y_continuous(breaks = breaks_pretty(n=10)) +
  labs(x = "", y = "Confirmed deaths") + 
  geom_line() + 
  theme_bw()
ggplotly(p_deaths)

