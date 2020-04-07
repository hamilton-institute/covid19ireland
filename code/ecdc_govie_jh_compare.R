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
  geom_point() +
  geom_line() + 
  labs(x = "", y = "Confirmed cases") + 
  scale_x_date(breaks = breaks_pretty(n=10)) + 
  # scale_y_continuous(breaks = breaks_log(n=10, base = 10), 
  #                    trans = 'log10') +
  scale_y_continuous(breaks = breaks_pretty(n=10)) +
  theme_bw() +
  theme(legend.title = element_blank())

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
  geom_point() +
  theme_bw()


# Have a look at which countires are correlated beween the two ------------

ecdc_data = readRDS(file = 'latest_ECDC_data.rds') %>% 
  rename(Date = dateRep,
         Country = countriesAndTerritories) %>% 
  arrange(Date) %>% 
  group_by(Country) %>% 
  mutate(ecdc_cases = cumsum(cases),
         ecdc_deaths = cumsum(deaths)) %>% 
  ungroup() %>% 
  arrange(Country, Date) %>% 
  mutate(Date = as_date(Date),
         Country = str_to_title(str_replace_all(Country, '_', ' ')),
         Country = recode(Country, 
                         'United States Of America' = 'Us'
                  )) %>% 
  select(Date, ecdc_cases, ecdc_deaths, Country, popData2018)
jh_data = download_jhu_csse_covid19_data(silent = TRUE, cached = TRUE) %>% 
  rename(Date = date,
         Country = country,
         jh_cases = confirmed,
         jh_deaths = deaths) %>% 
  mutate(Country = str_to_title(str_replace_all(Country, '_', ' '))) %>% 
  arrange(Country, Date) %>% 
  select(Date, jh_cases, jh_deaths, Country)

all_data = inner_join(ecdc_data,jh_data,
                     by = c('Country', 'Date')) %>% 
  arrange(Country, Date) %>% 
  drop_na()

# Look at overall correlation
all_data %>% select(ecdc_cases, ecdc_deaths, jh_cases, jh_deaths) %>% cor

# 7/4/20
# ecdc_cases   1.0000000   0.8111611 0.9981300 0.8148840
# ecdc_deaths  0.8111611   1.0000000 0.8057374 0.9981440
# jh_cases     0.9981300   0.8057374 1.0000000 0.8118104
# jh_deaths    0.8148840   0.9981440 0.8118104 1.0000000

# Compute correlation between countries
all_data %>% group_by(Country) %>% 
  summarise(cor_cases = cor(ecdc_cases, jh_cases),
            cor_deaths = cor(jh_cases, jh_deaths),
            max_deaths = max(jh_deaths),
            max_cases = max(jh_cases),
            popn2018 = max(popData2018)) %>% 
  filter(max_deaths > 100) %>% 
  arrange(cor_deaths, cor_cases) %>% 
  as.data.frame

# Plot one of the bad ones
all_data %>% filter(Country %in% c('Sweden', 'Germany', 'China',
                                   'United Kingdom', 'Us', 
                                   'Italy', 'Spain', 'Iran')) %>% 
  ggplot(aes(x = ecdc_deaths, y = jh_deaths, colour = Date)) + 
  geom_point() +
  theme_bw() +
  labs(x = "European Centre for Disease Control", y = 'John Hopkins',
       title = "Deaths: John Hopkins vs European Centre for Disease Control") + 
  geom_abline(intercept = 0, slope = 1) + 
  geom_abline(intercept = 0, slope = 1.1, linetype = 'dotted') +
  geom_abline(intercept = 0, slope = 1.2, linetype = 'dotted') +
  facet_wrap(~ Country, scales = "free")
