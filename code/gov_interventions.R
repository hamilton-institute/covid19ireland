## Libraries
library(ggplot2) # plots
library(tidyverse) # data wrangling
library(dplyr)
library(plotly) # web graphics


# Data Source:
# https://data.humdata.org/dataset/acaps-covid19-government-measures-dataset

# Note: I took the second sheet from the excel book download above ^ and saved it as a csv. 
# There is an R package "tidycovid19" that can access this data.

## Load data 
gov_interventions_df <- read.csv("data/20200330-acaps-covid-19-goverment-measures-dataset.csv", header = TRUE, stringsAsFactors = FALSE)
glimpse(gov_interventions_df)

## Editing Country names to match other dataframes and changing date implemented to date class

gov_interventions_df$COUNTRY[gov_interventions_df$COUNTRY=="United States of America"] <- "United States"
gov_interventions_df$COUNTRY[gov_interventions_df$COUNTRY=="Viet Nam"] <- "Vietnam"
gov_interventions_df$COUNTRY[gov_interventions_df$COUNTRY=="Korea Republic of"] <- "South Korea"

gov_interventions_df <- gov_interventions_df %>% # Filtering out blanks and formating dates
  filter(COUNTRY != "") %>%
  mutate(DATE_IMPLEMENTED=as.POSIXct(DATE_IMPLEMENTED, format = "%d/%m/%Y")) %>%
  mutate(ENTRY_DATE=as.POSIXct(ENTRY_DATE, format = "%d/%m/%Y"))


## Create subset of data 

timeline_measures <- gov_interventions_df[,c(2,3,7,8,12)]

head(timeline_measures)

my_countries <- c("United States","United Kingdom","Italy","Spain","Ireland","South Korea","France","Germany") # Creating a subset of countries

my_country_timeline_measures <- timeline_measures %>% filter(
  COUNTRY %in% my_countries
)

head(my_country_timeline_measures)

first_measures <- my_country_timeline_measures %>% # Getting the first date for each measure in selected countries
  group_by(COUNTRY, CATEGORY) %>%
  top_n(n=1, wt=desc(DATE_IMPLEMENTED))

# Timeline of events 

ggplotly(
  ggplot(data=first_measures, aes(x=DATE_IMPLEMENTED, y=COUNTRY, color=CATEGORY)) + 
  geom_point(aes(shape=CATEGORY), position = position_jitter(w =0.2, h=0.02)) 
)
