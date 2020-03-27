# libraries
library(ggplot2) # plots
library(tidyverse) # data wrangling
library(plotly) # web graphics

## ECDC_data_20200325.rds is on github in the data/scraped folder, this is out of date though.

## source for tests data: https://ourworldindata.org/grapher/tests-vs-confirmed-cases-covid-19
## source for ECDC_data: https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

ECDC_data_20200326 <- read.csv("data/COVID-19-geographic-disbtribution-worldwide.csv")
ECDC_data_20200326 <- ECDC_data_20200326 %>% rename ( # Rename to common format
  Country = Countries.and.territories 
)

pop_data <- unique(ECDC_data_20200326[,c(7,9)])

# Read in tests vs confirmed cases data 
cov19_tc <- read.csv("data/tests-vs-confirmed-cases-covid-19.csv")
cov19_tc <- cov19_tc %>% rename ( # Rename to common format
  Country = Entity
)
glimpse(cov19_tc)

# Merging total tests, cases and population data  
cov19_tcp <- merge(cov19_tc,pop_data,by=c("Country"))
glimpse(cov19_tcp)

# Creating proportion of tests/population
for (i in 1:nrow(cov19_tcp)) {
  if (is.na(cov19_tcp$Total.COVID.19.tests[i])==TRUE) {
    cov19_tcp$tests_pop_proportion[i] <- NA
  } else {
    cov19_tcp$tests_pop_proportion[i] <- (cov19_tcp$Total.COVID.19.tests[i]/cov19_tcp$Pop_Data.2018[i])
  }
}


# Dataframe of proportions in decreasing order removing NAs

test_prop <- cov19_tcp %>% 
  filter(Total.confirmed.cases.of.COVID.19..cases. != 0) %>%
  na.omit(cov19_tcp[tests_pop_proportion, ]) 

test_prop <- test_prop[order(test_prop$tests_pop_proportion,decreasing = TRUE), ] 
test_prop$Country <- factor(test_prop$Country, levels = test_prop$Country) # to retain the order in plot

top10_test_prop <- test_prop[c(1:10),]
bottom10_test_prop <-  test_prop[c(35:nrow(test_prop)),]

p1 <- ggplotly(ggplot(top10_test_prop, aes(tests_pop_proportion, Country, colour=Country)) +
                 geom_point() +
                 ylab("Country") +
                 xlab("No.of Tests/Population size") +
                 theme(legend.position='none') +
                 xlim(0.00001,0.03))

p2<- ggplotly(ggplot(bottom10_test_prop, aes(tests_pop_proportion, Country,  colour=Country)) +
                geom_point() +
                labs(title="Top 10 Countries with Highest and Lowest Number of Tests per Population") +
                ylab("Country") +
                xlab("No.of Tests/Population size") +
                theme(legend.position='none') +
                xlim(0,0.001))

subplot(p1,p2, shareX = TRUE) 