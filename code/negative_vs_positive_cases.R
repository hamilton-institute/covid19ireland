# libraries
library(ggplot2) # plots
library(tidyverse) # data wrangling
library(plotly) # web graphics

## source for tests data: https://ourworldindata.org/grapher/tests-vs-confirmed-cases-covid-19

# Read in tests vs confirmed cases data 
cov19_tc_raw <- read.csv("data/tests-vs-confirmed-cases-covid-19.csv")
cov19_tc <- cov19_tc_raw %>% rename ( # Rename to common format
  Country = Entity)  %>% 
  filter(Total.confirmed.cases.of.COVID.19..cases. != 0) %>%
  na.omit(Total.COVID.19.tests ) 

glimpse(cov19_tc)

# Creating Negative column
 for (i in 1:nrow(cov19_tc)) {
cov19_tc$Total.negative.tests[i] <- cov19_tc$Total.COVID.19.tests[i] -cov19_tc$Total.confirmed.cases.of.COVID.19..cases.[i]
 }

test_results <- cov19_tc[,c(1,4,5,6)]

test_results <- test_results  %>% 
  gather(key, value, -c(Country,Total.COVID.19.tests)) # reshaping data to have grouping column

for (i in 1:nrow(test_results)) {
  if (test_results$key[i]=="Total.confirmed.cases.of.COVID.19..cases.") { # Renaming for consiseness (if that's a  word.)
    test_results$key[i] <- "Positive test results"
  } else {
    test_results$key[i] <- "Negative test results"
  }
}

glimpse(test_results)

test_country <- unique(test_results$Country)

for (c in 1:length(test_country)) { # creating proportions
  for (i in 1:nrow(test_results))
  if (test_results$Country[i]==test_country[c]) {
    test_results$proportions[i] <- test_results$value[i]/test_results$Total.COVID.19.tests[i]
  }
}

test_results_order <- arrange(test_results, key, desc(proportions))
test_results_order$Country <- factor(test_results_order$Country, levels = unique(test_results_order$Country)) # reordering data

positive_prop <- test_results_order %>% # Creating postive prop table
  filter(key == "Positive test results")

ggplotly(ggplot(test_results_order, aes(y=proportions, x=Country))+ # plot of both positive and negative proportions
  geom_col(aes(fill=factor(key)), width = 0.8) +
  guides(shape = guide_legend(override.aes = list(size = 1)))  +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  coord_flip() +
  xlab("Country") +
  ylab("Proportions") +
  theme(legend.title = element_text(size = 6), legend.text = element_text(size = 6)))


ggplotly(ggplot(positive_prop, aes(y=proportions, x=Country, fill=Country))+ # plot of just positive proportion
           geom_col() +
           theme(legend.position = "none") +
           coord_flip() +
           xlab("Country") +
           ylab("Proportions"))

