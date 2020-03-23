# scraping hospital info from gov.ie


library(tidyverse)
library(xml2)
library(rvest)
library(stringr)

url <-'https://www.gov.ie/en/collection/ef2560-analysis-of-confirmed-cases-of-covid-19-coronavirus-in-ireland/'
webpage <- read_html(url) 

all.urls <- html_attr(html_nodes(webpage, "a"), "href")  

publications <- all.urls[which(str_detect(urls, pattern = "/en/publication/"))] 


Total.number.hospitalised <- c()
Total.number.admitted.to.ICU <- c()
Total.number.deaths <- c()
Case.fatality.rate <- c()
Total.number.healthcare.workers <- c()
Number.clusters.notified <- c()
Median.age <- c()

dates <- c()

for (analysis in publications){
  this.analysis <- paste0("https://www.gov.ie", analysis)
  this.analysis <- read_html(this.analysis) 
  print(this.analysis)
  
  #getting date in nice format
  h.ones <- html_nodes(this.analysis, 'h1')
  h.ones <- html_text(h.ones)
  h.ones <- gsub("[\\(\\)]", "", regmatches(h.ones, gregexpr("\\(.*?\\)", h.ones))[[1]])
  h.ones <- gsub('as of ', '', h.ones)
  h.ones <- sub(".*? ", "", h.ones)
  dates <- c(dates, h.ones)
  
  tables <- html_nodes(this.analysis, 'table')
  
  for(i in seq(1, length(tables))){
    x <- html_table(tables[[i]], fill = TRUE, header = TRUE)

    if(names(x)[1] == 'Total number of cases' | names(x)[1] == 'Source: HSPC'){
      names(x) <- c('category', 'num', 'prec')
      
      Total.number.hospitalised <- c(Total.number.hospitalised, (x %>% filter(category == 'Total number hospitalised') %>% select(num)))
      Total.number.hospitalised[sapply(Total.number.hospitalised, function(Total.number.hospitalised) length(Total.number.hospitalised)==0)] <- NA
      
      Total.number.admitted.to.ICU <- c(Total.number.admitted.to.ICU, (x %>% filter(category == 'Total number admitted to ICU') %>% select(num)))
      Total.number.admitted.to.ICU[sapply(Total.number.admitted.to.ICU, function(Total.number.admitted.to.ICU) length(Total.number.admitted.to.ICU)==0)] <- NA
      
      Total.number.deaths <- c(Total.number.deaths, (x %>% filter(category %in% c('Total number deaths', 'Total number of deaths')) %>% select(num)))
      Total.number.deaths[sapply(Total.number.deaths, function(Total.number.deaths) length(Total.number.deaths)==0)] <- NA
      
      Case.fatality.rate <- c(Case.fatality.rate, (x %>% filter(category == 'Case fatality rate') %>% select(num)))
      Case.fatality.rate[sapply(Case.fatality.rate, function(Case.fatality.rate) length(Case.fatality.rate)==0)] <- NA
      
      Total.number.healthcare.workers <- c(Total.number.healthcare.workers, (x %>% filter(category %in% c('Total number healthcare workers','Total number of healthcare workers')) %>% select(num)))
      Total.number.healthcare.workers[sapply(Total.number.healthcare.workers, function(Total.number.healthcare.workers) length(Total.number.healthcare.workers)==0)] <- NA
      
      Number.clusters.notified <- c(Number.clusters.notified, (x %>% filter(category == 'Number clusters notified') %>% select(num)))
      Number.clusters.notified[sapply(Number.clusters.notified, function(Number.clusters.notified) length(Number.clusters.notified)==0)] <- NA
      
      Median.age <- c(Median.age, (x %>% filter(category == 'Median age') %>% select(num)))
      Median.age[sapply(Median.age, function(Median.age) length(Median.age)==0)] <- NA
    }
  }
}
  

Total.number.hospitalised <- unlist(Total.number.hospitalised)
Total.number.admitted.to.ICU <- unlist(Total.number.admitted.to.ICU)
Total.number.deaths <- unlist(Total.number.deaths)
Case.fatality.rate <- unlist(Case.fatality.rate)
Total.number.healthcare.workers <- unlist(Total.number.healthcare.workers)
Number.clusters.notified <- unlist(Number.clusters.notified)
Median.age <- unlist(Median.age)




data <- data.frame(dates,
                   Total.number.hospitalised,
                   Total.number.admitted.to.ICU,
                   Total.number.deaths,
                   Case.fatality.rate,
                   Total.number.healthcare.workers,
                   Number.clusters.notified,
                   Median.age)
        

write.csv(data, 'data/scraped/gov_hospital_data.csv')
