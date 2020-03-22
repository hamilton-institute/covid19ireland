#loading the package:
library(xml2)
library(rvest)
library(stringr)


url <- 'https://www.worldometers.info/coronavirus/country/ireland'
webpage <- read_html(url)


summary.stats <- html_nodes(webpage, 'div#maincounter-wrap')
summary.stats <- html_text(summary.stats)
head(summary.stats)



# replace new lines and spaces
summary.stats <- str_replace_all(summary.stats, '[\r\n\t]' , '')
summary.stats <- str_replace_all(summary.stats, ' ' , '')
summary.stats <- str_trim(summary.stats)




summary_stats <- data.frame(as.numeric(gsub("[^0-9.]", "",  summary.stats[1])),
                            as.numeric(gsub("[^0-9.]", "",  summary.stats[2])),
                            as.numeric(gsub("[^0-9.]", "",  summary.stats[3])))

names(summary_stats) <- c('Cases', 'Deaths', 'Recovered')


write_csv(summary_stats, "data/scraped/summary_stats.csv")
