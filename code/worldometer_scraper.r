#loading the package:
library(xml2)
library(rvest)
library(stringr)



# get ireland data
url <- 'https://www.worldometers.info/coronavirus/country/ireland'
webpage <- read_html(url)


summary_stats_irl <- html_nodes(webpage, 'div#maincounter-wrap')
summary_stats_irl <- html_text(summary_stats_irl)



# replace new lines and spaces
summary_stats_irl <- str_replace_all(summary_stats_irl, '[\r\n\t]' , '')
summary_stats_irl <- str_replace_all(summary_stats_irl, ' ' , '')
summary_stats_irl <- str_trim(summary_stats_irl)




summary_stats_irl <- data.frame('ireland',as.numeric(gsub("[^0-9.]", "",  summary_stats_irl[1])),
                            as.numeric(gsub("[^0-9.]", "",  summary_stats_irl[2])),
                            as.numeric(gsub("[^0-9.]", "",  summary_stats_irl[3])))

names(summary_stats_irl) <- c('Region', 'Cases', 'Deaths', 'Recovered')



# get world data
url <- 'https://www.worldometers.info/coronavirus/'
webpage <- read_html(url)

summary_stats_world <- html_nodes(webpage, 'div#maincounter-wrap')
summary_stats_world <- html_text(summary_stats_world)

# replace new lines and spaces
summary_stats_world <- str_replace_all(summary_stats_world, '[\r\n\t]' , '')
summary_stats_world <- str_replace_all(summary_stats_world, ' ' , '')
summary_stats_world <- str_trim(summary_stats_world)

summary_stats_world <- data.frame('world',as.numeric(gsub("[^0-9.]", "",  summary_stats_world[1])),
                            as.numeric(gsub("[^0-9.]", "",  summary_stats_world[2])),
                            as.numeric(gsub("[^0-9.]", "",  summary_stats_world[3])))

names(summary_stats_world) <- c('Region', 'Cases', 'Deaths', 'Recovered')

all.data <- rbind(summary_stats_world, summary_stats_irl)


# write data
write_csv(all.data, "data/scraped/summary_stats.csv")
write_csv(all.data, "shiny/covid19_hamilton/summary_stats_current.csv")
write_csv(all.data, "shiny/covid19_hamilton_v2/summary_stats_current.csv")
