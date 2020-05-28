library(tidyverse)
library(lubridate)
library(geofacet)


# read in data--------------
d <- read.csv('http://opendata-geohive.hub.arcgis.com/datasets/4779c505c43c40da9101ce53f34bb923_0.csv')

d$Date <- as.Date(d$TimeStampDate, "%Y/%m/%d")



d <- mutate(d, TotCases100K = PopulationProportionCovidCases)


mindate <- filter(d, ConfirmedCovidCases > 1 ) %>% summarise(date=min(Date))  %>% pull(date)
maxdate <- max(d$Date)


d1 <- filter(d, Date>=mindate) # no cases recorded before then

d1 <- d1%>%
  group_by(CountyName) %>%
  mutate(dailyCases = (ConfirmedCovidCases - lag(ConfirmedCovidCases))) %>%
  ungroup() %>%
  mutate(dailyCases100K = dailyCases*100000/PopulationCensus16)
#----------------------------------------------
# setup grid for geofacet-----------------------
g <- matrix(NA, nrow= 7, ncol=4)

g[1,] <- c("Kerry","Cork", "Waterford","Wexford")
g[2,] <- c("Limerick","Tipperary", "Kilkenny","Carlow")
g[3,] <- c("Clare","Offaly", "Laois","Wicklow")
g[4,] <- c("Galway","Westmeath", "Kildare","Dublin")
g[5,] <- c("Mayo","Roscommon","Longford", "Meath")
g[6,] <- c("Sligo","Leitrim", "Cavan", "Louth")
g[7,] <- c("Donegal",NA, "Monaghan",NA)

g[1:7,]<- g[7:1,]

ire_grid <- data.frame(name=as.vector(g), row = as.vector(row(g)), col= as.vector(col(g)))
ire_grid$name <- as.character(ire_grid$name)
ire_grid$code <- ire_grid$name
ire_grid  <- ire_grid[!is.na(ire_grid$name),]


#--------------set up factor indicating level per population-------------------------------

countyRate <- d1 %>%
  group_by(CountyName) %>% summarise(countyRate  = max(TotCases100K)) %>%
  mutate(countyRateLevel = cut_interval(countyRate,3))

levels(countyRate$countyRateLevel) <- c("lo", "mid", "hi")
d1 <- left_join(d1, countyRate, by="CountyName")

#--------------plot of county cases-------------------------------

ggplot(data=d1, aes(x=Date, y=dailyCases, color=countyRateLevel)) +
  geom_smooth(se=F, span=.25) +
  facet_geo(~CountyName, grid=ire_grid, scales="free_y")+
  labs(color = "Per 100K")+
  theme(legend.position="bottom") +xlab("")+
  scale_x_date(labels = scales::date_format("%e/%m"))+
  scale_color_manual(values = c(lo="skyblue", mid="blue", hi="red"))+
  ylab("Smoothed daily cases")+ ggtitle(paste0("Data up to ", strftime(maxdate,format="%d %b %Y")))





