# Produce a gapminder type plot of the data for all countries

# Call in packages
library(tidyverse)
library(gganimate)
library(gapminder)
library(lubridate)
library(gifski)

# Load in ECDC data
ecdc = readRDS(file = 'data/scraped/ECDC_data_20200321.rds')

# Use the gapminder data to add in colours and continent information
gap2 = gapminder %>% 
  group_by(country) %>% 
  filter(year == max(year)) %>% 
  select(country, continent, pop) %>% 
  ungroup()

country_colors2 = tibble(
  Country = names(country_colors), 
  Colours = country_colors)

ecdc_plot = ecdc %>% 
  mutate(day_month = paste0(Day,'/',Month)) %>% 
  select(-c(Day,Month,Year,GeoId)) %>% 
  rename(Country = `Countries and territories`) %>% 
  mutate(Country = gsub('_', ' ', Country),
         Date = ymd(DateRep)) %>% 
  left_join(gap2, by = c('Country' = 'country')) %>% 
  left_join(country_colors2, by = 'Country')
# %>% 
#   filter(Country == 'Ireland' |
#          Country == 'United_Kingdom' | 
#          Country == 'China' | 
#          Country == 'Italy')

p = ggplot(ecdc_plot, aes(Cases, Deaths, colour = Country, size = Cases)) +
  #geom_point(alpha = 0.7) +
  geom_label(aes(label = Country)) +
  #scale_colour_manual(aes(values = Colours)) +
  scale_size(range = c(2, 12)) +
  scale_x_sqrt() +
  scale_y_sqrt() +
  facet_wrap(~continent) +
  theme(legend.position = 'none') +
  labs(title = 'Date: {frame_time}', x = 'Cases', y = 'Deaths') +
  transition_time(Date) +
  ease_aes('linear') + 
  theme_bw() + 
  theme(legend.position = 'None')
animate(p, 
        nframes = 100, 
        fps = 10,
        duration = 60,
        renderer = gifski_renderer(loop = FALSE))
anim_save('plots/covid_anim.gif')