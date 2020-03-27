# Produce a gapminder type plot of the data for all countries

# Call in packages
library(tidyverse)
library(gganimate)
library(gapminder)
library(lubridate)
library(gifski)
library(stringi)
library(grid)

# Load in ECDC data
ecdc = readRDS(file = 'shiny/covid19_hamilton/ECDC_data_current.rds')

# Use the gapminder data to add in colours and continent information
gap2 = gapminder %>% 
  group_by(country) %>% 
  filter(year == max(year)) %>% 
  select(country, continent, pop) %>% 
  ungroup() %>% 
  rename(Country = country) %>% 
  mutate(Country = stri_trans_general(Country, id = "Title")) #%>% 
#   add_row(Country = 'Luxembourg', continent = 'Europe', pop = 1)
# add_row(Country = c('Luxembourg', 'Qatar', 'South Korea', 'United States of America'), 
#         continent = c('Europe', 'Asia', 'Asia', ''), 
#         pop = c(1))
  
  
country_colors2 = tibble(
  Country = names(country_colors), 
  Colours = country_colors) %>% 
  mutate(Country = stri_trans_general(Country, id = "Title"))


ecdc_plot = ecdc %>% 
  mutate(day_month = paste0(Day,'/',Month)) %>% 
  select(-c(Day,Month,Year,GeoId)) %>% 
  rename(Country = `Countries and territories`) %>% 
  mutate(Country = gsub('_', ' ', Country),
         Country = stri_trans_general(Country, id = "Title"),
         Date = ymd(DateRep)) %>% 
  left_join(gap2, by = 'Country') %>% 
  left_join(country_colors2, by = 'Country') %>% 
  group_by(Country) %>% 
  mutate(max_cases = max(Cases)) %>% 
  ungroup() %>% 
  filter(max_cases > 100) %>% 
  mutate(Country = recode(Country,
         `Cases On An International Conveyance Japan` = 'Other',
         'United States Of America' = 'USA',
         'United Kingdom' = 'UK'))
# %>% 
#   filter(Country == 'Ireland' |
#          Country == 'United_Kingdom' | 
#          Country == 'China' | 
#          Country == 'Italy')

# p_tmp = ggplot(ecdc_plot %>% top_n(100), aes(Cases, Deaths, colour = Country, size = Cases)) +
#   geom_text(aes(x = 4000, y = 200, label = day_month),  size = 50, col = 'grey') +
#   # annotation_custom(grid::textGrob(ecdc_plot$day_month[1],
#   #                                  gp = gpar(fontsize = 100, col = 'grey')),
#   #                   xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
#   geom_label(aes(label = Country)) +
#   scale_x_sqrt() +
#   scale_y_sqrt() +
#   theme(legend.position = 'None') +
#   theme_bw()
# print(p_tmp)

# p = ggplot(ecdc_plot, aes(Cases, Deaths, colour = Country, size = Cases)) +
#   geom_text(aes(x = 4000, y = 200, label = day_month),  size = 50, col = 'grey') +
#   #geom_point(alpha = 0.7) +
#   geom_label(aes(label = Country)) +
#   #scale_colour_manual(aes(values = Colours)) +
#   scale_size(range = c(2, 12)) +
#   scale_x_sqrt() +
#   scale_y_sqrt() +
#   #facet_wrap(~continent) +
#   # annotation_custom(grid::textGrob(day_time,
#   #                                  gp = gpar(fontsize = 80, col = 'grey')),
#   #                   xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
#   labs(title = 'Covid-19 cases', x = 'Cases', y = 'Deaths') + #: {frame_time}
#   transition_time(Date) +
#   ease_aes('linear') + 
#   theme_bw() + 
#   theme(legend.position = 'None')
# animate(p, 
#         nframes = 100, 
#         fps = 10,
#         duration = 60,
#         height = 600,
#         width = 800)
# anim_save('plots/covid_anim.gif',
#           renderer = gifski_renderer(loop = FALSE))
#system("convert ~/GitHub/hamilton-monitor/plot/covid_anim.gif -loop 1 ~/GitHub/hamilton-monitor/plot/covid_anim.gif")

# Cumulative plot

ecdc_plot2 = ecdc_plot %>% 
  arrange(Country, DateRep) %>% 
  group_by(Country) %>% 
  mutate(cum_sum_cases = cumsum(Cases),
         cum_sum_death = cumsum(Deaths)) %>% 
  ungroup()

# p2_tmp = ggplot(ecdc_plot2 %>% top_n(200), aes(cum_sum_cases, cum_sum_death, colour = Country, size = cum_sum_cases)) +
#   #geom_text(aes(x = 20000, y = 1000, label = day_month),  size = 50, col = 'grey') +
#   # annotation_custom(grid::textGrob(ecdc_plot$day_month[1],
#   #                                  gp = gpar(fontsize = 100, col = 'grey')),
#   #                   xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
#   geom_label(aes(label = Country)) +
#   labs(title = 'Daily Covid-19 cases', x = 'Cases', y = 'Deaths') +
#   scale_x_sqrt(breaks = scales::pretty_breaks(n = 10)) +
#   scale_y_sqrt(breaks = scales::pretty_breaks(n = 10)) +
#   theme_bw() +
#   theme(legend.position = 'None')
# print(p2_tmp)

p2 = ggplot(ecdc_plot2, aes(cum_sum_cases, cum_sum_death, colour = Country, size = cum_sum_death)) +
  geom_text(aes(x = 20000, y = 1000, label = day_month),  size = 50, col = 'grey') +
  geom_point(alpha = 0.7) +
  geom_label(aes(label = Country)) +
  #scale_colour_manual(aes(values = Colours)) +
  scale_size(range = c(2, 12)) +
  scale_x_sqrt(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_sqrt(breaks = scales::pretty_breaks(n = 10)) +
  #facet_wrap(~continent) +
  # annotation_custom(grid::textGrob(day_time,
  #                                  gp = gpar(fontsize = 80, col = 'grey')),
  #                   xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  labs(title = 'Cumulative Covid-19 cases', x = 'Cases', y = 'Deaths') + #: {frame_time}
  transition_time(Date) +
  ease_aes('linear') + 
  theme_bw() + 
  theme(legend.position = 'None')
p2_anim = animate(p2, 
        nframes = 100, 
        fps = 10,
        duration = 60,
        height = 600,
        width = 800)
anim_save(filename = 'plots/covid_anim_cumulative.gif',
          animation = p2_anim,
          renderer = gifski_renderer(loop = FALSE))
# anim_save('covid_anim_cumulative.mp4',
#           animation = p2_anim,
#           renderer = av_renderer(loop = FALSE))

#system("pwd")
#system("cd plots; convert covid_anim_cumulative.gif -loop 1 covid_anim_cumulative.gif")

