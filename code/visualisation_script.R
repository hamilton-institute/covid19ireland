# A simple visualisation of the ECDC data

# Call in packages
library(tidyverse)
library(plotly)

# Load in ECDC data
ecdc = readRDS(file = 'data/scraped/ECDC_data_20200321.rds')

# Pick a country 
co = 'China'

# Extract out the data
ecdc_plot = ecdc %>% 
  filter(`Countries and territories` == co) %>% 
  select(-c(Day,Month,Year,`Countries and territories`,GeoId)) %>% 
  pivot_longer(names_to = 'Type', values_to = 'Number', -DateRep)

p = ecdc_plot %>% ggplot(aes(x = DateRep, y = Number, colour = Type)) + 
  geom_line() + 
  theme_bw() + 
  scale_x_datetime(date_breaks = "1 week")
ggsave(p, file = paste0('plots/Ireland_',format(Sys.time(), "%Y%m%d"),'.pdf'))

# Plotly version
fig <- plot_ly(ecdc_plot, x = ~DateRep, y = ~Number, type = 'scatter', mode = 'lines', color = ~Type) 
fig <- fig %>% layout(title = paste('Number of cases/deaths for',co),
                      xaxis = list(title = 'Date'),
                      yaxis = list (title = 'Number of individuals'))
print(fig)
