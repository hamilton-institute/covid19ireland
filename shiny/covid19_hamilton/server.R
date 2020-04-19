library(shiny)
library(plotly)
library(tidyverse)
library(leaflet)
library(jsonlite)
library(shinydashboard)
library(rgdal)
library(DT)
library(lubridate)
library(ggdark)
library(leafpop)
#library(viridis)
library(scales)
library(ggdark)
library(stringr)
library(grid)
library(RColorBrewer)
library(wesanderson)
library(rlist)
library(readxl)

# Create a ggplot theme to match the background
theme_shiny_dashboard <- function (base_size = 12, base_family = "") {
  theme_dark(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      axis.text = element_text(colour = rgb(205/255,205/255,205/255)), # 205 205 205
      axis.title = element_text(colour = rgb(205/255,205/255,205/255)),
      axis.title.x = element_text(colour = rgb(205/255,205/255,205/255)),
      axis.title.y = element_text(colour = rgb(205/255,205/255,205/255)),
      plot.title = element_text(colour = rgb(205/255,205/255,205/255)),
      legend.title = element_text(colour = rgb(205/255,205/255,205/255)),
      legend.text = element_text(colour = rgb(205/255,205/255,205/255)),
      legend.background = element_rect(fill=rgb(70/255,80/255,90/255)),
      panel.background = element_rect(fill=rgb(70/255,80/255,90/255)), # 70 80 89
      #panel.grid.minor.y = element_line(size=3),
      #panel.grid.major = element_line(colour = "orange"),
      plot.background = element_rect(fill=rgb(52/255,62/255,72/255)) # 52 62 72
    )   
}

global_raw <- readRDS('latest_global_data.rds') %>% 
  mutate(countriesAndTerritories = 
           recode(countriesAndTerritories, 
                  'Cases_on_an_international_conveyance_Japan' = 'Cruise_ship',
                  'United_States_of_America' = 'USA',
                  'United_Kingdom' = 'UK'
                  ))

global_world = global_raw %>% 
  group_by(dateRep) %>% 
  summarise(deaths = sum(deaths, na.rm = TRUE),
            cases = sum(cases, na.rm = TRUE),
            popData2018 = sum(popData2018, na.rm = TRUE),
            day = min(day),
            month = min(month)) %>% 
  mutate(countriesAndTerritories = 'Global')

# Bind together and add colours - specify some that are required
global = bind_rows(global_raw, global_world) 

#All tables contains information on a county-by-county basis
#will be used in the Counties tab
#all_tables <- readRDS('all_tables_current.rds')
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X,
                                                     na = "NA"))
  names(x) <- sheets
  return(x)
}
latest_irish_data <- read_excel_allsheets("latest_irish_data.xlsx")

# Get totals and one day old totals including complete data in case of missing
latest_irish_totals = latest_irish_data$totals %>% 
  filter(Date == max(Date))
previous_irish_totals = latest_irish_data$totals %>% 
  filter(Date == max(Date) - days(1))
latest_irish_complete = latest_irish_data$totals %>% 
  na.omit() %>% 
  filter(Date == max(Date))# - days(1))
previous_irish_complete = latest_irish_data$totals %>% 
  na.omit() %>% 
  filter(Date == max(Date) - days(1))

#Get the latest table containing info on all counties
latest_county_table = latest_irish_data$by_county %>%
  filter(Date == max(Date)) %>% 
  mutate(Cases = strtoi(gsub("[^0-9.-]", "", `Number of Cases`)))

# Read in NI data
#latest_NI_data = readRDS("latest_NI_data.rds")
NI_county_lookup = latest_irish_data$NI_county_lookup
latest_NI_county = latest_irish_data$NI_district %>% 
  #mutate(Date = as_datetime(Date)) %>% 
  filter(Date == max(Date)) %>% 
  left_join(NI_county_lookup, by = c("District")) %>%
  group_by(County) %>% 
  summarise(Date = max(Date),
            `Number of Cases` = as.character(sum(Value)),
            Cases = sum(Value)) %>% 
  select(Date, County, `Number of Cases`, Cases)
  
latest_county_table = bind_rows(latest_county_table, latest_NI_county)

#Read in the county shapes file and join it with county case info
cs2 <- rgdal::readOGR("counties_simple.geojson")
cs2 <- merge(cs2, latest_county_table, by.x='NAME_TAG', by.y='County')

#Color the counties by number of cases
pal2 <- colorNumeric("Blues", log2(cs2$Cases))
#pal2 <- colorNumeric(scales::viridis_pal(), log2(cs2$Cases))

#Since we don't have data on a county by county basis for
#Nor Ire, we fill it with data for the whole region
#nor_ire_counties = c('Antrim', 'Armagh', 'Down', 'Fermanagh', 'Londonderry','Tyrone')
#cs2 = cs2[!(cs2$NAME_TAG %in% nor_ire_counties), ]

#Aproximate to 5 the "<= 5 cases" - SHOULD IT BE 5? - I"m going to leave as NA
latest_county_table = latest_county_table %>% 
  mutate(`Number of Cases` = as.numeric(`Number of Cases`))# %>% 
           #replace_na(5))
#county_total_date$`Number of Cases`<-county_total_date$`Number of Cases` %>% as.numeric %>% ifelse(is.na(.),5,.) 

# Get all the county data in numeric format
all_NI_county = latest_irish_data$NI_district %>% 
  left_join(NI_county_lookup, by = c("District")) %>%
  mutate(Date = as_datetime(Date)) %>% 
  group_by(County, Date) %>% 
  summarise(`Number of Cases` = sum(Value),
            Cases = sum(Value)) %>% 
  select(Date, County, `Number of Cases`, Cases)

all_county_table = latest_irish_data$by_county %>% 
  mutate(`Number of Cases` = as.numeric(`Number of Cases`)) %>% 
  bind_rows(all_NI_county) %>% 
  group_by(County) %>% arrange(desc(Date)) # The arrrange is necessary to inform the right date value
                                                                                                         #at county map 
              
 
#Correcting the number of cases in Dublin on day 2020-04-09, the scrapped that recover the value of 34156 instead 4156                                                                                                         
all_county_table$`Number of Cases`[all_county_table$Date==date("2020-04-09") & all_county_table$County=="Dublin"]<-4156  
              
#Create the plots for county cumulatie
county_cumulative_cases = 
  map(cs2$NAME_TAG,
      ~ggplot(all_county_table 
              %>% filter(County==as.character(.x)),
              aes(x=as.Date(Date),y=`Number of Cases`,group=County))+
        geom_point()+geom_line()+
        theme_bw() + 
        scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
        ggtitle(label = paste0("Total cases in ",.x, 
                               " at ",all_county_table$Date[[1]],": ",
                               all_county_table$`Number of Cases`[all_county_table$Date==all_county_table$Date[[1]] & all_county_table$County==.x]))+
                               xlab('Date')+
                               ylab('Number of individuals')+
                               #geom_text(mapping = aes(x=date,y=`Number of Cases`,label=`Number of Cases`,vjust=-0.5))+
                               theme_bw()+
                               theme(axis.text.x = element_text(angle = 90)))

#Defining the trend icon to the county map
trend_icon<-makeIcon(iconUrl = "https://cdn2.iconfinder.com/data/icons/font-awesome/1792/line-chart-512.png",
                     iconWidth = 15,iconHeight = 15)

# Get the html message for the value boxes
get_html_message = function(pc) {
  if(pc == 0)  {
    html_message = fa(name = "arrow-right", fill = "grey")
  } else if(pc < 0) {
    html_message = fa(name = "arrow-down", fill = "white")
  } else if(pc > 0) {
    html_message = fa(name = "arrow-up", fill = "black")
  }
  return(html_message)
}

# Calculate biggest change in deaths
#pct <- function(x) {x/lag(x)}
big_change = function(x) {x - lag(x)}
is_bad <- function(x) is.na(x) | is.nan(x) | is.infinite((x))
global_change = global %>% group_by(countriesAndTerritories) %>% 
  mutate_each(big_change, c(cases, deaths)) %>% 
  filter_all(all_vars(!is_bad(.))) %>% 
  ungroup() %>% 
  filter(dateRep == max(dateRep))

# Get the latest update data
last_updated = read_csv(file = 'last_updated.csv')

# Create neat table of cases
global_table1 = global %>% 
  group_by(countriesAndTerritories) %>% 
  filter(dateRep == max(dateRep)) %>% 
  ungroup() %>% 
  arrange(desc(deaths)) %>% 
  mutate(Date = as.Date(dateRep),
         Country = countriesAndTerritories,
         `Daily cases` = cases,
         `Daily deaths` = deaths) %>% 
  select(Date, Country, `Daily cases`, `Daily deaths`)
global_table2 = global %>% 
  group_by(countriesAndTerritories) %>% 
  summarise(Date = as.Date(max(dateRep)),
            `Total cases` = sum(cases),
            `Total deaths` = sum(deaths)) %>% 
  ungroup() %>% 
  rename(Country = countriesAndTerritories) %>% 
  select(Country, `Total cases`, `Total deaths`)
global_table3 = left_join(global_table1, global_table2, by = 'Country') %>% 
  filter(Country != 'Global') %>% 
  arrange(desc(`Total deaths`))

# Get interventions data
latest_interventions_data = read_excel('latest_interventions_data.xlsx',
                                       sheet = "Database") %>% 
  mutate(COUNTRY = 
           recode(COUNTRY, 
                  'United States of America' = 'USA',
                  'United Kingdom' = 'UK',
                  "Czech Republic" = 'Czechia'
           ))

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # Interventions tab --------------------------------------------------------------
  
  output$InterventionsPlot <- renderPlotly({
    global_agg = global %>%
      filter(countriesAndTerritories %in% input$sel_ctry3) %>%
      select(dateRep, cases, deaths, countriesAndTerritories, popData2018) %>% 
      group_by(countriesAndTerritories) %>% 
      arrange(dateRep) %>% 
      mutate(cum_cases = cumsum(cases),
             cum_deaths = cumsum(deaths),
             log_cases = log(cum_cases),
             log_deaths = log(cum_deaths),
             cases_per_million = 1e6*cumsum(cases)/popData2018,
             deaths_per_million = 1e6*cumsum(deaths)/popData2018) %>% 
      ungroup()
    
    country_colours = global_agg %>%
      arrange(desc(popData2018)) %>%
      select(countriesAndTerritories) %>%
      distinct() %>%
      mutate(Colours = colorRampPalette(brewer.pal(8, "Set1"))(n())) %>%
      deframe()
    
    # Add in intervention data
    use_interventions = latest_interventions_data %>% 
      filter(COUNTRY %in% input$sel_ctry3) %>% 
      select(COUNTRY, DATE_IMPLEMENTED, MEASURE, COMMENTS) %>% 
      mutate(MEASURE = str_squish(str_to_sentence(MEASURE))) %>% 
      filter(MEASURE %in% input$sel_measure) %>% 
      arrange(DATE_IMPLEMENTED, MEASURE) %>% 
      distinct(DATE_IMPLEMENTED, COUNTRY, MEASURE, .keep_all = TRUE) %>% 
      group_by(DATE_IMPLEMENTED, COUNTRY) %>% 
      summarise(MEASURE2 = paste0(COUNTRY, ": ", unique(MEASURE), collapse = '; '),
                COMMENTS2 = paste0(COUNTRY, ": ", unique(COMMENTS), collapse = '; '),
                dateRep = max(DATE_IMPLEMENTED),
                countriesAndTerritories = COUNTRY[1]) %>% 
      ungroup() %>% 
      na.omit() %>% 
      mutate(days_since = as.integer(as.Date(DATE_IMPLEMENTED) - 
                                       as.Date(min(DATE_IMPLEMENTED))),
             date_end = dateRep + days(14),
             days_end = days_since + 14,
             data_point = MEASURE2,
             Number = 0) %>% 
      filter(days_since >= 0) %>% 
      ungroup()
    
    # Duplicate to enable different labels
    use_interventions2 = use_interventions %>% 
      mutate(data_point = str_wrap(COMMENTS2))
    
    shiny::validate(
      shiny::need(nrow(global_agg) > 0, "Please select some countries. Use Global for worldwide values.")
    )
    shiny::validate(
      shiny::need(nrow(use_interventions) > 0, "Measure not found for this country")
    )
    
    x_pick = switch(input$sel_axis3,
                    'Date' = 'dateRep',
                    'days_since')
    x_pick2 = switch(input$sel_axis3,
                    'Date' = 'date_end',
                    'days_end')
    
    # if(input$sel_axis3 == 'Days since 1st case' | input$sel_axis3 == 'Date') {
    #   global_agg = global_agg %>% filter(cum_cases > 0) 
    # } else if(input$sel_axis3 == 'Days since 1st death') {
    #   global_agg = global_agg %>% filter(cum_deaths > 0)
    # } else if(input$sel_axis3 == 'Days since 10th death') {
    #   global_agg = global_agg %>% filter(cum_deaths >= 10) 
    # } else if(input$sel_axis3 == 'Days since 10th case') {
    #   global_agg = global_agg %>% filter(cum_cases >= 10) 
    # }
    
    global_agg = global_agg %>% 
      group_by(countriesAndTerritories) %>% 
      mutate(days_since = as.integer(as.Date(dateRep) - 
                                       as.Date(min(use_interventions$DATE_IMPLEMENTED)))) %>%
      ungroup() %>% 
      pivot_longer(names_to = 'Type', values_to = 'Number', 
                   -c(dateRep, countriesAndTerritories, popData2018, 
                      days_since))
    if(x_pick == 'days_since') global_agg = global_agg %>% filter(days_since >= 0)
      
    y_pick <- sapply(seq_along(input$sel_var3), 
                     function(x) switch(input$sel_var3[x],
                                        'Cumulative cases' = 'cum_cases',
                                        'Cumulative deaths' = 'cum_deaths',
                                        'Daily cases' = 'cases',
                                        'Daily deaths' = 'deaths',
                                        'Log cumulative cases' = 'cum_cases',
                                        'Log cumulative deaths' = 'cum_deaths',
                                        'Cases per million population' = 'cases_per_million',
                                        'Deaths per million population' = 'deaths_per_million'))
    
    global_agg = global_agg %>% 
      filter(Type %in% y_pick)
    global_agg = global_agg %>% 
      mutate(data_point = paste0("\ncountry: ",
                                 global_agg$countriesAndTerritories,
                                 "\nx_axis: ", global_agg[[x_pick]], 
                                 "\n","y_axis: ", 
                                 formatC(signif(Number,digits=3), 
                                         digits=3, format="fg", 
                                         flag="#")))
    
    # Find the number of countries and the number of variables picked and remove the legend if bigger than 10
    n_countries = global_agg %>% select(countriesAndTerritories) %>% table %>% length
    n_vars = length(input$sel_var3)
    
    p = ggplot(global_agg, aes_string(x = x_pick, y = 'Number', 
                                      colour = 'countriesAndTerritories',
                                      label = "data_point")) + 
      { if(input$sel_window) {
        geom_rect(data = use_interventions2,
                  aes_string(xmin = x_pick,
                             xmax = x_pick2,
                             ymin = 0,
                             fill = "countriesAndTerritories",
                             ymax = max(global_agg[["Number"]])
                             ),
                  colour = NA,
                  alpha = 0.3)
      }} +
      geom_line(aes(linetype = Type)) + 
      geom_rug(data = use_interventions,
               aes_string(x = x_pick, 
                          colour = "countriesAndTerritories",
                          label = "data_point"), 
               inherit.aes = FALSE) +
      #geom_point(show.legend = FALSE) +
      labs(x = input$sel_axis3,
           y = paste(input$sel_var3, collapse = ',')) + 
      scale_color_manual(values = country_colours) +
      scale_fill_manual(values = country_colours) +
      #scale_colour_brewer(palette = "Set1") + 
      { if(x_pick == 'dateRep') {
        scale_x_datetime(breaks = '1 week', labels = scales::label_date("%d%b"))
      } else {
        scale_x_continuous(breaks =  breaks_pretty(n = 10))
      }} +
      theme_shiny_dashboard() +
      { if(x_pick == 'dateRep') theme(axis.text.x = element_text(angle = 45, hjust = 1)) } +
      theme(legend.title = element_blank()) +
      {if ('Log cumulative cases' %in% input$sel_var | 
           'Log cumulative deaths' %in% input$sel_var)  {
        scale_y_continuous(trans = log1p_trans(), breaks = scales::breaks_log(n = 5))
      } else {
        scale_y_continuous(breaks = scales::breaks_pretty(n = 5))
      }} + 
      theme(legend.position = "none") + 
      { if(input$sel_smooth) geom_smooth(se = FALSE) }
    
    ggplotly(p, tooltip=c("label")) %>% 
      layout(margin = list(l = 75))
    #ggplotly(p) %>% layout(margin = list(l = 75))
    
  })
  

# Graphs tab --------------------------------------------------------------

  output$CountryPlot <- renderPlotly({
    global_agg = global %>%
      filter(countriesAndTerritories %in% input$sel_ctry) %>%
      select(dateRep, cases, deaths, countriesAndTerritories, popData2018) %>% 
      group_by(countriesAndTerritories) %>% 
      arrange(dateRep) %>% 
      mutate(cum_cases = cumsum(cases),
             cum_deaths = cumsum(deaths),
             log_cases = log(cum_cases),
             log_deaths = log(cum_deaths),
             cases_per_million = 1e6*cumsum(cases)/popData2018,
             deaths_per_million = 1e6*cumsum(deaths)/popData2018) %>% 
      ungroup()
    
    country_colours = global_agg %>%
      arrange(desc(popData2018)) %>%
      select(countriesAndTerritories) %>%
      distinct() %>%
      mutate(Colours = colorRampPalette(brewer.pal(8, "Set1"))(n())) %>%
      deframe()
    
    shiny::validate(
      shiny::need(nrow(global_agg) > 0, "Please select some countries. Use Global for worldwide values.")
    )

    x_pick = switch(input$sel_axis,
                    'Date' = 'dateRep',
                    'days_since')
    
    if(input$sel_axis == 'Days since 1st case' | input$sel_axis == 'Date') {
      global_agg = global_agg %>% filter(cum_cases > 0) 
    } else if(input$sel_axis == 'Days since 1st death') {
      global_agg = global_agg %>% filter(cum_deaths > 0)
    } else if(input$sel_axis == 'Days since 10th death') {
        global_agg = global_agg %>% filter(cum_deaths >= 10) 
    } else if(input$sel_axis == 'Days since 10th case') {
      global_agg = global_agg %>% filter(cum_cases >= 10) 
    }
    
    global_agg = global_agg %>% 
      group_by(countriesAndTerritories) %>% 
      mutate(days_since = 1:n()) %>% 
      ungroup() %>% 
      pivot_longer(names_to = 'Type', values_to = 'Number', 
                   -c(dateRep, countriesAndTerritories, popData2018, 
                      days_since))
    
    y_pick <- sapply(seq_along(input$sel_var), 
                     function(x) switch(input$sel_var[x],
                                        'Cumulative cases' = 'cum_cases',
                                        'Cumulative deaths' = 'cum_deaths',
                                        'Daily cases' = 'cases',
                                        'Daily deaths' = 'deaths',
                                        'Log cumulative cases' = 'cum_cases',
                                        'Log cumulative deaths' = 'cum_deaths',
                                        'Cases per million population' = 'cases_per_million',
                                        'Deaths per million population' = 'deaths_per_million'))
    
    global_agg = global_agg %>% 
      filter(Type %in% y_pick)
    global_agg = global_agg %>% 
      mutate(data_point = paste0("\ncountry: ",global_agg$countriesAndTerritories,"\nx_axis: ", global_agg[[x_pick]], "\n","y_axis: ", formatC(signif(Number,digits=3), digits=3, format="fg", flag="#")))
    
    # Find the number of countries and the number of variables picked and remove the legend if bigger than 10
    n_countries = global_agg %>% select(countriesAndTerritories) %>% table %>% length
    n_vars = length(input$sel_var)
    
    p = ggplot(global_agg, aes_string(x = x_pick, y = 'Number', colour = 'countriesAndTerritories',
                                    label = "data_point")) + 
      geom_line(aes(linetype = Type)) + 
      #geom_point(show.legend = FALSE) +
      labs(x = input$sel_axis,
           y = paste(input$sel_var, collapse = ',')) + 
      scale_color_manual(values = country_colours) +
      #scale_colour_brewer(palette = "Set1") + 
      { if(x_pick == 'dateRep') {
        scale_x_datetime(breaks = '1 week', labels = scales::label_date("%d%b"))
      } else {
        scale_x_continuous(breaks =  breaks_pretty(n = 10), labels = comma)
      }} +
      theme_shiny_dashboard() +
      { if(x_pick == 'dateRep') theme(axis.text.x = element_text(angle = 45, hjust = 1)) } +
      theme(legend.title = element_blank()) +
      {if ('Log cumulative cases' %in% input$sel_var | 
           'Log cumulative deaths' %in% input$sel_var)  {
        scale_y_continuous(trans = log1p_trans(), 
                           labels = comma,
                           breaks = scales::breaks_log(n = 5))
      } else {
        scale_y_continuous(labels = comma,
                           breaks = scales::breaks_pretty(n = 5))
      }} + 
      {if(n_countries * n_vars > 10) theme(legend.position = "none")}
    
    ggplotly(p, tooltip=c("label")) %>% layout(margin = list(l = 75))
    #ggplotly(p) %>% layout(margin = list(l = 75))
    
  })
  

# Info Boxes --------------------------------------------------------------
  
  #Ireland cases infobox in summary tab
  output$ireCasesBox <- renderValueBox({
    if(!is.na(latest_irish_totals$`Total number of cases`)) {
      latest_val = latest_irish_totals$`Total number of cases`
      latest_date = format(latest_irish_data$updated[2] %>% pull, 
                                format = "%d-%b, %H:%M")
      #latest_date = format(latest_irish_totals$Date, "%d-%b")
      previous_val = previous_irish_totals$`Total number of cases`
      previous_date = format(previous_irish_totals$Date, "%d-%b")
    } else {
      latest_val = latest_irish_complete$`Total number of cases`
      latest_date = format(latest_irish_data$updated[1] %>% pull, 
                           "%d-%b, %H:%M")
      #latest_date = format(latest_irish_complete$Date, "%d-%b")
      previous_val = previous_irish_complete$`Total number of cases`
      previous_date = format(previous_irish_complete$Date, "%d-%b")
    }
    pc_change = round(100*(latest_val/previous_val - 1))
    html_message = get_html_message(pc_change)
    val = str_pad(format(latest_val, 
                         big.mark=','), 9, side = 'right')
    text = paste0("Ireland: Diagnoses",br(),
                  html_message,' ', 
                  pc_change,'% since previous day',
                  br(),em("Updated: ",latest_date))
    valueBox(value = tags$p(val, style = "font-size: 2vmax;"), 
             subtitle = tags$p(HTML(text)),#, style = "font-size: 0.6vw;"),
             color = 'olive',
             icon = icon("thermometer-three-quarters"))
  })
  
  #Ireland deaths infobox in summary tab
  output$ireDeathsBox <- renderValueBox({
    if(!is.na(latest_irish_totals$`Total number of deaths`)) {
      latest_val = latest_irish_totals$`Total number of deaths`
      latest_date = format(latest_irish_data$updated[2] %>% pull, "%d-%b, %H:%M")
      #latest_date = format(latest_irish_totals$Date, "%d-%b")
      previous_val = previous_irish_totals$`Total number of deaths`
      previous_date = format(previous_irish_totals$Date, "%d-%b")
    } else {
      latest_val = latest_irish_complete$`Total number of deaths`
      latest_date = format(latest_irish_data$updated[1] %>% pull, "%d-%b, %H:%M")
      #latest_date = format(latest_irish_complete$Date, "%d-%b")
      previous_val = previous_irish_complete$`Total number of deaths`
      previous_date = format(previous_irish_complete$Date, "%d-%b")
    }
    pc_change = round(100*(latest_val/previous_val - 1))
    html_message = get_html_message(pc_change)
    val = str_pad(format(latest_val, 
                         big.mark=','), 9, side = 'right')
    text = paste0("Ireland: Deaths",br(),
                  html_message,' ', 
                  pc_change,'% since previous day',
                  br(),em("Updated: ",latest_date))
    valueBox(value = tags$p(val, style = "font-size: 2vmax;"), 
             subtitle = tags$p(HTML(text)),#, style = "font-size: 0.6vw;"),
             color = 'olive',
             icon = icon("exclamation-triangle"))
  })
  
  #Ireland hospitalised infobox in summary tab
  output$ireHospBox <- renderValueBox({
    if(!is.na(latest_irish_totals$`Total number hospitalised`)) {
      latest_val = latest_irish_totals$`Total number hospitalised`
      latest_date = format(latest_irish_data$updated[2] %>% pull, "%d-%b, %H:%M")
      #latest_date = format(latest_irish_totals$Date, "%d-%b")
      previous_val = previous_irish_totals$`Total number hospitalised`
      previous_date = format(previous_irish_totals$Date, "%d-%b")
    } else {
      latest_val = latest_irish_complete$`Total number hospitalised`
      latest_date = format(latest_irish_data$updated[1] %>% pull, "%d-%b, %H:%M")
      #latest_date = format(latest_irish_complete$Date, "%d-%b")
      previous_val = previous_irish_complete$`Total number hospitalised`
      previous_date = format(previous_irish_complete$Date, "%d-%b")
    }
    pc_change = round(100*(latest_val/previous_val - 1))
    html_message = get_html_message(pc_change)
    val = str_pad(format(latest_val, big.mark=','), 9, side = 'right')
    
    text = paste0("Ireland: Hospitalised ",br(),
                  html_message,' ', 
                  pc_change,'% since previous day',
                  br(),em("Updated: ",latest_date))
    valueBox(value = tags$p(val, style = "font-size: 2vmax;"), 
             subtitle = tags$p(HTML(text)),#, style = "font-size: 0.6vw;"),
             color = 'olive',
             icon = icon("hospital"))
  })
  
  #Ireland ICU infobox in summary tab
  output$ireICUBox <- renderValueBox({
    if(!is.na(latest_irish_totals$`Total number admitted to ICU`)) {
      latest_val = latest_irish_totals$`Total number admitted to ICU`
      latest_date = format(latest_irish_data$updated[2] %>% pull, "%d-%b, %H:%M")
      #latest_date = format(latest_irish_totals$Date, "%d-%b")
      previous_val = previous_irish_totals$`Total number admitted to ICU`
      previous_date = format(previous_irish_totals$Date, "%d-%b")
    } else {
      latest_val = latest_irish_complete$`Total number admitted to ICU`
      latest_date = format(latest_irish_data$updated[1] %>% pull, "%d-%b, %H:%M")
      #latest_date = format(latest_irish_complete$Date, "%d-%b")
      previous_val = previous_irish_complete$`Total number admitted to ICU`
      previous_date = format(previous_irish_complete$Date, "%d-%b")
    }
    pc_change = round(100*(latest_val/previous_val - 1))
    html_message = get_html_message(pc_change)
    val = str_pad(format(latest_val,
                         big.mark=','), 9, side = 'right')
    text = paste0("Ireland: ICU",br(),
                  html_message,' ', 
                  pc_change,'% since previous day',
                  br(),em("Updated: ",latest_date))
    valueBox(value = tags$p(val, style = "font-size: 2vmax;"), 
             subtitle = tags$p(HTML(text)),#, style = "font-size: 0.6vw;"),
             color = 'olive',
             icon = icon("briefcase-medical"))
  })
  
  #Worldwide cases infobox in summary tab
  output$wCasesBox <- renderValueBox({
    updated_data = format(last_updated$dates[2], "%d-%b, %H:%M")
    latest_date = format(max(global_world$dateRep), "%d-%b")
    pc_change = round(100*(global_world %>% select(cases) %>% sum/global_world %>% select(cases) %>% slice(-n()) %>% sum - 1))
    html_message = get_html_message(pc_change)
    val = str_pad(format(global_world %>% select(cases) %>% sum, 
                         big.mark=','), 9, side = 'right')
    text = paste0("Global: Diagnoses",
                  br(),html_message,' ', 
                  pc_change,'% since previous day. ',
                  em("Updated: ",updated_data))
    valueBox(value = tags$p(val, style = "font-size: 2.5vmax;"), 
             subtitle = tags$p(HTML(text)),#, style = "font-size: 0.6vw;"),
             color = 'maroon',
             icon = icon("globe"))
  })
  
  #Worldwide deaths infobox in summary tab
  output$wDeathsBox <- renderValueBox({  
    latest_date = format(max(global_world$dateRep), "%d-%b")
    updated_data = format(last_updated$dates[2], "%d-%b, %H:%M")
    pc_change = round(100*(global_world %>% select(deaths) %>% sum/global_world %>% select(deaths) %>% slice(-n()) %>% sum - 1))
    html_message = get_html_message(pc_change)
    val = str_pad(format(global_world %>% select(deaths) %>% sum, 
                         big.mark=','), 9, side = 'right')
    text = paste0("Global: Deaths",
                  br(),html_message,' ', 
                  pc_change,'% since previous day. ',
                  em("Updated: ",updated_data))
    valueBox(value = tags$p(val, style = "font-size: 2.5vmax;"),
             subtitle = tags$p(HTML(text)),#, style = "font-size: 0.6vw;"),
             color = 'maroon',
             icon = icon("cross"))
  })
  
  # Worst hit country
  output$worstHitCountryBox <- renderValueBox({  
    latest_date = format(max(global_world$dateRep), "%d-%b")
    worst_countries = global %>% 
      filter(countriesAndTerritories != 'Global') %>% 
      group_by(countriesAndTerritories) %>% 
      summarise(totalDeaths = sum(deaths)) %>% 
      ungroup() %>% 
      arrange(desc(totalDeaths)) %>% 
      top_n(1)
    # name = str_sub(str_replace(worst_countries$countriesAndTerritories[1], '_', ' '),
    #                1, 10)
    name = str_replace(worst_countries$countriesAndTerritories[1], '_', ' ')
    val = format(worst_countries$totalDeaths[1], big.mark=',')
    valueBox(value = tags$p(name, 
                            style = paste0("font-size: ",ifelse(nchar(name)<10, 3, 3*9/nchar(name)),"vmax;")),
             subtitle = HTML(paste0("Most deaths overall: ", val)),
             color = 'light-blue',
             icon = icon("arrow-up"))
  })
  
  # Biggest Increase in Deaths Country
  output$increaseDeathBox <- renderValueBox({  
    latest_date = format(max(global_world$dateRep), "%d-%b")
    biggest_increase = global_change %>% 
      filter(deaths != 0) %>% 
      arrange(deaths)
    # name = str_sub(str_replace(biggest_increase$countriesAndTerritories[1], '_', ' '),
    #                1, 10)
    name = str_replace(biggest_increase$countriesAndTerritories[1], '_', ' ')
    val = format(-biggest_increase$deaths[1], big.mark=',')
    
    valueBox(value = tags$p(name, 
                            style = paste0("font-size: ",ifelse(nchar(name)<10, 3, 3*9/nchar(name)),"vmax;")),
             subtitle = HTML(paste0("Biggest increase in deaths since previous day: ", val)),
             color = 'light-blue',
             icon = icon("arrow-up"))
  })
  
  # Biggest decrease in deaths
  output$bigDecreaseBox <- renderValueBox({  
    latest_date = format(max(global_world$dateRep), "%d-%b")
    biggest_decrease = global_change %>% 
      filter(deaths != 0) %>% 
      arrange(desc(deaths)) %>% 
      slice(1)
    # name = str_sub(str_replace(biggest_decrease$countriesAndTerritories, '_', ' '),
    #                1, 10)
    name = str_replace(biggest_decrease$countriesAndTerritories, '_', ' ')
    val = format(abs(biggest_decrease$deaths), big.mark=',')
    
    valueBox(value = tags$p(name, 
                            style = paste0("font-size: ",ifelse(nchar(name)<10, 3, 3*9/nchar(name)),"vmax;")),
             subtitle = HTML(paste0("Biggest reduction in deaths since previous day: ", 
                                    val)),
             color = 'light-blue',
             icon = icon("arrow-down", class = "color: rgb(59, 91, 152)"))
  })
  
  output$bigDailyBox <- renderValueBox({  
    latest_date = format(max(global_world$dateRep), "%d-%b")
    daily_death = global %>% 
      filter(countriesAndTerritories != 'Global') %>% 
      group_by(countriesAndTerritories) %>% 
      filter(dateRep == max(dateRep)) %>% 
      ungroup() %>% 
      arrange(desc(deaths)) %>% 
      slice(1)
    # name = str_sub(str_replace(daily_death$countriesAndTerritories[1], '_', ' '),
    #                1, 10)
    name = str_replace(daily_death$countriesAndTerritories[1], '_', ' ')
    val = format(daily_death$deaths[1], big.mark=',')
    valueBox(value = tags$p(name, 
                            style = paste0("font-size: ",ifelse(nchar(name)<10, 3, 3*9/nchar(name)),"vmax;")),
             subtitle = HTML(paste0("Most deaths today: ",val)),
             color = 'light-blue',
             icon = icon("arrow-up"))
  })

# Data tables -------------------------------------------------------------

  
  # Highest daily
  output$highestDaily <- DT::renderDataTable({
    DT::datatable(global_table3 %>% select(Country, `Daily deaths`) %>% arrange(desc(`Daily deaths`)),
                  options = list(
                    pageLength = 10,
                    #scrollY='calc((100vh - 290px)/1.0)',
                    searching = TRUE,
                    paging=TRUE,
                    autoWidth = TRUE,
                    rownames=TRUE
                    #rowCallback = JS("function(r,d) {$(r).attr('height', '100px')}")
                  ))
  })  
  
  # HighestH total
  output$highestTotal <- DT::renderDataTable({
    DT::datatable(global_table3 %>% select(Country, `Total deaths`) %>% arrange(desc(`Total deaths`)),
                  options = list(
                    pageLength = 10,
                    #scrollY='calc((100vh - 290px)/1.0)',
                    searching = TRUE,
                    paging=TRUE,
                    autoWidth = TRUE,
                    rownames=TRUE
                  ))
  })  
  
  # Biggest change
  output$biggestChange <- DT::renderDataTable({
    biggest_change = global_change %>% 
      filter(deaths != 0) %>% 
      arrange(desc(deaths)) %>% 
      rename(Country = countriesAndTerritories) %>% 
      mutate(`Change in deaths` = -deaths) %>% 
      select(Country, `Change in deaths`) %>% 
      arrange(desc(`Change in deaths`))
    DT::datatable(biggest_change,
                  options = list(
                    pageLength = 10,
                    #scrollY='calc((100vh - 290px)/1.0)',
                    searching = TRUE,
                    paging=TRUE,
                    autoWidth = TRUE,
                    rownames=TRUE
                  ))
  })  
  

# Counties tab ------------------------------------------------------------
  
  #Counties table in Counties tab
  output$countyCasesTable <- DT::renderDataTable({
    DT::datatable(caption = paste0("Updated: ",all_county_table$Date[[1]]),
                  latest_county_table[order(latest_county_table$Cases, decreasing=TRUE), c('County', 'Number of Cases')],
                  options = list(
                    pageLength = 20,
                    scrollY='calc((100vh - 290px)/1.0)',
                    searching = FALSE,
                    paging=FALSE
                  ),
                  rownames=FALSE
    )
  })  
  
  #Map in Counties tab
  output$covidMap <- renderLeaflet({
    leaflet(cs2) %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      setView(lng = -7.635498, lat = 53.186288, zoom = 7) %>% 
      addMarkers(lat = ~LATITUDE,lng = ~LONGITUDE,
                 icon = trend_icon,popup = popupGraph(county_cumulative_cases)) %>% 
      addPolygons(stroke = FALSE, 
                  smoothFactor = 0.3, 
                  fillOpacity = 0.7,
                  fillColor = ~pal2(log2(Cases)),
                  label = ~paste0(NAME_TAG, ": ", `Number of Cases`, ' cases') ) %>%
      addLegend(pal = pal2, title='Cases', values = ~log2(Cases), opacity = 1.0,
                labFormat = labelFormat(transform = function(x) round(2^x)))
  })
  
  output$covidMap2 <- renderLeaflet({
    leaflet(cs2) %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      setView(lng = -7.635498, lat = 53.186288, zoom = 6) %>% 
      # addMarkers(lat = ~LATITUDE,lng = ~LONGITUDE,
      #            icon = trend_icon,popup = popupGraph(county_cumulative_cases)) %>% 
      addPolygons(stroke = FALSE, 
                  smoothFactor = 0.3, 
                  fillOpacity = 0.7,
                  fillColor = ~pal2(log2(Cases)),
                  #fillColor = ~viridis_pal(option = "B")(log2(Cases)),
                  label = ~paste0(NAME_TAG, ": ", `Number of Cases`, ' cases') ) #%>%
      # addLegend(pal = pal2, title='Cases', values = ~log2(Cases), opacity = 1.0,
      #           labFormat = labelFormat(transform = function(x) round(2^x)))
  })
  

# Animation tab -----------------------------------------------------------

  observe({
    # Find the max and min dates for these countries
    global_use = global %>% 
      filter(countriesAndTerritories %in% input$sel_ctry2) %>% 
      group_by(countriesAndTerritories) %>% 
      arrange(dateRep) %>% 
      mutate(cum_cases = cumsum(cases),
             cum_deaths = cumsum(deaths),
             daily_cases = cases,
             daily_deaths = deaths,
             cases_per_million = 1e6*cumsum(cases)/popData2018,
             deaths_per_million = 1e6*cumsum(deaths)/popData2018) %>% 
      ungroup() %>% 
      filter(cum_cases >0)
    
    country_colours2 <<- global_use %>%
      arrange(desc(popData2018)) %>% 
      select(countriesAndTerritories) %>% 
      distinct() %>%
      mutate(Colours = colorRampPalette(brewer.pal(8, "Set1"))(n())) %>%
      deframe()
    
    if(str_detect(input$sel_horiz,'death') | 
       str_detect(input$sel_vert,'death')) {
      global_use = global_use %>% 
        filter(cum_deaths > 0)
    }
    
    if(str_detect(input$sel_horiz,'Daily cases') |
       str_detect(input$sel_horiz,'Daily cases') | 
       str_detect(input$sel_vert,'daily cases') | 
       str_detect(input$sel_vert,'daily cases')) {
      global_use = global_use %>% 
        filter(daily_cases > 0)
    }
    
    if(str_detect(input$sel_horiz,'daily deaths') | 
       str_detect(input$sel_vert,'daily deaths') |
       str_detect(input$sel_horiz,'Daily deaths') | 
       str_detect(input$sel_vert,'Daily deaths')) {
      global_use = global_use %>% 
        filter(daily_deaths > 0)
    }
    
    shiny::validate(
      shiny::need(nrow(global_use) > 0, "Please select some countries. Use Global for worldwide values.")
    )
    
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session, "theDate", 
                      min = min(global_use$dateRep),
                      timeFormat = "%d/%b")
  })
  
  ani_graph = reactive({
    global_agg = global %>%
      filter(countriesAndTerritories %in% input$sel_ctry2) %>%
      select(dateRep, cases, deaths, countriesAndTerritories, popData2018, day, month) %>% 
      group_by(countriesAndTerritories) %>% 
      arrange(dateRep) %>% 
      mutate(cum_cases = cumsum(cases),
             cum_deaths = cumsum(deaths),
             daily_cases = cases,
             daily_deaths = deaths,
             cases_per_million = 1e6*cumsum(cases)/popData2018,
             deaths_per_million = 1e6*cumsum(deaths)/popData2018) %>% 
      ungroup()
    
    
    # country_colours = global %>% arrange(desc(popData2018)) %>% 
    #   select(countriesAndTerritories) %>% 
    #   distinct() %>%
    #   #mutate(Colours = div_gradient_pal()(seq(0, 1, length.out = n()))) %>%
    #   mutate(Colours = colorRampPalette(brewer.pal(8, "Set1"))(n())) %>%
    #   # mutate(Colours = sample(new_colors, n())) %>% 
    #   # mutate(Colours = replace(Colours,
    #   #                          countriesAndTerritories == "Ireland", "green")) %>%
    #   # mutate(Colours = replace(Colours,
    #   #                          countriesAndTerritories == "Ireland", "#40C575")) %>%
    #   deframe()
    
    shiny::validate(
      shiny::need(nrow(global_agg) > 0, "Please select some countries. Use Global for worldwide values.")
    )
    
    x_pick = switch(input$sel_horiz,
                    'Cumulative cases' = 'cum_cases', 
                    'Cumulative deaths' = 'cum_deaths',
                    'Daily cases' = 'daily_cases',
                    'Daily deaths' = 'daily_deaths',
                    'Logp1 daily cases' = 'daily_cases',
                    'Logp1 daily deaths' = 'daily_deaths',
                    'Sqrt daily cases' = 'daily_cases',
                    'Sqrt daily deaths' = 'daily_deaths',
                    'Sqrt cumulative cases' = 'cum_cases',
                    'Sqrt cumulative deaths' = 'cum_deaths',
                    'Logp1 cumulative cases' = 'cum_cases',
                    'Logp1 cumulative deaths' = 'cum_deaths',
                    'Cumulative cases per million population' = 'cases_per_million',
                    'Cumulative deaths per million population' = 'deaths_per_million')
    y_pick = switch(input$sel_vert,
                    'Cumulative cases' = 'cum_cases', 
                    'Cumulative deaths' = 'cum_deaths',
                    'Sqrt cumulative cases' = 'cum_cases',
                    'Sqrt cumulative deaths' = 'cum_deaths',
                    'Logp1 cumulative cases' = 'cum_cases',
                    'Logp1 cumulative deaths' = 'cum_deaths',
                    'Daily cases' = 'daily_cases',
                    'Daily deaths' = 'daily_deaths',
                    'Logp1 daily cases' = 'daily_cases',
                    'Logp1 daily deaths' = 'daily_deaths',
                    'Sqrt daily cases' = 'daily_cases',
                    'Sqrt daily deaths' = 'daily_deaths',
                    'Cumulative cases per million population' = 'cases_per_million',
                    'Cumulative deaths per million population' = 'deaths_per_million')
    
    global_agg = global_agg %>%
      mutate(countriesAndTerritories = parse_factor(countriesAndTerritories),
             month_day = format(dateRep, format = "%b-%d")) %>% 
             #day_month = paste0(day,'/', month)) %>% 
      select("dateRep", x_pick, y_pick, "countriesAndTerritories", 'month_day')
    
    # Correct for if log is in the title
    if(str_detect(input$sel_horiz,'Logp1')) {
      global_agg = global_agg %>% mutate(!!x_pick := get(x_pick) + 1)
    }
    if(str_detect(input$sel_vert,'Logp1')) {
      global_agg = global_agg %>% mutate(!!y_pick := get(y_pick) + 1)
    }
    
    # Find the median values of the biggest country
    ggplot(global_agg %>% filter(dateRep == input$theDate), 
           aes_string(x_pick, y_pick, colour = "countriesAndTerritories", 
                      size = y_pick)) +
      scale_color_manual(values = country_colours2) +
      annotation_custom(grid::textGrob(global_agg$month_day[match(input$theDate, global_agg$dateRep)],
                                       gp=gpar(fontsize=200, col="grey")), 
                        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
      geom_point(data = global_agg %>% filter(dateRep <= input$theDate)) + 
      geom_line(data = global_agg %>% filter(dateRep <= input$theDate), alpha = 0.4) +
      geom_label(aes(label = countriesAndTerritories)) +
      labs(x = str_to_sentence(str_remove(str_remove(input$sel_horiz, "Sqrt "), 'Log ')),
           y = str_to_sentence(str_remove(str_remove(input$sel_vert, "Sqrt "), 'Log '))) +
      scale_size(range = c(2, 12)) +
      { if(str_detect(input$sel_horiz,'Sqrt')) {
        scale_x_sqrt(breaks = scales::breaks_pretty(n = 7),
                     labels = comma,
                     limits = c(min(global_agg[[x_pick]]), max(global_agg[[x_pick]])))
      } else if(str_detect(input$sel_horiz,'Logp1')) {
        scale_x_continuous(trans = log1p_trans(), labels = comma,
                           breaks = scales::breaks_log(n = 7),
                           limits = c(min(global_agg[[x_pick]]), max(global_agg[[x_pick]])))
      } else {
        scale_x_continuous(labels = comma,
                           breaks = scales::breaks_pretty(n = 7),
                           limits = c(min(global_agg[[x_pick]]), max(global_agg[[x_pick]])))
      }} +
      { if(str_detect(input$sel_vert,'Sqrt')) {
        scale_y_sqrt(labels = comma,
                     breaks = scales::breaks_pretty(n = 7),
                     limits = c(min(global_agg[[y_pick]]), max(global_agg[[y_pick]])))
      } else if(str_detect(input$sel_vert,'Logp1')) {
        scale_y_continuous(labels = comma,
                           trans = log1p_trans(), 
                           breaks = scales::breaks_log(n = 7),
                           limits = c(min(global_agg[[y_pick]]), max(global_agg[[y_pick]])))
      } else {
        scale_y_continuous(labels = comma,
                           breaks = scales::breaks_pretty(n = 7),
                           limits = c(min(global_agg[[y_pick]]), max(global_agg[[y_pick]])))
      }} +
      theme_shiny_dashboard() +
      theme(legend.position = 'None',
            axis.title.y = element_text(angle = 0, vjust = 1, hjust=0))
    
  })
  
  output$AnimPlot <- renderPlot({
    ani_graph()
  })
    

# Hospital tab ------------------------------------------------------------


# age in hospital plot ----------------------------------------------------

  # Create latest hospitalised data
  latest_data_hosp = latest_irish_data$by_age %>% 
    # Reocde the two different age cateogisationss
    mutate("Age2" = recode(.$Age, 
                           "<1" = "<5",
                           "1 - 4" = "<5",
                           .default = as.character(.$Age)),
           "Age2" = factor(Age2, 
                           levels = c("<5", "5 - 14", "15 - 24", 
                                      "25 - 34", "35 - 44", "45 - 54", 
                                      "55 - 64", "65+", "Unknown"), 
                           ordered = TRUE)) %>% 
    # Turn the case numbers into integers
    mutate_at(vars(`Number of cases`, `Number of hospitalised cases`),
              as.integer) %>%
    # Group by date and age to correct age categories
    group_by(Date, Age2) %>% 
    # Summarise to sum over <1, 1-4, and <5
    summarise("Total cases" = sum(`Number of cases`, na.rm = TRUE),
              "Hospitalised cases" = sum(`Number of hospitalised cases`, 
                                                   na.rm = TRUE)) %>% 
    ungroup() %>% 
    # Arrange in a nice format for easy inspection
    arrange(desc(Date), Age2) %>% 
    # Create non-hospitalised cases
    mutate("Non-hospitalised cases" = 
             `Total cases` - `Hospitalised cases`) %>% 
    # Create nice format for plotting
    pivot_longer(names_to = 'Type', values_to = "Cases",
                 c(-Date, -Age2)) %>% 
    mutate("data_point" = paste0("\n<b>Date: </b>", .$Date, "\n","<b>Age: </b>", .$Age2, "\n","<b>Count:</b> ", .$Cases))

  output$ageHist <- renderPlotly({
    g <- ggplot(data = latest_data_hosp %>% filter(Date == max(Date),
                                                   Type != "Total cases"),
                aes(x = Age2, y = Cases, fill = Type, 
                    text = data_point)) +
      geom_bar(stat = 'identity') +
      theme_shiny_dashboard() +
      labs(x="Age Group", y = "Count") +
      theme(legend.title = element_blank())+
      ggtitle('Cases by age: Ireland')
    
    ggplotly(g, tooltip = c("text")) %>% 
      layout(margin = list(l = 75)) %>%
      plotly::config(displayModeBar = FALSE)
    
  })
  
  
  output$ageHospHistory <- renderPlotly({
    
    g <- ggplot(data = latest_data_hosp %>% 
                 filter(Type == "Hospitalised cases",
                        Age2 != "Unknown"), 
               aes(x = as.Date(Date), y = Cases, color = Age2))+
      geom_line(aes(group = Age2, text = data_point)) +
      theme_shiny_dashboard() +
      scale_x_date(breaks = '3 days',
                       date_labels = "%d%b") +
      labs(x="Date", y = "Count") +
      ggtitle('Hospitalised by Age Group') + 
      theme(legend.title = element_blank(),
            axis.ticks = element_blank())
    
    ggplotly(g,tooltip = c("text")) %>% 
      plotly::config(displayModeBar = FALSE) %>% 
      layout(margin = list(l = 75))
      
    
  })
  
  output$ageTotalHistory <- renderPlotly({
    
    g <- ggplot(data = latest_data_hosp %>% 
                 filter(Type == "Total cases",
                        Age2 != "Unknown"), 
               aes(x = as.Date(Date), y = Cases, color = Age2))+
      geom_line(aes(group = Age2, text = data_point)) +
      theme_shiny_dashboard() +
      labs(x="Date", y = "Count") +
      scale_x_date(breaks = '3 days',
                   date_labels = "%d%b") +
      ggtitle('Total cases by Age Group') + 
      theme(legend.title = element_blank(),
            axis.ticks = element_blank())
    
    ggplotly(g,tooltip = c("text")) %>% layout(margin = list(l = 75))    %>%
      plotly::config(displayModeBar = FALSE)

  })

# Gender Breakdown --------------------------------------------------------
  
  # latest gender plot (histogram)
  output$genderCases <- renderPlotly({
    gender_data = latest_irish_data$by_gender %>% 
      mutate(data_point = paste0("\n<b>Date:</b> ", as.Date(Date), "\n","<b>Gender:</b> ", Gender, "\n","<b>Percentage: </b>", `% Total`,"%"),
             "% Total" = `% Total`)

    g <- ggplot(data = gender_data %>% filter(Date == max(Date)), 
                aes(Gender, `% Total`, fill=Gender, text=data_point))+
      geom_bar(stat = 'identity',width = 0.7, alpha=0.8, position = position_dodge())+
      theme_shiny_dashboard() +
      labs(y="%", x = "") +
      theme(legend.position = 'none')+
      ggtitle('Gender Breakdown')
    
    ggplotly(g,tooltip = c("text")) %>% 
      layout(margin = list(l = 75)) %>%
      plotly::config(displayModeBar = FALSE)
  })
  
  # time series of gender data
  output$genderCasesHistory <- renderPlotly({
    gender_data_by_time = latest_irish_data$by_gender %>% 
      mutate(data_point = paste0("\n<b>Date:</b> ", Date, "\n","<b>Gender:</b> ", Gender, "\n","<b>Percentage: </b>", `% Total`,"%"),
             "% Total" = `% Total`)
    
    g <- ggplot(gender_data_by_time, 
                aes(x = as.Date(Date), y = `% Total`,colour = Gender)) + 
      geom_line(size=1, aes(group = Gender, text = data_point)) +
      theme_shiny_dashboard() +
      scale_x_date(breaks = '3 days',
                   date_labels = "%d%b") +
      labs(x="Date", y = "Percentage") +
      ggtitle('Gender Breakdown') + 
      theme(
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(face = "bold", 
                                   size = 12, angle = 45))
    
    ggplotly(g,tooltip = c("text")) %>% layout(margin = list(l = 75))    %>%
      plotly::config(displayModeBar = FALSE)
    
  })
  

# Proportion healthcare patients ------------------------------------------

  healthcare_data = latest_irish_data$totals %>% 
    rename(`Total cases` = `Total number of cases`,
           `Healthcare workers` = `Total number of healthcare workers`) %>%
    select(Date, `Total cases`, `Healthcare workers`) %>% 
    drop_na %>% 
    pivot_longer(names_to = "Type", values_to = 'Cases', -Date) %>% 
    mutate(data_point = paste0("\n<b>Date:</b> ", Date, "\n","<b>Type:</b> ", Type, "\n","<b>Cases: </b>", Cases))
    
  
  # histogram
  output$healthcarePatients <- renderPlotly({
    
    g <- ggplot(data = healthcare_data %>% filter(Date == max(Date)), 
                aes(Type, Cases, fill=Type, text = data_point))+
      geom_bar(stat = 'identity',width = 0.7, alpha=0.8, position = position_dodge())+
      theme_shiny_dashboard() +
      labs(y="Count", x = "") +
      theme(legend.position = 'none')+
      ggtitle('Hospitalisation type') +
      scale_fill_brewer(palette="Accent")
    
    ggplotly(g,tooltip = c("text")) %>% layout(margin = list(l = 75))    %>%
      plotly::config(displayModeBar = FALSE)
    
    
  })  
  
  
  # timeseries
  output$healthcarePatientsHistory <- renderPlotly({
    
    g <- ggplot(healthcare_data, 
                aes(x = as.Date(Date), y = Cases, colour = Type)) + 
      geom_line(size=1, aes(group = Type, text = data_point))+
      theme_shiny_dashboard() +
      scale_x_date(breaks = '3 days',
                       date_labels = "%d%b") +
      labs(x="Date", y = "Count") +
      ggtitle('Number of Healthcare Workers Tested Positive') + 
      theme(
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(face = "bold", 
                                   size = 12, angle = 45))+
      scale_color_brewer(palette="Accent")
    
    ggplotly(g,tooltip = c("text")) %>% layout(margin = list(l = 75))    %>%
      plotly::config(displayModeBar = FALSE)
    
  })
  
# How virus spreads -------------------------------------------------------

  
  transmission_data = latest_irish_data$by_transmission %>% 
    filter(Type == 'Percentage') %>% 
    mutate(data_point = paste0("\n<b>Date:</b> ", Date, "\n","<b>Type:</b> ", Transmission, "\n","<b>%: </b>", Cases),
           Transmission = recode(Transmission, 
                                 "Close contact with confirmed case" = 
                                   "Close contact with\nconfirmed case",
                                 "Community transmission" = 
                                   "Community\ntransmission",
                                 "Under investigation" = 
                                   "Under\ninvestigation"))
  
  #histogram
  output$howContracted <- renderPlotly({
      
      g <- ggplot(data = transmission_data %>% filter(Date == max(Date)), 
                  aes(x = reorder(Transmission, Cases), 
                      y = Cases, fill=Transmission, text = data_point))+
        geom_bar(stat = 'identity',width = 0.7, alpha=0.8, 
                 position = position_dodge())+
        theme_shiny_dashboard() +
        labs(y="%", x = "") +
        coord_flip()+
        theme(legend.position = 'none')+
        ggtitle('How is COVID-19 Being Transmitted?')+
        scale_fill_brewer(palette="Set2")
    
    ggplotly(g,tooltip = c("text")) %>% layout(margin = list(l = 75))    %>%
      plotly::config(displayModeBar = FALSE)
    
  }) 
  
  
  #timeseries
  
  output$howContractedHistory <- renderPlotly({
    
    g <- ggplot(transmission_data, 
                aes(x = as.Date(Date), y = Cases, colour = Transmission)) + 
      geom_line(size=1, aes(group = Transmission, text = data_point))+
      theme_shiny_dashboard() +
      scale_x_date(breaks = '3 days',
                       date_labels = "%d%b") +
      labs(x="Date", y = "%") +
      ggtitle('How is COVID-19 Being Transmitted?') + 
      theme(
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(face = "bold", 
                                   size = 12, angle = 45))+
      scale_color_brewer(palette="Set2")
    
    
    ggplotly(g,tooltip = c("text")) %>% layout(margin = list(l = 75))    %>%
      plotly::config(displayModeBar = FALSE)
    
    
  })
  

# ICU percentage ----------------------------------------------------------
  
  #timeseries
  ICU_data = latest_irish_data$totals %>% 
    select(Date,
           `Total number hospitalised`,
           `Total number admitted to ICU`) %>% 
    rename(`Hospitalised` = `Total number hospitalised`,
           `ICU` = `Total number admitted to ICU`) %>% 
    pivot_longer(names_to = "Type", values_to = "Cases", -Date) %>% 
    mutate(data_point = paste0("\n<b>Date:</b> ", Date, "\n","<b>Type:</b> ", Type, "\n","<b>Count: </b>", Cases)) %>% 
    drop_na
  
  output$icuProportionHistory <- renderPlotly({
    
    g <- ggplot(ICU_data, 
                aes(x = as.Date(Date), y = Cases, colour = Type)) + 
      geom_line(size=1, aes(group = Type, text = data_point))+
      theme_shiny_dashboard() +
      scale_x_date(breaks = '3 days',
                       date_labels = "%d%b") +
      labs(x="Date", y = "Count") +
      ggtitle('Hospitalised Patients') + theme(
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(face = "bold", 
                                   size = 12, angle = 45))
    
    ggplotly(g,tooltip = c("text") ) %>% layout(margin = list(l = 75))    %>%
      plotly::config(displayModeBar = FALSE)
    
    
  }) 
  
  
  #histogram
  output$icuProportion <- renderPlotly({
    
    g <- ggplot(data = ICU_data %>% filter(Date == max(Date)), 
                aes(x =Type, y = Cases, fill=Type, text =data_point))+
      geom_bar(stat = 'identity',width = 0.7, alpha=0.8, 
               position = position_dodge())+
      theme_shiny_dashboard() +
      labs(y="Count", x = "") +
      theme(legend.position = 'none')+
      ggtitle('Hospitalised Patients')
    
    ggplotly(g,tooltip = c("text") ) %>% layout(margin = list(l = 75))    %>%
      plotly::config(displayModeBar = FALSE)
  })
    
})



