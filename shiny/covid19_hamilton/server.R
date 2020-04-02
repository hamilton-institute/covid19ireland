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
#library(leafpop)
#library(viridis)
library(scales)
library(ggdark)
library(stringr)
library(grid)
library(RColorBrewer)
library(wesanderson)
library(rlist)

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

ecdc_raw <- readRDS('ECDC_data_current.rds') %>% 
  mutate(countriesAndTerritories = 
           recode(countriesAndTerritories, 
                  'Cases_on_an_international_conveyance_Japan' = 'Cruise_ship',
                  'United_States_of_America' = 'USA',
                  'United_Kingdom' = 'UK'
                  ))

ecdc_world = ecdc_raw %>% 
  group_by(dateRep) %>% 
  summarise(deaths = sum(deaths, na.rm = TRUE),
            cases = sum(cases, na.rm = TRUE),
            popData2018 = sum(popData2018, na.rm = TRUE),
            day = min(day),
            month = min(month)) %>% 
  mutate(countriesAndTerritories = 'Global')

# Bind together and add colours - specify some that are required
ecdc = bind_rows(ecdc_raw, ecdc_world) 

#All tables contains information on a county-by-county basis
#will be used in the Counties tab
#all_tables <- readRDS('all_tables_current.rds')
all_tables <- readRDS('/Users/dairehealy/OneDrive\ -\ Maynooth\ University/github-projects/hamilton-monitor/all_tables_current.rds')






#Get the latest table containing info on all counties
latest_county_table <- head(all_tables, n=1)[[1]]$counties
#Change the number of cases from char to int
latest_county_table$Cases <- strtoi(gsub("[^0-9.-]", "", latest_county_table$`Number of Cases`))

#Read in the county shapes file and join it with county case info
cs2 <- rgdal::readOGR("counties_simple.geojson")
cs2 <- merge(cs2, latest_county_table, by.x='NAME_TAG', by.y='County')
#Color the counties by number of cases
pal2 <- colorNumeric("Blues", log2(cs2$Cases))
#pal2 <- colorNumeric(scales::viridis_pal(), log2(cs2$Cases))

#Since we don't have data on a county by county basis for
#Nor Ire, we fill it with data for the whole region
nor_ire_counties = c('Antrim', 'Armagh', 'Down', 'Fermanagh', 'Londonderry','Tyrone')
cs2 = cs2[!(cs2$NAME_TAG %in% nor_ire_counties), ]

#Read in summary stats for Ireland
sum_stats <- read.csv('summary_stats_current.csv')
yesterday = format(as.Date(Sys.time(), tz = "Europe/Dublin") - 1, "%Y%m%d")
sum_stats_yesterday = read_csv(paste0("old_data/summary_stats",yesterday,".csv"))

#Create the datable with county and date information
county_total_date<-map_df(all_tables,~add_column(.x$counties,date=lubridate::dmy(.x$published)))%>% arrange(desc(date))

#Aproximate to 5 the "<= 5 cases"
county_total_date$`Number of Cases`<-county_total_date$`Number of Cases` %>% as.numeric %>% ifelse(is.na(.),5,.) 

#Create the plots for county cumulatie
county_cumulative_cases<-map(cs2$NAME_TAG,
                             ~ggplot(county_total_date 
                                     %>% filter(County==as.character(.x)),
                                     aes(x=date,y=`Number of Cases`,group=County))+
                               geom_point()+geom_line()+
                               ggtitle(label = paste0("Total cases in ",.x, " at ",county_total_date$date[[1]],": ",
                                                      county_total_date$`Number of Cases`[county_total_date$date==county_total_date$date[[1]] & county_total_date$County==.x]))+
                               xlab('Date')+
                               ylab('Number of individuals')+
                               #geom_text(mapping = aes(x=date,y=`Number of Cases`,label=`Number of Cases`,vjust=-0.5))+
                               theme_bw())

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
ecdc_change = ecdc %>% group_by(countriesAndTerritories) %>% 
  mutate_each(big_change, c(cases, deaths)) %>% 
  filter_all(all_vars(!is_bad(.))) %>% 
  ungroup() %>% 
  filter(dateRep == max(dateRep))

ecdc_table1 = ecdc %>% 
  group_by(countriesAndTerritories) %>% 
  filter(dateRep == max(dateRep)) %>% 
  ungroup() %>% 
  arrange(desc(deaths)) %>% 
  mutate(Date = as.Date(dateRep),
         Country = countriesAndTerritories,
         `Daily cases` = cases,
         `Daily deaths` = deaths) %>% 
  select(Date, Country, `Daily cases`, `Daily deaths`)
ecdc_table2 = ecdc %>% 
  group_by(countriesAndTerritories) %>% 
  summarise(Date = as.Date(max(dateRep)),
            `Total cases` = sum(cases),
            `Total deaths` = sum(deaths)) %>% 
  ungroup() %>% 
  rename(Country = countriesAndTerritories) %>% 
  select(Country, `Total cases`, `Total deaths`)
ecdc_table3 = left_join(ecdc_table1, ecdc_table2, by = 'Country') %>% 
  filter(Country != 'Global') %>% 
  arrange(desc(`Total deaths`))

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  #Ireland cumulative plot in Summary tab
  output$CountryPlot <- renderPlotly({
    ecdc_agg = ecdc %>%
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
    
    country_colours = ecdc_agg %>%
      select(countriesAndTerritories) %>% 
      distinct() %>%
      #mutate(Colours = gradient_n_pal()(seq(0, 1, length.out = n()))) %>%
      #mutate(Colours = div_gradient_pal()(seq(0, 1, length.out = n()))) %>%
      mutate(Colours = colorRampPalette(brewer.pal(8, "Set1"))(n())) %>%
      # mutate(Colours = sample(new_colors, n())) %>% 
      # mutate(Colours = replace(Colours,
      #                          countriesAndTerritories == "Ireland", "green")) %>%
      # mutate(Colours = replace(Colours,
      #                          countriesAndTerritories == "Ireland", "#40C575")) %>%
      deframe()
    
    shiny::validate(
      shiny::need(nrow(ecdc_agg) > 0, "Please select some countries. Use Global for worldwide values.")
    )

    x_pick = switch(input$sel_axis,
                    'Date' = 'dateRep',
                    'days_since')
    
    if(input$sel_axis == 'Days since 1st case' | input$sel_axis == 'Date') {
      ecdc_agg = ecdc_agg %>% filter(cum_cases > 0) 
    } else if(input$sel_axis == 'Days since 1st death') {
      ecdc_agg = ecdc_agg %>% filter(cum_deaths > 0) 
    } else if(input$sel_axis == 'Days since 10th case') {
      ecdc_agg = ecdc_agg %>% filter(cum_cases >= 10) 
    }
    
    ecdc_agg = ecdc_agg %>% 
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
    ecdc_agg = ecdc_agg %>% 
      filter(Type %in% y_pick)
    
    p = ggplot(ecdc_agg, aes_string(x = x_pick, y = 'Number', colour = 'countriesAndTerritories')) + 
      geom_line(aes(linetype = Type)) + 
      #geom_point(show.legend = FALSE) +
      labs(x = input$sel_axis,
           y = paste(input$sel_var, collapse = ',')) + 
      scale_color_manual(values = country_colours) +
      #scale_colour_brewer(palette = "Set1") + 
      { if(x_pick == 'dateRep') {
        scale_x_datetime(breaks = '1 week', labels = scales::label_date("%d%b"))
      } else {
        scale_x_continuous(breaks =  breaks_pretty(n = 10))
      }} +
      theme_shiny_dashboard() +
      { if(x_pick == 'dateRep') theme(axis.text.x = element_text(angle = 45, hjust = 1)) } +
      theme(legend.title = element_blank(),
            legend.position = 'bottom') +
      {if ('Log cumulative cases' %in% input$sel_var | 
           'Log cumulative deaths' %in% input$sel_var)  {
        scale_y_continuous(trans = log_trans(), breaks = scales::breaks_log(n = 5))
      } else {
        scale_y_continuous(breaks = scales::breaks_pretty(n = 5))
      }}
    
    ggplotly(p) %>% layout(margin = list(l = 75))
    
  })
  

# Info Boxes --------------------------------------------------------------
  
  #Ireland cases infobox in summary tab
  output$ireCasesBox <- renderValueBox({
    ecdc_current = ecdc %>% filter(countriesAndTerritories == 'Ireland') %>% 
      arrange(desc(dateRep)) %>% 
      top_n(2)
    pc_change = round(100*(sum(ecdc_current$cases)/sum(ecdc_current$cases[2:nrow(ecdc_current)]) - 1))
    #pc_change = round(100*(sum_stats$Cases[sum_stats$Region == 'ireland']/sum_stats_yesterday$Cases[sum_stats$Region == 'ireland'] - 1))
    update_date = format(as.Date(ecdc_current$dateRep[1]), "%d-%b")
    html_message = get_html_message(pc_change)
    val = str_pad(format(sum_stats$Cases[sum_stats$Region == 'ireland'], big.mark=','), 9, side = 'right')
    valueBox(value = tags$p(val, style = "font-size: 120%;"),
             subtitle = HTML(paste0("Ireland: Cases",br(),html_message,' ', pc_change,'% since yesterday')),
                                    # br(),em('Last updated:',update_date, style = "font-size: 80%;"))),
             color = 'olive',
             icon = icon("thermometer-three-quarters"))
  })
  
  #Ireland deaths infobox in summary tab
  output$ireDeathsBox <- renderValueBox({
    ecdc_current = ecdc %>% filter(countriesAndTerritories == 'Ireland') %>% 
      arrange(desc(dateRep)) %>% 
      top_n(2)
    pc_change = round(100*(sum(ecdc_current$deaths)/sum(ecdc_current$deaths[2:nrow(ecdc_current)]) - 1))
    #pc_change = round(100*(sum_stats$Cases[sum_stats$Region == 'ireland']/sum_stats_yesterday$Cases[sum_stats$Region == 'ireland'] - 1))
    update_date = format(as.Date(ecdc_current$dateRep[1]), "%d-%b")
    #pc_change = round(100*(sum_stats$Deaths[sum_stats$Region == 'ireland']/sum_stats_yesterday$Deaths[sum_stats$Region == 'ireland'] - 1))
    html_message = get_html_message(pc_change)
    val = str_pad(format(sum_stats$Deaths[sum_stats$Region == 'ireland'], big.mark=','), 9, side = 'right')
    valueBox(value = tags$p(val, style = "font-size: 120%;"),
             subtitle = HTML(paste0("Ireland: Deaths",br(),html_message,' ', pc_change,'% since yesterday')),
                                    #br(),em('Last updated:',update_date,style = "font-size: 80%"))),
             color = 'olive',
             icon = icon("exclamation-triangle"))
    
  })
  
  #Ireland hospitalised infobox in summary tab
  output$ireHospBox <- renderValueBox({
    pc_change = str_pad(round(100*(sum_stats$Hospitalised[sum_stats$Region == 'ireland']/sum_stats_yesterday$Hospitalised[sum_stats$Region == 'ireland'] - 1)),3, 'left')
    html_message = get_html_message(pc_change)
    val = str_pad(format(sum_stats$Hospitalised[sum_stats$Region == 'ireland'], big.mark=','), 9, side = 'right')
    valueBox(value = tags$p(val, style = "font-size: 120%;"),
             subtitle = HTML(paste0("Ireland: Hospitalised ",br(),html_message,' ', pc_change,'% since yesterday')),
             color = 'olive',
             icon = icon("hospital"))
  })
  
  #Ireland ICU infobox in summary tab
  output$ireICUBox <- renderValueBox({
    pc_change = round(100*(sum_stats$ICU[sum_stats$Region == 'ireland']/sum_stats_yesterday$ICU[sum_stats$Region == 'ireland'] - 1))
    html_message = get_html_message(pc_change)
    val = str_pad(format(sum_stats$ICU[sum_stats$Region == 'ireland'], big.mark=','), 9, side = 'right')
    valueBox(value = tags$p(val, style = "font-size: 120%;"),
             subtitle = HTML(paste0("Ireland: ICU",br(),html_message,' ', pc_change,'% since yesterday')),
             color = 'olive',
             icon = icon("briefcase-medical"))
  })
  
  #Worldwide cases infobox in summary tab
  output$wCasesBox <- renderValueBox({
    pc_change = round(100*(sum_stats$Cases[sum_stats$Region == 'world']/sum_stats_yesterday$Cases[sum_stats$Region == 'world'] - 1))
    html_message = get_html_message(pc_change)
    val = str_pad(format(sum_stats$Cases[sum_stats$Region == 'world'], big.mark=','), 9, side = 'right')
    valueBox(value = tags$p(val, style = "font-size: 120%;"),
             subtitle = HTML(paste0("Global: Cases",br(),html_message,' ', pc_change,'% since yesterday')),
             color = 'maroon',
             icon = icon("globe"))
  })
  
  #Worldwide deaths infobox in summary tab
  output$wDeathsBox <- renderValueBox({  
    pc_change = round(100*(sum_stats$Deaths[sum_stats$Region == 'world']/sum_stats_yesterday$Deaths[sum_stats$Region == 'world'] - 1))
    html_message = get_html_message(pc_change)
    val = str_pad(format(sum_stats$Deaths[sum_stats$Region == 'world'], big.mark=','), 9, side = 'right')
    valueBox(value = tags$p(val, style = "font-size: 120%;"),
             subtitle = HTML(paste0("Global: Deaths",br(),html_message,' ', pc_change,'% since yesterday')),
             color = 'maroon',
             icon = icon("cross"))
  })
  
  #Worldwide recovered infobox in summary tab
  output$wRecovBox <- renderValueBox({  
    pc_change = round(100*(sum_stats$Recovered[sum_stats$Region == 'world']/sum_stats_yesterday$Recovered[sum_stats$Region == 'world'] - 1))
    html_message = get_html_message(pc_change)
    val = str_pad(format(sum_stats$Recovered[sum_stats$Region == 'world'], big.mark=','), 9, side = 'right')
    valueBox(value = tags$p(val, style = "font-size: 120%;"),
             subtitle = HTML(paste0("Global: Recovered",br(),html_message,' ', pc_change,'% since yesterday')),
             color = 'maroon',
             icon = icon("heart"))
  })
  
  # Worst hit country
  output$worstHitCountryBox <- renderValueBox({  
    worst_countries = ecdc %>% 
      filter(countriesAndTerritories != 'Global') %>% 
      group_by(countriesAndTerritories) %>% 
      summarise(totalDeaths = sum(deaths)) %>% 
      ungroup() %>% 
      arrange(desc(totalDeaths)) %>% 
      top_n(1)
    name = str_sub(str_replace(worst_countries$countriesAndTerritories[1], '_', ' '),
                   1, 10)
    valueBox(value = tags$p(name, 
                            style = "font-size: 110%;"),
             subtitle = HTML(paste0("Most deaths overall: ",worst_countries$totalDeaths[1])),
             color = 'light-blue',
             icon = icon("arrow-up"))
  })
  
  # Biggest Increase in Deaths Country
  output$increaseDeathBox <- renderValueBox({  
    biggest_increase = ecdc_change %>% 
      filter(deaths != 0) %>% 
      arrange(deaths)
    name = str_sub(str_replace(biggest_increase$countriesAndTerritories[1], '_', ' '),
                   1, 10)
    
    valueBox(value = tags$p(name, 
                            style = "font-size: 110%;"),
             subtitle = HTML(paste0("Biggest increase in deaths since yesterday: ", -biggest_increase$deaths[1])),
             color = 'light-blue',
             icon = icon("arrow-up"))
  })
  
  # Biggest decrease in deaths
  output$bigDecreaseBox <- renderValueBox({  
    biggest_decrease = ecdc_change %>% 
      filter(deaths != 0) %>% 
      arrange(desc(deaths)) %>% 
      slice(1)
    name = str_sub(str_replace(biggest_decrease$countriesAndTerritories, '_', ' '),
                   1, 10)
    
    valueBox(value = tags$p(name, 
                            style = "font-size: 110%;"),
             subtitle = HTML(paste0("Biggest reduction in deaths since yesterday: ", 
                                    abs(biggest_decrease$deaths))),
             color = 'light-blue',
             icon = icon("arrow-down", class = "color: rgb(59, 91, 152)"))
  })
  
  output$bigDailyBox <- renderValueBox({  
    daily_death = ecdc %>% 
      filter(countriesAndTerritories != 'Global') %>% 
      group_by(countriesAndTerritories) %>% 
      filter(dateRep == max(dateRep)) %>% 
      ungroup() %>% 
      arrange(desc(deaths)) %>% 
      slice(1)
    name = str_sub(str_replace(daily_death$countriesAndTerritories[1], '_', ' '),
                   1, 10)
    valueBox(value = tags$p(name, 
                            style = "font-size: 110%;"),
             subtitle = HTML(paste0("Most deaths today: ",daily_death$deaths[1])),
             color = 'light-blue',
             icon = icon("arrow-up"))
  })

# Data tables -------------------------------------------------------------

  
  # Highest daily
  output$highestDaily <- DT::renderDataTable({
    DT::datatable(ecdc_table3 %>% select(Country, `Daily deaths`) %>% arrange(desc(`Daily deaths`)),
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
    DT::datatable(ecdc_table3 %>% select(Country, `Total deaths`) %>% arrange(desc(`Total deaths`)),
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
    biggest_change = ecdc_change %>% 
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
  
  #################################COUNTIES TAB#################################
  #Counties table in Counties tab
  output$countyCasesTable <- DT::renderDataTable({
    DT::datatable(caption = paste0("Updated: ",all_tables[[1]]$published),
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
                  label = ~paste0(NAME_TAG, ": ", `Number of Cases`, ' cases') ) %>%
      addLegend(pal = pal2, title='Cases', values = ~log2(Cases), opacity = 1.0,
                labFormat = labelFormat(transform = function(x) round(2^x)))
  })
  

# Animation tab -----------------------------------------------------------

  observe({
    # Find the max and min dates for these countries
    ecdc_use = ecdc %>% 
      filter(countriesAndTerritories %in% input$sel_ctry2) %>% 
      group_by(countriesAndTerritories) %>% 
      arrange(dateRep) %>% 
      mutate(cum_cases = cumsum(cases),
             cum_deaths = cumsum(deaths),
             cases_per_million = 1e6*cumsum(cases)/popData2018,
             deaths_per_million = 1e6*cumsum(deaths)/popData2018) %>% 
      ungroup() %>% 
      filter(cum_cases >0)
    
    if(str_detect(input$sel_horiz,'death') | str_detect(input$sel_vert,'death')) {
      ecdc_use = ecdc_use %>% 
        filter(cum_deaths > 1)
    }
    
    shiny::validate(
      shiny::need(nrow(ecdc_use) > 0, "Please select some countries. Use Global for worldwide values.")
    )
    
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session, "theDate", 
                      min = min(ecdc_use$dateRep),
                      timeFormat = "%d/%b")
  })
  
  ani_graph = reactive({
    
    ecdc_agg = ecdc %>%
      filter(countriesAndTerritories %in% input$sel_ctry2) %>%
      select(dateRep, cases, deaths, countriesAndTerritories, popData2018, day, month) %>% 
      group_by(countriesAndTerritories) %>% 
      arrange(dateRep) %>% 
      mutate(cum_cases = cumsum(cases),
             cum_deaths = cumsum(deaths),
             cases_per_million = 1e6*cumsum(cases)/popData2018,
             deaths_per_million = 1e6*cumsum(deaths)/popData2018) %>% 
      ungroup()
    
    country_colours = ecdc %>% arrange(desc(popData2018)) %>% 
      select(countriesAndTerritories) %>% 
      distinct() %>%
      #mutate(Colours = div_gradient_pal()(seq(0, 1, length.out = n()))) %>%
      mutate(Colours = colorRampPalette(brewer.pal(8, "Set1"))(n())) %>%
      # mutate(Colours = sample(new_colors, n())) %>% 
      # mutate(Colours = replace(Colours,
      #                          countriesAndTerritories == "Ireland", "green")) %>%
      # mutate(Colours = replace(Colours,
      #                          countriesAndTerritories == "Ireland", "#40C575")) %>%
      deframe()
    
    shiny::validate(
      shiny::need(nrow(ecdc_agg) > 0, "Please select some countries. Use Global for worldwide values.")
    )
    
    x_pick = switch(input$sel_horiz,
                    'Cumulative cases' = 'cum_cases', 
                    'Cumulative deaths' = 'cum_deaths',
                    'Sqrt cumulative cases' = 'cum_cases',
                    'Sqrt cumulative deaths' = 'cum_deaths',
                    'Log cumulative cases' = 'cum_cases',
                    'Log cumulative deaths' = 'cum_deaths',
                    'Cumulative cases per million population' = 'cases_per_million',
                    'Cumulative deaths per million population' = 'deaths_per_million')
    y_pick = switch(input$sel_vert,
                    'Cumulative cases' = 'cum_cases', 
                    'Cumulative deaths' = 'cum_deaths',
                    'Sqrt cumulative cases' = 'cum_cases',
                    'Sqrt cumulative deaths' = 'cum_deaths',
                    'Log cumulative cases' = 'cum_cases',
                    'Log cumulative deaths' = 'cum_deaths',
                    'Cumulative cases per million population' = 'cases_per_million',
                    'Cumulative deaths per million population' = 'deaths_per_million')
  
    ecdc_agg = ecdc_agg %>%
      mutate(countriesAndTerritories = parse_factor(countriesAndTerritories),
             month_day = format(dateRep, format = "%b-%d")) %>% 
             #day_month = paste0(day,'/', month)) %>% 
      select("dateRep", x_pick, y_pick, "countriesAndTerritories", 'month_day')
    
      if(str_detect(input$sel_horiz,'cases') | str_detect(input$sel_horiz,'death')) {
        ecdc_agg = ecdc_agg %>% 
          filter(x_pick > 1)
      }
      if(str_detect(input$sel_vert,'cases') | str_detect(input$sel_vert,'death') ) {
      ecdc_agg = ecdc_agg %>% 
        filter(y_pick > 1)
      }
    
    
    # Find the median values of the biggest country
    ggplot(ecdc_agg %>% filter(dateRep == input$theDate), 
           aes_string(x_pick, y_pick, colour = "countriesAndTerritories", 
                      size = y_pick)) +
      scale_color_manual(values = country_colours) +
      # scale_colour_discrete(drop=TRUE,
      #                       limits = levels(ecdc_agg$countriesAndTerritories)) +
      annotation_custom(grid::textGrob(ecdc_agg$month_day[match(input$theDate, ecdc_agg$dateRep)],
                                       gp=gpar(fontsize=200, col="grey")), 
                        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
      geom_point(data = ecdc_agg %>% filter(dateRep < input$theDate)) + 
      geom_point(alpha = 0.7) +
      geom_label(aes(label = countriesAndTerritories)) +
      labs(x = str_to_sentence(str_remove(str_remove(input$sel_horiz, "Sqrt "), 'Log ')),
           y = str_to_sentence(str_remove(str_remove(input$sel_vert, "Sqrt "), 'Log '))) +
      scale_size(range = c(2, 12)) +
      { if(input$sel_horiz == 'Sqrt cumulative cases' | input$sel_horiz == 'Sqrt cumulative deaths') {
        scale_x_sqrt(breaks = scales::breaks_pretty(n = 10),
                     limits = c(min(ecdc_agg[[x_pick]]), max(ecdc_agg[[x_pick]])))
      } else if(input$sel_horiz == 'Log cumulative cases' | input$sel_horiz == 'Log cumulative deaths') {
        scale_x_continuous(trans = log_trans(), breaks = scales::breaks_log(n = 10),
                           limits = c(min(ecdc_agg[[x_pick]]), max(ecdc_agg[[x_pick]])))
      } else {
        scale_x_continuous(limits = c(min(ecdc_agg[[x_pick]]), max(ecdc_agg[[x_pick]])))
      }} +
      { if(input$sel_vert == 'Sqrt cumulative cases' | input$sel_vert == 'Sqrt cumulative deaths') {
        scale_y_sqrt(breaks = scales::breaks_pretty(n = 10),
                     limits = c(min(ecdc_agg[[y_pick]]), max(ecdc_agg[[y_pick]])))
      } else if(input$sel_vert == 'Log cumulative cases' | input$sel_vert == 'Log cumulative deaths') {
        scale_y_continuous(trans = log_trans(), breaks = scales::breaks_log(n = 10),
                           limits = c(min(ecdc_agg[[y_pick]]), max(ecdc_agg[[y_pick]])))
      } else {
        scale_y_continuous(limits = c(min(ecdc_agg[[y_pick]]), max(ecdc_agg[[y_pick]])))
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

  
  
  # data for age plots data
  hosp.ages = c("<5","5 - 14","15 - 24","25 - 34","35 - 44","45 - 54","55 - 64","65+" )
  
  age.hosp <- all_tables %>% 
    map('age_hospitalised') %>% 
    list.clean(., fun = is.null) %>%
    map(1) %>%
    unlist() %>%
    .[. %in% c(hosp.ages,"25 - 34 69")]
  
  age.hosp <- replace(age.hosp, age.hosp=="25 - 34 69", "25 - 34")
  
  num.dates = length(all_tables %>% 
                       map('age_hospitalised') %>% 
                       list.clean(., fun = is.null) )
  
  date.hosp <- all_tables %>% 
    map('published') %>% 
    map(1) %>%
    unlist() %>%
    .[1:num.dates]
  
  date.hosp <- rep(date.hosp,each=length(hosp.ages))
  
  count.hosp <- all_tables %>% 
    map('age_hospitalised') %>% 
    list.clean(., fun = is.null) %>%
    map(2) %>%
    lapply(., function(x) x[1:length(hosp.ages)]) %>%
    unlist() 
  
  # formatting error on gov.ie
  count.hosp <- replace(count.hosp, count.hosp=="8.3", "69")
  
  all.data <- data.frame(date.hosp, age.hosp, count.hosp)
  names(all.data) <- c('date', 'age', 'count')
  all.data$age <- as.character(all.data$age)
  all.data$date <- as.Date(all.data$date, format = '%d %B %y')
  all.data$count<- as.numeric(as.character(all.data$count))
  
  
  # time series for positive cases data
  
  nothosp.ages = c("<1","1 - 4","5 - 14","15 - 24","25 - 34","35 - 44","45 - 54","55 - 64","65+" )
  
  age.not.hosp <- all_tables %>% 
    map('age') %>% 
    list.clean(., fun = is.null) %>%
    map(1) %>%
    unlist() %>%
    .[. %in% nothosp.ages]
  
  num.dates.not.hosp = length(all_tables %>% 
                                map('age') %>% 
                                list.clean(., fun = is.null) )
  
  date.not.hosp <- all_tables %>% 
    map('published') %>% 
    map(1) %>%
    unlist() %>%
    .[1:num.dates.not.hosp]
  
  date.not.hosp <- rep(date.not.hosp,each=length(nothosp.ages))
  count.not.hosp <- all_tables %>% 
    map('age') %>% 
    list.clean(., fun = is.null) %>%
    map(2) %>%
    lapply(., function(x) x[1:length(nothosp.ages)]) %>%
    unlist() 
  
  all.data.not.hosp <- data.frame(date.not.hosp, age.not.hosp, count.not.hosp)
  names(all.data.not.hosp) <- c('date', 'age', 'count')
  
  all.data.not.hosp$age <- as.character(all.data.not.hosp$age)
  all.data.not.hosp$date <- as.Date(all.data.not.hosp$date, format = '%d %B %y')
  all.data.not.hosp$count <- as.numeric(as.character(all.data.not.hosp$count))
  all.data.not.hosp[(all.data.not.hosp$age=='<1'),]$count = all.data.not.hosp[(all.data.not.hosp$age=='<1'),]$count + all.data.not.hosp[(all.data.not.hosp$age=='1 - 4'),]$count
  all.data.not.hosp[(all.data.not.hosp$age=='<1'),]$age = '<5'
  all.data.not.hosp = all.data.not.hosp[(all.data.not.hosp$age!='1 - 4'),]
  
  
  
  
  
  
  
  output$ageHist <- renderPlotly({
    
    dates <- all_tables %>% map('published') %>% lubridate::dmy()
    
    latest.data.age = all.data.not.hosp %>% filter(date == dates[1])
    latest.data.hosp = all.data %>% filter(date == dates[1])
    
    # order bars according to age
    latest.data.age$age <- factor(latest.data.age$age, levels = latest.data.age$age)
    latest.data.hosp$age <- factor(latest.data.hosp$age, levels = latest.data.hosp$age)
    
    g <- ggplot() +
      geom_bar(data = latest.data.age, aes(age, count, fill = 'Total'),stat = 'identity', width=0.5) +
      geom_bar(data = latest.data.hosp, aes(age, count, fill = 'Hospitalised'),stat = 'identity', width=0.5) +
      theme_shiny_dashboard() +
      labs(x="Age Group", y = "Count") +
      theme(legend.title = element_blank())+
      ggtitle('Cases by Age: Ireland')
    
    ggplotly(g) %>% layout(margin = list(l = 75))    %>%
      config(displayModeBar = FALSE)
    
  })
  
  
  output$ageHospHistory <- renderPlotly({
    g<- ggplot(all.data, aes(x = date, y = count, color = age))+
      geom_line(aes(group = age, linetype=age)) +
      theme_shiny_dashboard() +
      labs(x="Date", y = "Count") +
      ggtitle('Hospitalised by Age Group') + theme(
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(face = "bold", 
                                   size = 12, angle = 45))
    
    ggplotly(g) %>% layout(margin = list(l = 75))    %>%
      config(displayModeBar = FALSE)
    
  })
  
  
  
  output$ageTotalHistory <- renderPlotly({
    
    g<- ggplot(all.data.not.hosp, aes(x = date, y = count, color = age))+
      geom_line(aes(group = age, linetype=age)) +
      theme_shiny_dashboard() +
      labs(x="Date", y = "Count") +
      ggtitle('Positive Cases by Age Group') + theme(
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(face = "bold", 
                                   size = 12, angle = 45))
    
    ggplotly(g) %>% layout(margin = list(l = 75))    %>%
      config(displayModeBar = FALSE)

  })

# Gender Breakdown --------------------------------------------------------

  
  
  # data necessary for gender plots
  dates <- all_tables %>% map('published') %>% lubridate::dmy()
  gender.data <- all_tables %>% map('gender')
  female.prec = gender.data %>% map('% Total') %>% map(1) %>% unlist() %>% as.numeric() # female
  male.prec =  gender.data %>% map('% Total') %>% map(2) %>% unlist() %>% as.numeric()# male
  unknown.prec =  gender.data %>% map('% Total') %>% map(3) %>% unlist() %>% as.numeric() %>% replace_na(.,0)# unknown
  dates = dates[1:length(female.prec)]
  
  data <- tibble(dates,female.prec,male.prec,unknown.prec)
  names(data) <- c('Date','Female','Male','Unknown')
  
  
  
  # latest gender plot (histogram)
  output$genderCases <- renderPlotly({
    
    latest.data = data %>% dplyr::filter(Date == dates[1])
    x<-as.character(names(latest.data))
    x <- x[-1]
    data3 = latest.data %>% 
      pivot_longer(names_to = 'type', values_to = 'Precentage', -Date) 
    
    g <- ggplot(data = data3, aes(type, Precentage, fill=type))+
      geom_bar(stat = 'identity',width = 0.7, alpha=0.8, position = position_dodge())+
      theme_shiny_dashboard() +
      labs(y="%", x = "") +
      theme(legend.position = 'none')+
      ggtitle('Gender Breakdown')
    
    ggplotly(g) %>% layout(margin = list(l = 75))    %>%
      config(displayModeBar = FALSE)
  })
  
  
  
  # time series of gender data
  output$genderCasesHistory <- renderPlotly({
    data2 = data %>% 
      pivot_longer(names_to = 'type', values_to = 'patients', -Date) %>% mutate(Date = as.POSIXct(Date))
    
    g <- ggplot(data2, aes(x = Date, y = patients, colour = type)) + 
      geom_line(size=1)+
      theme_shiny_dashboard() +
      scale_x_datetime(breaks = '2 days',
                       date_labels = "%d%b") +
      labs(x="Date", y = "Precentage") +
      ggtitle('Gender Breakdown') + theme(
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(face = "bold", 
                                   size = 12, angle = 45))
    
    ggplotly(g) %>% layout(margin = list(l = 75))    %>%
      config(displayModeBar = FALSE)
    
    
  })
  

# Proportion healthcare patients ------------------------------------------

  
  #data for proportion of health care patients
  dates <- all_tables %>% map('published') %>% lubridate::dmy()
  
  host.count.data <- all_tables %>% 
    map('totals') %>% 
    lapply(., function(x) dplyr::filter(x, Totals %in% c("Total number of healthcare workers",
                                                         "Total number healthcare workers")))%>%
    map(2) %>%
    unlist() %>%
    as.numeric()
  
  
  tot.count.data <- all_tables %>% 
    map('totals') %>% 
    lapply(., function(x) dplyr::filter(x, Totals %in% c("Total number of cases"))) %>%
    map(2) %>%
    unlist() %>%
    as.numeric()
  
  
  host.count.data = host.count.data[-length(host.count.data)]
  dates.host.count = dates[-length(dates)]
  
  # timeseries
  data.host.count <- tibble(dates.host.count, 
                            host.count.data, 
                            tot.count.data)
  
  
  names(data.host.count) <- c('Date',
                              'HealthCareWorkers',
                              'TotalCases')
  
  
  # histogram
  output$helthcarePatients <- renderPlotly({
    
    
    latest.data = data.host.count %>% filter(Date == dates[1])
    latest.data$GeneralPopulation = latest.data$TotalCases - latest.data$HealthCareWorkers
    latest.data = latest.data%>%dplyr::select(-TotalCases)
    
    names(latest.data) <- c("Date","Health Care Workers","General Pop.")
    
    data3 = latest.data %>% 
      pivot_longer(names_to = 'type', values_to = 'Count', -Date) 
    
    g <- ggplot(data = data3, aes(type, Count, fill=type))+
      geom_bar(stat = 'identity',width = 0.7, alpha=0.8, position = position_dodge())+
      theme_shiny_dashboard() +
      labs(y="Count", x = "") +
      theme(legend.position = 'none')+
      ggtitle('Hospitalisation type') +
      scale_fill_brewer(palette="Accent")
    
    ggplotly(g) %>% layout(margin = list(l = 75))    %>%
      config(displayModeBar = FALSE)
    
    
  })  
  
  
  # timeseries
  output$helthcarePatientsHistory <- renderPlotly({
    
    data.host.count$GeneralPopulation = data.host.count$TotalCases - data.host.count$HealthCareWorkers
    data2 = data.host.count %>% 
      pivot_longer(names_to = 'type', values_to = 'count', -Date) %>% mutate(Date = as.POSIXct(Date))
    
    g <- ggplot(data2, aes(x = Date, y = count, colour = type)) + 
      geom_line(size=1)+
      theme_shiny_dashboard() +
      scale_x_datetime(breaks = '2 days',
                       date_labels = "%d%b") +
      labs(x="Date", y = "Count") +
      ggtitle('Number of Healthcare Workers Tested Positive') + theme(
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(face = "bold", 
                                   size = 12, angle = 45))+
      scale_color_brewer(palette="Accent")
    
    
    ggplotly(g) %>% layout(margin = list(l = 75))    %>%
      config(displayModeBar = FALSE)
    
    
  })
  

# How virus spreads -------------------------------------------------------

  
  # data required for how virus spreads plots
  dates.transmission <- all_tables %>% map('published') %>% lubridate::dmy()
  transmission.data <- all_tables %>% map('transmission')
  
  transmission.data = list.clean(transmission.data, fun = is.null)
  
  transmission.data[[length(transmission.data)]] <- NULL
  
  Community.count <- transmission.data %>% 
    lapply(., function(x) dplyr::filter(x, Transmission %in% c("Community transmission"))) %>%
    map(2) %>%
    unlist() %>%
    lapply(., function(x) as.numeric(sub("%", "", x))) %>%
    as.numeric()
  
  
  travel.count <- transmission.data %>% 
    lapply(., function(x) dplyr::filter(x, Transmission %in% c("Travel Abroad"))) %>%
    map(2) %>%
    unlist() %>%
    lapply(., function(x) as.numeric(sub("%", "", x))) %>%
    as.numeric()
  
  contact.count <- transmission.data %>% 
    lapply(., function(x) dplyr::filter(x, Transmission %in% c("Close contact with confirmed case"))) %>%
    map(2) %>%
    unlist() %>%
    lapply(., function(x) as.numeric(sub("%", "", x))) %>%
    as.numeric()
  
  investigation.count <- transmission.data %>% 
    lapply(., function(x) dplyr::filter(x, Transmission %in% c("Under investigation"))) %>%
    map(2) %>%
    replace_na(.,0) %>%
    lapply(., function(x) as.numeric(sub("%", "", x))) %>%
    lapply(., function(x) as.numeric(sub("258", "0", x))) %>%
    as.numeric()
  
  
  
  dates.transmission = dates[1:length(investigation.count)]
  # timeseries
  data.transmission <- tibble(dates.transmission, 
                              Community.count, 
                              travel.count,
                              contact.count,
                              investigation.count)
  
  
  names(data.transmission) <- c('Date',
                                'Community',
                                'Travel',
                                'Close Contact',
                                'Investigation')
  
  
  #histogram
  output$howContracted <- renderPlotly({
    
    
    latest.data = data.transmission %>% filter(Date == dates[1])
    x<-as.character(names(latest.data))
    x <- x[-1]
    
    
    if(latest.data$Investigation==0){
      latest.data = latest.data %>% dplyr::select(-Investigation)
      data3 = latest.data %>% 
        pivot_longer(names_to = 'type', values_to = 'Precentage', -Date) 
      
      g <- ggplot(data = data3, aes(x = reorder(type, -Precentage), Precentage, fill=type))+
        geom_bar(stat = 'identity',width = 0.7, alpha=0.8, position = position_dodge())+
        theme_shiny_dashboard() +
        labs(y="%", x = "") +
        coord_flip()+
        theme(legend.position = 'none')+
        ggtitle('How is COVID-19 Being Transmitted?')+
        scale_fill_brewer(palette="Set2")
      
    }else{
      data3 = latest.data %>% 
        pivot_longer(names_to = 'type', values_to = 'Precentage', -Date) 
      
      g <- ggplot(data = data3, aes(x = reorder(type, -Precentage), Precentage, fill=type))+
        geom_bar(stat = 'identity',width = 0.7, alpha=0.8, position = position_dodge())+
        theme_shiny_dashboard() +
        labs(y="%", x = "") +
        coord_flip()+
        theme(legend.position = 'none')+
        ggtitle('How is COVID-19 Being Transmitted?')+
        scale_fill_brewer(palette="Set2")
      
    }
    
    ggplotly(g) %>% layout(margin = list(l = 75))    %>%
      config(displayModeBar = FALSE)
    
  }) 
  
  
  #timeseries
  
  output$howContractedHistory <- renderPlotly({
    
    
    data2 = data.transmission %>% 
      pivot_longer(names_to = 'type', values_to = 'patients', -Date) %>% mutate(Date = as.POSIXct(Date))
    
    g <- ggplot(data2, aes(x = Date, y = patients, colour = type)) + 
      geom_line(size=1)+
      theme_shiny_dashboard() +
      scale_x_datetime(breaks = '2 days',
                       date_labels = "%d%b") +
      labs(x="Date", y = "Precentage") +
      ggtitle('How is COVID-19 Being Transmitted?') + theme(
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(face = "bold", 
                                   size = 12, angle = 45))+
      scale_color_brewer(palette="Set2")
    
    
    ggplotly(g) %>% layout(margin = list(l = 75))    %>%
      config(displayModeBar = FALSE)
    
    
  })
  

# ICU percentage ----------------------------------------------------------

  #data for icu plots
  dates <- all_tables %>% map('published') %>% lubridate::dmy()
  
  total.hosp <- all_tables %>% 
    map('totals') %>% 
    lapply(., function(x) dplyr::filter(x, Totals %in% c("Total number hospitalised")))%>%
    map(2) %>%
    unlist() %>%
    as.numeric()
  
  
  total.icu <- all_tables %>% 
    map('totals') %>% 
    lapply(., function(x) dplyr::filter(x, Totals %in% c("Total number admitted to ICU"))) %>%
    map(2) %>%
    unlist() %>%
    as.numeric()
  
  data.icu.prop <- tibble(dates, 
                          total.hosp, 
                          total.icu)
  
  
  names(data.icu.prop) <- c('Date',
                            'Hospital',
                            'ICU')
  
  
  #timeseries
  output$icuProportionHistory <- renderPlotly({
    data2 = data.icu.prop %>% 
      pivot_longer(names_to = 'type', values_to = 'count', -Date) %>% mutate(Date = as.POSIXct(Date))
    
    g <- ggplot(data2, aes(x = Date, y = count, colour = type)) + 
      geom_line(size=1)+
      theme_shiny_dashboard() +
      scale_x_datetime(breaks = '2 days',
                       date_labels = "%d%b") +
      labs(x="Date", y = "Count") +
      ggtitle('Hospitalised Patients') + theme(
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(face = "bold", 
                                   size = 12, angle = 45))
    
    
    ggplotly(g) %>% layout(margin = list(l = 75))    %>%
      config(displayModeBar = FALSE)
    
    
  }) 
  
  
  #histogram
  output$icuProportion <- renderPlotly({
    latest.data = data.icu.prop %>% filter(Date == dates[1])
    
    
    data3 = latest.data %>% 
      pivot_longer(names_to = 'type', values_to = 'Count', -Date) 
    
    g <- ggplot(data = data3, aes(type, Count, fill=type))+
      geom_bar(stat = 'identity',width = 0.7, alpha=0.8, position = position_dodge())+
      theme_shiny_dashboard() +
      labs(y="Count", x = "") +
      theme(legend.position = 'none')+
      ggtitle('Hospitalised Patients')
    
    ggplotly(g) %>% layout(margin = list(l = 75))    %>%
      config(displayModeBar = FALSE)
  })
    
    
    
})
