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
all_tables <- readRDS('all_tables_current.rds')
#Get the latest table containing info on all counties
latest_county_table <- head(all_tables, n=1)[[1]]$counties
#Change the number of cases from char to int
latest_county_table$Cases <- strtoi(gsub("[^0-9.-]", "", latest_county_table$`Number of Cases`))

#Read in the county shapes file and join it with county case info
cs2 <- rgdal::readOGR("counties_simple.geojson")
cs2 <- merge(cs2, latest_county_table, by.x='NAME_TAG', by.y='County')
#Color the counties by number of cases
pal2 <- colorNumeric("Reds", log2(cs2$Cases))

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
      mutate(Colours = replace(Colours,
                               countriesAndTerritories == "Ireland", "#40C575")) %>%
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
    pc_change = round(100*(sum_stats$Cases[sum_stats$Region == 'ireland']/sum_stats_yesterday$Cases[sum_stats$Region == 'ireland'] - 1))
    html_message = get_html_message(pc_change)
    val = str_pad(format(sum_stats$Cases[sum_stats$Region == 'ireland'], big.mark=','), 9, side = 'right')
    valueBox(value = tags$p(val, style = "font-size: 120%;"),
             subtitle = HTML(paste0("Ireland: Cases",br(),html_message,' ', pc_change,'% since yesterday')),
             color = 'olive',
             icon = icon("thermometer-three-quarters"))
  })
  
  #Ireland deaths infobox in summary tab
  output$ireDeathsBox <- renderValueBox({
    pc_change = round(100*(sum_stats$Deaths[sum_stats$Region == 'ireland']/sum_stats_yesterday$Deaths[sum_stats$Region == 'ireland'] - 1))
    html_message = get_html_message(pc_change)
    val = str_pad(format(sum_stats$Deaths[sum_stats$Region == 'ireland'], big.mark=','), 9, side = 'right')
    valueBox(value = tags$p(val, style = "font-size: 120%;"),
             subtitle = HTML(paste0("Ireland: Deaths",br(),html_message,' ', pc_change,'% since yesterday')),
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
             icon = icon("heart"))
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
                            style = "font-size: 120%;"),
             subtitle = HTML(paste0("Most deaths: ",worst_countries$totalDeaths[1])),
             color = 'light-blue',
             icon = icon("arrow-up"))
  })
  
  # Biggest Increase in Deaths Country
  output$increaseDeathBox <- renderValueBox({  
    biggest_increase = ecdc_change %>% 
      filter(deaths != 0) %>% 
      arrange(desc(deaths))
    
    name = str_sub(str_replace(biggest_increase$countriesAndTerritories[1], '_', ' '),
                   1, 10)
    
    valueBox(value = tags$p(name, 
                            style = "font-size: 120%;"),
             subtitle = HTML(paste0("Biggest increase in deaths since yesterday: ", biggest_increase$deaths[1])),
             color = 'light-blue',
             icon = icon("arrow-up"))
  })
  
  # Biggest decrease in deaths
  output$bigDecreaseBox <- renderValueBox({  
    biggest_decrease = ecdc_change %>% 
      filter(deaths != 0) %>% 
      arrange(deaths) %>% 
      slice(1)
    name = str_sub(str_replace(biggest_decrease$countriesAndTerritories, '_', ' '),
                   1, 10)
    
    valueBox(value = tags$p(name, 
                            style = "font-size: 120%;"),
             subtitle = HTML(paste0("Biggest increase in deaths since yesterday: ", 
                                    abs(biggest_decrease$deaths))),
             color = 'light-blue',
             icon = icon("arrow-down", class = "color: rgb(59, 91, 152)"))
  })
  

# Data tables -------------------------------------------------------------

  
  # Highest daily
  output$highestDaily <- DT::renderDataTable({
    DT::datatable(ecdc_table3 %>% select(Country, `Daily deaths`) %>% arrange(desc(`Daily deaths`)),
                  options = list(
                    pageLength = 10,
                    scrollY='calc((100vh - 290px)/1.0)',
                    searching = TRUE,
                    paging=TRUE,
                    autoWidth = TRUE,
                    rownames=TRUE
                  ))
  })  
  
  # HighestH total
  output$highestTotal <- DT::renderDataTable({
    DT::datatable(ecdc_table3 %>% select(Country, `Total deaths`) %>% arrange(desc(`Total deaths`)),
                  options = list(
                    pageLength = 10,
                    scrollY='calc((100vh - 290px)/1.0)',
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
      rename(Country = countriesAndTerritories,
             `Change in deaths` = deaths) %>% 
      select(Country, `Change in deaths`) %>% 
      arrange(desc(`Change in deaths`))
    DT::datatable(biggest_change,
                  options = list(
                    pageLength = 10,
                    scrollY='calc((100vh - 290px)/1.0)',
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
      mutate(Colours = replace(Colours,
                               countriesAndTerritories == "Ireland", "#40C575")) %>%
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

  
  
  #############################################
  #             age in hospital plot
  #############################################
  output$ageCases <- renderPlotly({

    # Main data on hospitalisation
    age.hosp <- (all_tables[[1]]$age_hospitalised) %>% 
      rename('Age' = "Hospitalised Age",
             `Hospitalised cases` = `Number of Cases`) %>% 
      select(Age, `Hospitalised cases`)
    
    # Data on all cases  
    age.all = (all_tables[[1]]$age) %>% 
      rename(`All cases` = `Number of Cases`) %>% 
      mutate(`All cases` = as.numeric(`All cases`)) %>% 
      select(Age, `All cases`)
    
    age_bind = left_join(age.all, age.hosp, 
                          by = c("Age")) %>% 
      replace_na(list(`Hospitalised cases` = 0)) %>% 
      mutate('Non-hospitalised cases' = `All cases` - `Hospitalised cases`,
             Age = factor(Age, 
                          levels = Age,
                          ordered = TRUE)) %>% 
      select(-`All cases`) %>% 
      filter(Age != 'Unknown') %>% 
      pivot_longer(names_to = 'Type', values_to = 'Cases', -Age)
    
    g<-ggplot(data = age_bind, aes(Age, Cases, fill=Type))+
      geom_bar(stat = 'identity') +
      theme_shiny_dashboard() +
      ggtitle('Cases by Age: Ireland')
    
    ggplotly(g, tooltip=c("Type", "Cases"))
    
  })
  
    
    #############################################
    #             Gender Breakdown
    #############################################
    output$genderCases <- renderPlotly({
      
      gender <- all_tables[[1]]$gender
      x<-as.character(gender$Gender)
      x <- x[-length(x)]
      
      y<-as.numeric(gender$`Number of Cases`)
      y <- y[-length(y)]
      sum <- sum(y)
      y <- (y/sum)*100
      text<- paste0(y, ' patients')
      
      
      data <- data.frame(x, y)
      data$x <- factor(data$x, levels = x)
      
      data$col <- ' '
      names(data) <- c('Gender', 'Percentage', 'holder')
      
      
      if(length(data$Gender) == 3){
        g<-ggplot(data = data, aes(Gender, Percentage, fill=Gender))+
          geom_bar(stat = 'identity',width = 0.7, position = position_dodge()) +
          theme_shiny_dashboard() +
          labs(y="%", x = "") +
          ggtitle('Gender Breakdown') + 
          theme(legend.position = 'none')+
          scale_fill_manual("legend", values = c("Male" = "darkorchid3", "Female" = "aquamarine4", 'Unknown' = 'grey'))
        
      }else{
        g<-ggplot(data = data, aes(Gender, Percentage, fill=Gender))+
          geom_bar(stat = 'identity',width = 0.7, position = position_dodge()) +
          theme_shiny_dashboard() +
          labs(y="%", x = "") +
          ggtitle('Gender Breakdown') + 
          theme(legend.position = 'none')+
          scale_fill_manual("legend", values = c("Male" = "darkorchid3", "Female" = "aquamarine4"))
        
      }
      
      
      ggplotly(g, tooltip = 'Percentage')
      
      
      
    })
    
  
  
  
  
  #############################################
  #      Proporting on healthcare patients
  ############################################# 
    
    output$helthcarePatients <- renderPlotly({
      helthcare.workers <- all_tables[[1]]$totals %>% 
        filter(Totals == 'Total number of healthcare workers') %>%
        select('Number of Cases') %>%
        as.numeric()
      
      total.cases <- all_tables[[1]]$totals %>% 
        filter(Totals == 'Total number of cases') %>%
        select('Number of Cases') %>%
        as.numeric()
        x<- c( "Healthcare Workers", 'Total Cases')
      
      y<-c(helthcare.workers, total.cases)
      text<- paste0(y, ' patients')

      data <- data.frame(x, y, text)
      data$x <- factor(data$x, levels = x)
      
      
      g <- ggplot(data = data, aes(x, y))+
        geom_bar(stat = 'identity', fill = 'deepskyblue2', alpha = 0.7,width = 0.7) +
        theme_shiny_dashboard() +
        labs(y="Count", x = " ") +
        ggtitle('Number of Healthcare Workers Tested Positive') +
        geom_text(aes(label=x),nudge_y = -100) +
        theme(
          axis.text.x = element_blank(),
          axis.ticks = element_blank()) 
      
      
      ggplotly(g)  %>% layout(margin = list(l = 75))
      
      
      
    })  
    
    
  
  
  
  
  #############################################
  #            How virus spreads
  ############################################# 
    output$howContracted <- renderPlotly({
      how.transmitted <- all_tables[[1]]$transmission
      
      x<-as.character(how.transmitted$Transmission)
      y<-as.numeric(unlist(regmatches(how.transmitted$Cases, gregexpr("[[:digit:]]+", how.transmitted$Cases))))
      
      total.cases <- all_tables[[1]]$totals %>% 
        filter(Totals == 'Total number of cases') %>%
        select('Number of Cases') %>%
        as.numeric()
      y<-(total.cases/100)*y
      
      text<- paste0(as.numeric(unlist(regmatches(how.transmitted$Cases, gregexpr("[[:digit:]]+", how.transmitted$Cases)))), ' %')
      x <- c('Community\ntransmission','Contact with\nknown case', 'Travel\nAbroad')
      
      data <- data.frame(x, y, text)
      data$x <- factor(data$x, levels = x)
      
      names(data) <- c("Class",    "Count",    "text")
      g <- ggplot(data = data, aes(Class, Count, fill=Class, text = text))+
        geom_bar(stat = 'identity') +
        theme_shiny_dashboard() +
        coord_flip()+
        #geom_text(aes(label=Class),nudge_y = -85) +
        # annotate(geom = "text",
        #          x = 1:4,
        #          y = 0,
        #          label = data$Class,
        #          hjust = 0) +
        #geom_text(aes(y = 0, label = Class, hjust = 0)) +
        labs(y="Count", x = "") +
        ggtitle('How is COVID-19 Being Transmitted?') + theme(
          #axis.text.y= element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none")
      
      
      ggplotly(g, tooltip=c("Count")) %>% layout(margin = list(l = 75))
      
      
      
    }) 
    
    
    
    
    
    output$icuProportion <- renderPlotly({
      dates <- all_tables %>% map('published') %>% lubridate::dmy()
      hosp.data <- all_tables %>% map('totals')
      
      icu.pats <- c()
      for (df in hosp.data){
        icu.pats <- c(icu.pats, (df %>% 
                                   filter(Totals == 'Total number admitted to ICU') %>% 
                                   select('Number of Cases') %>% 
                                   unlist() %>% 
                                   as.numeric()) )
      }
      
      hosp.pats <- c()
      for (df in hosp.data){
        hosp.pats <- c(hosp.pats, (df %>% 
                                     filter(Totals == 'Total number hospitalised') %>% 
                                     select('Number of Cases') %>% 
                                     unlist() %>% 
                                     as.numeric()) )
      }

      data <- tibble(dates, 
                     ICU = icu.pats, 
                     Hospital = hosp.pats)
      data2 = data %>% 
        pivot_longer(names_to = 'type', values_to = 'patients', -dates) %>% 
        mutate(datetime = as.POSIXct(dates))
      
      p = ggplot(data2, aes(x = datetime, y = patients, colour = type)) + 
        geom_line() +
        theme_shiny_dashboard() +
        scale_x_datetime(breaks = '2 days',
                         date_labels = "%d%b") +
        labs(x="Date", y = "Number of patients") +
        ggtitle('Hospitalised Patients') + theme(
          legend.title = element_blank(),
          axis.ticks = element_blank())

      ggplotly(p) %>% layout(margin = list(l = 75))    
      # fig <- plot_ly(x = ~ data$dates) %>% 
      #   add_lines(y = ~ data$hosp.pats, text = paste(data$hosp.pats, " patients in hospital"), name = "Hospitalised Patients") %>%
      #   add_lines(y = ~ data$icu.pats, text = paste(data$icu.pats, " patients in ICU"), name = "ICU Patients") 
      # 
      # fig <- fig %>% layout(title = 'Hospitalised Patients', yaxis = list(title = "Count"), xaxis = list(title = "Date"))
      # fig
    }) 
    
    
    
})
