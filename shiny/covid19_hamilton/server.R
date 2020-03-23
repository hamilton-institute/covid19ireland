library(shiny)
library(plotly)
library(tidyverse)
library(leaflet)
library(jsonlite)
library(shinydashboard)

ecdc = readRDS(file = '../../data/scraped/ECDC_data_20200321.rds')

#All tables contains information on a county-by-county basis
all_tables <- readRDS('../../data/scraped/all_tables.rds')

#Get the latest table containing info on all counties
latest_county_table <- head(all_tables, n=1)[[1]]$counties
#Change the number of cases from char to int
latest_county_table$Cases <- strtoi(gsub("[^0-9.-]", "", latest_county_table$`Number of Cases`))

#Read in the county shapes file and join it with county case info
cs2 <- rgdal::readOGR("../../data/counties_simple.geojson")
cs2 <- merge(cs2, latest_county_table, by.x='NAME_TAG', by.y='County')
#Color the counties by number of cases
pal2 <- colorNumeric("Reds", log2(cs2$Cases))

#Read in summary stats for Ireland
ire_sum_stats <- read.csv('../../data/scraped/summary_stats.csv')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$choose_country <- renderUI({
        selectInput("co", 
        "Select a Country", 
        unique(ecdc$`Countries and territories`))
    })

    output$covidPlot <- renderPlotly({
        
        # Extract out the data
        ecdc_plot = ecdc %>% 
            filter(`Countries and territories` == input$co) %>% 
            select(-c(Day,Month,Year,`Countries and territories`,GeoId)) %>% 
            pivot_longer(names_to = 'Type', values_to = 'Number', -DateRep)
        
        plot_ly(ecdc_plot, x = ~DateRep, y = ~Number, type = 'scatter', 
                mode = 'lines', color = ~Type) %>% 
            layout(title = paste('Number of cases/deaths for',input$co),
                   xaxis = list(title = 'Date'),
                   yaxis = list (title = 'Number of individuals'),
                   font = list(color = '#FFFFFF'),
                   plot_bgcolor='black',
                   paper_bgcolor='black')
        
    })
    
    output$covidMap <- renderLeaflet({
        library(rgdal)
        
        leaflet(cs2) %>% 
            addProviderTiles(providers$Stamen.TonerLite,
                    options = providerTileOptions(noWrap = TRUE)
                ) %>%
            setView(lng = -7.635498, lat = 53.186288, zoom = 6) %>%
            addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
            fillColor = ~pal2(log2(Cases)),
            label = ~paste0(NAME_TAG, ": ", formatC(Cases, big.mark = ","), ' cases') ) %>%
            addLegend(pal = pal2, title='Cases', values = ~log2(Cases), opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(2^x)))
    })
    
    output$mapPlot <- renderPlotly({
        
        # Extract out the data
        ecdc_plot = ecdc %>% 
            filter(`Countries and territories` == 'Ireland') %>% 
            select(-c(Day,Month,Year,`Countries and territories`,GeoId)) %>% 
            pivot_longer(names_to = 'Type', values_to = 'Number', -DateRep)
        
        plot_ly(ecdc_plot, x = ~DateRep, y = ~Number, type = 'scatter', 
                mode = 'lines', color = ~Type) %>% 
            layout(title = paste('Number of new cases/deaths for','Ireland'),
                   xaxis = list(title = 'Date'),
                   yaxis = list (title = 'Number of individuals'),
                   font = list(color = '#FFFFFF'),
                   plot_bgcolor='black',
                   paper_bgcolor='black')
        
    })
    
    output$casesBox <- renderInfoBox({
        
        #This is an old data source now using summary stats file
        # ire_cases <- ecdc %>% 
              # filter(`Countries and territories` == 'Ireland') %>%
              # summarize(Cases = sum(Cases))
  
        infoBox(
            HTML(paste0("Confirmed Cases",br()," in Ireland:")), 
            ire_sum_stats['Cases'], 
            color='black', 
            fill = FALSE)
    })
    
    output$deathsBox <- renderInfoBox({
        #This is an old data source now using summary stats file
        # ire_deaths <- ecdc %>% 
              # filter(`Countries and territories` == 'Ireland') %>%
              # summarize(Deaths = sum(Deaths))
              
        infoBox(
            HTML(paste0("Total Deaths",br()," in Ireland:")), 
            ire_sum_stats['Deaths'], 
            color='red', 
            fill = FALSE)
    })
    
    output$recoverBox <- renderInfoBox({
        infoBox(
            HTML(paste0("Total Recovered",br()," in Ireland:")), 
            ire_sum_stats['Recovered'], 
            color='green', 
            fill = FALSE)
    })
    
    output$countyCasesTable <- renderTable({
        latest_county_table[,c('County', 'Number of Cases')] %>% map_df(rev)
  })   
})
