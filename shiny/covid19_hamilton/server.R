library(shiny)
library(plotly)
library(tidyverse)
library(leaflet)
library(jsonlite)
library(shinydashboard)

ecdc = readRDS(file = '../../data/scraped/ECDC_data_20200321.rds')
countiesShapes <- readLines("../../data/counties_simple.geojson", warn = FALSE) %>%
      paste(collapse = "\n") %>%
      fromJSON(simplifyVector = FALSE)
#Default styles for all features
countiesShapes$style = list(
      weight = 1,
      color = "#555555",
      opacity = 1,
      fillOpacity = 0.8
    )
#All tables contains information on a county-by-county basis
all_tables <- readRDS('../../data/scraped/all_tables.rds')

#Get the latest table containing info on all counties
latest_county_table <- head(all_tables, n=1)[[1]]$counties
#Change the number of cases from char to int
latest_county_table$Cases <- strtoi(gsub("[^0-9.-]", "", latest_county_table$`Number of Cases`))

#Get the number of cases in each county in the same order as the counties
#are in the geojson - not sure order is necessary
countyCases <- sapply(countiesShapes$features, function(feat) {
  cases <- latest_county_table$Cases[latest_county_table$County == feat$properties$NAME_TAG]
  
  if(length(cases)) cases else 0
})

#Add number of cases as a feature to each county in the geojson
countiesShapes$features <- lapply(countiesShapes$features, function(feat) {
  cases <- latest_county_table$Cases[latest_county_table$County == feat$properties$NAME_TAG]
  cases <- if(length(cases)) cases else 0
  feat$properties$cases <- cases
  feat
})

#Color the counties by number of cases
pal <- colorNumeric("Reds", countyCases)

# Add a properties$style list to each feature
countiesShapes$features <- lapply(countiesShapes$features, function(feat) {
        feat$properties$style <- list(
            fillColor = pal(feat$properties$cases)
        )
        feat
})

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

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
                   yaxis = list (title = 'Number of individuals'))
        
    })
    output$covidMap <- renderLeaflet({
        leaflet() %>% 
            addProviderTiles(providers$Stamen.TonerLite,
                    options = providerTileOptions(noWrap = TRUE)
                ) %>%
            setView(lng = -7.635498, lat = 53.186288, zoom = 6) %>%
            addGeoJSON(countiesShapes)
    })
    
    output$mapPlot <- renderPlotly({
        
        # Extract out the data
        ecdc_plot = ecdc %>% 
            filter(`Countries and territories` == 'Ireland') %>% 
            select(-c(Day,Month,Year,`Countries and territories`,GeoId)) %>% 
            pivot_longer(names_to = 'Type', values_to = 'Number', -DateRep)
        
        plot_ly(ecdc_plot, x = ~DateRep, y = ~Number, type = 'scatter', 
                mode = 'lines', color = ~Type) %>% 
            layout(title = paste('Number of cases/deaths for','Ireland'),
                   xaxis = list(title = 'Date'),
                   yaxis = list (title = 'Number of individuals'))
        
    })
    
    output$casesBox <- renderInfoBox({
        ire_cases <- ecdc %>% 
              filter(`Countries and territories` == 'Ireland') %>%
              summarize(Cases = sum(Cases))
  
        infoBox(
            HTML(paste0("Confirmed Cases",br()," in Ireland:")), 
            ire_cases, 
            color='black', 
            fill = FALSE)
    })
    
    output$deathsBox <- renderInfoBox({
        ire_deaths <- ecdc %>% 
              filter(`Countries and territories` == 'Ireland') %>%
              summarize(Deaths = sum(Deaths))
              
        infoBox(
            HTML(paste0("Total Deaths",br()," in Ireland:")), 
            ire_deaths, 
            color='red', 
            fill = FALSE)
    })
    
    output$countyCasesTable <- renderTable({
        latest_county_table[,c('County', 'Number of Cases')] %>% map_df(rev)
  })   
})
