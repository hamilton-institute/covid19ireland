library(shiny)
library(plotly)
library(tidyverse)
library(leaflet)
library(jsonlite)

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

})
