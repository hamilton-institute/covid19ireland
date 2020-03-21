library(shiny)
library(plotly)
ecdc = readRDS(file = '../../data/scraped/ECDC_data_20200321.rds')

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

})
