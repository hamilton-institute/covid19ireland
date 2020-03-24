library(shiny)
library(plotly)
library(tidyverse)
library(leaflet)
library(jsonlite)
library(shinydashboard)
library(rgdal)
library(DT)

ecdc <- readRDS('ECDC_data_current.rds')
#For summary of world plot
ecdc_world_agg <- ecdc %>%
  select(-c(Day, Month, Year, `Countries and territories`, GeoId)) %>%
  group_by(DateRep) %>% 
  summarise(New_Cases = sum(Cases), New_Deaths = sum(Deaths)) %>% 
  mutate(`Total Cases` = cumsum(New_Cases), `Total Deaths` = cumsum(New_Deaths)) %>%
  gather('Type', 'Number', -DateRep) %>%
  filter(Type == 'Total Cases' | Type == 'Total Deaths') %>%
  arrange(DateRep)

#For Trends tab plots
ecdc_country_agg <- ecdc %>%
  select(-c(Day, Month, Year, GeoId)) %>%
  group_by(`Countries and territories`, DateRep) %>% 
  summarise(`New Cases` = sum(Cases), `New Deaths` = sum(Deaths)) %>% 
  mutate(`Total Cases` = cumsum(`New Cases`), `Total Deaths` = cumsum(`New Deaths`)) %>%
  gather('Type', 'Number', -c(DateRep, `Countries and territories`)) %>%
  #filter(Type == 'Total Cases' | Type == 'Total Deaths') %>%
  arrange(DateRep)
  
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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    #Input selection tool in Trends tab
    output$choose_country <- renderUI({
        selectInput("co", 
            "Select a Country", 
            unique(ecdc$`Countries and territories`),
            selected = 'Ireland',
            multiple=TRUE)
    })

    #Cumulative plot in Trends tab
    output$covidCumPlot <- renderPlotly({
        # Extract out the data
        ecdc_plot = ecdc_country_agg %>% 
            filter(`Countries and territories` %in% input$co) %>%
            filter(Type == 'Total Cases' | Type == 'Total Deaths') %>%
            unite(`Countries and territories`, Type, col='CountryType', sep=' ')
            
        plot_ly(ecdc_plot, x = ~DateRep, y = ~Number, type = 'scatter', 
                mode = 'lines+markers', color = ~CountryType) %>% 
                layout(title = 'Number of cumulative cases/deaths for selected countries',
                   xaxis = list(title = 'Date'),
                   yaxis = list (title = 'Number of individuals'))        
    })
    
    #New plot in Trends tab
    output$covidNewPlot <- renderPlotly({
        # Extract out the data
        ecdc_plot = ecdc_country_agg %>% 
            filter(`Countries and territories` %in% input$co) %>%
            filter(Type == 'New Cases' | Type == 'New Deaths') %>%
            unite(`Countries and territories`, Type, col='CountryType', sep=' ')
            
        plot_ly(ecdc_plot, x = ~DateRep, y = ~Number, type = 'scatter', 
                mode = 'lines+markers', color = ~CountryType) %>% 
                layout(title = 'Number of new cases/deaths for selected countries',
                   xaxis = list(title = 'Date'),
                   yaxis = list (title = 'Number of individuals'))        
    })
    
    #Counties table in Counties tab
    output$countyCasesTable <- DT::renderDataTable({
        DT::datatable(
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
            setView(lng = -7.635498, lat = 53.186288, zoom = 6) %>%
            addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.7,
            fillColor = ~pal2(log2(Cases)),
            label = ~paste0(NAME_TAG, ": ", `Number of Cases`, ' cases') ) %>%
            addLegend(pal = pal2, title='Cases', values = ~log2(Cases), opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(2^x)))
    })
    
    #Worldwide plot in Summary tab
    output$summaryWorldPlot <- renderPlotly({
        plot_ly(ecdc_world_agg, x = ~DateRep, y = ~Number, type = 'scatter', 
                mode = 'lines+markers', color = ~Type) %>% 
            layout(title = 'Worldwide number of cumulative cases/deaths',
                   xaxis = list(title = 'Date'),
                   yaxis = list (title = 'Number of individuals'))
        
    })
    
    #Ireland plot in Summary tab
    output$summaryIrelandPlot <- renderPlotly({
        ecdc_ire_agg = ecdc_country_agg %>%
            filter(`Countries and territories` == 'Ireland') %>%
            filter(Type == 'Total Cases' | Type == 'Total Deaths')
            
        plot_ly(ecdc_ire_agg, x = ~DateRep, y = ~Number, type = 'scatter', 
                mode = 'lines+markers', color = ~Type) %>% 
            layout(title = 'Number of cumulative cases/deaths for Ireland',
                   xaxis = list(title = 'Date'),
                   yaxis = list (title = 'Number of individuals'),
                   dragmode='pan') %>%
            config(scrollZoom = TRUE)
        
    })
    
    output$ireCasesBox <- renderInfoBox({
        
        #This is an old data source now using summary stats file
        # ire_cases <- ecdc %>% 
              # filter(`Countries and territories` == 'Ireland') %>%
              # summarize(Cases = sum(Cases))
  
        infoBox(
            HTML(paste0("Confirmed Cases",br()," in Ireland:")), 
            format(sum_stats$Cases[sum_stats$Region == 'ireland'], big.mark=','), 
            color='black', 
            fill = FALSE)
    })
    
    output$ireDeathsBox <- renderInfoBox({
        #This is an old data source now using summary stats file
        # ire_deaths <- ecdc %>% 
              # filter(`Countries and territories` == 'Ireland') %>%
              # summarize(Deaths = sum(Deaths))
              
        infoBox(
            HTML(paste0("Total Deaths",br()," in Ireland:")), 
            format(sum_stats$Deaths[sum_stats$Region == 'ireland'], big.mark=','), 
            color='red', 
            fill = FALSE)
    })
    
    output$ireRecoverBox <- renderInfoBox({
        infoBox(
            HTML(paste0("Total Recovered",br()," in Ireland:")), 
            format(sum_stats$Recovered[sum_stats$Region == 'ireland'], big.mark=','), 
            color='green', 
            fill = FALSE)
    })
    
    output$wCasesBox <- renderInfoBox({
        infoBox(
            HTML(paste0("Confirmed Cases",br()," Worldwide:")), 
            format(sum_stats$Cases[sum_stats$Region == 'world'], , big.mark=','), 
            color='black', 
            fill = FALSE)
    })
    
    output$wDeathsBox <- renderInfoBox({      
        infoBox(
            HTML(paste0("Total Deaths",br()," Worldwide:")), 
            format(sum_stats$Deaths[sum_stats$Region == 'world'], , big.mark=','), 
            color='red', 
            fill = FALSE)
    })
    
    output$wRecoverBox <- renderInfoBox({
        infoBox(
            HTML(paste0("Total Recovered",br()," Worldwide:")), 
            format(sum_stats$Recovered[sum_stats$Region == 'world'], big.mark=','), 
            color='green', 
            fill = FALSE)
    }) 
})
