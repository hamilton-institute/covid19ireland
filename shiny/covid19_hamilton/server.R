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
library(viridis)
library(scales)
library(ggdark)

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



ecdc <- readRDS('ECDC_data_current.rds')
#For summary of world plot
ecdc_world_agg <- ecdc %>%
  select(-c(day, month, year, countriesAndTerritories, geoId)) %>%
  group_by(dateRep) %>% 
  summarise(New_Cases = sum(cases), New_Deaths = sum(deaths)) %>% 
  mutate(`Total Cases` = cumsum(New_Cases), `Total Deaths` = cumsum(New_Deaths)) %>%
  gather('Type', 'Number', -dateRep) %>%
  arrange(dateRep)

#For Trends tab plots
ecdc_country_agg <- ecdc %>%
  select(-c(day, month, year, geoId)) %>%
  group_by(countriesAndTerritories, dateRep) %>% 
  summarise(`New Cases` = sum(cases), `New Deaths` = sum(deaths)) %>% 
  mutate(`Total Cases` = cumsum(`New Cases`), `Total Deaths` = cumsum(`New Deaths`)) %>%
  gather('Type', 'Number', -c(dateRep, countriesAndTerritories)) %>%
  #filter(Type == 'Total Cases' | Type == 'Total Deaths') %>%
  arrange(dateRep)

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

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  #################################SUMMARY TAB#################################
  #Worldwide cumulative plot in Summary tab
  output$cumSumWorldPlot <- renderPlotly({
    ecdc_world_plot <- ecdc_world_agg %>%
      filter(Type == 'Total Cases' | Type == 'Total Deaths')
    
    p = ggplot(ecdc_world_plot, aes(x = dateRep, y = Number, colour = Type)) + 
      geom_line() + 
      geom_point() +
      labs(title = 'Global cumulative values',
           x = '',
           y = 'Number of individuals') + 
      scale_color_manual(values=c("orange", "red")) +
      scale_x_datetime(breaks = pretty_breaks(n = 5), labels = scales::label_date("%d%b"),
                       limits = c(as.POSIXct('2020-03-1'), max(ecdc_world_plot$dateRep))) +
      theme_shiny_dashboard() +
      theme(legend.title = element_blank()) +
      {if (input$logY)  {
        scale_y_continuous(trans = log_trans(), breaks = scales::pretty_breaks(n = 10))
      } else {
        scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
      }}
    ggplotly(p)
    
    # plot_ly(ecdc_world_plot, x = ~dateRep, y = ~Number, type = 'scatter', 
    #         mode = 'lines+markers', color = ~Type) %>% 
    #   layout(title = 'Worldwide number of cumulative cases/deaths',
    #          xaxis = list(title = 'Date', range = ~c(as.POSIXct('2020-02-02'), max(dateRep))),
    #          yaxis = list (title = 'Number of individuals',
    #                        type = if(input$logY) "log" else "linear"))
    
  })
  
  #Worldwide new daily plot in Summary tab
  output$newSumWorldPlot <- renderPlotly({
    ecdc_world_plot <- ecdc_world_agg %>%
      filter(Type == 'New_Cases' | Type == 'New_Deaths')
    p = ggplot(ecdc_world_plot, aes(x = dateRep, y = Number, colour = Type)) + 
      geom_line() + 
      geom_point() +
      labs(title = 'Global daily values',
           x = '',
           y = 'Number of individuals') + 
      scale_color_manual(values=c("orange", "red")) +
      scale_x_datetime(breaks = pretty_breaks(n = 5), labels = scales::label_date("%d%b"),
                       limits = c(as.POSIXct('2020-03-1'), max(ecdc_world_plot$dateRep))) +
    
      theme_shiny_dashboard() +
      theme(legend.title = element_blank()) +
    {if (input$logY) { 
      scale_y_continuous(trans = log_trans(),
                         breaks = scales::pretty_breaks((n = 10))) 
    } else {
        scale_y_continuous(breaks = scales::pretty_breaks((n = 10)))
    }}
    ggplotly(p) #%>% layout(type = if(input$logY) "log" else "linear")
    
    # plot_ly(ecdc_world_plot, x = ~dateRep, y = ~Number, type = 'scatter', 
    #         mode = 'lines+markers', color = ~Type) %>% 
    #   layout(title = 'Worldwide number of new daily cases/deaths',
    #          xaxis = list(title = 'Date', range = ~c(as.POSIXct('2020-02-02'), max(dateRep))),
    #          yaxis = list (title = 'Number of individuals',
    #                        type = if(input$logY) "log" else "linear"))
    
  })
  
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
      
    x_pick = switch(input$sel_axis,
                    'Date' = 'dateRep',
                    'days_since')
    
    if(input$sel_axis == 'Days since 1st case') {
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
                   -c(dateRep, countriesAndTerritories, popData2018, days_since))
    
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
      #scale_color_manual(values=c("orange", "red")) +
      scale_colour_brewer(palette = "Set1") + 
      {if(x_pick == 'dateRep') {
        scale_x_datetime(breaks = pretty_breaks(n = 5), labels = scales::label_date("%d%b"))
      } else {
        scale_x_continuous(breaks = pretty_breaks(n = 5))
      }} + 
      theme_shiny_dashboard() +
      theme(legend.title = element_blank()) +
      {if ('Log cumulative cases' %in% input$sel_var | 
           'Log cumulative deaths' %in% input$sel_var)  {
        scale_y_continuous(trans = log_trans(), breaks = scales::breaks_log(n = 5))
      } else {
        scale_y_continuous(breaks = scales::pretty_breaks(n = 5))
      }}
    
    ggplotly(p)
    
    # plot_ly(ecdc_ire_agg, x = ~dateRep, y = ~Number, type = 'scatter', 
    #         mode = 'lines+markers', color = ~Type) %>% 
    #   layout(title = 'Number of cumulative cases/deaths for Ireland',
    #          xaxis = list(title = 'Date', range = ~c(as.POSIXct('2020-03-10'), max(dateRep))),
    #          yaxis = list (title = 'Number of individuals',
    #                        type = if(input$logY) "log" else "linear"),
    #          dragmode='pan') %>%
    #   plotly::config(scrollZoom = TRUE)
  })
  
  #Ireland new daily plot in Summary tab
  output$newSumIrelandPlot <- renderPlotly({
    ecdc_ire_agg = ecdc_country_agg %>%
      filter(countriesAndTerritories == 'Ireland') %>%
      filter(Type == 'New Cases' | Type == 'New Deaths')
    
    p = ggplot(ecdc_ire_agg, aes(x = dateRep, y = Number, colour = Type)) + 
      geom_line() + 
      geom_point() +
      labs(title = 'Ireland: daily values',
           x = '',
           y = 'Number of individuals') + 
      scale_color_manual(values=c("orange", "red")) +
      scale_x_datetime(breaks = pretty_breaks(n = 5), labels = scales::label_date("%d%b"),
                       limits = c(as.POSIXct('2020-03-1'), max(ecdc_ire_agg$dateRep))) +
      theme_shiny_dashboard() +
      theme(legend.title = element_blank()) +
      {if (input$logY) scale_y_continuous(trans = log_trans())}
    ggplotly(p)
    
    # plot_ly(ecdc_ire_agg, x = ~dateRep, y = ~Number, type = 'scatter', 
    #         mode = 'lines+markers', color = ~Type) %>% 
    #   layout(title = 'Number of new daily cases/deaths for Ireland',
    #          xaxis = list(title = 'Date', range = ~c(as.POSIXct('2020-03-10'), max(dateRep))),
    #          yaxis = list (title = 'Number of individuals',
    #                        type = if(input$logY) "log" else "linear"),
    #          dragmode='pan') %>%
    #   plotly::config(scrollZoom = TRUE)
  })
  
  #Ireland cases infobox in summary tab
  output$ireCasesBox <- renderInfoBox({
    
    infoBox(
      HTML(paste0("Confirmed Cases",br()," in Ireland:")), 
      format(sum_stats$Cases[sum_stats$Region == 'ireland'], big.mark=','), 
      color="yellow",
      width = 10,
      icon = icon("thermometer-three-quarters"),
      fill = TRUE)
  })
  
  #Ireland deaths infobox in summary tab
  output$ireDeathsBox <- renderInfoBox({

    infoBox(
      HTML(paste0("Total Deaths",br()," in Ireland:")), 
      format(sum_stats$Deaths[sum_stats$Region == 'ireland'], big.mark=','), 
      icon = icon("exclamation-triangle"),
      color = "red",
      fill = TRUE)
  })
  
  #Ireland recovered infobox in summary tab
  output$ireRecoverBox <- renderInfoBox({
    infoBox(
      HTML(paste0("Total Recovered",br()," in Ireland:")), 
      format(sum_stats$Recovered[sum_stats$Region == 'ireland'], big.mark=','), 
      color="green", 
      icon = icon("heart"),
      fill = TRUE)
  })
  
  #Worldwide cases infobox in summary tab
  output$wCasesBox <- renderInfoBox({
    infoBox(
      HTML(paste0("Confirmed Cases",br()," Worldwide:")), 
      format(sum_stats$Cases[sum_stats$Region == 'world'], big.mark=','), 
      color='yellow', 
      icon = icon("globe"),
      fill = TRUE)
  })
  
  #Worldwide deaths infobox in summary tab
  output$wDeathsBox <- renderInfoBox({      
    infoBox(
      HTML(paste0("Total Deaths",br()," Worldwide:")), 
      format(sum_stats$Deaths[sum_stats$Region == 'world'], big.mark=','), 
      icon = icon("exclamation-triangle"),
      color='red', 
      fill = TRUE)
  })
  
  #Worldwide recovered infobox in summary tab
  output$wRecoverBox <- renderInfoBox({
    infoBox(
      HTML(paste0("Total Recovered",br()," Worldwide:")), 
      format(sum_stats$Recovered[sum_stats$Region == 'world'], big.mark=','), 
      color='green', 
      icon = icon("heart"),
      fill = TRUE)
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
  
  #################################INTERNATIONAL TRENDS TAB#################################
  #Input selection tool in Trends tab
  output$choose_country <- renderUI({
    selectInput("co", 
                "Select Countries", 
                unique(ecdc$countriesAndTerritories),
                selected = 'Ireland',
                multiple = TRUE)
  })
  
  #Country comparison table in trends tab
  output$compareTable <- DT::renderDataTable({
    # Extract out the data
    ecdc_table = ecdc_country_agg %>% 
      filter(countriesAndTerritories %in% input$co) %>%
      pivot_wider(names_from = Type, values_from = Number) %>%
      group_by(countriesAndTerritories) %>%
      summarise(`Total Cases` = sum(`New Cases`), `Total Deaths` = sum(`New Deaths`))
    
    DT::datatable(
      ecdc_table,
      caption='Comparison of selected countries',
      options = list(
        pageLength = 20,
        scrollY='calc((100vh - 290px)/1.0)',
        searching = FALSE,
        paging=FALSE
      ),
      rownames=FALSE
    )
  })
  
  #Cumulative plot in Trends tab
  output$covidCumPlot <- renderPlotly({
    # Extract out the data
    ecdc_plot = ecdc_country_agg %>% 
      filter(countriesAndTerritories %in% input$co) %>%
      filter(Type == 'Total Cases' | Type == 'Total Deaths') %>%
      unite(countriesAndTerritories, Type, col='CountryType', sep=' ')
    
    plot_ly(ecdc_plot, x = ~dateRep, y = ~Number, type = 'scatter', 
            mode = 'lines+markers', color = ~CountryType) %>% 
      layout(title = 'Number of cumulative cases/deaths for selected countries',
             xaxis = list(title = 'Date', range = ~c(as.POSIXct('2020-03-01'), max(dateRep))),
             yaxis = list (title = 'Number of individuals',
                           type = if(input$logY) "log" else "linear"))        
  })
  
  #New plot in Trends tab
  output$covidNewPlot <- renderPlotly({
    # Extract out the data
    ecdc_plot = ecdc_country_agg %>% 
      filter(countriesAndTerritories %in% input$co) %>%
      filter(Type == 'New Cases' | Type == 'New Deaths') %>%
      unite(countriesAndTerritories, Type, col='CountryType', sep=' ')
    
    plot_ly(ecdc_plot, x = ~dateRep, y = ~Number, type = 'scatter', 
            mode = 'lines+markers', color = ~CountryType) %>% 
      layout(title = 'Number of new cases/deaths for selected countries',
             xaxis = list(title = 'Date', range = ~c(as.POSIXct('2020-03-01'), max(dateRep))),
             yaxis = list (title = 'Number of individuals',
                           type = if(input$logY) "log" else "linear"))        
  })

    
    #age in hospital plot
    output$ageCases <- renderPlotly({
      
      
      
      age.hosp <- (all_tables[[1]]$age_hospitalised)
      x<-as.character(age.hosp$`Hospitalised Age`)
      y<-as.numeric(age.hosp$`Number of Cases`)
      text<- paste0(age.hosp$`Number of Cases`, ' patients')
      data1 <- data.frame(x, y, 'Hospitalised')
      data1$x <- factor(data1$x, levels = as.character(age.hosp$`Hospitalised Age`))
      names(data1) <- c('Age','Count','Classification')
      

      age <- (all_tables[[1]]$age)
      y<-as.numeric(age$`Number of Cases`)
      y <- y[-length(y)]
      y <- y[-length(y)]
      
      # combine first two bins
      y[1] = y[1] + y[2]
      y = y[-2]
      
      
      text<- paste0(y, ' patients')
      
      
      data2 <- data.frame(x, y, 'Cases')
      data2$x <- factor(data2$x, levels = as.character(age.hosp$`Hospitalised Age`))
      names(data2) <- c('Age','Count','Classification')
      
      
      all.data <- rbind(data2,data1)
      
      g<-ggplot(data = all.data, aes(Age, Count, fill=Classification))+
        geom_bar(stat = 'identity', position=position_dodge(0)) +
        theme_minimal() +
        ggtitle('Cases by Age: Ireland')
        
      ggplotly(g, tooltip=c("Classification", "Count"))
      

    })
    
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
      
      
      
      g<-ggplot(data = data, aes(Gender, Percentage, fill=Gender))+
        geom_bar(stat = 'identity',width = 0.7, position = position_dodge()) +
        theme_minimal() +
        labs(y="%", x = "") +
        ggtitle('Gender Breakdown') + 
        theme(legend.position = 'none')+
        scale_fill_manual("legend", values = c("Male" = "darkorchid3", "Female" = "aquamarine4"))
        
      
      ggplotly(g, tooltip = 'Percentage')
  
 
    
      })
    
    
    
    
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
        theme_minimal() +
        labs(y="Count", x = " ") +
        ggtitle('Number of Healthcare Workers Tested Positive') +
        geom_text(aes(label=x),nudge_y = -100) +
        theme(
          axis.text.x = element_blank(),
          axis.ticks = element_blank()) 
      
      
      ggplotly(g) 
      
      
      
    })  
    
    
    
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
      x <- c('Community\ntransmission','Contact with\nknown case',
             'Travel\nAbroad','Under\ninvestigation') 

      data <- data.frame(x, y, text)
      data$x <- factor(data$x, levels = x)

      names(data) <- c("Class",    "Count",    "text")
      g <- ggplot(data = data, aes(Class, Count, fill=Class, text = text))+
        geom_bar(stat = 'identity') +
        theme_minimal() +
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
      
      
      ggplotly(g, tooltip=c("Count")) 
      
      
      
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

      data <- tibble(dates, icu.pats, hosp.pats)
      fig <- plot_ly(x = ~ data$dates) %>% 
        add_lines(y = ~ data$hosp.pats, text = paste(data$hosp.pats, " patients in hospital"), name = "Hospitalised Patients") %>%
        add_lines(y = ~ data$icu.pats, text = paste(data$icu.pats, " patients in ICU"), name = "ICU Patients") 
      
      fig <- fig %>% layout(title = 'Hospitalised Patients', yaxis = list(title = "Count"), xaxis = list(title = "Date"))
      fig
    }) 
    
    
    
})
