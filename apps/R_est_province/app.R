# Estimate R0 app - for each province

rm(list = ls(all = TRUE))
library(R0)
library(tidyverse)
library(tidycovid19)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(plotly)
library(gridExtra)

# Get provinces
provinces = read.csv('provinces.csv')

# Get latest county data
latest = read.csv('http://opendata-geohive.hub.arcgis.com/datasets/d9be85b30d7748b5b7c09450b8aede63_0.csv',
                  stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(TimeStamp)) %>% 
  add_count(CountyName) %>% 
  filter(n == max(n)) %>% 
  left_join(provinces, by = c('CountyName' = 'County'))
# saveRDS(latest, file = 'latest_irish_county_data.rds')

old_irish_county_data = readRDS('latest_irish_county_data.rds')
if(nrow(latest) > nrow(old_irish_county_data)) {
  saveRDS(latest, file = 'latest_irish_county_data.rds')  
} else {
  latest = readRDS(file = 'latest_irish_county_data.rds')
}

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      useShinyjs(),
      fluidRow(
        column(width=12,
               
               setSliderColor(c(rep("#b2df8a", 3)), sliderId=c(8,9,10)),
               # Input: Selector for choosing dataset ----
               
               dateInput("date_end", "End of two week period to estimate R:",
                         value = max(latest$Date),
                         format = "dd/mm/yyyy"),
               
               actionButton(inputId = "button", label = "show extra options"),
               
               pickerInput("R_method",
                           "Method for computing R", 
                           choices = c("EG", "ML", "SB"),
                           selected = c('SB'),
                           multiple = FALSE),
               
               pickerInput("GD_dist",
                           "Generation time distribution", 
                           choices = c("gamma", "weibull", "lognormal"),
                           selected = c('gamma'),
                           multiple = FALSE),
               
               numericInput(inputId = "GT_mean",
                            label = "Generation time mean",
                            value = 3.0),
               
               numericInput(inputId = "GT_sd",
                            label = "Generation time standard deviation",
                            value = 0.4),
               
               numericInput(inputId = "num_sim",
                            label = "Number of simulations to run (higher = slower but more accurate)",
                            value = 200),
               
        ))),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      navbarPage("COVID-19 R at province level",
                 # Output: HTML table with requested number of observations ----
                 tabPanel("Estimation",
                          fluidPage(
                            fluidRow(
                              plotlyOutput("R_estim", height = 500) %>% 
                                withSpinner(color="#1E90FF"),
                            )
                          ),
                          
                          
                 ),
                 
                 tabPanel("Assumptions",
                          fluidPage(
                            fluidRow(
                              
                              p(HTML("<p> This visualisation plots the raw number of cases in a selected province and calculates the R number for that period using the methods described in <a href = 'https://bmcmedinformdecismak.biomedcentral.com/articles/10.1186/1472-6947-12-147'>Obadia et al, BMC Medical Informatics and Decision Making, 2012</a>.<p> The method relies on an estimate of the Generation Time of the disease; this is the time from becoming infected with COVID-19 to the time of generating a secondary case. The estimated generation time distribution and its parameters have been taken from <a href = 'https://onlinelibrary.wiley.com/doi/full/10.1111/biom.13325'>Yuhao et al Biometrics, 2020</a>. The values can be changed by clicking the 'show extra options' button.<p> The R0 package allows for different methods to calculate the R value. We use the Sequential Bayesian method which also provides a 95% confidence interval. Other methods can be selected in the extra options.<p> If there are large number of zero cases, or the date range is too large/small, the estimate may fail and an R0 number will not be shown. Try increasing/decreasing the date range to get a valid estimate.<p> Be aware that most of these methods have hidden assumptions (e.g. that the date range shows a period of exponential growth). If you are changing the method, we would recommend reading the above papers first to avoid mistaken readings."))
                              
                            )
                          )
                 )
      )
    )
    
    
    ########################
  ) 
)




server <- function(input, output) {
  
  
  observeEvent(input$button, {
    shinyjs::toggle("R_method")
    shinyjs::toggle("GD_dist")
    shinyjs::toggle("GT_mean")
    shinyjs::toggle("GT_sd")
    shinyjs::toggle("num_sim")
  }, ignoreNULL = FALSE)
  
  output$R_estim <- renderPlotly({
    
    # Get the data
    data_use = latest %>% 
      dplyr::select(Date, CountyName, Province, ConfirmedCovidCases, PopulationCensus16) %>% 
      group_by(Province, Date) %>% 
      summarise(cum_cases = sum(ConfirmedCovidCases),
                pop = sum(PopulationCensus16)) %>% 
      ungroup() %>% 
      group_by(Province) %>% 
      mutate(cases = c(cum_cases[1], pmax(0, diff(cum_cases)))) %>% 
      ungroup() %>% 
      filter(Date >= input$date_end - 14, Date <= input$date_end) #%>% 
      #na.omit()
    
    # COVID generation time
    GT = generation.time(input$GD_dist, c(input$GT_mean, input$GT_sd))
    
    # Now get R0
    provinces = unique(data_use$Province)
    n_provinces = length(provinces)
    estR0 = vector('list', length = n_provinces)
    
    for(i in 1:n_provinces) {
      curr_data = data_use %>% filter(Province == provinces[i])
      estR0[[i]] = try(estimate.R(epid = curr_data$cases,
                                  t = curr_data$Date, 
                                  begin = as.integer(1),
                                  end = as.integer(length(curr_data$cases)),
                                  GT = GT, 
                                  methods = input$R_method, 
                                  pop.size = curr_data$pop[1], 
                                  nsim = input$num_sim), silent = TRUE)
      
      if(class(estR0[[i]]) != 'try-error') {
        
        if(input$R_method == "SB") {
          R_est = signif(tail(estR0[[i]]$estimates[[input$R_method]]$R, 1), 3)
        } else {
          R_est = signif(estR0[[i]]$estimates[[input$R_method]]$R, 3)
        }
        new_province_name = paste0(provinces[i],"; R = ", R_est)
      } else {
        new_province_name = paste0(provinces[i],"; R0 not estimated")
      }
      data_use$Province2[data_use$Province == provinces[i]] = new_province_name
    }
    
    # Put it in as an ordered factor
    data_use$Province2 = factor(data_use$Province2,
                                levels = unique(data_use$Province2)[c(1,4,3,2)],
                                ordered = TRUE)
    
    # Put the R values into the province titles
    p = ggplot(data = data_use, aes(x = Date, y = cases)) + 
      geom_point() + 
      facet_wrap(~ Province2, nrow = 2) +
      labs(x = 'Date',
           y = 'Cases',
           title = paste('Cases from', 
                         format(input$date_end-14, '%d-%b'), 'to',
                         format(input$date_end, '%d-%b'))) + 
      theme_bw() + 
      geom_smooth(se = FALSE)
    
    ggplotly(p)
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)


