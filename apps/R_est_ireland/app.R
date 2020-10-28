# Estimate R0 app - for entire country using Catherine's layout

rm(list = ls(all = TRUE))
library(R0)
library(tidyverse)
library(tidycovid19)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(plotly)
library(gridExtra)
library(geofacet)

latest = read.csv('http://opendata-geohive.hub.arcgis.com/datasets/d9be85b30d7748b5b7c09450b8aede63_0.csv',
                  stringsAsFactors = FALSE) %>%
  mutate(Date = as.Date(TimeStamp)) %>% 
  add_count(CountyName) %>% 
  filter(n == max(n))
# saveRDS(latest, file = 'latest_irish_county_data.rds')

old_irish_county_data = readRDS('latest_irish_county_data.rds')
if(nrow(latest) > nrow(old_irish_county_data)) {
  saveRDS(latest_irish_county_data, file = 'latest_irish_county_data.rds')  
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
               
               dateRangeInput("date_range", "Date range:",
                              start  = max(latest$Date) - 21,
                              end    = max(latest$Date),
                              min    = min(latest$Date),
                              max    = max(latest$Date),
                              format = "dd/mm/yyyy",
                              separator = " - "),
               
               actionButton(inputId = "button", label = "show extra options"),
              
               pickerInput("R_method",
                           "Method for computing R", 
                           choices = c("EG", "ML", "TD", "AR"),
                           selected = c('EG'),
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
      navbarPage("COVID-19 R at county level",
                 # Output: HTML table with requested number of observations ----
                 tabPanel("Estimation",
                          fluidPage(
                            fluidRow(
                              plotlyOutput("R_estim", height = 1000) %>% withSpinner(color="#1E90FF"),
                            )
                          ),
                          
                          
                 ),
                 
                 tabPanel("Assumptions",
                          fluidPage(
                            fluidRow(
                              
                              p(HTML("<p> This visualisation plots the raw number of cases in a selected county and calculates the R number for that period using the methods described in <a href = 'https://bmcmedinformdecismak.biomedcentral.com/articles/10.1186/1472-6947-12-147'>Obadia et al, BMC Medical Informatics and Decision Making, 2012</a>.<p> The method relies on an estimate of the Generation Time of the disease; this is the time from becoming infected with COVID-19 to the time of generating a secondary case. The estimated generation time distribution and its parameters have been taken from <a href = 'https://onlinelibrary.wiley.com/doi/full/10.1111/biom.13325'>Yuhao et al Biometrics, 2020</a>. The values can be changed by clicking the 'show extra options' button.<p> The R0 package allows for different methods to calculate the R value. We use the Maximum Likelihood method which also provides a 95% confidence interval. Other methods can be selected in the extra options.<p> If there are large number of zero cases, or the date range is too large/small, the estimate may fail and an R0 number will not be shown. Try increasing/decreasing the date range to get a valid estimate."))
                              
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

    g = matrix(NA, nrow = 7, ncol = 4)
    
    g[1,] = c("Kerry","Cork", "Waterford","Wexford")
    g[2,] = c("Limerick","Tipperary", "Kilkenny","Carlow")
    g[3,] = c("Clare","Offaly", "Laois","Wicklow")
    g[4,] = c("Galway","Westmeath", "Kildare","Dublin")
    g[5,] = c("Mayo","Roscommon","Longford", "Meath")
    g[6,] = c("Sligo","Leitrim", "Cavan", "Louth")
    g[7,] = c("Donegal",NA, "Monaghan",NA)
    
    g[1:7,] = g[7:1,]
    g[1,2] = "All"
    ire_grid = data.frame(name = as.vector(g),
                          row = as.vector(row(g)),
                          col = as.vector(col(g)))
    ire_grid$name = as.character(ire_grid$name)
    ire_grid$code = ire_grid$name
    ire_grid = ire_grid[!is.na(ire_grid$name),]
    ire_grid = ire_grid[-8,]
    
    # Get the data
    data_use = latest %>% 
      group_by(CountyName) %>% 
      mutate(cum_cases = ConfirmedCovidCases,
             cases = c(cum_cases[1], pmax(0, diff(cum_cases)))) %>% # NOTE: removing wrong neg values
      ungroup() %>% 
      dplyr::select(Date, cases, CountyName, PopulationCensus16) %>% 
      filter(Date >= input$date_range[1], Date <= input$date_range[2]) %>% 
      na.omit()
    
    # COVID generation time
    GT = generation.time(input$GD_dist, c(input$GT_mean, input$GT_sd))
    
    # Now get R0
    counties = unique(data_use$CountyName)
    n_counties = length(counties)
    estR0 = vector('list', length = n_counties)
    data_use$CountyName2 = data_use$CountyName
    for(i in 1:n_counties) {
      curr_data = data_use %>% filter(CountyName == counties[i])
      estR0[[i]] = try(estimate.R(epid = curr_data$cases, 
                                  GT = GT, 
                                  methods = input$R_method, 
                                  pop.size = curr_data$PopulationCensus16[1], 
                                  nsim = input$num_sim), silent = TRUE)
      if(class(estR0[[i]]) != 'try-error') {
        # new_county_name = paste0(counties[i],"; R = ", 
        #                         signif(estR0[[i]]$estimates[[input$R_method]]$R, 3),
        #                         "\n(95% CI: ", 
        #                         signif(estR0[[i]]$estimates[[input$R_method]]$conf.int[1], 3),', ',
        #                         signif(estR0[[i]]$estimates[[input$R_method]]$conf.int[2], 3), ')')
        new_county_name = paste0(counties[i],"; R = ",
                                signif(estR0[[i]]$estimates[[input$R_method]]$R, 3))
      } else {
        new_county_name = paste0(counties[i],"; R0 not estimated")
      }
      data_use$CountyName2[data_use$CountyName == counties[i]] = new_county_name
      ire_grid$name[ire_grid$name == counties[i]] = new_county_name
      ire_grid$code[ire_grid$code == counties[i]] = new_county_name
    }
    
    # Put the R values into the county titles
    p = ggplot(data = data_use, aes(x = Date, y = cases)) + 
      geom_point() + 
      facet_geo(~ CountyName2, grid = ire_grid, scales = "free_y") +
      labs(x = 'Date',
           y = 'Cases',
           title = paste('Cases from', 
                         format(input$date_range[1], '%d-%b%-%Y'), 'to',
                         format(input$date_range[2], '%d-%b%-%Y'))) + 
      theme_bw() + 
      geom_smooth(se = FALSE)
    
    ggplotly(p)
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)


