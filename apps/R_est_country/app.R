# Estimate R0 app for each country

rm(list = ls(all = TRUE))
library(R0)
library(tidyverse)
library(tidycovid19)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(plotly)
library(lubridate)

latest = download_merged_data(silent = TRUE, cached = TRUE)

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      useShinyjs(),
      fluidRow(
        column(width=12,
               
               setSliderColor(c(rep("#b2df8a", 3)), sliderId=c(8,9,10)),
               # Input: Selector for choosing dataset ----
               
               dateRangeInput("date_range", "Date range:",
                              start  = max(latest$date) - 21,
                              end    = max(latest$date),
                              min    = min(latest$date),
                              max    = max(latest$date),
                              format = "dd/mm/yyyy",
                              separator = " - "),
               
               pickerInput("sel_cty",
                           "Select country", 
                           choices = sort(unique(latest$country)),
                           selected = c('Ireland'),
                           options = list(`actions-box` = TRUE,
                                          `live-search` = TRUE),
                           multiple = FALSE),
               actionButton(inputId = "button", label = "show extra options"),
              
               pickerInput("R_method",
                           "Method for computing R", 
                           choices = c("EG", "ML", "TD", "AR"),
                           selected = c('ML'),
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
      navbarPage("COVID-19 R at country level",
                 # Output: HTML table with requested number of observations ----
                 tabPanel("Estimation",
                          fluidPage(
                            fluidRow(
                              plotlyOutput("R_estim") %>% withSpinner(color="#1E90FF"),
                            )
                          ),
                          
                          
                 ),
                 
                 tabPanel("Assumptions",
                          fluidPage(
                            fluidRow(
                              
                              p(HTML("<p> This visualisation plots the raw number of cases in a selected country and calculates the R number for that period using the methods described in <a href = 'https://bmcmedinformdecismak.biomedcentral.com/articles/10.1186/1472-6947-12-147'>Obadia et al, BMC Medical Informatics and Decision Making, 2012</a>.<p> The method relies on an estimate of the Generation Time of the disease; this is the time from becoming infected with COVID-19 to the time of generating a secondary case. The estimated generation time distribution and its parameters have been taken from <a href = 'https://onlinelibrary.wiley.com/doi/full/10.1111/biom.13325'>Yuhao et al Biometrics, 2020</a>. The values can be changed by clicking the 'show extra options' button.<p> The R0 package allows for different methods to calculate the R value. We use the Maximum Likelihood method which also provides a 95% confidence interval. Other methods can be selected in the extra options.<p> If there are large number of zero cases, or the date range is too large/small, the estimate may fail and an R0 number will not be shown. Try increasing/decreasing the date range to get a valid estimate."))
                              
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
      filter(country == input$sel_cty) %>% 
      mutate(cum_cases = confirmed,
             cases = c(cum_cases[1], diff(confirmed))) %>% 
      dplyr::select(date, cases, population) %>% 
      filter(date >= input$date_range[1], date <= input$date_range[2]) %>% 
      na.omit()
    
    # COVID generation time
    GT = generation.time(input$GD_dist, c(input$GT_mean, input$GT_sd))
    
    # Now get R0
    estR0 = try(estimate.R(epid = data_use$cases, 
                           GT = GT, 
                           methods = input$R_method, 
                           pop.size = data_use$population[1], 
                           nsim = input$num_sim), silent = TRUE)
    
    
    p = ggplot(data = data_use, aes(x = date, y = cases)) + 
      geom_point() + 
      labs(x = 'Date',
           y = 'Cases',
           title = paste('Cases in',input$sel_cty, 'from', 
                         format(input$date_range[1], '%d-%b'), 'to',
                         format(input$date_range[2], '%d-%b'))) + 
      theme_bw() + 
      geom_smooth(se = FALSE)
    
    ggp <- ggplot_build(p)
    yrange = ggp$layout$panel_params[[1]]$y.range
    xrange = ggp$layout$panel_params[[1]]$x.range
    
    
    
    # Add the annotation
    a <- list(
      x = ggp$layout$panel_scales_x[[1]]$range$range[1],
      y = ggp$layout$panel_scales_y[[1]]$range$range[2],
      xref = "x",
      yref = "y",
      xanchor = 'left',
      showarrow = FALSE,
      font = list(size = 20)
    )
    
    # shiny::validate(
    #   shiny::need(class(estR0) != "try-error", "Case values or date range not appropriate for R0 estimation using this method.")
    # )
    if(class(estR0) == "try-error") {
      a$text = "R0 not estimated (bad case values or date range)"
      a$font = list(size = 14)
    } else {
      a$text = paste0("R = ", signif(estR0$estimates[[input$R_method]]$R, 3),
                    " (95% CI: ", signif(estR0$estimates[[input$R_method]]$conf.int[1], 3),', ',
                    signif(estR0$estimates[[input$R_method]]$conf.int[2], 3), ')')
    }
    ggplotly(p) %>% layout(annotations = a)  
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)


