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
library(ranger) # For making predictions 

latest <- download_merged_data(silent = TRUE, cached = TRUE)

find_data <- function(date_max, latest_data = latest, 
                      current_country = current_country){
  data_use <- latest_data %>% 
    dplyr::filter(country == current_country) %>% 
    dplyr::mutate(cum_cases = ecdc_cases,
                  cases = c(cum_cases[1], diff(ecdc_cases))) %>% 
    #dplyr::filter(date >= date_max - 90, date <= date_max) %>% 
    #dplyr::mutate(cases = scale(cases)) %>% 
    dplyr::select(date, cases, country) %>% 
    dplyr::filter(date >= date_max - 21, date <= date_max) %>% 
    na.omit() %>% 
    dplyr::group_by(country) %>% 
    dplyr::mutate(
      n_ind = 1:n(), 
      R_name = paste0("R", n_ind)) %>% 
    dplyr::select(-date) %>% 
    dplyr::arrange(country) %>% 
    dplyr::ungroup() %>% 
    dplyr::as_tibble() %>% 
    tidyr::complete(R_name, fill = list(cases = NA)) %>% 
    dplyr::group_by(country) %>% 
    dplyr::arrange(country, n_ind) %>% 
    tidyr::fill(cases, .direction = "down") %>% 
    dplyr::select(-n_ind) %>% 
    tidyr::spread(R_name, cases) %>% 
    dplyr::ungroup() 
  
  df_remove <-  latest_data %>% 
    dplyr::mutate(cum_cases = ecdc_cases,
                  cases = c(cum_cases[1], diff(ecdc_cases))) %>% 
    dplyr::select(date, cases, country) %>% 
    dplyr::filter(date >= date_max - 22, date <= date_max) %>% 
    na.omit() %>% 
    dplyr::group_by(country) %>% 
    dplyr::summarise(s = sum(cases)) %>% 
    dplyr::filter(s == 0) %>% 
    dplyr::pull(country)
  
  data_use %>% 
    dplyr::filter(!(country %in% df_remove))
}



# This model uses the last 21 days of R 
model <-  readRDS("est_R0_final_model_comp.rds")

pred_country <- function(data, rf_model = model){
  #data[, -1] <- scale(data[, -1])
  pred.R <- predict(rf_model, data = data,
                    type = 'quantiles')
  df <- data.frame(
    low = pred.R$predictions[,1],
    upp = pred.R$predictions[,3],
    pred = pred.R$predictions[,2]
  ) %>% 
    dplyr::bind_cols(data)
  df
}

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      useShinyjs(),
      fluidRow(
        column(width=12,
               
               setSliderColor(c(rep("#b2df8a", 3)), sliderId=c(8,9,10)),
               # Input: Selector for choosing dataset ----
               
               dateInput("date_end", "End of three week period to estimate R:",
                         value = max(latest$date),
                         format = "dd/mm/yyyy"),
               
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
                              
                              p(HTML("<p> This visualisation plots the raw number of cases in a selected country and calculates the R number for that period using the methods described in <a href = 'https://bmcmedinformdecismak.biomedcentral.com/articles/10.1186/1472-6947-12-147'>Obadia et al, BMC Medical Informatics and Decision Making, 2012</a>.<p> The method relies on an estimate of the Generation Time of the disease; this is the time from becoming infected with COVID-19 to the time of generating a secondary case. The estimated generation time distribution and its parameters have been taken from <a href = 'https://onlinelibrary.wiley.com/doi/full/10.1111/biom.13325'>Yuhao et al Biometrics, 2020</a>. The values can be changed by clicking the 'show extra options' button.<p> The R0 package allows for different methods to calculate the R value. We use the Sequential Bayes method which also provides a 95% confidence interval. Other methods can be selected in the extra options.<p> If there are large number of zero cases, or the date range is too large/small, the estimate may fail and an R0 number will not be shown.<p> Be aware that most of these methods have hidden assumptions (e.g. that the date range shows a period of exponential growth). If you are changing the method, we would recommend reading the above papers first to avoid mistaken readings."))
                              
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
    
    # # Get the data
    # data_use = latest %>% 
    #   filter(country == input$sel_cty) %>% 
    #   mutate(cum_cases = ecdc_cases,
    #          cases = c(cum_cases[1], diff(ecdc_cases))) %>% 
    #   dplyr::select(date, cases, population) %>% 
    #   filter(date >= input$date_end - 14, date <= input$date_end) %>% 
    #   na.omit()
    # 
    # # COVID generation time
    # GT = generation.time(input$GD_dist, c(input$GT_mean, input$GT_sd))
    # 
    # # Now get R0
    # estR0 = try(estimate.R(epid = data_use$cases,
    #                        t = data_use$date, 
    #                        begin = as.integer(1),
    #                        end = as.integer(length(data_use$cases)),
    #                        GT = GT, 
    #                        methods = input$R_method, 
    #                        pop.size = data_use$population[1], 
    #                        nsim = input$num_sim), silent = TRUE)
    
    current_country <- input$sel_cty
    date_max <- input$date_end
    
    seq_dates <- seq.Date(date_max - 45, date_max,  by = 1)
    
    data_seq_dates <- purrr:::map(seq_dates, find_data, 
                                  current_country = current_country) %>% 
      dplyr::bind_rows()
    
    data_seq_dates <- data_seq_dates %>% 
      dplyr::mutate_if(is.numeric, scale)
  
    pred_all_dates <- pred_country(data_seq_dates) 
    
    estR0 <-  pred_all_dates %>% dplyr::slice(nrow(pred_all_dates))
    
    data_use = latest %>%
      filter(country == input$sel_cty) %>%
      mutate(cum_cases = ecdc_cases,
             cases = c(cum_cases[1], diff(ecdc_cases))) %>%
      dplyr::select(date, cases, population) %>%
      filter(date >= input$date_end - 14, date <= input$date_end) %>%
      na.omit()
    
    
    
    p = ggplot(data = data_use, aes(x = date, y = cases)) + 
      geom_point() + 
      labs(x = 'Date',
           y = 'Cases',
           title = paste('Cases in',input$sel_cty, 'from', 
                         format(input$date_end - 14, '%d-%b'), 'to',
                         format(input$date_end, '%d-%b'))) + 
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
    
    # if(class(estR0) == "try-error" | any(data_use$cases < 10)) {
    #   a$text = "R0 not estimated (bad case values or date range)"
    #   a$font = list(size = 14)
    # } else {
    #   if(input$R_method == "SB") {
    #     R_est = signif(tail(estR0$estimates[[input$R_method]]$R, 1), 3)
    #     R_low = signif(tail(estR0$estimates[[input$R_method]]$conf.int[1], 1), 3)
    #     R_high = signif(tail(estR0$estimates[[input$R_method]]$conf.int[2], 1), 3)
    #   } else {
    #     R_est = signif(estR0$estimates[[input$R_method]]$R, 3)
    #     R_low = signif(estR0$estimates[[input$R_method]]$conf.int[1], 3)
    #     R_high = signif(estR0$estimates[[input$R_method]]$conf.int[2], 3)
    #   }
    
    #if(nrow(estR0) == 0 | any(data_use$cases < 10)) {
    if(nrow(estR0) == 0) {
      a$text = "R0 not estimated (bad case values or date range)"
      a$font = list(size = 14)
    } else {
      #if(input$R_method == "SB") {
      R_est = signif(estR0$pred, 3)
      R_low = signif(estR0$low, 3)
      R_high = signif(estR0$upp, 3)
      #} else {
      
      a$text = paste0("Estimated R = ", R_est,
                      ",  10-90 Quantile Interval: (", R_low,', ',
                      R_high, ')')
    }
    ggplotly(p) %>% layout(annotations = a)  
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)


