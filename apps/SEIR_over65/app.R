
rm(list = ls(all = TRUE))

source("twoagesR.R")

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(cowplot)
library(scales)
library(reshape2)
library(shinyWidgets)
library(plotly)
library(shinycssloaders)
library(shinyjs)

# Load in latest data
latest = read.csv("https://opendata.arcgis.com/datasets/d8eb52d56273413b84b0187a4e9117be_0.csv")

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      useShinyjs(),
      fluidRow(
        column(width=12,
               
               setSliderColor(c(rep("#b2df8a", 3)), sliderId=c(8,9,10)),
               # Input: Selector for choosing dataset ----
               
               sliderInput("R0", "Average number of infections from each infected person (R0) for under 65s", 0, 10, 1.5, step=0.1),
               
               sliderInput("R0_1", "Average number of infections from each infected person (R0) for over 65s", 0, 10, 0.8, step=0.1),
               
               sliderInput(inputId = "R0_O_Y",
                           label = "Average number of infections passed between under and over 65s per infected person (Cross R0)",
                           0, 10, 0.3, step=0.1),
               
               sliderInput("dead_0", "Case fatality rate (%) for under 65s", 0, 20, 0.5, step = 0.1),
               
               sliderInput("dead_1", "Case fatality rate (%) for over 65s", 0, 20, 10, step = 0.1),
               
               actionButton(inputId = "button", label = "show extra options"),
               
               #numericInput("pop","Number of susceptible under 65s",value = 4.0E6),
               
               #numericInput("pop2","Number of susceptible over 65s",value = 0.9E6),
               
               numericInput(inputId = "exp",
                            label = "Current number of asymptomatic spreaders under 65",
                            value = 250),
               
               numericInput(inputId = "inf",
                            label = "Current number of symptomatic spreaders under 65",
                            value = 250),
               
               numericInput(inputId = "exp2",
                            label = "Current number of asymptomatic spreaders over 65",
                            value = 25),
               
               numericInput(inputId = "inf2",
                            label = "Current number of symptomatic spreaders over 65",
                            value = 25),
               
               numericInput(inputId = "rec",
                            label = "Initial number of recovered (i.e. immune) people under 65",
                            value = 200000),
               
               numericInput(inputId = "rec2",
                            label = "Initial number of recovered (i.e. immune) people over 65",
                            value = 100000),

               sliderInput("dead_shift", "Gap (days) between cases and deaths", 0, 50, 21, step = 1)

        ))),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      # navbarPage("Output:",
      #            # Output: HTML table with requested number of observations ----
      #            tabPanel("Spread",  
      fluidPage(
        fluidRow(
          plotlyOutput("plot", height = 500,) %>% withSpinner(color="#1E90FF"),
        )
      ),
      checkboxInput("log_scale", "Log scale?", value = FALSE),
      checkboxInput("show_data", "Show data?", value = FALSE),
      
            
      #              ),
      #              
      #              
      #              tabPanel("About",
      #                       fluidPage(
      #                         fluidRow(
      #                           
      #                           p(HTML("<p> This visualisation is for a simple, standard stochastic model of an epidemic where individuals can be in one of four states: S, susceptible to infection; E, exposed to the infection, and so infectious, but asymptomatic; I, infectious and symptomatic; R, recovered from the infection and immune to further infection.<p> Exposed and Infectious people are the actors in the system. They interact a random number of times each day with Susceptible, Exposed, Infectious, and Recovered people. The probability that a given interaction is with Susceptible person is the fraction of people in the population that are Susceptible at that time. When they interact with a Susceptible person, the Susceptible person moves to being Exposed. An interaction with an Exposed, Infectious or Recovered person leads to no change in the system.<p> Exposed people stay in that state for an approximately exponentially random time, with an average given by the model parameters, whereupon they become Infectious. Infectious people stay in that state for an approximately exponentially random time, with an average given by the model parameters, whereupon they become Recovered. Once there are no Exposed or Infectious people left, the epidemic has ended.<p> As the system is stochastic, significant heterogeneity occurs when the number of Exposed and Infectious people is small. When started with a small number of Exposed and Infectious people, there is a chance that the epidemic dies out before it can get going, or that it expands into a full-blown epidemic. Towards the end of a full blown epidemic, there is significant heterogeneity in the time until it ends. The closer the effective replicative value is to 1, the greater this variability.")) 
      #                           
      #                         )
      #                       )
      #              )
      #   )  
    )
    
    
    ########################
  ) 
)




server <- function(input, output) {
  
  observeEvent(input$button, {
    shinyjs::toggle("exp")
    shinyjs::toggle("exp2")
    shinyjs::toggle("inf")
    shinyjs::toggle("inf2")
    shinyjs::toggle("rec")
    shinyjs::toggle("rec2")
    shinyjs::toggle("dead_shift")
  }, ignoreNULL = FALSE)
  
  #realisation <- reactive({
  output$plot <- renderPlotly({
  
    # Get the most recent data according to the dead shift value
    dead_shift = input$dead_shift
    start_date = as.Date(Sys.time()) - input$dead_shift
    diff2 = function(x) return(c(NA, diff(x)))
    data_use = latest %>% 
      mutate(Date = as.Date(Date)) %>% 
      select(Date, TotalConfirmedCovidCases, Aged1, 
             Aged1to4, Aged5to14, Aged15to24, Aged25to34, Aged35to44, 
             Aged45to54, Aged55to64, Aged65up) %>% 
      mutate(`Under 65s` = Aged1+ 
               Aged1to4+ Aged5to14+ Aged15to24+ Aged25to34+ Aged35to44+ 
               Aged45to54+ Aged55to64,
             `Over 65s` = Aged65up) %>% 
      select(Date, `Under 65s`, `Over 65s`) %>% 
      mutate_if(is.numeric, diff2) %>% 
      filter(Date >= start_date) %>% 
      pivot_longer(names_to = 'Age group', values_to = 'median', -Date)

    ##### General setup
    # Inputs are YS, YE, YI, YR, OS, OE, OI, OR, YR0Y, YR0O, OR0Y, OR0O
    
    # Number of simulations
    num_sim = 200
    store = vector('list', 200)
    for (i in 1:num_sim) {
      store[[i]] = twoages(YS = 4.0e6, # Under 65s susceptible
                           YE = input$exp,
                           YI = input$inf,
                           YR = input$rec,
                           OS = 0.9e6,
                           OE = input$exp2,
                           OI = input$inf2,
                           OR = input$rec2,
                           YR0Y = input$R0,
                           YR0O = input$R0_O_Y,
                           OR0Y = input$R0_O_Y,
                           OR0O = input$R0_1) %>% 
        as.data.frame %>% 
        rename("Time" = 1, "YS" = 2,"YE" = 3,
               "YI" = 4, "YR" = 5, "OS" = 6,
               "OE" = 7, "OI" = 8, "OR" = 9)
    }
    
    
    # Quick plot
    # plot(result$Time, result$YI, type = 'l')
    # lines(result$Time, result$OI, col = 'red')
    
    # Extract out the infections and quantiles for each group
    YI_all = lapply(store, "[", "YI")
    
    # Add 0s to each vector to make them the same length
    nrows = lapply(YI_all, 'nrow') %>% unlist
    max_row = max(nrows)
    time_max = store[[which.max(nrows)]]$Time
    YI_padded = matrix(NA, ncol = num_sim, nrow = length(time_max))
    for(i in 1:length(YI_all)) {
      YI_padded[,i] = c(YI_all[[i]][,1], rep(0, max_row - nrows[i]))
    }
    
    # Now calculate medians and 90% CI
    YI_median = (apply(YI_padded, 1, 'quantile', 0.5))
    YI_high = (apply(YI_padded, 1, 'quantile', 0.95))
    YI_low = (apply(YI_padded, 1, 'quantile', 0.05))
    
    # Final data frame for YI
    dead_shift = input$dead_shift # Gap between cases and deaths
    dates = as.Date(Sys.time())+time_max - dead_shift # Start from 3 weeks ago
    YI_final = tibble(Date = dates, 
                      `Under 65sXXXInfected - median` = YI_median,
                      `Under 65sXXXInfected - low est` = YI_low,
                      `Under 65sXXXInfected - high est` = YI_high,
                      `Under 65sXXXDead - median` = c(rep(0, dead_shift), head(YI_median, -dead_shift)*input$dead_0/100),
                      `Under 65sXXXDead - low est` = c(rep(0, dead_shift), head(YI_low, -dead_shift)*input$dead_0/100),
                      `Under 65sXXXDead - high est` = c(rep(0, dead_shift), head(YI_high, -dead_shift)*input$dead_0/100))
    
    # Now do the same thing for old infected
    OI_all = lapply(store, "[", "OI")
    
    # Add 0s to each vector to make them the same length
    nrows = lapply(OI_all, 'nrow') %>% unlist
    max_row = max(nrows)
    time_max = store[[which.max(nrows)]]$Time
    OI_padded = matrix(NA, ncol = num_sim, nrow = length(time_max))
    for(i in 1:length(OI_all)) {
      OI_padded[,i] = c(OI_all[[i]][,1], rep(0, max_row - nrows[i]))
    }
    
    # Now calculate medians and 90% CI
    OI_median = (apply(OI_padded, 1, 'quantile', 0.5))
    OI_high = (apply(OI_padded, 1, 'quantile', 0.95))
    OI_low = (apply(OI_padded, 1, 'quantile', 0.05))
    
    # Final data frame for OI
    OI_final = tibble(Date = dates, 
                      `Over 65sXXXInfected - median` = OI_median,
                      `Over 65sXXXInfected - low est` = OI_low,
                      `Over 65sXXXInfected - high est` = OI_high,
                      `Over 65sXXXDead - median` = c(rep(0, dead_shift), head(OI_median, -dead_shift)*input$dead_1/100),
                      `Over 65sXXXDead - low est` = c(rep(0, dead_shift), head(OI_low, -dead_shift)*input$dead_1/100),
                      `Over 65sXXXDead - high est` = c(rep(0, dead_shift), head(OI_high, -dead_shift)*input$dead_1/100))
    
    # Tidy up into one data frame
    final = left_join(YI_final, OI_final, by = "Date") %>% 
      pivot_longer(names_to = 'Type', values_to = 'Count', -Date)
    final_twocols = as.matrix(str_split(final$Type, 'XXX', simplify = TRUE))    
    final$`Age group` = final_twocols[,1]
    final$Type = final_twocols[,2]
    final2 = final %>% 
      pivot_wider(names_from = "Type", values_from = "Count") %>% 
      mutate(across(where(is.numeric), round, 0)) %>% 
      pivot_longer(names_to = "Type", values_to = "Count", -c(Date, `Age group`))
    final2_twocols = as.matrix(str_split(final2$Type, ' - ', simplify = TRUE))    
    final2$Type = final2_twocols[,1]
    final2$Est = final2_twocols[,2]
    final2 = final2 %>% 
      pivot_wider(names_from = "Est", values_from = "Count")
    final2$Type = factor(final2$Type, levels = c('Infected', 'Dead'), ordered = TRUE)
    
    # This caused a load of pain but replaced three of the above lines  
    #   tidyr::separate(Type, c("Age group", "Type"), sep = "XXX") %>% 
    plt1 = ggplot(final2, aes(x = Date, colour = `Age group`)) +
      geom_line(aes(y = `median`)) +
      #geom_ribbon(aes(ymin = `low est`, ymax = `high est`, fill = `Age group`), alpha = 0.1) +
      labs(x = "Date", title = "Cases/deaths per day", y = NULL) +
      scale_x_date(date_breaks = "4 weeks", date_labels = "%d-%b") + 
      scale_y_continuous(expand = c(0, 0), labels = comma) +
      theme_bw() + 
      facet_grid(Type ~ ., scales = 'free_y')
      # theme(axis.title.y = element_text(angle = 0, vjust = 1, hjust=0))
    if(input$log_scale) plt1 = plt1 + scale_y_log10(expand = c(0, 0), labels = comma)
    
    if(input$show_data) {
      df_use = tibble(
        Date = data_use$Date,
        Type = 'Infected',
        `Age group` = data_use$`Age group`,
        median = data_use$median
      )
      plt1 = plt1 + geom_point(data = df_use, aes(y = median))
    }
    ggplotly(plt1)
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)


