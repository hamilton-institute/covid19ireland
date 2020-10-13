rm(list = ls(all = TRUE))

source("twoagesR.R")

library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(scales)
library(reshape2)
library(shinyWidgets)
library(plotly)
library(shinycssloaders)

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(width=12,
               
               setSliderColor(c(rep("#b2df8a", 3)), sliderId=c(8,9,10)),
               # Input: Selector for choosing dataset ----
               
               sliderInput("R0", "Average number of infections from each infected person (R0) for under 60s", 0, 10, 3.6, step=0.1),
               
               sliderInput("R0_1", "Average number of infections from each infected person (R0) for over 60s", 0, 10, 0.8, step=0.1),
               
               sliderInput(inputId = "R0_O_Y",
                           label = "Average number of infections passed between under and over 60s per infected person (Cross R0)",
                           0, 10, 0.3, step=0.1),
               
               #numericInput("pop","Number of susceptible under 60s",value = 4.0E6),
               
               #numericInput("pop2","Number of susceptible over 60s",value = 0.9E6),
               
               numericInput(inputId = "exp",
                            label = "Current number of asymptomatic spreaders under 60",
                            value = 2000),
               
               numericInput(inputId = "inf",
                            label = "Current number of symptomatic spreaders under 60",
                            value = 2000),
               
               numericInput(inputId = "exp2",
                            label = "Current number of asymptomatic spreaders over 60",
                            value = 200),
               
               numericInput(inputId = "inf2",
                            label = "Current number of symptomatic spreaders over 60",
                            value = 200),

               numericInput(inputId = "rec",
                            label = "Initial number of recovered (i.e. immune) people under 60",
                            value = 200000),

               numericInput(inputId = "rec2",
                            label = "Initial number of recovered (i.e. immune) people over 60",
                            value = 100000)
               
               
        ))),
    

    # Main panel for displaying outputs ----
    mainPanel(
      # navbarPage("Output:",
      #            # Output: HTML table with requested number of observations ----
      #            tabPanel("Spread",  
                          fluidPage(
                            fluidRow(
                              plotlyOutput("plot") %>% withSpinner(color="#1E90FF"),
                            )
                          )
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

  #realisation <- reactive({
  output$plot <- renderPlotly({
    
    ##### General setup
    # Inputs are YS, YE, YI, YR, OS, OE, OI, OR, YR0Y, YR0O, OR0Y, OR0O
    
    # Number of simulations
    num_sim = 200
    store = vector('list', 200)
    for (i in 1:num_sim) {
      store[[i]] = twoages(YS = 4.0e6, # Under 60s susceptible
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
    YI_median = apply(YI_padded, 1, 'quantile', 0.5)
    YI_high = apply(YI_padded, 1, 'quantile', 0.95)
    YI_low = apply(YI_padded, 1, 'quantile', 0.05)
    
    # Final data frame for YI
    YI_final = tibble(Time = time_max, 
                          `Under 60sXXXInfected - median` = YI_median,
                          `Under 60sXXXInfected - low est` = YI_low,
                          `Under 60sXXXInfected - high est` = YI_high)
    
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
    OI_median = apply(OI_padded, 1, 'quantile', 0.5)
    OI_high = apply(OI_padded, 1, 'quantile', 0.95)
    OI_low = apply(OI_padded, 1, 'quantile', 0.05)
    
    # Final data frame for OI
    OI_final = tibble(Time = time_max, 
                          `Over 60sXXXInfected - median` = OI_median,
                          `Over 60sXXXInfected - low est` = OI_low,
                          `Over 60sXXXInfected - high est` = OI_high)
    
    # Tidy up into one data frame
    final = left_join(YI_final, OI_final, by = "Time") %>% 
      pivot_longer(names_to = 'Type', values_to = 'Count', -Time) %>% 
      tidyr::separate(Type, c("Age group", "Type"), sep = "XXX") %>% 
      pivot_wider(names_from = "Type", values_from = "Count")

    plt1 = ggplot(final, aes(x = Time, y = `Infected - median`, fill = `Age group`, colour = `Age group`)) +
      geom_ribbon(aes(ymin = `Infected - low est`, ymax = `Infected - high est`), alpha = 0.1) +
      geom_line() +
      labs(x = "Day", y = "Number of infected people per day") +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0), labels = comma) +
      theme(plot.title = element_text(size = 10, face = "bold")) + 
      theme_bw()
    ggplotly(plt1)

  })
  
}

# Run the application
shinyApp(ui = ui, server = server)


