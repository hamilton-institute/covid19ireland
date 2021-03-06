
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


diff2 = function(x) return(c(NA, diff(x)))
diff3 = function(x) return(c(0, diff(x)))

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      useShinyjs(),
      fluidRow(
        column(width=12,
               
               setSliderColor(c(rep("#b2df8a", 3)), sliderId=c(8,9,10)),
               # Input: Selector for choosing dataset ----
               
               sliderInput("R0", "Average number of infections from each infected person (R0) for the general population", 0, 10, 0.3, step=0.1),
               
               sliderInput("R0_1", "Average number of infections from each infected person (R0) for Covidiots", 0, 10, 3.0, step=0.1),
               
               sliderInput(inputId = "R0_O_Y",
                           label = "Average number of infections passed between general population and Covidiots per infected person (Cross R0)",
                           0, 10, 1.2, step=0.1),
               
               sliderInput("prop_id", "Percentage of population who are Covidiots (%)", 0, 20, 4, step = 0.1),
               
               sliderInput("dead_0", "Case fatality rate (%)", 0, 20, 2, step = 0.1),

               actionButton(inputId = "button", label = "show extra options"),
              
               numericInput(inputId = "exp",
                            label = "Number of asymptomatic spreaders among general population at start date",
                            value = 3000),

               numericInput(inputId = "inf",
                            label = "Number of symptomatic spreaders among general population at start date",
                            value = 3000),
               
               numericInput(inputId = "exp2",
                            label = "Number of asymptomatic spreaders among Covidiots at start date",
                            value = 400),
               
               numericInput(inputId = "inf2",
                            label = "Number of symptomatic spreaders among Covidiots at start date",
                            value = 400),
               
               numericInput(inputId = "rec",
                            label = "Number of recovered (i.e. immune) people at start date",
                            value = 200000),

               numericInput(inputId = "pop",
                            label = "Population of Ireland",
                            value = 4900000),

               numericInput(inputId = "num_sim",
                            label = "Number of simulations to run (higher = slower but more accurate)",
                            value = 200),

        ))),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      navbarPage("COVID-19 Covidiots effect:",
                 # Output: HTML table with requested number of observations ----
                 tabPanel("Spread",
      fluidPage(
        fluidRow(
          plotlyOutput("plot", height = 500) %>% withSpinner(color="#1E90FF"),
        )
      ),
      checkboxInput("log_scale", "Log scale?", value = FALSE),

            
                   ),


                   tabPanel("Assumptions",
                            fluidPage(
                              fluidRow(

                                p(HTML("<p> This visualisation is standard model of an epidemic where an individuals at any point in time can be in one of four states: S, susceptible to infection; E, exposed to the infection, and so infectious, but asymptomatic; I, infectious and symptomatic; R, recovered from the infection and immune to further infection. It is known as an SEIR model<p> Exposed and Infectious people are the main actors in the system. They interact a random number of times each day with Susceptible, Exposed, Infectious, and Recovered people. The probability that a given interaction is with a Susceptible person is the fraction of people in the population that are Susceptible at that time. When they interact with a Susceptible person, the Susceptible person moves to being Exposed. An interaction with an Exposed, Infectious or Recovered person leads to no change in the system. We have extended this model to allow for two populations (here represented as 'general population' or 'Covidiots') which can mix together at a set rate, which we call the 'Cross R0'.<p> Exposed people stay in that state for a random amount of time, with an average given by the model parameters, whereupon they become Infectious. Infectious people stay in that state for a random amount of time, with an average given by the model parameters, whereupon they become Recovered. Once there are no Exposed or Infectious people left, the epidemic has ended.<p> As the system is stochastic, significant variability occurs when the number of Exposed and Infectious people is small. When started with a small number of Exposed and Infectious people, there is a chance that the epidemic dies out before it can get going, or that it expands into a full-blown epidemic. Towards the end of a full blown epidemic, there is significant heterogeneity in the time until it ends. The closer the effective replicative value is to 1, the greater this variability. We have suppressed this variability in these plots, but they are available in some of other apps.<p> The forecasts produced by this system are inherently unrealistic. By creating such a prediction and presenting it to you makes this forecast less likely to happen. The government are likely to act, or people will react by themselves if there are large numbers of deaths.<p> The code presented here has been written by academics and not by professional coders. It may contain bugs or other mistakes which we have not disovered yet. All the code for this app is available in our <a href = 'https://github.com/hamilton-institute/covid19ireland'>GitHub</a> repository which we encourage you to look at and improve."))

                              )
                            )
                   )
        )
    )
    
    
    ########################
  ) 
)




server <- function(input, output) {
  
  start_date = reactive({
    as.Date(Sys.time())
  })

  observeEvent(input$button, {
    shinyjs::toggle("exp")
    shinyjs::toggle("exp2")
    shinyjs::toggle("inf")
    shinyjs::toggle("inf2")
    shinyjs::toggle("rec")
    shinyjs::toggle("pop")
    shinyjs::toggle("num_sim")
  }, ignoreNULL = FALSE)

  
  #realisation <- reactive({
  output$plot <- renderPlotly({
    ##### General setup
    # Inputs are YS, YE, YI, YR, OS, OE, OI, OR, YR0Y, YR0O, OR0Y, OR0O
    
    # Number of simulations
    num_sim = input$num_sim
    store = vector('list', num_sim)
    covidiot_pop = input$pop * input$prop_id/100
    covidiot_rec = input$rec * input$prop_id/100
    for (i in 1:num_sim) {
      store[[i]] = twoages(YS = input$pop - covidiot_pop, # Non-Covidiot population
                           YE = input$exp,
                           YI = input$inf,
                           YR = input$rec - covidiot_rec, # Assume the same propotion
                           OS = covidiot_pop, # Covidiot population
                           OE = input$exp2,
                           OI = input$inf2,
                           OR = covidiot_rec,
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
    YR_all = lapply(store, "[", "YR")
    
    # Add 0s to each vector to make them the same length
    nrows = lapply(YR_all, 'nrow') %>% unlist
    max_row = max(nrows)
    time_max = store[[which.max(nrows)]]$Time
    YR_padded = matrix(NA, ncol = num_sim, nrow = length(time_max))
    for(i in 1:length(YR_all)) {
      YR_padded[,i] = c(diff3(YR_all[[i]][,1]), rep(0, max_row - nrows[i]))
    }
    
    # Now calculate medians and 90% CI
    YR_median = (apply(YR_padded, 1, 'quantile', 0.5))
    YR_high = (apply(YR_padded, 1, 'quantile', 0.95))
    YR_low = (apply(YR_padded, 1, 'quantile', 0.05))
    
    # Final data frame for YR
    dates = as.Date(Sys.time())+time_max
    YR_final = tibble(Date = dates, 
                      `General popXXXInfected - Value` = YR_median,
                      `General popXXXInfected - low est` = YR_low,
                      `General popXXXInfected - high est` = YR_high,
                      `General popXXXDead - Value` = YR_median*input$dead_0/100,
                      `General popXXXDead - low est` = YR_low*input$dead_0/100,
                      `General popXXXDead - high est` = YR_high*input$dead_0/100)
    
    # Now do the same thing for old infected
    OR_all = lapply(store, "[", "OR")
    
    # Add 0s to each vector to make them the same length
    nrows = lapply(OR_all, 'nrow') %>% unlist
    max_row = max(nrows)
    time_max = store[[which.max(nrows)]]$Time
    OR_padded = matrix(NA, ncol = num_sim, nrow = length(time_max))
    for(i in 1:length(OR_all)) {
      OR_padded[,i] = c(diff3(OR_all[[i]][,1]), rep(0, max_row - nrows[i]))
    }
    
    # Now calculate medians and 90% CI
    OR_median = (apply(OR_padded, 1, 'quantile', 0.5))
    OR_high = (apply(OR_padded, 1, 'quantile', 0.95))
    OR_low = (apply(OR_padded, 1, 'quantile', 0.05))
    
    # Final data frame for OR
    OR_final = tibble(Date = dates, 
                      `CovidiotsXXXInfected - Value` = OR_median,
                      `CovidiotsXXXInfected - low est` = OR_low,
                      `CovidiotsXXXInfected - high est` = OR_high,
                      `CovidiotsXXXDead - Value` = OR_median*input$dead_0/100,
                      `CovidiotsXXXDead - low est` = OR_low*input$dead_0/100,
                      `CovidiotsXXXDead - high est` = OR_high*input$dead_0/100,
                      `TotalXXXDead - Value` = (OR_median + YR_median)*input$dead_0/100)
    
    # Tidy up into one data frame
    final = left_join(YR_final, OR_final, by = "Date") %>% 
      pivot_longer(names_to = 'Type', values_to = 'Count', -Date)
    final_twocols = as.matrix(str_split(final$Type, 'XXX', simplify = TRUE))    
    final$`Group` = final_twocols[,1]
    final$Type = final_twocols[,2]
    final2 = final %>% 
      pivot_wider(names_from = "Type", values_from = "Count") %>% 
      mutate(across(where(is.numeric), round, 0)) %>% 
      pivot_longer(names_to = "Type", values_to = "Count", -c(Date, `Group`))
    final2_twocols = as.matrix(str_split(final2$Type, ' - ', simplify = TRUE))    
    final2$Type = final2_twocols[,1]
    final2$Est = final2_twocols[,2]
    final2 = final2 %>% 
      pivot_wider(names_from = "Est", values_from = "Count")
    final2$Type = factor(final2$Type, levels = c('Infected', 'Dead'), ordered = TRUE)
    
    # This caused a load of pain but replaced three of the above lines  
    #   tidyr::separate(Type, c("Age group", "Type"), sep = "XXX") %>% 
    
    plt1 = ggplot(final2, aes(x = Date, colour = `Group`)) +
      geom_line(aes(y = `Value`)) +
      #geom_ribbon(aes(ymin = `low est`, ymax = `high est`, fill = `Group`), alpha = 0.1) +
      labs(x = "Date", title = "Infected/dead per day", y = NULL) +
      scale_x_date(date_breaks = "4 weeks", date_labels = "%d-%b") + 
      scale_y_continuous(expand = c(0, 0), labels = comma) +
      theme_bw() + 
      facet_wrap(~ Type , nrow = 2, scales = 'free_y')
      # theme(axis.title.y = element_text(angle = 0, vjust = 1, hjust=0))
    if(input$log_scale) plt1 = plt1 + scale_y_log10(expand = c(0, 0), labels = comma)
    
    ggplotly(plt1)
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)


