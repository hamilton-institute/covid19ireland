library(readxl)
library(dplyr)
library(lubridate)
library(Jmisc)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(cowplot)
library(scales)
library(shiny)
library(shinyWidgets)
library(plotly)
library("survival")
library(shinycssloaders)


source('seir.R')
source('utils.R')
rm(list = ls())

ui <- fluidPage(
  
  titlePanel("SEIR Simulation App"),
  hr(),
  p(div(HTML("Version: 3.0"))),
  
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(width=12,
               
               setSliderColor(c(rep("#b2df8a", 3)), sliderId=c(8,9,10)),
               h4(div(HTML("<em>Initial Conditions</em>"))),
               # Input: Selector for choosing dataset ----
               
               sliderInput("R0", "The average number of people infected before an exposed person recovers at the start of the epidemic", 0, 10, 3.6, step=0.1),
               
               sliderInput("E", "Average time that a newly infected person spends as an asymptomatic spreader before becoming symptomatic", 0, 20, 6.6, step=0.2, post = " days"),
               
               sliderInput("I", "Average time that a symptomatic person spends before recovering", 0, 20, 7.4, step=0.2, post = " days"),
               
               # numericInput(inputId = "R0",
               #              label = "R0 Value",
               #              value = 3.3),
               
               numericInput("pop","Initial number of susceptible people, S compartment",value = 4.9E6),
               
               numericInput(inputId = "exp",
                            label = "Initial number of asymptomatic spreaders, E compartment",
                            value = 0),
               
               numericInput(inputId = "inf",
                            label = "Initial number of symptomatic spreaders, I compartment",
                            value = 1),
               
               numericInput(inputId = "rec",
                            label = "Initial number of recovered (i.e. immune) people, R compartment",
                            value = 0)
               # numericInput(inputId = "E",
               #              label = "Mean holding times at compartment E",
               #              value = 6.6),
               # 
               # numericInput(inputId = "I",
               #              label = "Mean holding times at compartment I",
               #              value = 7.4)
               
        ))),
    
    
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      navbarPage("Output:",
                 # Output: HTML table with requested number of observations ----
                 tabPanel("Spread",  
                          fluidPage(
                            fluidRow(
                              plotOutput("plot") %>% withSpinner(color="#1E90FF"),
                              
                              
                              radioButtons("yscale", "Y axis scale:",
                                           choices = list("Linear" = "linear","Log10" = "log"),inline=TRUE)
                              
                            )
                          )
                 ),
                 
                 tabPanel("Days to epidemic's end",
                          fluidPage(
                            fluidRow(
                              plotOutput("plot2") %>% withSpinner(color="#1E90FF"),
                              uiOutput("Report")    
                              
                              
                            )
                          )
                 ),
                 
                 tabPanel("About",
                          fluidPage(
                            fluidRow(
                              
                              p(HTML("<p> This visualisation is for a simple, standard stochastic model of an epidemic where individuals can be in one of four states: S, susceptible to infection; E, exposed to the infection, and so infectious, but asymptomatic; I, infectious and symptomatic; R, recovered from the infection and immune to further infection.<p> Exposed and Infectious people are the actors in the system. They interact a random number of times each day with Susceptible, Exposed, Infectious, and Recovered people. The probability that a given interaction is with Susceptible person is the fraction of people in the population that are Susceptible at that time. When they interact with a Susceptible person, the Susceptible person moves to being Exposed. An interaction with an Exposed, Infectious or Recovered person leads to no change in the system.<p> Exposed people stay in that state for an approximately exponentially random time, with an average given by the model parameters, whereupon they become Infectious. Infectious people stay in that state for an approximately exponentially random time, with an average given by the model parameters, whereupon they become Recovered. Once there are no Exposed or Infectious people left, the epidemic has ended.<p> As the system is stochastic, significant heterogeneity occurs when the number of Exposed and Infectious people is small. When started with a small number of Exposed and Infectious people, there is a chance that the epidemic dies out before it can get going, or that it expands into a full-blown epidemic. Towards the end of a full blown epidemic, there is significant heterogeneity in the time until it ends. The closer the effective replicative value is to 1, the greater this variability.")) 
                              
                            )
                          )
                 )
      )  
    )
    
    
    ########################
  ) 
)




server <- function(input, output) {
  
  real = 200 # number of simulation
  
  realisation <- reactive({
    
    
    ##### General setup
    compt = c('Susceptible', 
              'Exposed', 
              'Infectious', 
              'Recovered') # SEIR compartment
    N = matrix(0, nrow = 1, ncol = length(compt)) # a matrix to store number of people in each compartment
    
    
    ##### Epidemic model setup: parameters
    N_phases = 1 # number of phases (e.g. intervention at t=t*)
    
    # equal mean holding times for E and I compartments (mean(E+I) = 14days)
    mean_holding_times = c(input$E, input$I)  # mean holding times at compartment E and I
    total_holding_times = sum(mean_holding_times)
    beta = input$R0 / c(total_holding_times, total_holding_times)
    
    ##### Assign initial conditions
    N[1,1] = input$pop  # number of susceptible people (population of ROI)
    N[1,2] = input$exp   # number of exposed people
    N[1,3] = input$inf   # number of infected people
    N[1,4] = input$rec
    # initialise simulation condition
    t = 0
    dt = 1 # time increment in days
    t_phase = Inf
    
    ##### Run simulation
    realisation = list(Time = list(), S = list(), E = list(), I = list(), R = list()) # a list of lists to store results
    for (r in 1:real) {
      for(i in 1:N_phases){
        if (i == 1) { # for first phase of the epimedic
          res = seir_model(t_phase, t, dt, N, mean_holding_times, beta)
        }
        else {
          # TODO: pass results from the first phase with different infection parameters
        }
        
      }
      # rename the column names to appropriate compartments and convert it to a data frame
      res = as.data.frame(res) %>% rename_at(vars(paste0(c(rep("V", length(compt))), 2:5)), ~compt)
      
      # save the results as a list of vectors
      realisation$Time[[r]] = res$Time
      realisation$S[[r]] = res$Susceptible
      realisation$E[[r]] = res$Exposed
      realisation$I[[r]] = res$Infectious
      realisation$R[[r]] = res$Recovered
    }
    
    realisation
    
  })
  
  output$plot <- renderPlot({
    
    # convert to matrix (NB: because each realisation has different length, fill the gap with NAs)
    extractS = sapply(realisation()$S, `length<-`, max(lengths(realisation()$S)))
    extractE = sapply(realisation()$E, `length<-`, max(lengths(realisation()$E)))
    extractI = sapply(realisation()$I, `length<-`, max(lengths(realisation()$I)))
    extractR = sapply(realisation()$R, `length<-`, max(lengths(realisation()$R)))
    
    # calculate mean across rows
    Time = rowMeans(sapply(realisation()$Time, `length<-`, max(lengths(realisation()$S))), na.rm = TRUE)
    meanS = rowMeans(extractS, na.rm = TRUE)
    meanE = rowMeans(extractE, na.rm = TRUE)
    meanI = rowMeans(extractI, na.rm = TRUE)
    meanR = rowMeans(extractR, na.rm = TRUE)
    
    # calculate percentile confidence intervals
    low = 0.025 # lower percentile
    upp = 0.975 # upper percentile
    quantilesS.lower = apply(extractS, 1, quantile, low, na.rm = TRUE)
    quantilesS.upper = apply(extractS, 1, quantile, upp, na.rm = TRUE)
    quantilesE.lower = apply(extractE, 1, quantile, low, na.rm = TRUE)
    quantilesE.upper = apply(extractE, 1, quantile, upp, na.rm = TRUE)
    quantilesI.lower = apply(extractI, 1, quantile, low, na.rm = TRUE)
    quantilesI.upper = apply(extractI, 1, quantile, upp, na.rm = TRUE)
    quantilesR.lower = apply(extractR, 1, quantile, low, na.rm = TRUE)
    quantilesR.upper = apply(extractR, 1, quantile, upp, na.rm = TRUE)
    
    # convert the mean values into a data frame
    final = data.frame(
      Time = Time, 
      Susceptible = meanS, 
      Exposed = meanE, 
      Infectious = meanI, 
      Recovered = meanR
    )
    
    
    plt1 <- ggplot(final, aes(Time)) +
      geom_ribbon(aes(ymin = quantilesS.lower, ymax = quantilesS.upper), alpha = 0.3, fill = "black") +
      geom_ribbon(aes(ymin = quantilesE.lower, ymax = quantilesE.upper), alpha = 0.3, fill = "purple") +
      geom_ribbon(aes(ymin = quantilesI.lower, ymax = quantilesI.upper), alpha = 0.3, fill = "red") +
      geom_ribbon(aes(ymin = quantilesR.lower, ymax = quantilesR.upper), alpha = 0.3, fill = "blue") +
      
      geom_line(aes(y = Susceptible, color = "A_black")) +  # S
      geom_line(aes(y = Exposed, color = "B_purple")) +     # E
      geom_line(aes(y = Infectious, color = "C_red")) +     # I
      geom_line(aes(y = Recovered, color = "D_blue")) +     # R
      
      labs(x = "time [day]", y = "number of people", colour = "") +
      scale_colour_manual(
        name = "Comp.",
        values = c(A_black = "black", B_purple = "purple", C_red = "red", D_blue = "blue"),
        labels = c("S", "E", "I", "R")
      ) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0), labels = scientific_10) + 
      theme(legend.position = c(0.9, 0.5), plot.title = element_text(size = 10, face = "bold"))
    
    plt2 = plt1 + 
      scale_y_log10(limits = c(1E-2, NA), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
      labs(x = "time [day]", y = "number of people (logscale)", colour = "") +
      theme(legend.position = "none")
    
    if(input$yscale == "linear"){
      
      final_plot <- plt1
      
    } else {
      
      final_plot <- plt2
      
    }
    
    final_plot
    
  })
  
        d <- reactive({
    
                          x <- c()
    
                          for(i in 1:real){
      
                          x[i] <- max(unlist(realisation()$Time[i]))
      
                                        }
    
                          as.data.frame(x)
    
                      })
  
  
  
  
  
  output$plot2 <- renderPlot({
    
    ggplot(d(), aes(x)) +
      geom_histogram(aes(x, fill = ..count.., colour = x), show.legend = FALSE) +
      theme_minimal() +
      labs(x = "Days", y = "Number of future scenarios") +
      ggtitle("Possible future scenarios") 
  })
  
  output$Report <- renderUI({
    
    fit = survfit(Surv(d()$x)~1)
    df <- as.data.frame(cbind(fit$time,fit$surv))
    x <- df %>% filter(V2 > 0.49 & V2 < 0.6)
    y <- df %>% filter(V2 < 0.059 & V2 > 0.01)
    x <- max(x$V1)
    y <- min(y$V1) 
    
    
    
    paste0("There is a 50% chance that COVID-19 will be gone by day ", x, " and 95% chance that it will be gone by day ",y,".")
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)


