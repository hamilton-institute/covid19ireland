rm(list = ls(all = TRUE))

source("2b_multi_seir.R")
source("_utils.R")
library(dplyr)
library(ggplot2)
library(cowplot)
library(scales)
library(reshape2)


ui <- fluidPage(
  
  titlePanel("SEIR Simulation App"),
  hr(),
  p(div(HTML("Version: 4.0"))),
  
  
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
  
  


  #realisation <- reactive({
  output$plot <- renderPlot({
    
    
    
    ##### General setup
    
    compt1 = c("S1", "E1", "I1", "R1")  # Ireland SEIR
    compt2 = c("S2", "E2", "I2", "R2")  # UK SEIR
    compt_names = c(compt1, compt2)
    
    N1 = matrix(0, nrow = 1, ncol = length(compt1)) # a matrix to store number of people in each compartment
    N2 = matrix(0, nrow = 1, ncol = length(compt2))
    real = 200 # number of simulation
    
    ##### Epidemic model setup: parameters
    N_phases = 1 # number of phases (e.g. intervention at t=t*)
    gamma1 = c(input$E, input$I)  # mean holding times at compartment E and I 
    total1 = sum(gamma1)  # assuming beta_E = beta_I => include comments
    beta1 = input$R0 / c(total1, total1)
    
    R0_2 = 1.2
    gamma2 = c(6.6, 7.4)
    total2 = sum(gamma2)
    beta2 = R0_2 / c(total2, total2)
    
    
    
    
    # equal mean holding times for E and I compartments (mean(E+I) = 14days)
    ##### Assign initial conditions
    N1[1,1] = input$pop  # number of susceptible people (population of ROI)
    N1[1,2] = input$exp   # number of exposed people
    N1[1,3] = input$inf   # number of infected people
    N1[1,4] = input$rec
    
    N2[1,1] = 6.8E7
    N2[1,2] = 0
    N2[1,3] = 10
    N2[1,4] = 0
    
    t = 0
    dt = 1  # time increment
    t_phase = Inf
   
    
    # https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/638137/Additional_Data_Paper_-_Northern_Ireland_Common_Travel_Area.pdf
    transport_rate = 1.54E7 # 15.4 million people moving between UK and Ireland annually
    wS1E1 = 0.99  # 99% of chance that exposed people will be moved to local E compartment
    wS1E2 = 1 - wS1E1  # (1 - wS1E2)% chance of jumping to the other E compartment
    wS1S2 = transport_rate * (1/365)  # rate of people leaving Ireland per day
    
    wS2E2 = 0.99
    wS2E1 = 1 - wS2E2
    wS2S1 = transport_rate * (1/365)  # rate of people leaving (arriving) UK (Ireland) per day
    
    
    ##### Run simulation
    realisation = list( # a list of lists to store realisation results
      Time = list(), 
      S1 = list(), E1 = list(), I1 = list(), R1 = list(),
      S2 = list(), E2 = list(), I2 = list(), R2 = list()
    ) 
    
    for (r in 1:real) {
      for(i in 1:N_phases){
        if (i == 1) { # for first phase of the epidemic
          res = coupled_seir_model(t_phase, t, dt, N1, gamma1, beta1, N2, gamma2, beta2)
        }
        else {
          # TODO: pass results from the first phase with different infection parameters
        }
      }
      res = as.data.frame(res) %>% rename_at(vars(paste0(c(rep("V", length(compt_names))), 2:9)), ~compt_names)
      
      # save the results as a list of vectors
      realisation$Time[[r]] = res$Time
      for (cname in compt_names) {
        realisation[[cname]][[r]] = res[[cname]]
      }
    }
    
    #realisation
    
 # })
  
  #output$plot <- renderPlot({
    
    Time = rowMeans(sapply(realisation$Time, `length<-`, max(lengths(realisation$S1))), na.rm = TRUE)
    extract = NULL
    mean = NULL
    quantileCIs = NULL
    low = 0.025 # lower percentile
    upp = 0.975 # upper percentile
    for (cname in compt_names) {
      # convert to matrix (NB: because each realisation has different length, fill the gap with NAs)
      extract[[cname]] = sapply(realisation[[cname]], `length<-`, max(lengths(realisation[[cname]])))

      # calculate mean across rows
      mean[[cname]] = rowMeans(extract[[cname]], na.rm = TRUE)

      # calculate percentile confidence intervals
      quantileCIs[[cname]][["lower"]] = apply(extract[[cname]], 1, quantile, low, na.rm = TRUE)
      quantileCIs[[cname]][["upper"]] = apply(extract[[cname]], 1, quantile, upp, na.rm = TRUE)
    }

    # convert the mean values into a data frame
    final = data.frame(
      Time = Time,
      S1 = mean[["S1"]], E1 = mean[["E1"]], I1 = mean[["I1"]], R1 = mean[["R1"]],
      S2 = mean[["S2"]], E2 = mean[["E2"]], I2 = mean[["I2"]], R2 = mean[["R2"]]
    )
    #############################
    ##### Plot number of people in each compartment
    title1 = paste0("#S_IR = ", N1[1,1], ", #E_IR = ", N1[1,2], ", #I_IR = ", N1[1,3], ", R0_IR = ", R0_1,  "\n#S_UK = ", N2[1,1], ", #E_UK = ", N2[1,2], ", #I_UK = ", N2[1,3], ", R0_UK = ", R0_2)
    # title2 = paste0("#S_UK = ", N2[1,1], ", #E_UK = ", N2[1,2], ", #I_UK = ", N2[1,3])
    plt1 = ggplot(final, aes(Time)) +
      geom_ribbon(aes(ymin = quantileCIs$S1$lower, ymax = quantileCIs$S1$upper), alpha = 0.3, fill = "black") +
      geom_ribbon(aes(ymin = quantileCIs$E1$lower, ymax = quantileCIs$E1$upper), alpha = 0.3, fill = "purple") +
      geom_ribbon(aes(ymin = quantileCIs$I1$lower, ymax = quantileCIs$I1$upper), alpha = 0.3, fill = "red") +
      geom_ribbon(aes(ymin = quantileCIs$R1$lower, ymax = quantileCIs$R1$upper), alpha = 0.3, fill = "blue") +

      geom_ribbon(aes(ymin = quantileCIs$S2$lower, ymax = quantileCIs$S2$upper), alpha = 0.1, fill = "black") +
      geom_ribbon(aes(ymin = quantileCIs$E2$lower, ymax = quantileCIs$E2$upper), alpha = 0.1, fill = "purple") +
      geom_ribbon(aes(ymin = quantileCIs$I2$lower, ymax = quantileCIs$I2$upper), alpha = 0.1, fill = "red") +
      geom_ribbon(aes(ymin = quantileCIs$R2$lower, ymax = quantileCIs$R2$upper), alpha = 0.1, fill = "blue") +

      geom_line(aes(y = S1, color = "A_black", linetype = "Ireland")) +  # S1 Ireland
      geom_line(aes(y = E1, color = "B_purple", linetype = "Ireland")) + # E1
      geom_line(aes(y = I1, color = "C_red", linetype = "Ireland")) +    # I1
      geom_line(aes(y = R1, color = "D_blue", linetype = "Ireland")) +   # R1

      geom_line(aes(y = S2, color = "E_black", linetype = "UK")) +  # S2 UK
      geom_line(aes(y = E2, color = "F_purple", linetype = "UK")) + # E2
      geom_line(aes(y = I2, color = "G_red", linetype = "UK")) +    # I2
      geom_line(aes(y = R2, color = "H_blue", linetype = "UK")) +   # R2

      labs(x = "Time [day]", y = "Number of people", title = title1, colour = "") +
      scale_colour_manual(
        name = "Comp.",
        values = c(
          A_black = "black", B_purple = "purple", C_red = "red", D_blue = "blue",
          E_black = "black", F_purple = "purple", G_red = "red", H_blue = "blue"
        ),
        labels = c(
          "S_IR", "E_IR", "I_IR", "R_IR",
          "S_UK", "E_UK", "I_UK", "R_UK"
        )
      ) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0), labels = scientific_10) +
      theme(plot.title = element_text(size = 10, face = "bold"))

    # A second plot in log scale, otherwise identical to plt1
    plt2 = plt1 +
      scale_y_log10(limits = c(1E-2, NA), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
      labs(x = "Time [day]", y = "Number of people (logscale)", title = title1, colour = "") +
      theme(plot.title = element_text(size = 10, face = "bold"))

    plt1

  })
  
  # d <- reactive({
  #   
  #   x <- c()
  #   
  #   for(i in 1:real){
  #     
  #     x[i] <- max(unlist(realisation()$Time[i]))
  #     
  #   }
  #   
  #   as.data.frame(x)
  #   
  # })
  
  
  
  
  
  # output$plot2 <- renderPlot({
  #   
  #   ggplot(d(), aes(x)) +
  #     geom_histogram(aes(x, fill = ..count.., colour = x), show.legend = FALSE) +
  #     theme_minimal() +
  #     labs(x = "Days", y = "Number of future scenarios") +
  #     ggtitle("Possible future scenarios") 
  # })
  
  # output$Report <- renderUI({
  #   
  #   fit = survfit(Surv(d()$x)~1)
  #   df <- as.data.frame(cbind(fit$time,fit$surv))
  #   x <- df %>% filter(V2 > 0.49 & V2 < 0.6)
  #   y <- df %>% filter(V2 < 0.059 & V2 > 0.01)
  #   x <- max(x$V1)
  #   y <- min(y$V1) 
  #   
  #   
  #   
  #   paste0("There is a 50% chance that COVID-19 will be gone by day ", x, " and 95% chance that it will be gone by day ",y,".")
  #   
  # })
  
}

# Run the application
shinyApp(ui = ui, server = server)


