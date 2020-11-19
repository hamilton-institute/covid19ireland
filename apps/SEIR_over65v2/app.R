
rm(list = ls(all = TRUE))

source("twoagesR2.R")

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
diff3 = function(x) return(c(diff(x), 0))

pad_fun = function(x) {
  # Take a ragged list and pad it to matrix with zeros
  # Find out which is the longest bit of X
  nrows = lapply(x, 'nrow') %>% unlist
  max_row = max(nrows)
  size_max = nrow(x[[which.max(nrows)]])
  
  x_padded = matrix(NA, ncol = length(x), nrow = size_max)
  for(i in 1:length(x)) {
    x_padded[,i] = c(diff3(x[[i]][,1]), rep(0, max_row - nrows[i]))
  }
  return(x_padded)
}

# Function extract ane element from a list and tidy it up
grab_all = function(lst, el, name) {
  vals = lapply(lst, "[", el)
  
  # Add 0s to each vector to make them the same length
  val_padded = pad_fun(vals)
  
  # Now calculate medians and 90% CI
  val_median = (apply(val_padded, 1, 'quantile', 0.5))
  #val_high = (apply(val_padded, 1, 'quantile', 0.95))
  #val_low = (apply(val_padded, 1, 'quantile', 0.05))
  
  # Final data frame for YR
  nrows = lapply(lst, 'nrow') %>% unlist
  time_max = lst[[which.max(nrows)]]$Time
  dates = as.Date("2021-01-01") + time_max 
  val_final = tibble(Date = dates, 
                    `Median` = val_median)
                    # `Low5` = val_low,
                    # `High95` = val_high)
  names(val_final) = c('Date', name)
                       # paste(name,'- Median'),
                       # paste(name, '- Low'),
                       # paste(name, 'High'))
  return(val_final)
}

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      useShinyjs(),
      fluidRow(
        column(width=12,
               
               setSliderColor(c(rep("#b2df8a", 3)), sliderId=c(8,9,10)),
               # Input: Selector for choosing dataset ----
               
               sliderInput("vacc_Y", "Daily number of vaccinations for under 65s", 0, 50000, 2000, step = 100),
               
               sliderInput("vacc_O", "Daily number of vaccinations for over 65s", 0, 50000, 18000, step = 100),
               
               sliderInput("vacc_ef", "Vaccine effectiveness (%)", 50, 100, 90, step = 1),
               sliderInput("vacc_re", "Vaccine refusal rate (%)", 0, 100, 30, step = 1),
               # https://www.irishpost.com/news/almost-a-third-of-irish-people-would-refuse-covid-19-vaccine-survey-says-194257
               
               sliderInput("R0_Y", "Average number of infections from each infected person (R number) for under 65s", 0, 10, 0.8, step=0.1),
               
               sliderInput("R0_O", "Average number of infections from each infected person (R number) for over 65s", 0, 10, 0.8, step=0.1),
               
               sliderInput(inputId = "R0_O_Y",
                           label = "Average number of infections passed between under and over 65s per infected person (Cross R number)",
                           0, 10, 0.3, step=0.1),
               
               actionButton(inputId = "button", label = "Show/hide extra options"),
               
               numericInput(inputId = "exp_Y",
                            label = "Number of asymptomatic spreaders under 65 at start date",
                            value = 2000),
               
               numericInput(inputId = "inf_Y",
                            label = "Number of symptomatic spreaders under 65 at start date",
                            value = 2000),
               
               numericInput(inputId = "exp_O",
                            label = "Number of asymptomatic spreaders over 65 at start date",
                            value = 200),
               
               numericInput(inputId = "inf_O",
                            label = "Number of symptomatic spreaders over 65 at start date",
                            value = 200),
               
               numericInput(inputId = "rec_Y",
                            label = "Number of recovered (i.e. immune) people under 65 at start date",
                            value = 200000),
               
               numericInput(inputId = "rec_O",
                            label = "Number of recovered (i.e. immune) people over 65 at start date",
                            value = 100000),
               
               numericInput(inputId = "pop_Y",
                            label = "Population of Ireland under 65",
                            value = 4000000),
               
               numericInput(inputId = "pop_O",
                            label = "Population of Ireland over 65",
                            value = 900000),
               
               numericInput(inputId = "num_sim",
                            label = "Number of simulations to run (higher = slower but more accurate)",
                            value = 200),
               
        ))),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      navbarPage("COVID-19 Vaccination Planning:",
                 # Output: HTML table with requested number of observations ----
                 tabPanel("Spread",
                          fluidPage(
                            fluidRow(
                              plotlyOutput("plot", height = 500) %>% withSpinner(color="#1E90FF"),
                            )
                          ),
                          checkboxInput("log_scale", "Log scale?", value = FALSE)
                          
                 ),
                 
                 
                 tabPanel("Assumptions",
                          fluidPage(
                            fluidRow(
                              
                              p(HTML("<p> This visualisation shows the standard model of an epidemic where an individuals at any point in time can be in one of four states: S, susceptible to infection; E, exposed to the infection, and so infectious, but asymptomatic; I, infectious and symptomatic; R, recovered from the infection and immune to further infection. It is known as an SEIR model.<p> Exposed and Infectious people are the main actors in the system. They interact a random number of times each day with Susceptible, Exposed, Infectious, and Recovered people. The probability that a given interaction is with a Susceptible person is the fraction of people in the population that are Susceptible at that time. When they interact with a Susceptible person, the Susceptible person moves to being Exposed. An interaction with an Exposed, Infectious or Recovered person leads to no change in the system. We have extended this model to allow for two populations (here represented as under 65 or over 65) which can mix together at a set rate, which we call the 'Cross R number'.<p> Exposed people stay in that state for a random amount of time, with an average given by the model parameters, whereupon they become Infectious. Infectious people stay in that state for a random amount of time, with an average given by the model parameters, whereupon they become Recovered. Once there are no Exposed or Infectious people left, the epidemic has ended.<p> As the system is stochastic, significant variability occurs when the number of Exposed and Infectious people is small. When started with a small number of Exposed and Infectious people, there is a chance that the epidemic dies out before it can get going, or that it expands into a full-blown epidemic. Towards the end of a full blown epidemic, there is significant heterogeneity in the time until it ends. The closer the effective replicative value is to 1, the greater this variability. We have suppressed this variability in these plots, but they are available in some of other apps.<p> The vaccination aspect of the model is induced by instantly moving a set number of people from from state S to state R. This continues on a daily basis until there are no more people in the S category.<p> The forecasts produced by this system are inherently unrealistic. By creating such a prediction and presenting it to you makes this forecast less likely to happen. The government are likely to act, or people will react by themselves if there are large numbers of deaths.<p> The code presented here has been written by academics and not by professional coders. It may contain bugs or other mistakes which we have not disovered yet. All the code for this app is available in our <a href = 'https://github.com/hamilton-institute/covid19ireland'>GitHub</a> repository which we encourage you to look at and improve."))
                              
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
    shinyjs::toggle("exp_Y")
    shinyjs::toggle("exp_O")
    shinyjs::toggle("inf_Y")
    shinyjs::toggle("inf_O")
    shinyjs::toggle("rec_Y")
    shinyjs::toggle("rec_O")
    shinyjs::toggle("pop_Y")
    shinyjs::toggle("pop_O")
    shinyjs::toggle("num_sim")
  }, ignoreNULL = FALSE)
  
  
  #realisation <- reactive({
  output$plot <- renderPlotly({
    ##### General setup
    
    # Inputs are YSU, YSNV, YE, YI, YR, OSU, OSNV, OE, OI, OR, YR0Y, YR0O, OR0Y, OR0O, Yvac, Ovac, Veff
    # where YSU = Young Susceptible Not Yet Vaccinated
    # YSNV = Young Susceptible Refused Vaccine
    # etc
    
    # Outputs are Time YSU YSV YSVNE YSNV YE YI YR YRV OSU OSV OSVNE OSNV OE OI OR ORV
    # These mean
    # YSV = Young Susceptible And Vaccinated - waiting for it to become effective
    # YSVNE = Young Susceptible but Vaccine Not Effective
    # YSNV = Young Susceptible But Refused Vaccine
    # YR = Young Recovered due to having the disease
    # YVR = Young Recovered due to having vaccination
    
    # Number of simulations
    num_sim = input$num_sim
    store = vector('list', num_sim)
    for (i in 1:num_sim) {
      start_S_Y = input$pop_Y - 
        input$exp_Y - 
        input$inf_Y - 
        input$rec_Y # Total people in S = pop - E - I - R
      start_S_O = input$pop_O - 
        input$exp_O - 
        input$inf_O - 
        input$rec_O
      store[[i]] = twoagesv2(
        YSU = start_S_Y * (1-input$vacc_re/100),# Young Susceptible Not Yet Vaccinated - calculated as the proportion of people who are willing to be vaccinated
        YSNV = start_S_Y*input$vacc_re/100, # Young Susceptible Refused Vaccine - this is all the people who refused to be vaccinated, 
        YE = input$exp_Y, 
        YI = input$inf_Y, 
        YR = input$rec_O, 
        OSU = start_S_O * (1-input$vacc_re/100), 
        OSNV = start_S_O*input$vacc_re/100, 
        OE = input$exp_O, 
        OI = input$inf_O, 
        OR = input$rec_O, 
        YR0Y = input$R0_Y, 
        YR0O = input$R0_O_Y, 
        OR0Y = input$R0_O_Y, 
        OR0O = input$R0_O, 
        Yvac = rep(input$vacc_Y, 10000),  
        Ovac = rep(input$vacc_O, 10000), 
        Veff = input$vacc_ef) %>% 
        as.data.frame %>% 
        rename("Time" = 1, "YSU" = 2, "YSV" = 3, 
               "YSVNE" = 4, "YSNV" = 5, "YE" = 6,
               "YI" = 7, "YR" = 8, "YRV" = 9, 
               "OSU" = 10, "OSV" = 11, 
               "OSVNE" = 12, "OSNV" = 13, "OE" = 14,
               "OI" = 15, "OR" = 16, "ORV" = 17)
      #Time YSU YSV YSVNE YSNV YE YI YR YRV OSU OSV OSVNE OSNV OE OI OR ORV
      
      len = length(store[[i]]$Time)
    }
    
    # Quick plot
    # plot(store[[1]]$Time, store[[1]]$ORV, type = 'l') # Vaccinated and recovered
    # lines(store[[1]]$Time, store[[1]]$OR, col = 'red') # Vaccinated after disease
    # plot(store[[1]]$Time, store[[1]]$YRV, type = 'l') # Vaccinated and recovered
    # lines(store[[1]]$Time, store[[1]]$YR, col = 'red') # Vaccinated after disease
    
    # Extract out the infections and quantiles for each group
    YR_final = grab_all(store, "YR", "Under 65s recovered from disease")
    OR_final = grab_all(store, "OR", "Over 65s recovered from disease")
    OR_final = grab_all(store, "OR", "Over 65s recovered from disease")
    YRV_final = grab_all(store, "YRV", "Under 65s successfully vaccinated")
    ORV_final = grab_all(store, "ORV", "Over 65s successfully vaccinated")
    
    # Tidy up into one data frame
    final = left_join(YR_final, OR_final, by = "Date") %>% 
      left_join(YRV_final, by = 'Date') %>% 
      left_join(ORV_final, by = 'Date') %>% 
      pivot_longer(names_to = 'Type', values_to = 'Count', -Date) %>% 
      mutate(Count = round(Count))
    
    # This caused a load of pain but replaced three of the above lines  
    #   tidyr::separate(Type, c("Age group", "Type"), sep = "XXX") %>% 
    plt1 = ggplot(final,
                  aes(x = Date, colour = Type)) +
      geom_line(aes(y = `Count`)) +
      labs(x = "Date", title = "Virus progression with vaccination effectiveness and refusal", y = NULL) +
      scale_x_date(date_labels = "%d-%b-%y") + 
      scale_y_continuous(expand = c(0, 0), labels = comma) +
      theme_bw()
    # theme(axis.title.y = element_text(angle = 0, vjust = 1, hjust=0))
    if(input$log_scale) plt1 = plt1 + scale_y_log10(expand = c(0, 0), labels = comma)
    
    ggplotly(plt1)
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)


