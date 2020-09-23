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

#setwd("/Users/amin/Desktop/PhD/covid-19")

rm(list = ls())

ui <- fluidPage(
  
  titlePanel(""),
  hr(),
  p(div(HTML(""))),
  
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(width=12,
               
               setSliderColor(c(rep("#b2df8a", 3)), sliderId=c(8,9,10)),
               h4(div(HTML("<em>Assumptions</em>"))),
               # Input: Selector for choosing dataset ----
               
               sliderInput("R0", "Mortality rate %", 0.5, 6, 3, step=0.1),
               
               sliderInput("E", "Planes' occupancy before lockdown %", 20, 100, 85 ,step = 5),
               
               sliderInput("D", "Planes' occupancy after lockdown %", 20, 100, 35 ,step = 5)
               
               
          
               
        ))),
    
    
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
                 # Output: HTML table with requested number of observations ----
                 navbarPage("Output:",
                 tabPanel("Plot",  
                          fluidPage(
                            fluidRow(
                              plotOutput("plot")
                              
                              
    
                            )
                          )
                 ),
            
                 tabPanel("Assumptions",  
                          fluidPage(
                            fluidRow(
                              h5(div(HTML("1- <em>Mortality rates are accurate</em>."))),
                              h5(div(HTML("2- <em>Plane occupancy rates are assumed (best guess)</em>."))),
                              h5(div(HTML("3- <em>Mortality rates do not change with time</em>."))),
                              h5(div(HTML("4- <em>Virus carriers boarding are randomly sampled from each country's population</em>."))),
                              h5(div(HTML("5- <em>21 days lag is assumed from infection to death</em>."))),
                              h5(div(HTML("6- <em> All values are means.</em>"))),
                              
                              
                            )
                          )
                 )
                 
              
        
     )
    )
    
    ########################
  ) 
)




server <- function(input, output) {
  
  #merged3 <- read_csv("flightsdata_app.csv") #merged3 dataset in the original code
  merged3 <- read_csv("merged3.csv") #merged3 dataset in the original code
  merged <- read_csv("virusdata_app.csv") #merged dataset in the original code
  
  merged$country <- as.factor(merged$country) 
  
  merged2 <- merged %>% filter(country == "Austria"|  country == "Belgium" | country == "Switzerland" | country == "Czechia"|
                                 country == "Germany" | country == "Denmark" | country == "Spain" | country == "France"|
                                 country == "United Kingdom" | country == "Hungary" | country == "Iceland" | country == "Italy" |
                                 country == "Malta" | country == "Netherlands" | country == "Norway" | country == "Portugal" |
                                 country == "Russia" | country == "Sweden" | country == "Turkey" | country == "United States")
  
  
  
  
  # 
  # realisation <- reactive({
  #   
  #   
  #   
  # })
  
  output$plot <- renderPlot({
    
    merged2 <- merged2 %>% group_by(country) %>% mutate(new_cases = (lead(deaths,21)- deaths)/(input$R0/100)) 
    merged2 <- merged2[,c(3,4,39)]
    merged2$new_cases[is.na(merged2$new_cases)] <- 0
    names(merged2)[2] <- "day"
    
    ####Adding covid-19 data to flights data:
    merged4 <- left_join(merged3, merged2)
    merged4$new_cases[is.na(merged4$new_cases)] <- 0
    merged4$population <- as.numeric(merged4$population)
    merged4 <- merged4 %>% mutate(ratio = new_cases/population)
    merged4 <- merged4 %>% mutate(carriers = capacity*ratio)
    merged4$carriers[which(merged4$day < "2020-04-01")] <- merged4$carriers[which(merged4$day < "2020-04-01")] *input$E*0.01
    merged4$carriers[which(merged4$day >= "2020-04-01")] <- merged4$carriers[which(merged4$day >= "2020-04-01")] *input$D*0.01
    
     merged4 <- na.omit(merged4)
    
    x <- merged4 %>% group_by(country) %>% summarise(carriers = sum(carriers))
    x <- arrange(x,desc(carriers))
    
    ggplot(x, aes(reorder(country, -carriers),carriers, fill = "red")) + geom_col(show.legend = FALSE) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = NULL, y = "Number of infected") +
      ggtitle("Number of infected people coming to Ireland")
    
    
    merged4$country[which(merged4$country != as.character(x[1,1]) & merged4$country != as.character(x[2,1]) &  merged4$country != as.character(x[3,1]) & merged4$country != as.character(x[4,1]))] <- "Others"
    
    
    # ggplot() + 
    #   geom_col(data = merged4, aes(day, carriers, fill = country)) + 
    #   labs(y = "Virus carriers per day", fill = "Country") +
    #   theme_bw()
    
    
    ##### Weekly report:
    y <- merged4 %>% 
      group_by(day = floor_date(day, "week"), country) %>% 
      summarise(carriers = sum(carriers))
    
    ggplot() + 
      geom_col(data = y, aes(day, carriers, fill = country)) + 
      labs(x = NULL ,y = "Virus carriers per week", fill = "Country") +
      ggtitle("Estimated COVID-19 cases per week imported to Ireland") +
      scale_x_date(breaks = "2 weeks", date_labels = "%d-%b") +
      #scale_x_date(date_breaks = "2 week") +
      theme_bw() +
      theme(axis.title=element_text(size=15,face="bold"),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 12)) 
    
   
  })
  
  output$plot2 <- renderPlot({
    
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

