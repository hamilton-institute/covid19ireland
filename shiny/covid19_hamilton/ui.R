#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(leaflet)

body <- dashboardBody(
  fluidRow(
    box(
      title = "Box title", width = 6, status = "primary",
      "Box content"
    ),
    box(
      status = "warning", width = 6,
      "Box content"
    )
  ),
    
  fluidRow(
    column(width = 4,
      box(
        title = "Title 1", width = NULL, solidHeader = TRUE, status = "primary",
        "Box content"
      ),
      box(
        width = NULL, background = "black",
        "A box with a solid black background"
      )
    ),

    column(width = 4,
      box(
        title = "Title 3", width = NULL, solidHeader = TRUE, status = "warning",
        "Box content"
      ),
      box(
        title = "Title 5", width = NULL, background = "light-blue",
        "A box with a solid light-blue background"
      )
    ),

    column(width = 4,
      box(
        title = "Title 2", width = NULL, solidHeader = TRUE,
        "Box content"
      ),
      box(
        title = "Title 6", width = NULL, background = "maroon",
        "A box with a solid maroon background"
      )
    )
  )
)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        # Application title
        titlePanel("Covid-19 visualisation tool"),
        
        tabsetPanel(
            tabPanel("Map", fluid = TRUE,
                        #mainPanel(
                            fluidRow(
                                column(2,
                                    textOutput("tot_cases")
                                ),
                                column(6,  
                                    leafletOutput("covidMap")
                                ),
                                column(4,
                                    fluidRow(
                                        column(6,
                                            textOutput("tot_deaths")
                                        ),
                                        column(6,
                                            textOutput('tot_recovered')
                                        )
                                    ),
                                    fluidRow(
                                        column(12,
                                            plotlyOutput("mapPlot")
                                        )
                                    )
                                )
                            )
                        #)
                   
            ), #End Map tabsetPanel
            tabPanel("Plot", fluid = TRUE,
                sidebarLayout(
                    sidebarPanel(textInput("co", "Choose country", "Ireland")),
                        mainPanel(
                            fluidRow(
                                column(8,  
                                    plotlyOutput("covidPlot")
                                )
                            )
                        )
                ) #End Plot tabPanel
            )
        ) #End tabsetPanel
    )
)