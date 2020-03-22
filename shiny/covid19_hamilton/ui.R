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

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        # Application title
        titlePanel("Covid-19 visualisation tool"),
        
        tabsetPanel(
            tabPanel("Map", fluid = TRUE,
                        mainPanel(
                            leafletOutput("covidMap")
                        )
                   
            ), #End Map tabsetPanel
            tabPanel("Plot", fluid = TRUE,
                sidebarLayout(
                    sidebarPanel(textInput("co", "Choose country", "Ireland")),
                        mainPanel(
                            fluidRow(
                                column(12,  plotlyOutput("covidPlot"))
                            )
                        )
                ) #End Plot tabPanel
            )
        ) #End tabsetPanel
    )
)