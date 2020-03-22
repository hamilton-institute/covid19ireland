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

        fluidRow(
            column(2,
                  textInput("co", "Choose country", "Ireland")
             ),
             
            column(6,
                leafletOutput("covidMap")
            ),

            #Show a plot of the generated distribution
            column(4,
                    plotlyOutput("covidPlot")
            )
        )
    )
)
