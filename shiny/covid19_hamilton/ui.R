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
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("cyborg"),

    # Application title
    titlePanel("Covid-19 visualisation tool"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput("co", "Choose country", "Ireland")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("covidPlot")
        )
    )
))
