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
library(shinydashboard)

header <- dashboardHeader(
  title = "Hamilton Covid-19 Dashboard",
  titleWidth = 320
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Map", tabName = "map", icon = icon("dashboard")),
    menuItem("Information", icon = icon("th"), tabName = "info"),
    # Input inside of menuSubItem
      menuSubItem(icon = NULL,
        uiOutput("choose_country")
      )
  )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "map",
            fluidRow(
                column(width=4,
                    infoBoxOutput("casesBox"),
                    infoBoxOutput("deathsBox"),
                    infoBoxOutput('recoverBox'),
                    box(
                        title='Cases by County',
                        width=12,
                        tableOutput("countyCasesTable"),
                        style = "height: calc((100vh - 80px)/2); overflow-y: scroll;overflow-x: scroll;"
                    )
                ),
                column(width=7, 
                    box(
                      title = "COVID-19 in Ireland",
                      width=12,
                      leafletOutput('covidMap')
                    ),
                    box(
                        width=12,
                        plotlyOutput('mapPlot')
                    )
                )
            )
            
        ),

        tabItem(tabName = "info",
            fluidRow(
                column(width=12,
                    box(
                        width=12,
                        plotlyOutput("covidPlot")
                    )
                )
            )
        )
    ),
    #These style tags are necessary to cope with the
    #buggy renderInfoBox function
    tags$style("#casesBox {width:300px;}"),
    tags$style("#deathsBox {width:300px;}"),
    tags$style("#recoverBox {width:300px;}"),
    tags$style(type = "text/css", "#covidMap {height: calc((100vh - 80px)/2.5) !important;}")
)

# Put them together into a dashboardPage
dashboardPage(
  header,
  sidebar,
  body,
  skin='red'
)