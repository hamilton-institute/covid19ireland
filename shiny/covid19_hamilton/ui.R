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
    menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
    menuItem("By County", tabName = "county", icon = icon("dashboard")),
    menuItem("Trends", icon = icon("th"), tabName = "trends")
  )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = 'summary',
            fluidRow(
                infoBoxOutput("ireCasesBox"),
                infoBoxOutput("ireDeathsBox"),
                infoBoxOutput('ireRecoverBox')
            
            ),
            fluidRow(
                infoBoxOutput("wCasesBox"),
                infoBoxOutput("wDeathsBox"),
                infoBoxOutput('wRecoverBox')
            
            )
        
        ),
        
        tabItem(tabName = "county",
            fluidRow(
                column(width=3,
                    box(
                        title='Cases by County',
                        width=12,
                        tableOutput("countyCasesTable")
                    )
                ),
                column(width=9, 
                    box(
                      title = "COVID-19 in Ireland",
                      width=12,
                      leafletOutput('covidMap')
                    )
                )
            )
            
        ),

        tabItem(tabName = "trends",
            fluidRow(
                column(width=2, 
                    # Input inside of menuSubItem
                    menuSubItem(icon = NULL,
                        uiOutput("choose_country")
                    )
                ),
                column(width=10,
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
    #tags$style("#ireCasesBox {width:300px;}"),
    #tags$style("#ireDeathsBox {width:300px;}"),
    #tags$style("#ireRecoverBox {width:300px;}"),
    tags$style(type = "text/css", "#covidMap {height: calc((100vh - 80px)/1.0) !important;}")
)

# Put them together into a dashboardPage
dashboardPage(
  header,
  sidebar,
  body,
  skin='red'
)