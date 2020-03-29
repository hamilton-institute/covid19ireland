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
library(DT)
library(dashboardthemes)
library(fontawesome)
library(shinyWidgets)

last_update = format(file.info('summary_stats_current.csv')$mtime,
                     "%d-%b-%Y, %H:%M")

header <- dashboardHeader(
  title = paste("Hamilton Covid-19 Dashboard: Updated", last_update),
  titleWidth = 600
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
    menuItem("By County", tabName = "county", icon = icon("map")),
    menuItem("International Trends", icon = icon("chart-line"), tabName = "trends"),
    menuItem("Hospitalisation Stats", tabName = "patientprofile
             ", icon = icon("hospital")),
    checkboxInput("logY", "Show Y-axis log scaled", FALSE)
  )
)

body <- dashboardBody(
  shinyDashboardThemes(
    theme = "grey_dark"
  ),
  tabItems(
    tabItem(tabName = 'summary',
            fluidRow(infoBoxOutput("ireCasesBox"),
                     infoBoxOutput("ireDeathsBox"),
                     infoBoxOutput('ireRecoverBox'),
                     infoBoxOutput("wCasesBox"),
                     infoBoxOutput("wDeathsBox"),
                     infoBoxOutput('wRecoverBox')
            ),
            fluidRow(
              column(width = 4,
                     selectInput("sel_ctry", 
                                 "Select Countries", 
                                 unique(ecdc$countriesAndTerritories),
                                 selected = c('Ireland'),
                                 multiple = TRUE)
              ),
              column(width = 4,
                     selectInput("sel_var", 
                                 "Select variables", 
                                 c('Cumulative cases', 'Cumulative deaths', 
                                   'Daily cases', 'Daily deaths', 
                                   'Log cumulative cases', 'Log cumulative deaths', 
                                   'Cases per million population',
                                   'Deaths per million population'),
                                 selected = c('Cumulative cases', 'Cumulative deaths'),
                                 multiple = TRUE)
              ),
              column(width = 4,
                     selectInput("sel_axis", 
                                 "Choose axis", 
                                 c('Date', 'Days since 1st case', 'Days since 10th case',
                                   'Days since 1st death'),
                                 selected = c('Days since 1st death'),
                                 multiple = FALSE)
              ),
              tabBox(width=12,
                     tabPanel('Interactive plot', plotlyOutput("CountryPlot")),
                     tabPanel('Daily', plotlyOutput('newSumIrelandPlot')),
                     tabPanel('Cumulative Global', plotlyOutput("cumSumWorldPlot")),
                     tabPanel('Daily Global', plotlyOutput('newSumWorldPlot'))
              )
            )
    ),
    tabItem(tabName = "county",
            fluidRow(
                column(width=3,
                    box(
                        title='Cases by County',
                        width=12,
                        DT::dataTableOutput("countyCasesTable")
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
                column(width=4, 
                    # Input inside of menuSubItem
                    menuSubItem(icon = NULL,
                        uiOutput("choose_country")
                    ),
                    DT::dataTableOutput("compareTable")
                ),
                column(width=8,
                    box(
                        width=12,
                        plotlyOutput("covidCumPlot")
                    ),
                    box(
                        width=12,
                        plotlyOutput("covidNewPlot")
                    )
                )
            )
        ),
        
        tabItem(tabName = "patientprofile",
                fluidRow(
                  fluidRow(
                    box(h4('These graphics represent the population of The Republic of Ireland', align = "center"), width ='100%')
                  ),
                  fluidRow(
                    box(plotlyOutput('ageCases'), width = '40%')
                  ),
                  fluidRow(
                    box(plotlyOutput('howContracted')),
                    box(plotlyOutput('icuProportion'))
                    
                  ),
                  fluidRow(
                    box(plotlyOutput('genderCases')),
                    box(plotlyOutput('helthcarePatients'))
                  )
                  
                )
        )
    ),
    #These style tags are necessary to cope with the
    #buggy renderInfoBox function
    tags$style("#ireCasesBox {width:300px;}"),
    tags$style("#ireDeathsBox {width:300px;}"),
    tags$style("#ireRecoverBox {width:300px;}"),
    tags$style("#wCasesBox {width:300px;}"),
    tags$style("#wDeathsBox {width:300px;}"),
    tags$style("#wRecoverBox {width:300px;}"),
    
    #The tags allow for nice vertical spacing
    tags$style(type = "text/css", "#covidMap {height: calc((100vh - 200px)/1.0) !important;}"),
    tags$style(type = "text/css", "#newSumIrelandPlot {height: calc((100vh - 250px)/2.0) !important;}"),
    tags$style(type = "text/css", "#cumSumIrelandPlot {height: calc((100vh - 250px)/2.0) !important;}"),
    tags$style(type = "text/css", "#newSumWorldPlot {height: calc((100vh - 250px)/2.0) !important;}"),
    tags$style(type = "text/css", "#cumSumWorldPlot {height: calc((100vh - 250px)/2.0) !important;}")
)

# Put them together into a dashboardPage
dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  title = 'Hamilton Insitute Covid-19 Visualisation'
)