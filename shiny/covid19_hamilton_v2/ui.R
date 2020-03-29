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
#library(fontawesome)
library(shinyWidgets)

last_update = format(file.info('summary_stats_current.csv')$mtime,
                     "%d-%b-%Y, %H:%M")

ecdc_raw <- readRDS('ECDC_data_current.rds')

ecdc_world = ecdc_raw %>% 
  group_by(dateRep) %>% 
  summarise(deaths = sum(deaths),
            cases = sum(cases),
            popData2018 = sum(popData2018)) %>% 
  mutate(countriesAndTerritories = 'Global')

ecdc = bind_rows(ecdc_raw, ecdc_world)

header <- dashboardHeader(
  title = paste("Hamilton Covid-19 Dashboard: Updated", last_update),
  titleWidth = 600
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
    menuItem("Animations", icon = icon("chart-line"), tabName = "animation"),
    menuItem("By County", tabName = "county", icon = icon("map")),
    menuItem("Hospitalisation Stats", tabName = "patientprofile
             ", icon = icon("hospital"))
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
                     pickerInput("sel_ctry",
                                 "Select countries", 
                                 choices= unique(ecdc$countriesAndTerritories),
                                 selected = c('Ireland', 'Global', 'United_Kingdom'),
                                 options = list(`actions-box` = TRUE,
                                                `live-search` = TRUE),
                                 multiple = TRUE)
              ),
              column(width = 4,
                     pickerInput("sel_var",
                                 "Select variables", 
                                 choices=c('Cumulative cases', 'Cumulative deaths', 
                                           'Daily cases', 'Daily deaths', 
                                           'Log cumulative cases', 'Log cumulative deaths', 
                                           'Cases per million population',
                                           'Deaths per million population'),
                                 selected = c('Deaths per million population'),
                                 multiple = TRUE)
              ),
              column(width = 4,
                     pickerInput("sel_axis",
                                 "Select horizontal axis", 
                                 choices=c('Date', 'Days since 1st case', 'Days since 10th case',
                                           'Days since 1st death'),
                                 selected = c('Date'),
                                 multiple = FALSE)
              ),
              column(width = 12,
                     plotlyOutput("CountryPlot", height = "500px")
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
    
        tabItem(tabName = "animation",
                tags$style(type="text/css", ".recalculating {opacity: 1.0;}"),
                fluidRow(
                  column(width = 4,
                         pickerInput("sel_ctry2",
                                     "Select Countries", 
                                     choices=unique(ecdc$countriesAndTerritories),
                                     selected = c('Global'),
                                     options = list(`actions-box` = TRUE,
                                                    `live-search` = TRUE),
                                     multiple = TRUE)
                  ),
                  column(width = 4,
                         pickerInput("sel_horiz",
                                     "Select horizontal axis", 
                                     choices=c('Cumulative cases', 'Cumulative deaths', 
                                               'Log cumulative cases', 'Log cumulative deaths', 
                                               'Sqrt cumulative cases', 'Sqrt cumulative deaths', 
                                               'Cumulative cases per million population',
                                               'Cumulative deaths per million population'),
                                     selected = c('Log cumulative cases'),
                                     multiple = FALSE)
                  ),
                  column(width = 4,
                         pickerInput("sel_vert",
                                     "Select vertical axis", 
                                     choices=c('Cumulative cases', 'Cumulative deaths', 
                                               'Log cumulative cases', 'Log cumulative deaths', 
                                               'Sqrt cumulative cases', 'Sqrt cumulative deaths', 
                                               'Cumulative cases per million population',
                                               'Cumulative deaths per million population'),
                                     selected = c('Log cumulative deaths'),
                                     multiple = FALSE)
                  )
                ),
                fluidRow(
                  column(width = 12,
                         sliderInput("theDate", "Date (click play or move slider)", min = min(ecdc$dateRep), 
                                     max = max(ecdc$dateRep), value = min(ecdc$dateRep),
                                     width = "75%",
                                     timeFormat = "%d/%b",
                                     animate=animationOptions(interval=1000, loop = FALSE)
                         )
                  ),
                  column(width = 12,
                         plotOutput("AnimPlot", height = "700px")
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