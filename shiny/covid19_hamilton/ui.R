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
library(fontawesome) # remotes::install_github('rstudio/fontawesome')
library(shinyWidgets)
library(readxl)

# Data wrangling ----------------------------------------------------------

# Get the latest update data
last_updated = read_csv(file = 'last_updated.csv') %>% deframe

global_raw <- readRDS('latest_global_data.rds') %>% 
  mutate(countriesAndTerritories = 
           recode(countriesAndTerritories, 
                  'Cases_on_an_international_conveyance_Japan' = 'Cruise_ship',
                  'United_States_of_America' = 'USA',
                  'United_Kingdom' = 'UK'
           ))

global_world = global_raw %>% 
  group_by(dateRep) %>% 
  summarise(deaths = sum(deaths),
            cases = sum(cases),
            popData2018 = sum(as.numeric(popData2018))) %>% 
  mutate(countriesAndTerritories = 'Global')

global = bind_rows(global_world, global_raw)

# Get interventions data
latest_interventions_data = read_excel('latest_interventions_data.xlsx',
                                       sheet = "Database") %>% 
  mutate(COUNTRY = 
           recode(COUNTRY, 
                  'United States of America' = 'USA',
                  'United Kingdom' = 'UK',
                  "Czech Republic" = 'Czechia'
           ))

# Header and sidebar ------------------------------------------------------



header <- dashboardHeader(
  title = "Hamilton Institute Covid-19 Dashboard",
  titleWidth = 380,
  tags$li(class = 'dropdown',
          a(icon("github"),
            href = "https://github.com/hamilton-institute/covid19ireland")),
  tags$li(class = 'dropdown',
          a(icon("envelope"),
            href = "mailto:andrew.parnell@mu.ie"))
)

sidebar <- dashboardSidebar(
  width = 150,
  sidebarMenu(
    menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
    menuItem("Map", tabName = "county", icon = icon("map")),
    menuItem("Hospitals", tabName = "patientprofile", 
             icon = icon("hospital")),
    menuItem("Graphs", tabName = "graphs", icon = icon("bar-chart-o")),
    menuItem("Animations", icon = icon("chart-line"), tabName = "animation"),
    menuItem("Interventions", tabName = "interventions", icon = icon("user-plus")),
    menuItem("Sources", icon = icon("list-alt"), tabName = "sources")
  )
)

# Body -------------------------------------------------------------------

body <- dashboardBody(
  tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important; }")),
  shinyDashboardThemes(
    theme = "grey_light"
  ),
  

# Sources -----------------------------------------------------------------

  
  tabItems(
    tabItem(tabName = 'sources',
            helpText(h4("Data sources:"),
                     h4(fa(name = "hospital", fill = "#FFFFFF", height = 20),
                        a("ECDC", href= "https://www.ecdc.europa.eu/en", target="_blank")),
                     h4(fa(name = "landmark", fill = "#FFFFFF", height = 20),
                        a("Irish government data", href= "https://www.gov.ie/en/news/7e0924-latest-updates-on-covid-19-coronavirus/", target="_blank")),
                     h4(fa(name = "university", fill = "#FFFFFF", height = 20),
                        a("Northern Ireland government data", href= "https://www.publichealth.hscni.net/publications/covid-19-surveillance-reports", target="_blank")),
                     h4(fa(name = "user-plus", fill = "#FFFFFF", height = 20),
                        a("Interventions data", href= "https://data.humdata.org/dataset/e1a91ae0-292d-4434-bc75-bf863d4608ba/resource/93108e8e-8afc-4f26-950b-0c1e587ee5c2/download/20200416-acaps-covid-19-goverment-measures-dataset-v8.xlsx", target="_blank")),
                     h5("ECDC data are updated for the next day usually around 12pm, with sporadic updates occuring at other times. Irish goverment provisional figures are updated daily at around 6pm with confirmed figures given for two days previous."), 
                     h5("Irish hospitalistion statistics are only given in the confirmed figures so are slightly older than the provisional data."),
                     h5("The Irish confirmed figures are significantly higher than the corresponding estimates given for those days by the ECDC."),
                     h5("The Northern Ireland figures are given by district which does not match precisely into counties. We have made an arbitrary decision as to which district are allocated to which counties. Details are provided in the GitHub repository in the Irish data spreadsheet"),
                     br(),
                     h4(fa(name = "users", fill = "#FFFFFF", height = 20),
                        'Contributors:', a("GitHub contributors page", href= "https://github.com/hamilton-institute/covid19ireland/graphs/contributors")),
                     h4(fa(name = "bug", fill = "#FFFFFF", height = 20),
                        'Report bugs and suggest features at the', a("Github issues page", href= "https://github.com/hamilton-institute/covid19ireland/issues")),
                     h4(fa(name = "github", fill = "#FFFFFF", height = 20),
                        'See the code on ', a("Github", href= "https://github.com/hamilton-institute/covid19ireland")))
    ),
    
    # Summary tab -------------------------------------------------------------
    
    tabItem(tabName = 'summary',
            # column(width = 12,
            #        helpText(HTML(paste0("<h4><em>global data updated ",
            #                             format(last_updated['global'],
            #                                    "%d-%b-%Y %H:%M"), 
            #                             ". Irish data updated ",
            #                             format(last_updated['GOV_IE'],
            #                                    "%d-%b-%Y %H:%M"), "</em></h3>")))),
            column(width = 9,
                   fluidRow(
                     column(width = 3, valueBoxOutput("ireCasesBox", width = NULL)),
                     column(width = 3, valueBoxOutput("ireDeathsBox", width = NULL)),
                     column(width = 3, valueBoxOutput("ireHospBox", width = NULL)),
                     column(width = 3, valueBoxOutput("ireICUBox", width = NULL))
                   ),
                   fluidRow(
                     column(width = 6, valueBoxOutput("wCasesBox", width = NULL)),
                     column(width = 6, valueBoxOutput("wDeathsBox", width = NULL)),
                     #scolumn(width = 4, valueBoxOutput("wRecovBox", width = NULL)),
                   ),
                   tags$head(tags$style(HTML(".small-box {height: 150px;}"))),
                   fluidRow(
                     column(width = 3, valueBoxOutput("bigDailyBox", width = NULL)),
                     column(width = 3, valueBoxOutput("worstHitCountryBox", width = NULL)),
                     column(width = 3, valueBoxOutput("increaseDeathBox", width = NULL)),
                     column(width = 3, valueBoxOutput("bigDecreaseBox", width = NULL))
                   )),
            column(width = 3, 
                   leafletOutput('covidMap2')
            ),
            
            column(width = 12,
                   box(
                     # tags$style(HTML('table.dataTable tr:nth-child(even) {background-color: #3c8dbc !important;}')),
                     # tags$style(HTML('table.dataTable tr:nth-child(odd) {background-color: #3c8dbc !important;}')),
                     # tags$style(HTML('table.dataTable th {background-color: #3c8dbc !important;}')),
                     width = 4,
                     title=HTML(fa(name = "calendar-day", fill = "#3d9970"),
                                paste0("Daily deaths: ", format(max(global$dateRep), '%d-%b-%Y'))),
                     DT::dataTableOutput("highestDaily")
                   ),
                   box(
                     width = 4,
                     title=HTML(fa(name = "exclamation-triangle", fill = "#d81b60"), "Total deaths"),
                     DT::dataTableOutput("highestTotal")
                   ),
                   box(
                     width = 4,
                     title=HTML(fa(name = "chart-line", fill = "#3c8dbc"), "Deaths increase from yesterday"),
                     DT::dataTableOutput("biggestChange")
                   )
                   
            )
    ),
    
    # interventions tab -------------------------------------------------------------
    
    tabItem(tabName = 'interventions',
            fluidRow(
              column(width = 3,
                     pickerInput("sel_ctry3",
                                 "Select countries", 
                                 choices= unique(global$countriesAndTerritories),
                                 selected = c('Ireland'),
                                 options = list(`actions-box` = TRUE,
                                                `live-search` = TRUE),
                                 multiple = TRUE)
              ),
              column(width = 3,
                     pickerInput("sel_var3",
                                 "Select a variable", 
                                 choices=c('Cumulative cases', 'Cumulative deaths', 
                                           'Daily cases', 'Daily deaths', 
                                           'Log cumulative cases', 'Log cumulative deaths', 
                                           'Cases per million population',
                                           'Deaths per million population'),
                                 selected = c('Daily deaths'),
                                 multiple = FALSE)
              ),
              column(width = 3,
                     pickerInput("sel_axis3",
                                 "Select horizontal axis", 
                                 choices=c('Date', 'Days since measure introduced'),
                                 selected = c('Days since measure introduced'),
                                 multiple = FALSE)
              )
            ),
            fluidRow( 
              column(width = 3,
                     pickerInput("sel_measure",
                                 "Select intervention measure", 
                                 choices = latest_interventions_data$MEASURE %>% 
                                   unique() %>% sort() %>% str_to_sentence() %>% 
                                   str_squish(),
                                 selected = "Schools closure",
                                 options = list(`actions-box` = TRUE,
                                                `live-search` = TRUE),
                                 multiple = TRUE)
              ),
              column(width = 3,
                     checkboxInput("sel_smooth",
                                 label = "Include smooth", 
                                 value = FALSE),
              ),
              column(width = 3,
                     checkboxInput("sel_window",
                                   label = "Include 2-week window\n (mouse-over for detail)", 
                                   value = TRUE),
              )
            ),
            fluidRow(
              column(width = 9,
                     plotlyOutput("InterventionsPlot", height = "500px")
              )
            )        
    ),
    
    
    
    # graphs tab -------------------------------------------------------------
    
    tabItem(tabName = 'graphs',
            fluidRow(
              column(width = 3,
                     pickerInput("sel_ctry",
                                 "Select countries", 
                                 choices= unique(global$countriesAndTerritories),
                                 selected = c('France', 'Ireland', 'UK', 'USA', 'Spain', 'Belgium', 'Italy'),
                                 options = list(`actions-box` = TRUE,
                                                `live-search` = TRUE),
                                 multiple = TRUE)
              ),
              column(width = 3,
                     pickerInput("sel_var",
                                 "Select variables", 
                                 choices=c('Cumulative cases', 'Cumulative deaths', 
                                           'Daily cases', 'Daily deaths', 
                                           'Logp1 cumulative cases', 
                                           'Logp1 cumulative deaths', 
                                           'Logp1 daily cases', 
                                           'Logp1 daily deaths', 
                                           'Cases per million population',
                                           'Deaths per million population'),
                                 selected = c('Deaths per million population'),
                                 multiple = TRUE)
              ),
              column(width = 3,
                     pickerInput("sel_axis",
                                 "Select horizontal axis", 
                                 choices=c('Date', 'Days since 1st case', 'Days since 10th case',
                                           'Days since 1st death', 'Days since 10th death'),
                                 selected = c('Days since 1st death'),
                                 multiple = FALSE)
              ),
              column(width = 9,
                     plotlyOutput("CountryPlot", height = "500px")
              )
            )        
    ),
    
    
    # County tab --------------------------------------------------------------
    
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
    
    
    # Animation tab -----------------------------------------------------------
    
    tabItem(tabName = "animation",
            tags$style(type="text/css", ".recalculating {opacity: 1.0;}"),
            fluidRow(
              column(width = 3,
                     pickerInput("sel_ctry2",
                                 "Select countries", 
                                 choices=unique(global$countriesAndTerritories),
                                 selected = c('Ireland', 'UK', 'Italy', 'USA',
                                              'Spain', 'France', 'Belgium'),
                                 options = list(`actions-box` = TRUE,
                                                `live-search` = TRUE),
                                 multiple = TRUE)
              ),
              column(width = 3,
                     pickerInput("sel_horiz",
                                 "Select horizontal axis", 
                                 choices=c('Cumulative cases', 'Cumulative deaths', 
                                           'Logp1 cumulative cases', 'Logp1 cumulative deaths', 
                                           'Sqrt cumulative cases', 'Sqrt cumulative deaths', 
                                           'Daily cases', 'Sqrt daily cases', 'Logp1 daily cases',
                                           'Daily deaths', 'Sqrt daily deaths', 'Logp1 daily deaths',
                                           'Cumulative cases per million population',
                                           'Cumulative deaths per million population'),
                                 selected = c('Sqrt cumulative cases'),
                                 multiple = FALSE)
              ),
              column(width = 3,
                     pickerInput("sel_vert",
                                 "Select vertical axis", 
                                 choices=c('Cumulative cases', 'Cumulative deaths', 
                                           'Logp1 cumulative cases', 'Logp1 cumulative deaths', 
                                           'Sqrt cumulative cases', 'Sqrt cumulative deaths', 
                                           'Daily cases', 'Sqrt daily cases', 'Logp1 daily cases',
                                           'Daily deaths', 'Sqrt daily deaths', 'Logp1 daily deaths',
                                           'Cumulative cases per million population',
                                           'Cumulative deaths per million population'),
                                 selected = c('Sqrt cumulative deaths'),
                                 multiple = FALSE)
              )
            ),
            fluidRow(
              column(width = 9,
                     sliderInput("theDate", "Date (click play or move slider)", min = min(global$dateRep), 
                                 max = max(global$dateRep), 
                                 value = max(min(global$dateRep), as.POSIXct("2020-03-01")),
                                 width = "75%",
                                 timeFormat = "%d/%b",
                                 animate=animationOptions(interval=1000, 
                                                          loop = FALSE)
                     )
              ),
              column(width = 9,
                     plotOutput("AnimPlot", height = "500px")
              )
            )
    ),
    
    
    # patientprofile tab ------------------------------------------------------
    
    tabItem(tabName = "patientprofile",
            fluidRow(
              fluidRow(
                box(h4('These graphics represent the population of The Republic of Ireland', align = "center"), width ='100%')
              ),
              fluidRow(
                box(tabsetPanel(type = "tabs",
                                tabPanel("Latest", plotlyOutput("ageHist")),
                                tabPanel("Hospitalised History", plotlyOutput("ageHospHistory")),
                                tabPanel("Total History", plotlyOutput("ageTotalHistory"))), width = '40%')
              ),
              fluidRow(
                box(tabsetPanel(type = "tabs",
                                tabPanel("Latest", plotlyOutput("howContracted")),
                                tabPanel("History", plotlyOutput("howContractedHistory")))),
                box(tabsetPanel(type = "tabs",
                                tabPanel("Latest", plotlyOutput("icuProportion")),
                                tabPanel("History", plotlyOutput("icuProportionHistory"))))
              ),
              fluidRow(
                box(tabsetPanel(type = "tabs",
                                tabPanel("Latest", plotlyOutput("genderCases")),
                                tabPanel("History", plotlyOutput("genderCasesHistory")))),
                box(tabsetPanel(type = "tabs",
                                tabPanel("Latest", plotlyOutput("healthcarePatients")),
                                tabPanel("History", plotlyOutput("healthcarePatientsHistory"))))
              )
              
            )
    )
  ),


  tags$style(type = "text/css", "#covidMap {height: calc((100vh - 200px)/1.0) !important;}"),
  tags$style(type = "text/css", "#covidMap2 {height: calc((48vh)/1.0) !important;}"),
  tags$style(type = "text/css", "#newSumIrelandPlot {height: calc((100vh - 250px)/2.0) !important;}"),
  tags$style(type = "text/css", "#cumSumIrelandPlot {height: calc((100vh - 250px)/2.0) !important;}"),
  tags$style(type = "text/css", "#newSumWorldPlot {height: calc((100vh - 250px)/2.0) !important;}"),
  tags$style(type = "text/css", "#cumSumWorldPlot {height: calc((100vh - 250px)/2.0) !important;}")
)


# Dashboard page ----------------------------------------------------------

# Put them together into a dashboardPage
dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  title = 'Hamilton Insitute Covid-19 Visualisation'
)