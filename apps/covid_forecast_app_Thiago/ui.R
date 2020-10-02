# Ui

# Packages
#-----------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
require(dplyr)
library(DT)
require(ggplot2)
library(plotly)
#=======================================================================
# Body
#=======================================================================
body  <- dashboardBody(
  tags$head(
    tags$meta(charset="UTF-8"),
    tags$meta(name="description", content="..."),
    tags$meta(name="keywords", content="..."),
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
    #-------------------------------------------------------------------
    # Authors bar
    tags$style(HTML('.navbar-custom-menu>.navbar-nav>li>.dropdown-menu {
                     width:450px;
                     background-color: #483C32;
                     color: #F0FFFF;}')),
    #-------------------------------------------------------------------
    # Shiny Color
    #-------------------------------------------------------------------
    tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #9F1D35;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #f4b943;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #483C32;
                              }

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #483C32;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #3B2F2F;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #3B2F2F;
                              color: #F0FFFF;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #ff69b4;
                              }
        /* toggle button when hovered  */
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #ff69b4;
                              }
                              '
        )
        )
  ),
  #---------------------------------------------------------------------
  setShadow("box"),
  setShadow("info-box"),
  setShadow("progress"),
  #---------------------------------------------------------------------
  withMathJax(),
  #---------------------------------------------------------------------
  tabItems(
    #-------------------------------------------------------------------
    tabItem(
      tabName = "plots",
      navbarPage(
        "",
        #---------------------------------------------------------------
        tabPanel(
          "Graphical Output",
          icon = icon("fas fa-chart-line"),
          fluidPage(
            gradientBox(
              width = 12,
              icon = "fa fa-th",
              title = "7-step-ahead Forecast - COVID-19 Coronavirus Pandemic",
              boxToolSize = "lg",
              footer = column(
                width = 12,
                align = "center",
                fluidRow(
                  column(
                    width = 2,
                    checkboxInput(
                      inputId = "log",
                      label = "Log Scale (base 10)",
                      value = FALSE)
                  ),
                  column(
                    width = 3,
                    selectInput(
                      inputId = "countryA",
                      label = "Country:",
                      choices = levels(shiny_data$Country),
                      selected = c("Brazil", "United States of America"),
                      multiple = TRUE
                    )
                  ),
                  column(
                    width = 3,
                    sliderInput("time",
                                "Dates:",
                                min = min(shiny_data$time),
                                max = max(shiny_data$time),
                                value = c(max(shiny_data$time) - 20,
                                          max(shiny_data$time))
                                )
                  ),
                  column(
                    width = 1,
                    numericInput(
                      inputId = "forecast2_height",
                      label = "Height in px:",
                      value = 450,
                      min = 250,
                      max = 8000
                    )
                  ),
                  column(
                    width = 1,
                    numericInput(
                      inputId = "forecast2_width",
                      label = "Width in %:",
                      value = 60,
                      min = 250,
                      max = 8000
                    )
                  )
                ),
                HTML("<div style ='overflow:auto;  ' >"),
                uiOutput("forecast_ui"),
                HTML("</div>")
              ),
              "Reported Cases by Country, Territory, or Conveyance. Users are advised to use all data and forecast with caution and awareness of their limitations."
            )
          )
        ),
        #---------------------------------------------------------------
        tabPanel(
          "General Description",
          icon = icon("fas fa-file-alt"),
          fluidPage(
            column(
              width = 12,
              boxPlus(
                width = NULL,
                status = "warning",
                closable = TRUE,
                collapsible = TRUE,
                tags$div(
                  style="text-align:center",
                  tags$hr(style="border-color: black;"),
                  tags$h3("Description"),
                  tags$hr(style="border-color: black;"),
                  ),
                tags$div(
                  style="text-align:justified",
                  tags$p("Our model displayed an excellent predictive performance for short-term forecasting, especially for the first six days ahead.",
                         style = "font-size:20px", align="justify"),
                  tags$div("The modelling framework allows for forecasting the daily number of new COVID-19 cases for each country and territory for which data has been gathered by the", tags$b("European Centre for Disease Prevention and Control"), "(ECDC). It introduces statistical novelty in terms of modelling the autoregressive parameter as a function of time. This makes it highly flexible to adapt to changes in the autoregressive structure of the data over time. In the COVID-19 pandemic, this translates directly in improved predictive power in terms of forecasting future number of daily cases. Our objective here is to provide a simple, yet not overly simplistic, framework for country-level decision-making, and we understand this might be easier for smaller countries when compared to nations of continental dimensions, where state-level decision-making should be more effective.",
                         style = "font-size:20px", align = "justify"),
                  tags$p("To forecast future observations, we use the median value rather than the mean value,  and this is reasonable for short-term forecasting, since the error accumulates from one time step to the other.", style = "font-size:20px", align = "justify"),
                  tags$p(tags$b("Results are updated twice a week, monday and thursday at 14:00 GMT"),
                         style = "font-size:20px", align = "justify")
                )
              )
            )
          )
        )
        #---------------------------------------------------------------
      )
    ),
    #-------------------------------------------------------------------
    tabItem(
      tabName = "plots2",
      fluidRow(
        gradientBox(
          width = 12,
          icon = "fa fa-th",
          title = "7-step-ahead Forecast - COVID-19 Coronavirus Pandemic",
          boxToolSize = "lg",
          footer = column(
            width = 12,
            align = "center",
            fluidRow(
              column(
                width = 2,
                checkboxInput(
                  inputId = "log2",
                  label = "Log Scale (base 10)",
                  value = FALSE)
              ),
              column(
                width = 2,
                selectInput(
                  inputId = "countryC",
                  label = "Country:",
                  choices = levels(shiny_data$Country),
                  selected = c("Ireland", "United Kingdom"),
                  multiple = TRUE
                )
              ),
              column(
                width = 3,
                sliderInput("time2",
                            "Dates:",
                            min = min(shiny_data$time),
                            max = max(shiny_data$time),
                            value = c(max(shiny_data$time) - 20,
                                      max(shiny_data$time))
                            )
              ),
              column(
                width = 2,
               selectInput(
                 inputId = "cred_int",
                 label = "Credible Interval:",
                 choices = c("80%", "90%", "95%"),
                 selected = "80%",
                 multiple = FALSE
               )
              ),
              column(
                width = 1,
                numericInput(
                  inputId = "int_cols",
                  label = "Columns:",
                  value = 2,
                  min = 1,
                  max = 7
                )
              ),
              column(
                width = 1,
                numericInput(
                  inputId = "forecast_height",
                  label = "Height in px:",
                  value = 300,
                  min = 250,
                  max = 8000
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                uiOutput("forecast_int_ui")
              )
            )
          ),
          "Reported Cases by Country, Territory, or Conveyance. Users are advised to use all data and forecast with caution and awareness of their limitations.",
          )
      )
    ),
    #-------------------------------------------------------------------
    tabItem(
      tabName = "table",
      fluidRow(
        gradientBox(
          width = 12,
          icon = "fa fa-th",
          title = "7-step-ahead Forecast - COVID-19 Coronavirus Pandemic",
          boxToolSize = "lg",
          footer = column(
            width = 12,
            align = "center",
            fluidRow(
              column(
                width = 5,
                selectInput(
                  inputId = "countryB",
                  label = "Country:",
                  choices = levels(shiny_data$Country),
                  selected = c("Brazil", "United States of America"),
                  multiple = TRUE
                )
              )
            )
          ),
          "Reported Cases by Country, Territory, or Conveyance. Users are advised to use all data and forecast with caution and awareness of their limitations."
        )
      ),
      fluidRow(
        column(
          width = 12,
          HTML("<div style ='overflow:auto;  ' >"),
          dataTableOutput("summary_table"),
          HTML("</div>")
        )
      )
    ),
    #-------------------------------------------------------------------
    tabItem(
      tabName = "ar_comp",
      navbarPage(
        "",
        selected = "General Description",
        #---------------------------------------------------------------
        tabPanel(
          "Graphical Output",
          icon = icon("fas fa-chart-line"),
          fluidPage(
            gradientBox(
              width = 12,
              icon = "fa fa-th",
              title = "Autoregressive Component with 95% credible interval - COVID-19 Coronavirus Pandemic",
              boxToolSize = "lg",
              footer = column(
                width = 12,
                align = "center",
                fluidRow(
                  column(
                    width = 3,
                    selectInput(
                      inputId = "country_ar",
                      label = "Country:",
                      choices = levels(fitted_values$country),
                      selected = c("China"),
                      multiple = TRUE
                    )
                  ),
                  column(
                    width = 3,
                    sliderInput("time_ar",
                                "Dates:",
                                min = min(fitted_values$day),
                                max = max(fitted_values$day),
                                value = c(min(fitted_values$day),
                                          max(fitted_values$day))
                                )
                  ),
                  column(
                    width = 1,
                    selectInput(
                      inputId = "dates_ar",
                      label = "Date Breaks:",
                      choices = c("day", "week", "2 weeks", "3 weeks", "month",
                                  "2 months", "3 months", "6 months"),
                      selected = c("month"),
                      multiple = FALSE
                    )
                  ),
                  column(
                    width = 1,
                    selectInput(
                      inputId = "ar_scale",
                      label = "Y-Scale:",
                      choices = c("fixed", "free",  "free_y"),
                      selected = c("fixed"),
                      multiple = FALSE
                    )
                  ),
                  column(
                    width = 1,
                    numericInput(
                      inputId = "ar_cols",
                      label = "Columns:",
                      value = 2,
                      min = 1,
                      max = 7
                    )
                  ),
                  column(
                    width = 1,
                    numericInput(
                      inputId = "height_ar",
                      label = "Height in px:",
                      value = 450,
                      min = 250,
                      max = 8000
                    )
                  )
                ),
                HTML("<div style ='overflow:auto;  ' >"),
                uiOutput("ar_ui"),
                HTML("</div>")
              ),
              "Reported Cases by Country, Territory, or Conveyance. Users are advised to use all data and,  consequently, results with caution and awareness of their limitations."
            )
          )
        ),
        #---------------------------------------------------------------
        tabPanel(
          "General Description",
          icon = icon("fas fa-file-alt"),
          fluidPage(
            column(
              width = 12,
              boxPlus(
                width = NULL,
                status = "warning",
                closable = TRUE,
                collapsible = TRUE,
                tags$div(
                  style="text-align:center",
                  tags$hr(style="border-color: black;"),
                  tags$h3("Description"),
                  tags$hr(style="border-color: black;"),
                  ),
                tags$div(
                  style="text-align:justified",
                  tags$p("The autoregressive component in the model has a direct relationship with the pandemic behaviour over time for each country. It is directly proportional to the natural logarithm of the daily number of cases, given what happened in the previous day. Therefore, it is sensitive to changes and can be helpful detecting possible waves.",
                         style = "font-size:20px", align="justify")
                )
              )
            )
          )
        )
        #---------------------------------------------------------------
      )
    ),
    #-------------------------------------------------------------------
    tabItem(
      tabName = "dendrogram",
      sidebarPanel(
        width = 12,
        tags$div(
          style="text-align:center",
          tags$hr(style="border-color: black;"),
          tags$h3("Description"),
          tags$hr(style="border-color: black;"),
          ),
            tags$div(
              style="text-align:justified",
              tags$p("Dendrogram representing the hierarchical clustering of countries based on their estimated autoregressive parameters. The clustering used Ward's method and pairwise dynamic time warp distances between the countries' time series. Each of 10 clusters is represented with a different colour. Country abbreviations: BSES = Bonaire, Saint Eustatius and Saba; IC Japan = Cases on an international conveyance - Japan; CAE = Central African Republic; DRC = Democratic Republic of the Congo; NMI = Northern Mariana Islands; SKN = Saint Kitts and Nevis; SVG = Saint Vincent and the Grenadines; STP = São Tomé and Príncipe; TC Islands = Turks and Caicos Islands; UAE = United Arab Emirates.",
                     style = "font-size:20px", align="justify")
            )
      ),
      fluidRow(
        column(
          width = 12,
          boxPlus(
            width = NULL,
            status = "success",
            closable = TRUE,
            collapsible = TRUE,
            fluidRow(
              column(
                width = 2,
                wellPanel(
                  selectInput(
                    inputId = "dendro_country",
                    label = "Country Query:",
                    choices = levels(fitted_values$country),
                    selected = NULL,
                    multiple = TRUE
                  )
                )
              ),
              #---------------------------------------------------------
              column(
                width = 2,
                wellPanel(
                  selectInput(
                    inputId = "dendro_type",
                    label = "Type of tree to be drawn:",
                    choices = c("phylogram", "cladogram", "fan",
                                "radial"),
                    selected = "cladogram",
                    multiple = FALSE
                  )
                )
              ),
              #---------------------------------------------------------
              column(
                width = 2,
                wellPanel(
                  numericInput(
                    inputId = "tree_col",
                    label = "Number of Clusters:",
                    value = 10,
                    min = 1,
                    max = 30
                  )
                )
              ),
              #---------------------------------------------------------
              column(
                width = 2,
                wellPanel(
                  numericInput(
                    inputId = "dendro_cex",
                    label = "Character Expansion:",
                    value = 1.2,
                    min = 0,
                    max = 6
                  )
                )
              ),
              #---------------------------------------------------------
              column(
                width = 2,
                wellPanel(
                  numericInput(
                    inputId = "dendro_edge",
                    label = "Width of the branches:",
                    value = 1,
                    min = 0,
                    max = 6
                  )
                )
              ),
              #---------------------------------------------------------
              column(
                width = 2,
                wellPanel(
                  numericInput(
                    inputId = "dendro_height",
                    label = "Height in px:",
                    value = 3000,
                    min = 250,
                    max = 8000
                  )
                )
              )
              #---------------------------------------------------------
            ),
            uiOutput("plot_dend_ui")
          )
        )
      )
    ),
    #-------------------------------------------------------------------
    tabItem(
      tabName = "validation",
      sidebarPanel(
        width = 12,
            tags$div(
              style="text-align:center",
              tags$hr(style="border-color: black;"),
              tags$h3("Description"),
              tags$hr(style="border-color: black;"),
              ),
        tags$style("#text_validation {font-size:24px;}"),
        uiOutput("text_validation")
      ),
      fluidRow(
        column(
          width = 12,
          boxPlus(
            width = NULL,
            status = "success",
            closable = TRUE,
            collapsible = TRUE,
            fluidRow(
              column(
                width = 4,
                wellPanel(
                  selectInput(
                    inputId = "country_validation",
                    label = "Country Query:",
                    choices = levels(data_forecast$country),
                    selected = NULL,
                    multiple = TRUE
                  )
                )
              ),
              #---------------------------------------------------------
              column(
                width = 2,
                wellPanel(
                  numericInput(
                    inputId = "vali_cex",
                    label = "Character Expansion:",
                    value = 4,
                    min = 0,
                    max = 8
                  )
                )
              ),
              #---------------------------------------------------------
              column(
                width = 2,
                wellPanel(
                  numericInput(
                    inputId = "vali_nudge",
                    label = "Horizontal adjustment:",
                    value = 0.6,
                    min = 0,
                    max = 4
                  )
                )
              ),
              #---------------------------------------------------------
               column(
                width = 2,
                wellPanel(
                  numericInput(
                    inputId = "valid_height",
                    label = "Height in px:",
                    value = 2800,
                    min = 250,
                    max = 8000
                  )
                )
               ),
              #---------------------------------------------------------
            ),
            uiOutput("plot_valid_ui")
          )
        )
      ),
      br(),
      sidebarPanel(
        width = 12,
        tags$div(
          style="text-align:center",
          tags$hr(style="border-color: black;"),
          tags$h3("Concordance Correlation Coefficient (CCC)"),
          tags$hr(style="border-color: black;"),
          ),
        tags$style("#text_validation2 {font-size:24px;}"),
        uiOutput("text_validation2")
      ),
      fluidRow(
        column(
          width = 12,
          boxPlus(
            width = NULL,
            status = "success",
            closable = TRUE,
            collapsible = TRUE,
            plotlyOutput("ccc_validation",  height = "400px")
          )
        )
      ),
      br(),
      fluidRow(
        sidebarPanel(
          width = 12,
          tags$style("#text_validation3 {font-size:24px;}"),
          uiOutput("text_validation3")
        )
      )
    )
    #-------------------------------------------------------------------
  )
)
#=======================================================================
# Sidebar
#=======================================================================
sidebar  <- dashboardSidebar(
  sidebarMenu(
    sidebarMenuOutput("Semi_collapsible_sidebar"),
    #-------------------------------------------------------------------
    # Create three menuIten table and plot
    menuItem(text = "Forecast",  tabName = "forecast",
             startExpanded = TRUE, icon = icon("laptop"),
             menuSubItem("Point Estimate",  tabName = "plots",
                         icon = icon("fas fa-chart-line")),
             menuSubItem("Interval Estimate",  tabName = "plots2",
                         icon = icon("fas fa-chart-area")),
             menuSubItem("Summary",  tabName = "table",
                         icon = icon("calculator"))
             ),
    menuItem(
      text = "Pandemic Waves",  tabName = "ar_comp", icon = icon("fas fa-exclamation-triangle")
    ),
    menuItem(
      text = "Dendrogram", tabName = "dendrogram", icon =icon("fas fa-project-diagram")
    ),
    #-------------------------------------------------------------------
    tags$hr(style="border-color: white;"),
    tags$div(
      style="text-align:center",
      tags$p("Additional Information")),
    tags$hr(style="border-color: white;"),
    menuItem(
      text = "Model Validation", tabName = "validation", icon =icon("bar-chart-o")
    )
  )
)
#=======================================================================
# Header
#=======================================================================
header  <- dashboardHeaderPlus(
  title = "SARS-CoV-2 Forecast",
  titleWidth = 350,
  enable_rightsidebar = TRUE,
  left_menu = tagList(
    dropdownBlock(
      id = "mydropdown",
      title = "Authors",
      icon = "users",
      #-----------------------------------------------------------------
      ## Box Header ##
      tags$li(
        class = "widget-user-header",
        tags$h3(class = "widget-user-username",
                tags$a(href="https://prof-thiagooliveira.netlify.app",
                       "Thiago de Paula Oliveira",
                       style = "font-size:15px")),
        tags$h5(class = "widget-user-desc", "NUI Galway,  ",
                tags$a(href = "https://twitter.com/_OliveiraTP_", "Twitter"))
      ),
      tags$li(
        class = "widget-user-header",
        tags$h3(class = "widget-user-username",
                tags$a(href="https://www.maynoothuniversity.ie/people/rafael-de-andrade-moral",
                       "Rafael de Andrade Moral",
                       style = "font-size:15px")),
        tags$h5(class = "widget-user-desc", "Maynooth University,  ",
                 tags$a(href = "https://twitter.com/rafamoral", "Twitter"))
      )
      #-----------------------------------------------------------------
    ),
    dropdownBlock(
      id = "mydropdown",
      title = "Useful Links",
      icon = "fas fa-link",
      #-----------------------------------------------------------------
      ## Box Header ##
      #-----------------------------------------------------------------
      tags$li(
        class = "widget-user-header",
        tags$h3(class = "widget-user-username",
                tags$a(href="http://arxiv.org/abs/2006.00111",
                       "Paper - preprint",
                       style = "font-size:15px")),
        tags$h5(class = "widget-user-desc", "")
      ),
      #-----------------------------------------------------------------
      tags$li(
        class = "widget-user-header",
        tags$h3(class = "widget-user-username",
                tags$a(href="https://github.com/Prof-ThiagoOliveira/covid_forecast",
                       "GitHub",
                       style = "font-size:15px")),
        tags$h5(class = "widget-user-desc", "All supplementary material and R codes")
      )
      #-----------------------------------------------------------------
    )
  )
#  rightSidebarIcon = "gears",
#  dropdownMenu(
#    type = "messages"
#  )
)
#=======================================================================
ui <- dashboardPagePlus(header, sidebar, body)
#=======================================================================
