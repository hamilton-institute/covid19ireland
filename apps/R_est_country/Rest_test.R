## app.R ##
rm(list = ls(all = TRUE))
library(shinydashboard)
library(R0)
library(tidyverse)
library(tidycovid19)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(plotly)
library(lubridate)
library(geofacet)

latest = download_merged_data(silent = TRUE, cached = TRUE)
load("r0_predictions.rda")

America <- data.frame(
  code = c("24", "12", "44", "8", "18", "41", "16", "32", "2", "47", "28", "29", "40", "36", "20", "4", "25", "38", "48", "17", "19", "27", "6", "34", "7", "30", "11", "33", "15", "26", "1", "39", "23", "45", "21", "46", "35", "5", "14", "10", "31", "42", "37", "9", "43", "13", "3", "22"),
  name = c("Greenland", "Canada", "United States", "Bermuda", "Cayman Islands", "Turks & Caicos Islands", "Cuba", "Mexico", "Anguilla", "British Virgin Islands", "Haiti", "Jamaica", "Sint Maarten", "Puerto Rico", "Dominican Republic", "Antigua & Barbuda", "Guatemala", "El Salvador", "U.S. Virgin Islands", "Curaçao", "Dominica", "Honduras", "Bahamas", "Panama", "Belize", "St. Kitts & Nevis", "Barbados", "Nicaragua", "Costa Rica", "Guyana", "Aruba", "Suriname", "Grenada", "St. Vincent & Grenadines", "Ecuador", "Venezuela", "Peru", "Caribbean Netherlands", "Colombia", "Brazil", "St. Lucia", "Trinidad & Tobago", "Paraguay", "Bolivia", "Uruguay", "Chile", "Argentina", "Falkland Islands"),
  row = c(2, 2, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 7, 6, 6, 7, 7, 8, 7, 7, 7, 7, 8, 9, 8, 8, 8, 8, 8, 9, 10, 9, 9, 9, 9, 9, 9, 10, 11, 10, 10, 10, 11, 11, 13),
  col = c(5, 3, 3, 8, 7, 6, 5, 3, 8, 7, 6, 5, 8, 7, 6, 8, 2, 3, 7, 6, 8, 2, 1, 4, 3, 7, 8, 1, 2, 4, 3, 5, 7, 8, 1, 4, 2, 9, 3, 5, 7, 8, 4, 3, 5, 4, 5, 7),
  stringsAsFactors = FALSE
)



Asia <- data.frame(
  code = c("20", "18", "29", "19", "45", "41", "3", "4", "11", "43", "25", "39", "10", "15", "14", "1", "31", "9", "22", "44", "24", "28", "8", "5", "13", "33", "6", "23", "17", "16", "35", "37", "36", "2", "40", "21", "46", "34", "30", "26", "32", "47", "12", "7", "38", "27", "42"),
  name = c("Kyrgyzstan", "Japan", "Mongolia", "Kazakhstan", "Uzbekistan", "Tajikistan", "Armenia", "Azerbaijan", "Georgia", "Turkey", "Lebanon", "Syria", "Cyprus", "Iraq", "Iran", "Afghanistan", "Nepal", "China", "South Korea", "Taiwan", "Laos", "Myanmar (Burma)", "Bhutan", "Bangladesh", "India", "Pakistan", "Bahrain", "Kuwait", "Jordan", "Israel", "Palestinian Territories", "Saudi Arabia", "Qatar", "United Arab Emirates", "Thailand", "Cambodia", "Vietnam", "Philippines", "Malaysia", "Sri Lanka", "Oman", "Yemen", "Indonesia", "Brunei", "Singapore", "Maldives", "Timor-Leste"),
  row = c(2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 9, 10, 10),
  col = c(9, 12, 10, 9, 8, 7, 6, 5, 4, 3, 2, 2, 3, 4, 5, 6, 7, 8, 9, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 3, 4, 5, 8, 9, 10, 11, 11, 7, 4, 3, 11, 12, 12, 9, 11),
  stringsAsFactors = FALSE
)

x <- list(America,Asia, geofacet::eu_grid1, geofacet::africa_countries_grid1)
names(x) <- c("America","Asia", "eu_grid1", "africa_countries_grid1")

ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 R Estimator"),
  dashboardSidebar(useShinyjs(),
                   fluidRow(
                     column(width=12,
   
    
    dateInput("date_end",
                   label = "End of two week period to estimate R:",
              max(latest$date)),
    
    pickerInput("sel_cty",
                "Select continent", 
                choices = c("Europe", "America", "Asia", "Africa"),
                selected = c("eu_grid1"),
                options = list(`actions-box` = TRUE,
                               `live-search` = TRUE),
                multiple = FALSE),
    
    
                     ))
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      
      
      plotOutput("R_estim", width = "100%", height = "800px") %>% withSpinner(color="#1E90FF"),
      
     
    )
  )
)

server <- function(input, output) {
  

  
  output$R_estim <- renderPlot({
    

    continent = input$sel_cty
    
    if(continent == "Europe"){continent = "eu_grid1"}
    if(continent == "Africa"){continent = "africa_countries_grid1"}
    
    
    mygrid <- x[[continent]]
    
    data_use = latest %>% 
      group_by(country) %>% 
      mutate(cum_cases = ecdc_cases,
             cases = c(cum_cases[1], diff(ecdc_cases))) %>% 
      ungroup() %>% 
      dplyr::select(country,date,cum_cases,cases,population) %>% 
      filter(date >= input$date_end - 14, date <= input$date_end) %>% 
      na.omit()
    
    data_use$country[which(data_use$country == "Czechia")] <- "Czech Republic"
    r0_predictions$country[which(r0_predictions$country == "Czechia")] <- "Czech Republic"
    
    # COVID generation time
    estR0 = r0_predictions %>% 
      group_by(country) %>% 
      dplyr::mutate(n_rows = length(country) - 1) %>% 
      do( data.frame(., date = seq.Date(Sys.Date() - min(.$n_rows), Sys.Date(),  by = 1))) %>% 
      dplyr::filter(date == input$date_end)
    
    
    estR0 = estR0 %>% mutate(R_est = signif(pred, 3)) %>% 
      mutate(new_county_name = paste0(country,"; R = ", R_est)) 
    
    
    
    merger = estR0 %>% select(country, new_county_name) %>% unique
    
    merger = merger %>% rename(name = country)
    merger = left_join(merger, mygrid)
    merger = na.omit(merger)
    
    data_use = data_use %>% rename(name = country)
    
    data_use <- left_join(data_use,merger)
    
    data_use = data_use %>% filter(is.na(code) == F) %>% select(date, cases, new_county_name)
    
    estR0 = estR0 %>% rename(name = country)
    
    mygrid <- left_join(mygrid, estR0) 
    
    mygrid = mygrid %>% rename(name = new_county_name, new_county_name = name) %>% 
      select(row, col, code, name) %>% na.omit()
    
    
    data_use <- data_use %>% mutate(R = as.numeric(gsub(".*=", "", data_use$new_county_name)))
    
    
    ggplot(data = data_use, aes(x = date, y = cases)) +
      geom_point() + 
      facet_geo(~ new_county_name, grid = mygrid, scales = "free_y") +
      labs(x = 'Date',
           y = 'Cases',
           title = paste('Cases from', 
                         format(Sys.Date()-14, '%d-%b'), 'to',
                         format(Sys.Date(), '%d-%b'))) + 
      theme_bw() + 
      geom_smooth(se = FALSE, aes(color= R)) +
      scale_color_viridis(option = "D")
  })
  
}

shinyApp(ui, server)