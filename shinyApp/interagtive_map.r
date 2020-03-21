library(shiny)
library(dplyr)
library(forcats)
library(leaflet)



ui <- bootstrapPage(
  tags$head(
    tags$style(type = "text/css", "html, body {width:100%;height:100%; font-family: Oswald, sans-serif;}")
  ),
  
  leafletOutput("map", width = "100%", height = "100%"),
  
  absolutePanel(
    top = 10, left = 10, style = "text-align: left;",
    tags$h2("COVID-19 Tracker")
  )
)



server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-8, 53.5, zoom = 6.5)
  })
  
}
shinyApp(ui, server)