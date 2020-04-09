library(shiny)
library(shinyMobile)
library(echarts4r)
library(shinyWidgets)

server = function(input, output, session) {
  
  # river plot
  dates <- reactive(seq.Date(Sys.Date() - 30, Sys.Date(), by = input$by))
  
  output$river <- renderEcharts4r({
    df <- data.frame(
      dates = dates(),
      apples = runif(length(dates())),
      bananas = runif(length(dates())),
      pears = runif(length(dates()))
    )
    
    df %>%
      e_charts(dates) %>%
      e_river(apples) %>%
      e_river(bananas) %>%
      e_river(pears) %>%
      e_tooltip(trigger = "axis") %>%
      e_title("River charts", "(Streamgraphs)") %>%
      e_theme("dark")
  })
  
  # network
  nodes <- reactive({
    data.frame(
      name = paste0(LETTERS, 1:300),
      value = rnorm(300, 10, 2),
      size = rnorm(300, 10, 2),
      grp = rep(c("grp1", "grp2", "grp3"), 100),
      stringsAsFactors = FALSE
    )
  })
  
  edges <- reactive({
    data.frame(
      source = sample(nodes()$name, 400, replace = TRUE),
      target = sample(nodes()$name, 400, replace = TRUE),
      stringsAsFactors = FALSE
    )
  })
  
  output$network <- renderEcharts4r({
    req(input$show)
    e_charts() %>%
      e_graph_gl() %>%
      e_graph_nodes(nodes(), name, value, size, grp) %>%
      e_graph_edges(edges(), source, target) %>%
      e_theme("dark")
  })
  
  
  # datatable
  output$data <- renderTable({
    mtcars[, c("mpg", input$variable), drop = FALSE]
  }, rownames = TRUE)
  
  output$data2 <- renderTable({
    mtcars[, c("mpg", input$variable), drop = FALSE]
  }, rownames = TRUE)
  
  
  # send the theme to javascript
  # observe({
  #   session$sendCustomMessage(
  #     type = "ui-tweak",
  #     message = list(os = input$theme, skin = input$color)
  #   )
  # })
  
}