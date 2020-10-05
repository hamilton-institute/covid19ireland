load("shiny_data.RData")


server <- function(input, output, session) {
  forecast_data <- reactive({
    if (input$log == FALSE) {
      shiny_data %>%
      dplyr::filter(Country %in% input$countryA) %>%
      dplyr::filter(time >= input$time[1]) %>%
      dplyr::filter(time <= input$time[2]) %>%
      droplevels()
    }else {
      shiny_data$cases[shiny_data$cases < 0] <- NA
      shiny_data$cases <- log(shiny_data$cases + 1, base = 10)
      shiny_data %>%
      dplyr::filter(Country %in% input$countryA) %>%
      dplyr::filter(time >= input$time[1]) %>%
      dplyr::filter(time <= input$time[2]) %>%
      droplevels()
    }
  })
  #---------------------------------------------------------------------
  output$forecast <- renderPlotly({
    h <- input$forecast2_height
    w <- input$forecast2_width
    w1 <- ggplot(forecast_data(),
                 aes(y = cases, x = time, group = Country,
                     colour = Country,
                     text = paste("Date: ", format(time, "%Y-%m-%d"),
                                  "<br>", Country, ": ", cases,
                                  "<br> Type: ", Indicator))) +
      geom_point(cex = 0.9,  alpha = 0.9,  stroke = 0.5) +
      geom_line() +
      theme_bw() +
      xlab("Time") +
      geom_vline(xintercept = max(shiny_data$time) - 6,
                 color = "gray", alpha = 0.7,  linetype = 2) +
      theme(legend.position = "top")
    if (input$log == FALSE) {
      w2 <- w1 + ylab("Number of Cases")
    }else {
      w2 <- w1 + ylab("log(number of cases + 1)")
    }
    ggplotly(w2, dynamicTicks = TRUE, tooltip = c("text")) %>%
      rangeslider() %>%
      layout(hovermode = "x", dragmode='pan',
             legend = list(orientation = "h")
             )
  })
  #---------------------------------------------------------------------
   output$forecast_ui <- renderUI({
     plotlyOutput("forecast",
                  height = paste0(input$forecast2_height, "px"),
                  width = paste0(input$forecast2_width, "%"))
  })
  #---------------------------------------------------------------------
  forecast_data_int2 <- reactive({
    if (input$log2 == FALSE) {
      shiny_data %>%
        dplyr::filter(Country %in% input$countryC) %>%
        dplyr::filter(time >= input$time2[1]) %>%
        dplyr::filter(time <= input$time2[2]) %>%
        mutate_at(5:10, funs(round(., 0)))%>%
        droplevels()
    }else {
      log_f <- function(x) log(x + 1)
      shiny_data$cases[shiny_data$cases < 0] <- NA
      shiny_data %>%
        dplyr::filter(Country %in% input$countryC) %>%
        dplyr::filter(time >= input$time2[1]) %>%
        dplyr::filter(time <= input$time2[2]) %>%
        mutate_at(5:10, funs(round(., 0)))%>%
        mutate_at(c(2, 5:10), funs(log_f)) %>%
        droplevels()
    }
   })
  output$forecast_int <- renderPlotly({
    h <- input$forecast_height
    if (input$cred_int == "80%") {
      w1 <- ggplot(forecast_data_int2(),
                   aes(y = cases, x = time, group = Country,
                       text = paste("Date: ", format(time, "%Y-%m-%d"),
                                    "<br>", Country, ": ", cases,
                                    "[", Lower10, ", ", Upper90, "]",
                                    "<br> Type: ", Indicator))) +
        geom_point(cex = 0.9,  alpha = 0.9,  stroke = 0.5) +
       geom_ribbon(aes(ymin = Lower10, ymax = Upper90), fill = "gray",
                   alpha=0.6,  col = NA) +
        geom_line() +
        facet_wrap(~Country,  scales = "free", ncol = input$int_cols) +
        theme_bw() +
        xlab("Time") +
        geom_vline(xintercept = max(shiny_data$time) - 6,
                   color = "gray", alpha = 0.7,  linetype = 2)
      if (input$log2 == FALSE) {
        w2 <- w1 + ylab("Number of Cases")
      }else {
        w2 <- w1 + ylab("log(number of cases + 1)")
      }
      ggplotly(w2, dynamicTicks = TRUE, tooltip = c("text"))
    }else {
      if (input$cred_int == "90%") {
        w1 <- ggplot(forecast_data_int2(),
                     aes(y = cases, x = time, group = Country,
                         text = paste("Date: ", format(time, "%Y-%m-%d"),
                                      "<br>", Country, ": ", cases,
                                      "[", Lower5, ", ", Upper95, "]",
                                      "<br> Type: ", Indicator))) +
          geom_point(cex = 0.9,  alpha = 0.9,  stroke = 0.5) +
          geom_ribbon(aes(ymin = Lower5, ymax = Upper95), fill = "gray",
                   alpha=0.6,  col = NA) +
          geom_line() +
          facet_wrap(~Country,  scales = "free", ncol = input$int_cols) +
          theme_bw() +
          xlab("Time") +
          geom_vline(xintercept = max(shiny_data$time) - 6,
                     color = "gray", alpha = 0.7,  linetype = 2)
        if (input$log2 == FALSE) {
          w2 <- w1 + ylab("Number of Cases")
        }else {
          w2 <- w1 + ylab("log(number of cases + 1)")
        }
        ggplotly(w2, dynamicTicks = TRUE, tooltip = c("text"))
      }else {
        if (input$cred_int == "95%") {
          w1 <- ggplot(forecast_data_int2(),
                       aes(y = cases, x = time, group = Country,
                           text = paste("Date: ", format(time, "%Y-%m-%d"),
                                        "<br>", Country, ": ", cases,
                                        "[", Lower2_5, ", ", Upper97_5, "]",
                                        "<br> Type: ", Indicator))) +
            geom_point(cex = 0.9,  alpha = 0.9,  stroke = 0.5) +
       geom_ribbon(aes(ymin = Lower2_5, ymax = Upper97_5), fill = "gray",
                   alpha=0.6,  col = NA) +
         geom_line() +
         facet_wrap(~Country,  scales = "free", ncol = input$int_cols) +
         theme_bw() +
         xlab("Time") +
         geom_vline(xintercept = max(shiny_data$time) - 6,
                    color = "gray", alpha = 0.7,  linetype = 2)
          if (input$log2 == FALSE) {
            w2 <- w1 + ylab("Number of Cases")
          }else {
            w2 <- w1 + ylab("log(number of cases + 1)")
          }
          ggplotly(w2, dynamicTicks = TRUE, tooltip = c("text"))
        }
      }
    }
  })
  #---------------------------------------------------------------------
  output$forecast_int_ui <- renderUI({
    plotlyOutput("forecast_int",
                 height = paste0(input$forecast_height, "px"),
                 width = "75%")
  })
  #---------------------------------------------------------------------
  myContainer <- htmltools::withTags(
    table(
      class = 'display',
      thead(
        tr(
          th(),
          th(),
          th(),
          th(colspan = 2, '80% CI', class = "dt-center"),
          th(colspan = 2, '90% CI', class = "dt-center"),
          th(colspan = 2, '95% CI', class = "dt-center")
        ),
        tr(
          lapply(c("Country", "Day", "Cases", "Lower",
                   "Upper",  "Lower",
                   "Upper", "Lower",
                   "Upper"), th)
        )
      )
    )
  )
  #---------------------------------------------------------------------
  forecast_data_table <- reactive({
    shiny_data %>%
      dplyr::filter(Country %in% input$countryB) %>%
      dplyr::filter(Indicator == "Forecast") %>%
      droplevels()
  })
  #---------------------------------------------------------------------
  output$summary_table <- DT::renderDataTable({
    data <- forecast_data_table()
    DT::datatable(data[, c(1, 3, 2, 5:10)], container = myContainer,
                  rownames = FALSE, filter = 'bottom',
                  options = list(lengthMenu = c(7, 14, 50, 100),
                                 pageLength = 7,  dom = "tip",
                                 autoWidth = TRUE,
                                 columnDefs = list(
                                   list(
                                     searchable = FALSE,
                                     targets = c(2:8))))) %>%
      formatRound(c(3:9), 2)
  })
  #---------------------------------------------------------------------
  #=====================================================================
  # AR Component
  #=====================================================================
  ar_data <- reactive({
    fitted_values %>%
      dplyr::filter(country %in% input$country_ar) %>%
      dplyr::filter(day >= input$time_ar[1]) %>%
      dplyr::filter(day <= input$time_ar[2]) %>%
      droplevels()
  })
  #---------------------------------------------------------------------
  output$ar_plot <- renderPlotly({
    h <- input$height_ar
    w1 <- ggplot(data = ar_data(),
                 aes(x = day, y = ar, group = country,
                     text = paste("Date: ", format(day, "%Y-%m-%d"),
                                  "<br>", country, ": ", ar))) +
      theme_bw() +
      geom_line() +
      geom_ribbon(aes(ymin = ar_low, ymax = ar_upp), alpha = .5) +
      geom_abline(intercept = 0, slope = 0, lty = 2, lwd = .25) +
      facet_wrap(~ country,  scales = input$ar_scale, ncol = input$ar_cols) +
      xlab("Time") +
      ylab("Autoregressive Component")

    if (input$dates_ar == "day" || input$dates_ar == "week" ||
          input$dates_ar == "2 weeks" || input$dates_ar == "3 weeks") {
      w2 <- w1 +
        scale_x_date(breaks = seq(min(ar_data()$day),
                                  max(ar_data()$day),
                                  by=input$dates_ar),
                     date_labels = "%y-%b-%d")
    }else {
      w2 <- w1 +
        scale_x_date(breaks = seq(min(ar_data()$day),
                                  max(ar_data()$day),
                                  by=input$dates_ar), date_labels = "%y-%b")
    }

    ggplotly(w2, dynamicTicks = FALSE, tooltip = c("text")) %>%
      layout(legend = list(orientation = "h"))
  })
  #---------------------------------------------------------------------
  output$ar_ui <- renderUI({
    plotlyOutput("ar_plot",
                 height = paste0(input$height_ar, "px"),
                 width = "70%")
  })
  #=====================================================================
  # Dendrogram
  #=====================================================================
  output$dend <- renderPlot({
    gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
    cols <- gg_color_hue(input$tree_col)
    if (input$tree_col == 10){
      cols5 <- cols[5]
      cols[5] <- cols[2]
      cols[2] <- cols5
    }
    clus10 <- cutree(hc, input$tree_col)

    country_query <- input$dendro_country
    country_fonts <- rep(1, nrow(tsdist))
    country_fonts[names(last_60) %in% country_query] <- 2
    country_cex <- rep(input$dendro_cex, nrow(tsdist))
    country_cex[names(last_60) %in% country_query] <-
      input$dendro_cex + 0.25 * input$dendro_cex
    plot(as.phylo(hc), type = input$dendro_type, tip.color = cols[clus10],
         cex = country_cex, font = country_fonts, edge.width =
                                                    input$dendro_edge,
         use.edge.length = TRUE)
  })
  #---------------------------------------------------------------------
  observeEvent(input$dendro_type, {
               if (input$dendro_type == "fan") {
                 updateNumericInput(
                   session,
                   inputId = "dendro_height",
                   label = "Height in px:",
                   value = 1000,
                   min = 250,
                   max = 8000
                 )
               }
               if (input$dendro_type == "radial") {
                 updateNumericInput(
                   session,
                   inputId = "dendro_height",
                   label = "Height in px:",
                   value = 1500,
                   min = 250,
                   max = 8000
                 )
               }
  }
  )
  output$plot_dend_ui <- renderUI({
    plotOutput("dend",  height = paste0(input$dendro_height, "px"))
  })
  #=====================================================================
  # Validation
  #=====================================================================
  output$text_validation <- renderUI({
    withMathJax(
      helpText('Logarithm of the observed $y_{it}$ versus the forecasted daily number of cases $y^*_{it}$ for each country, for up to seven days ahead, where each day ahead constitutes one panel. The forecasts were obtained from the autoregressive state-space hierarchical negative binomial model, fitted using data up to 6-May-2020, up to 29-Apr-2020, up to 13-May-2020 for the fisrt,  second,  and third forecast validation process. As an example,  the first day ahead of the third validation corresponds to 14-May-2020, and the seventh to 20-May-2020. Each dot represents a country. The results are very similar, with a concordance correlation between observed and forecasted values greaterthan 0.75 for up to five days ahead.'))
  })

  output$valid <- renderPlot({
    h <- input$valid_height
    country_list <- input$country_validation
    data_forecast %>%
      ggplot(aes(y = log(Y_forecast + 1, base = 10),
                 x = log(Y_obs + 1, base = 10),
                 label = country)) +
      theme_bw() +
      geom_point(data = subset(data_forecast, !country %in% country_list),
                 cex = input$vali_cex,  alpha = 0.4,  stroke = 0.5) +
      geom_abline(intercept = 0,  slope = 1, lty = 2, lwd = .55) +
      facet_grid(day2 ~ Type) +
      geom_point(data = subset(data_forecast, country %in%  country_list),
                 colour = "darkblue",  cex = input$vali_cex + 1,
                 pch = 24,  alpha = 1,
                 stroke = 0.2,
                 fill = "#32c6ff") +
      geom_text_repel(data = subset(data_forecast, country %in%  country_list &
                                                     data_forecast$Y_forecast >= data_forecast$Y_obs),
                      nudge_x = -input$vali_nudge,
                      min.segment.length = 1,
                      size = 5,
                      segment.size = 0.4) +
      geom_text_repel(data = subset(data_forecast, country %in%  country_list &
                                                     data_forecast$Y_forecast < data_forecast$Y_obs),
                      nudge_x = input$vali_nudge,
                      segment.size = 0.4,
                      min.segment.length = 1,
                      size = 5) +
      ylab(expression(log[10](y[it]^"*" + 1))) +
      xlab(expression(log[10](y[it] + 1))) +
      theme(strip.text.x = element_text(size = 14),
            strip.text.y = element_text(size = 14),
            axis.text=element_text(size=14),
            axis.title=element_text(size=18))
  })
  #---------------------------------------------------------------------
  output$plot_valid_ui <- renderUI({
    plotOutput("valid",  height = paste0(input$valid_height, "px"))
  })
  #---------------------------------------------------------------------
  output$text_validation2 <- renderUI({
    withMathJax(
      helpText('Observed concordance correlation coefficient (CCC), Pearson correlation ($r$), and bias corrector factor ($C_b$) between observed ($y_{it}$) and forecasted ($y^*_{it}$) values for each of the days ahead by validation process. Further details about CCC can be found in Lin (1989).'),
      br(),
      helpText("Lin, L. I. (1989). A Concordance Correlation Coefficient to Evaluate Reproducibility. Biometrics, 45(1):255–268."))
  })
  output$ccc_validation <- renderPlotly({
    ggplot(data_ccc, aes(y = Value, x = day2,  group = Method,
                         linetype = Method, shape = Method)) +
      geom_point() +
      geom_line() +
      theme_bw() +
      xlab("Days ahead") +
      ylim(0, 1) +
      scale_x_continuous(breaks = seq(1, 7, 1)) +
      facet_wrap(~Type)
  })
    output$text_validation3 <- renderUI({
    withMathJax(
      helpText('Even though performance falls substantially for the seventh day ahead, with the lowest CCC between observed and forecasted values close to $0.25$, there are still many countries for which the forecasted daily number of new cases is very close to the observed one. We also found a concordance correlation between observed and forecasted values greater than $0.75$ for up to five days ahead.'))
    })
  #=====================================================================
  # Approval Box
  #=====================================================================

}
