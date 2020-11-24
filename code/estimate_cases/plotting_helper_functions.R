theme_map <- function(world = FALSE) {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "serif", color = "#22211d"),
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_line(color = "#ebebe5", size = 0),
      panel.grid.major = ggplot2::element_line(color = "#ebebe5", size = 0),
      #plot.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA), 
      #panel.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA), 
      #legend.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
      panel.border = ggplot2::element_blank(),
      plot.margin = grid::unit(c(0,0,0,0), "mm"),
      legend.position = if(world == TRUE){"bottom"}
    )
}


mapPlottingFunction <- function(dataInput, europe = TRUE, plot_label)
{
  
  plotOutput <- dataInput %>%
    ggplot2::ggplot(ggplot2::aes(x = long, y = lat, group = group)) +
    ggplot2::geom_polygon(ggplot2::aes(fill = cumulative_incidence_mid*100)) + 
    ggplot2::geom_path(ggplot2::aes(x = long, y = lat, group = group), size = 0.3) + 
    theme_map(world = TRUE) +
    viridis::scale_fill_viridis(option = "magma", 
                                begin = 0.4, 
                                end = 0.95, 
                                name = "Estimated seroprevalence of SARS-CoV-2", 
                                direction = -1, 
                                breaks = c(0, 5, 10, 15, 20),
                                label = c("0%", "5%", "10%", "15%", "20%")) +
    ggplot2::labs(tag = plot_label)
  
  if(europe == TRUE)
  {
    plotOutput <- plotOutput + 
      ggplot2::coord_fixed(xlim = c(-9, 42.5),
                           ylim = c(36, 70.1),
                           ratio = 1.5) + 
      ggplot2::guides(fill = FALSE)
  }
  
  return(plotOutput)
  
}


binom_min <- function(x, n)
{
  tmp <- binom.test(x, n)
  signif(tmp$conf.int[1]*100, 2)
  
}

binom_max <- function(x, n)
{
  tmp <- binom.test(x, n)
  signif(tmp$conf.int[2]*100, 2)
  
}

#---------------------- figure 1 uses under-reporting and testing data function
figure_1_fun <- function(under_reporting_data, testing_data, iso_code_arg)
{
  
  country_names <- dplyr::tibble(country = countrycode::countrycode(iso_code_arg, "iso3c", "iso.name.en")) %>%
    dplyr::mutate(country = dplyr::case_when(country == "United States of America (the)" ~ "USA",
                                             country == "Russian Federation (the)" ~ "Russia",
                                             country == "Iran (Islamic Republic of)" ~ "Iran",
                                             country != "USA" || "Russia" || "Iran" ~ country))
  
  plot_dat_under_reporting <- under_reporting_data %>%
    dplyr::filter(iso_code == iso_code_arg) %>%
    dplyr::mutate(estimate = estimate*100,
                  lower = lower*100,
                  upper = upper*100) %>% 
    tidyr::drop_na()
  
  plot_dat_testing <- testing_data %>%
    dplyr::filter(iso_code == iso_code_arg) %>% 
    tidyr::drop_na()
  
  x_limits_both_dat <- plot_dat_under_reporting %>%
    dplyr::summarise(x_min = min(date),
                     x_max = max(date))
  
  x_limits_both <- c(x_limits_both_dat$x_min, x_limits_both_dat$x_max)
  
  y_limits_under_reporting <- c(y_min = 0, y_max = 100)
  
  # if(iso_code_arg %in% row1)
  # {
  #   y_limits_under_reporting <- c(y_min = 0, y_max = 100)
  # }
  # if(iso_code_arg %in% row2)
  # {
  #   y_limits_under_reporting <- c(y_min = 0, y_max = 100)
  # }
  # if(iso_code_arg %in% row3)
  # {
  #   y_limits_under_reporting <- c(y_min = 0, y_max = 40)
  # }
  
  
  # y_limits_under_reporting <- c(y_min = 0, y_max = 100)
  
  #--- uncomment for ad-hoc country runs
  # 
  # if(!(iso_code_arg %in% row1) | !(iso_code_arg %in% row2) | !(iso_code_arg %in% row3))
  # {
  #   y_limits_under_reporting_dat <- plot_dat_under_reporting %>%
  #     dplyr::summarise(y_min = min(lower),
  #                      y_max = max(upper))
  #   y_limits_under_reporting <- c(y_limits_under_reporting_dat$y_min, y_limits_under_reporting_dat$y_max)
  # }
  # 
  
  y_limits_testing_dat <- plot_dat_testing %>%
    dplyr::summarise(y_min = min(testing_effort),
                     y_max = max(testing_effort))
  
  
  y_limits_testing <- c(y_limits_testing_dat$y_min, y_limits_testing_dat$y_max)
  
  #--------------- plotting the first data set, the under-reporting estimate over time
  plot(plot_dat_under_reporting$date, plot_dat_under_reporting$estimate, 
       type = "l",
       las = 1,
       xlab = "", 
       ylab = "",
       bty = "n",
       axes = FALSE,
       xlim = x_limits_both, 
       ylim = y_limits_under_reporting)
  polygon(x = c(plot_dat_under_reporting$date, rev(plot_dat_under_reporting$date)),
          y = c(plot_dat_under_reporting$lower, rev(plot_dat_under_reporting$upper)),
          col =  adjustcolor("dodgerblue", alpha.f = 0.42), border = NA)
  axis(side = 2, ylim = y_limits_under_reporting, col = "black", las = 1)
  box()
  
  par(new = TRUE)
  #--------------- plotting the second data set, the under-reporting estimate over time
  plot(plot_dat_testing$date, plot_dat_testing$testing_effort,
       type = "l",
       lty = 2, 
       lwd = 1,
       xlab = "",
       ylab = "",
       main = country_names,
       xlim = x_limits_both,
       ylim = y_limits_testing,
       axes = FALSE)
  axis(side = 4, ylim = y_limits_under_reporting, col = "black", las = 1)
  
  # making the time axis
  tick_positions = seq(min(plot_dat_under_reporting$date), max(plot_dat_under_reporting$date), length.out = 10)
  axis.Date(side = 1, lubridate::month(plot_dat_under_reporting$date, label = TRUE), at = tick_positions, srt = 20)
}


#-----------------------------------------------
#--------------------- function for producing figure 3: true and adjusted daily
#--------------------- new case curves for top n countries

figure_2_fun <- function(log)
{
  
  all_adjusted_case_data <- getAdjustedCaseDataNational()
  
  options(scipen = 999)
  
  #--- making first panel
  
  
  plot_3_dat <- all_adjusted_case_data %>%
    dplyr::group_by(country) %>%
    dplyr::filter(country %in% "Ireland") %>% 
    dplyr::filter(new_cases > 1 & new_cases_adjusted_smooth_mid > 1 & new_cases_adjusted_smooth_low > 1 & new_cases_adjusted_smooth_high > 1)
  
  max_date_conf <- plot_3_dat %>% 
    dplyr::filter(new_cases_smoothed == max(new_cases_smoothed)) %>% 
    dplyr::pull(date)
  
  
  max_date_est <- plot_3_dat %>% 
    dplyr::filter(new_cases_adjusted_smooth_mid == max(new_cases_adjusted_smooth_mid)) %>% 
    dplyr::pull(date)
  
  p1 <- plot_3_dat %>%
    ggplot2::ggplot() + 
    ggplot2::geom_line(ggplot2::aes(x = date, y = new_cases_smoothed)) + 
    ggplot2::geom_ribbon(ggplot2::aes(x = date,
                                      ymin = new_cases_adjusted_smooth_low, 
                                      ymax = new_cases_adjusted_smooth_high), fill = "dodgerblue", alpha = 0.6) + 
    ggplot2::geom_line(ggplot2::aes(x = date, y = new_cases_adjusted_smooth_mid), linetype = 2, colour = "royalblue", size = 0.5) + 
    ggplot2::geom_vline(aes(xintercept = max_date_conf), 
                        colour = 'red') +
    ggplot2::geom_vline(aes(xintercept = max_date_est), 
                        colour = 'orange') +
    ggplot2::labs(x = "", y = "") + 
    ggplot2::scale_x_date(date_breaks = "1 month", 
                          labels = scales::date_format("%d-%b"),
                          limits = as.Date(c('2020-03-15','2020-11-15'))) + 
    theme_bw(18) + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 15)) + 
    ggplot2::theme(legend.position="none",
                   plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm")) + 
    ggplot2::labs(
      #tag = "C",
      x = "", y = "Cases by date of confirmation") + 
    viridis::scale_color_viridis(discrete = TRUE, begin = 0, end = 0.82) +
    ggplot2::geom_text(label = "Confirmed", size = 5,
                       aes(x = max(date), y = 
                             max(new_cases_smoothed)-300), 
                       hjust = -.1) +
    ggplot2::geom_text(label = "Adjusted", size = 5,
                       colour = "dodgerblue",
                       aes(x = max(date), y = 
                             max(new_cases_smoothed)+150), 
                       hjust = -.1)
  if(log){
    p1 +    ggplot2::scale_y_log10(breaks = c(
      1, 10, 30, 100, 250, 500, 1000, 2000, 5000, 10000)) 
    
    
  } else {
  p1 + scale_y_continuous(
    breaks = scales::pretty_breaks(n = 10)
  )
  }
  
}


plot_1 <- function(under_reporting_data, iso_code_arg){
  plot_dat_under_reporting <- under_reporting_data %>%
    dplyr::filter(iso_code == iso_code_arg) %>%
    dplyr::mutate(estimate = estimate*100,
                  lower = lower*100,
                  upper = upper*100) %>% 
    tidyr::drop_na()
  

  plot_dat_under_reporting %>% 
    ggplot(aes(x = date, y = estimate)) +
    geom_ribbon(
      aes(ymin = lower, 
          ymax = upper), fill = "dodgerblue", alpha = 0.6) + 
    geom_line(aes(y = estimate), 
              linetype = 2, colour = "black", size = 0.8) +
    ggplot2::scale_x_date(date_breaks = "1 month", 
                          labels = scales::date_format("%d-%b"),
                          limits = as.Date(c('2020-03-15','2020-11-05'))) + 
    scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
    ggthemes::theme_few(18) +
    ggplot2::labs(
      y = "Estimate of symptomatic \n cases reported (%)", 
      x = "Date") 
}


