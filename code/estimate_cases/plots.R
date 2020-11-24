library(tidyverse)
# ------- script to make all the figures ------- #

# ------- setting working directory -------#
setwd("estimate_cases")
source("data")
source("plotting_helper_functions")
#----------- making temporal variation under-reporting and testing effort figure -------------#

# calling the plotting function to make figure 1 style plots for every country in one large .pdf

countries_of_interest <- c("Ireland")

iso_codes_of_interest <- c("IRL")

#--- gathering data for figure 1
# testing_data <- getFigure1TestData()
under_reporting_data <- getFigure1UnderReportingData()
saveRDS(under_reporting_data, "under_reporting_24_11.rds")

#--- Figure 1
under_reporting_data %>% 
  mutate(estimate = estimate * 100, 
         lower = lower * 100, 
         upper = upper * 100) %>% 
  ggplot(aes(x = date, y = estimate)) +
  geom_ribbon(
    aes(ymin = lower, 
        ymax = upper), fill = "dodgerblue", alpha = 0.6) + 
  geom_line(aes(y = estimate), 
            linetype = 2, colour = "black", size = 0.8) +
  ggplot2::scale_x_date(date_breaks = "1 month", 
                        labels = scales::date_format("%d-%b"),
                        limits = as.Date(c('2020-03-15','2020-10-30')))+
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100))+
  theme_bw(17) +
  ggplot2::labs(
    y = "Estimate of symptomatic \n cases reported (%)", 
    x = "Date") 

ggplot2::ggsave("presentation/figure_3_Ireland_no_tests.png",
                #plot = figure2, 
                width = 7,
                height = 5)

# png("presentation/figure_1_Ireland.png",
#     width = 7, height = 5, units = "in", res = 100)
# fontsize1 <- 1.15
# #par(mar = c(3, 3, 3, 3))
# par(oma = c(2, 3, 2, 5))
# figure_1_fun(under_reporting_data, testing_data,
#              iso_code_arg = "IRL")
# 
# figure_1_fun(under_reporting_data, testing_data,
#              iso_code_arg = "IRL")
# title(main="", 
#       #col.main="red",
#       #sub="My Sub-title", 
#       col.sub="blue",
#       xlab = "Date", ylab="Symptomatic cases reported (%)",
#       cex.lab = fontsize1)
# mtext("Tests performed per new confirmed case",
#       side = 4,
#       line = 1,
#       outer = TRUE,
#       cex = fontsize1)
# dev.off()

#--- figure 2
options(scipen = 999)
figure2 <- figure_2_fun(log = FALSE)
figure2
ggplot2::ggsave("presentation/figure_2_Ireland.png",
                plot = figure2, 
                width = 10,
                height = 7,
                units = "in")

figure2_log <- figure_2_fun(log = TRUE)
figure2_log
ggplot2::ggsave("presentation/figure_2_Ireland_log.png",
                plot = figure2_log, 
                width = 10,
                height = 7,
                units = "in")
