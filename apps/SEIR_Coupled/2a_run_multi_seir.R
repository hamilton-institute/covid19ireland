### Last edit: 9 July 2020
# Authors: Ken Duffy, HoChan Cheon
# coupled SEIR model (hard-coded)

rm(list = ls(all = TRUE))

source("2b_multi_seir.R")
source("_utils.R")
library(dplyr)
library(ggplot2)
library(cowplot)
library(scales)
library(reshape2)

compt1 = c("S1", "E1", "I1", "R1")  # Ireland SEIR
compt2 = c("S2", "E2", "I2", "R2")  # UK SEIR
compt_names = c(compt1, compt2)

N1 = matrix(0, nrow = 1, ncol = length(compt1)) # a matrix to store number of people in each compartment
N2 = matrix(0, nrow = 1, ncol = length(compt2))
real = 200

##### Epidemic model setup: parameters
N_phases = 1 # number of phases (e.g. intervention at t=t*)
R0_1 = 0.9
gamma1 = c(6.6, 7.4)  # mean holding times at compartment E and I 
total1 = sum(gamma1)  # assuming beta_E = beta_I => include comments
beta1 = R0_1 / c(total1, total1)

R0_2 = 1.2
gamma2 = c(6.6, 7.4)
total2 = sum(gamma2)
beta2 = R0_2 / c(total2, total2)

##### Assign initial conditions
N1[1,1] = 5E6
N1[1,2] = 0
N1[1,3] = 0
N1[1,4] = 0

N2[1,1] = 6.8E7
N2[1,2] = 0
N2[1,3] = 10
N2[1,4] = 0

t = 0
dt = 1  # time increment
t_phase = Inf

# https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/638137/Additional_Data_Paper_-_Northern_Ireland_Common_Travel_Area.pdf
transport_rate = 1.54E7 # 15.4 million people moving between UK and Ireland annually
wS1E1 = 0.99  # 99% of chance that exposed people will be moved to local E compartment
wS1E2 = 1 - wS1E1  # (1 - wS1E2)% chance of jumping to the other E compartment
wS1S2 = transport_rate * (1/365)  # rate of people leaving Ireland per day

wS2E2 = 0.99
wS2E1 = 1 - wS2E2
wS2S1 = transport_rate * (1/365)  # rate of people leaving (arriving) UK (Ireland) per day

##### Run simulation
realisation = list( # a list of lists to store realisation results
  Time = list(), 
  S1 = list(), E1 = list(), I1 = list(), R1 = list(),
  S2 = list(), E2 = list(), I2 = list(), R2 = list()
) 
for (r in 1:real) {
  for(i in 1:N_phases){
    if (i == 1) { # for first phase of the epidemic
      res = coupled_seir_model(t_phase, t, dt, N1, gamma1, beta1, N2, gamma2, beta2)
    }
    else {
      # TODO: pass results from the first phase with different infection parameters
    }
  }
  res = as.data.frame(res) %>% rename_at(vars(paste0(c(rep("V", length(compt_names))), 2:9)), ~compt_names)
  
  # save the results as a list of vectors
  realisation$Time[[r]] = res$Time
  for (cname in compt_names) {
    realisation[[cname]][[r]] = res[[cname]]
  }
}

Time = rowMeans(sapply(realisation$Time, `length<-`, max(lengths(realisation$S1))), na.rm = TRUE)
extract = NULL
mean = NULL
quantileCIs = NULL
low = 0.025 # lower percentile
upp = 0.975 # upper percentile
for (cname in compt_names) {
  # convert to matrix (NB: because each realisation has different length, fill the gap with NAs)
  extract[[cname]] = sapply(realisation[[cname]], `length<-`, max(lengths(realisation[[cname]])))
  
  # calculate mean across rows
  mean[[cname]] = rowMeans(extract[[cname]], na.rm = TRUE)
  
  # calculate percentile confidence intervals
  quantileCIs[[cname]][["lower"]] = apply(extract[[cname]], 1, quantile, low, na.rm = TRUE)
  quantileCIs[[cname]][["upper"]] = apply(extract[[cname]], 1, quantile, upp, na.rm = TRUE)
}

# convert the mean values into a data frame
final = data.frame(
  Time = Time, 
  S1 = mean[["S1"]], E1 = mean[["E1"]], I1 = mean[["I1"]], R1 = mean[["R1"]],
  S2 = mean[["S2"]], E2 = mean[["E2"]], I2 = mean[["I2"]], R2 = mean[["R2"]]
)

##### Plot number of people in each compartment
title1 = paste0("#S_IR = ", N1[1,1], ", #E_IR = ", N1[1,2], ", #I_IR = ", N1[1,3], ", R0_IR = ", R0_1,  "\n#S_UK = ", N2[1,1], ", #E_UK = ", N2[1,2], ", #I_UK = ", N2[1,3], ", R0_UK = ", R0_2)
# title2 = paste0("#S_UK = ", N2[1,1], ", #E_UK = ", N2[1,2], ", #I_UK = ", N2[1,3])
plt1 = ggplot(final, aes(Time)) +
  geom_ribbon(aes(ymin = quantileCIs$S1$lower, ymax = quantileCIs$S1$upper), alpha = 0.3, fill = "black") +
  geom_ribbon(aes(ymin = quantileCIs$E1$lower, ymax = quantileCIs$E1$upper), alpha = 0.3, fill = "purple") +
  geom_ribbon(aes(ymin = quantileCIs$I1$lower, ymax = quantileCIs$I1$upper), alpha = 0.3, fill = "red") +
  geom_ribbon(aes(ymin = quantileCIs$R1$lower, ymax = quantileCIs$R1$upper), alpha = 0.3, fill = "blue") +
  
  geom_ribbon(aes(ymin = quantileCIs$S2$lower, ymax = quantileCIs$S2$upper), alpha = 0.1, fill = "black") +
  geom_ribbon(aes(ymin = quantileCIs$E2$lower, ymax = quantileCIs$E2$upper), alpha = 0.1, fill = "purple") +
  geom_ribbon(aes(ymin = quantileCIs$I2$lower, ymax = quantileCIs$I2$upper), alpha = 0.1, fill = "red") +
  geom_ribbon(aes(ymin = quantileCIs$R2$lower, ymax = quantileCIs$R2$upper), alpha = 0.1, fill = "blue") +
  
  geom_line(aes(y = S1, color = "A_black", linetype = "Ireland")) +  # S1 Ireland
  geom_line(aes(y = E1, color = "B_purple", linetype = "Ireland")) + # E1
  geom_line(aes(y = I1, color = "C_red", linetype = "Ireland")) +    # I1
  geom_line(aes(y = R1, color = "D_blue", linetype = "Ireland")) +   # R1
  
  geom_line(aes(y = S2, color = "E_black", linetype = "UK")) +  # S2 UK
  geom_line(aes(y = E2, color = "F_purple", linetype = "UK")) + # E2
  geom_line(aes(y = I2, color = "G_red", linetype = "UK")) +    # I2
  geom_line(aes(y = R2, color = "H_blue", linetype = "UK")) +   # R2
  
  labs(x = "Time [day]", y = "Number of people", title = title1, colour = "") +
  scale_colour_manual(
    name = "Comp.",
    values = c(
      A_black = "black", B_purple = "purple", C_red = "red", D_blue = "blue",
      E_black = "black", F_purple = "purple", G_red = "red", H_blue = "blue"
    ),
    labels = c(
      "S_IR", "E_IR", "I_IR", "R_IR",
      "S_UK", "E_UK", "I_UK", "R_UK"
    )
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), labels = scientific_10) +
  theme(plot.title = element_text(size = 10, face = "bold"))

# A second plot in log scale, otherwise identical to plt1
plt2 = plt1 + 
  scale_y_log10(limits = c(1E-2, NA), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  labs(x = "Time [day]", y = "Number of people (logscale)", title = title1, colour = "") +
  theme(plot.title = element_text(size = 10, face = "bold"))

ggsave("./out/result2_1.jpeg", plot = plt2, width = 7, height = 5, units = "in")
plt2

# final_plot = plot_grid(plt1, plt2, align='vh', vjust = 1, scale = 1)
# ggsave("./out/result2.jpeg", plot = final_plot, width = 7, height = 5, units = "in")
# final_plot