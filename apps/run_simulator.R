## Function to run the simulator for the SEIR model for time to extinction

run_simulator = function (R0, E, I, Rec, num_proc = 3, n_runs = 1000) {
  
  # Required packages
  library(foreach)
  require(doParallel)
  registerDoParallel(num_proc)
  
  # Source in functions
  source('seir_extinct.R')
  source('utils.R')
  
  gamma_E = 6.6 # Best guesses - should total about 14 days
  gamma_I = 7.4
  S_1 = 4.9*10^6 # Population of Ireland
  
  # Starting matrix
  compt = c('S', 'E', 'I', 'Rec') # SEIR compartment
  N = matrix(0, nrow = 1,  ncol = length(compt)) # a matrix to store number of people in each compartment
  colnames(N) = compt
  
  # define number of sub-compartments
  no_sub_compt = c("S" = 1, "E" = 5, "I" = 5, "Rec" = 1) # store number of sub-compartments
  sub_names = list("S" = NULL, "E" = NULL, "I" = NULL, "Rec" = NULL)
  SYS_compt = NULL
  for (c in compt) {
    number = no_sub_compt[[c]]
    if ( number == 1 ) {
      sub_names[[c]] = c
      SYS_compt = c(SYS_compt, c)
    }
    else {
      for (n in 1:number) {
        c_name = paste0(c, "_", n)
        sub_names[[c]] = c(sub_names[[c]], c_name)
        SYS_compt = c(SYS_compt, c_name)
      }
    }
  }
  
  SYS_N = matrix(0, nrow = 1, ncol = length(SYS_compt)) 
  colnames(SYS_N) = c(sub_names[["S"]], sub_names[["E"]], sub_names[["I"]], sub_names[["Rec"]])
  
  # equal mean holding times for E and I compartments (mean(E+I) = 14days)
  mean_holding_times = c(gamma_E, gamma_I)  # mean holding times at compartment E and I
  total_holding_times = sum(mean_holding_times)
  beta = R0 / c(total_holding_times, total_holding_times)
  
  ##### Assign initial conditions
  N[1,1] = S_1
  N[1,2] = E
  N[1,3] = I
  N[1,4] = Rec
  
  
  # by default, assign initial Exposed and Infectious people in the E_1 and I_1, respectively
  SYS_N[1,1] = N[1,1]
  SYS_N[1,length(sub_names$S)+1] = N[1,2]
  SYS_N[1,length(c(sub_names$S, sub_names$E))+1] = N[1,3]
  SYS_N[1,length(c(sub_names$S, sub_names$E, sub_names$I))+1] = N[1,4]
  
  # initialise simulation condition
  t = 0
  dt = 1/4 # time increment in days
  t_phase = Inf
  
  ##### Run simulation
  times = unlist(foreach(1:n_runs) %dopar% {
    seir_model(t_phase, t, dt, N, mean_holding_times, beta, SYS_N, sub_names)
  })

  # Return output
  out = c(R0, E, I, Rec, 
          quantile(times, 0.05),
          quantile(times, 0.1),
          quantile(times, 0.25),
          quantile(times, 0.5),
          quantile(times, 0.75),
          quantile(times, 0.9),
          quantile(times, 0.95))
  
  return(out)
  
}