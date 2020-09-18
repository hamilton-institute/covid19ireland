seir_model = function(t_phase, time, dt, N, mean_holding_times, beta, SYS_N, sub_names) {
  
  # summary matrix
  Ntmp = matrix(N, nrow = 1)
  N_compt = length(N)
  colnames(Ntmp) = colnames(N)
  
  # actual matrix to calculate with sub-compartments
  Ntmp_calc = matrix(SYS_N, nrow = 1)
  N_sub_compt = length(SYS_N)
  colnames(Ntmp_calc) = colnames(SYS_N)
  
  Time = time
  total_N = sum(Ntmp)
  
  # probability of E->I and I->R
  p = NULL
  E_length = length(sub_names[["E"]])
  I_length = length(sub_names[["I"]])
  for (E in 1:E_length) { p = c(p, pexp(dt, rate = E_length/mean_holding_times[1])) }
  for (I in 1:I_length) { p = c(p, pexp(dt, rate = I_length/mean_holding_times[2])) }
  
  i = 1
  while (Time[i] < t_phase & sum(Ntmp_calc[i,c(sub_names$E, sub_names$I)]) > 0) {
    Ntmp = rbind(Ntmp, rep(0, N_compt)) # append new empty row
    Ntmp_calc = rbind(Ntmp_calc, rep(0, N_sub_compt))
    
    # calculate number of people needs to be moved E -> I and I -> R
    movers = rbinom(length(c(sub_names$E, sub_names$I)), Ntmp_calc[i,c(sub_names$E, sub_names$I)], p)
    
    # calculate sum of exposed and infectious people 
    # => if it's greater than 0, there's a chance to infect susceptible people
    total_infected = 0
    if (sum(Ntmp_calc[i,c(sub_names$E, sub_names$I)]) > 0) {
      total_S = sum(Ntmp_calc[i,sub_names$S])
      total_E = sum(Ntmp_calc[i,sub_names$E])
      total_I = sum(Ntmp_calc[i,sub_names$I])
      N_infected = rpois(2, beta * c(total_E, total_I) * dt * total_S/total_N)
      total_infected = min(total_S, sum(N_infected))
    }
    
    # update number of people in each compartment
    Ntmp_calc[i+1,] = Ntmp_calc[i,] - 
      c(total_infected, movers, 0) + # remove from S, E, I compartment
      c(0, total_infected, movers) # add to E, I, R compartment
    
    Ntmp[i+1,] = c(
      sum(Ntmp_calc[i+1,sub_names$S]),
      sum(Ntmp_calc[i+1,sub_names$E]),
      sum(Ntmp_calc[i+1,sub_names$I]),
      sum(Ntmp_calc[i+1,sub_names$Rec])
    )
    
    Time = c(Time, Time[i]+dt)
    i = i + 1
  }
  
  return(max(Time))
}