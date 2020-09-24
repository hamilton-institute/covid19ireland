seir_model = function(t_phase, time, dt, N, mean_holding_times, beta) {
  N_compt = length(N)
  Ntmp = matrix(N, nrow = 1)
  Time = 0
  total_N = sum(Ntmp)
  
  p = pexp(dt, rate = 1/mean_holding_times, lower.tail = TRUE, log.p = FALSE)
  
  i = 1
  while (Time[i] < t_phase & sum(Ntmp[nrow(Ntmp), 2:(ncol(Ntmp)-1)]) > 0) {
    Ntmp = rbind(Ntmp, rep(0, N_compt)) # append new empty row
    
    # calculate number of people needs to be moved E -> I and I -> R
    movers = rbinom(length(Ntmp[i,2:(ncol(Ntmp)-1)]), Ntmp[i,2:(ncol(Ntmp)-1)], p)
    
    #### Ken's calculation using a normal distribution (NB: not working here...)
    # movers = rep(0, ncol(Ntmp)) # a vector to store number of migrants
    # for (j in 1:length(p)) {
    #   if (Ntmp[i,j+1]*p[j] < 1000) {
    #     movers[j+1] = rbinom(1, Ntmp[i,j+1], p[j])
    #   }
    #   else {
    #     movers[j+1] = min(floor(rnorm(1, Ntmp[i,j+1] * p[j], sqrt(p[j] * (1 - p[j]) * Ntmp[i,j+1]))), Ntmp[i,j+1])
    #   }
    # }
    
    # calculate sum of exposed and infectious people 
    # => if it's greater than 0, there's a chance to infect susceptible people
    if (sum(Ntmp[i,2:(ncol(Ntmp)-1)]) > 0) {
      N_infected = rpois(length(p), beta * Ntmp[i,2:(ncol(Ntmp)-1)] * dt * Ntmp[i,1]/total_N)
      total_infected = min(Ntmp[i,1], sum(N_infected))
    }
    
    # update number of people in each compartment
    Ntmp[i+1,] = Ntmp[i,] - 
      cbind(0, movers[1], movers[2], 0) + # take away no. of people from E, I compartment
      cbind(0, 0, movers[1], movers[2]) + # add movers to I, R compartment
      cbind(-total_infected, total_infected, 0, 0) # S -> E
    
    Time = c(Time, Time[i]+dt)
    i = i + 1
  }
  
  return(cbind(Time, Ntmp))
}