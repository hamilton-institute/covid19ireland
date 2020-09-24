coupled_seir_model = function(t_phase, t, dt, N1, gamma1, beta1, N2, gamma2, beta2) {
  Ntmp1 = matrix(N1, nrow = 1)
  Ntmp2 = matrix(N2, nrow = 1)
  Time = t
  
  prob1 = pexp(dt, rate = 1/gamma1)
  prob2 = pexp(dt, rate = 1/gamma2)
  
  i = 1
  while (Time[i] < t_phase & 
         (sum(Ntmp1[i, 2:(ncol(Ntmp1)-1)]) > 0 | sum(Ntmp2[i, 2:(ncol(Ntmp2)-1)]) > 0)) {
    # update total number of people per system
    total_N1 = sum(Ntmp1[i,])
    total_N2 = sum(Ntmp2[i,])
    
    Ntmp1 = rbind(Ntmp1, rep(0, length(N1)))
    Ntmp2 = rbind(Ntmp2, rep(0, length(N2)))
    
    # calculate SEIR 1
    movers1 = rbinom(length(Ntmp1[i,2:(ncol(Ntmp1)-1)]), Ntmp1[i,2:(ncol(Ntmp1)-1)], prob1)
    if (sum(Ntmp1[i,2:(ncol(Ntmp1)-1)]) > 0) {
      N_infected1 = rpois(length(prob1), beta1 * Ntmp1[i,2:(ncol(Ntmp1)-1)] * dt * Ntmp1[i,1]/total_N1)
      total_infected1 = min(Ntmp1[i,1], sum(N_infected1))
    } else {
      total_infected1 = 0
    }
    remainder_in_S1 = Ntmp1[i,1] - total_infected1
    departure1 = rbind(
      rbinom(1, size = remainder_in_S1, pexp(dt, rate = 1/wS1S2)),
      rmultinom(c(1, 1), size = total_infected1, prob = c(wS1E1, wS1E2))
    )
    
    # calculate SEIR 2
    movers2 = rbinom(length(Ntmp2[i,2:(ncol(Ntmp2)-1)]), Ntmp2[i,2:(ncol(Ntmp2)-1)], prob2)
    if (sum(Ntmp2[i,2:(ncol(Ntmp2)-1)]) > 0) {
      N_infected2 = rpois(length(prob2), beta2 * Ntmp2[i,2:(ncol(Ntmp2)-1)] * dt * Ntmp2[i,1]/total_N2)
      total_infected2 = min(Ntmp2[i,1], sum(N_infected2))
    } else {
      total_infected2 = 0
    }
    remainder_in_S2 = Ntmp2[i,1] - total_infected2
    departure2 = rbind(
      rbinom(1, size = remainder_in_S2, pexp(dt, rate = 1/wS2S1)),
      rmultinom(c(1, 1), size = total_infected2, prob = c(wS2E2, wS2E1))
    )
    
    Ntmp1[i+1,] = Ntmp1[i,] - 
      cbind(0, movers1[1], movers1[2], 0) + # take away no. of people from E, I compartment
      cbind(0, 0, movers1[1], movers1[2]) + # add movers to I, R compartment
      cbind(-sum(departure1) + departure2[1], departure1[2] + departure2[3], 0, 0)
    
    Ntmp2[i+1,] = Ntmp2[i,] -
      cbind(0, movers2[1], movers2[2], 0) +
      cbind(0, 0, movers2[1], movers2[2]) +
      cbind(-sum(departure2) + departure1[1], departure1[3] + departure2[2], 0, 0)
    
    Time = c(Time, Time[i]+dt)
    i = i + 1
  }
  return(cbind(Time, Ntmp1, Ntmp2))
}