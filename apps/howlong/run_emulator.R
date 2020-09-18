# Run the latest emulator function

run_emulator = function(R0, E, I, R) {
  
  ans = system(paste("python3 emulator.py",R0, E, I, R), intern = TRUE)
  out = as.numeric(strsplit(ans[3], "  ")[[1]][-1])
  
  # Now produce the predictions
  names(out) = c("q5", "q10", "q25", "q50", "q75", "q90", "q95")
  
  # Check for weird values - i.e. not in order
  if(any(diff(order(out))<0)) warning('Some values not in correct order - more emulator runs required')
  
  return(out)
}

# source('run_simulator.R')
# truth = run_simulator(0.55,347,437,884858)
# pred = run_emulator(0.55,347,437,884858)

# truth = run_simulator(1.1,25,35,300000)
# pred = run_emulator(1.1,25,35,300000)

# truth = run_simulator(1.2,2000,2000,300000)
# pred = run_emulator(1.2,2000,2000,300000)
