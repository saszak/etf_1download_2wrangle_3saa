# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts_state_machine/sm_engine.R
# Purpose: Core Hysteresis-Aware State Machine Logic
# ==============================================================================

calc_asymmetric_state <- function(dist_vector, thru = 0.04, thrd = -0.02) {
  # Initialize with 0 (Neutral/Down)
  n <- length(dist_vector)
  signal <- rep(0, n)
  curr_state <- 0 
  
  for(i in 1:n) {
    d <- dist_vector[i]
    if(is.na(d)) {
      signal[i] <- 0
      next
    }
    
    # The Hysteresis Logic
    if(curr_state == 0 && d > thru) {
      curr_state <- 1 # Breakout to Trending Up
    } else if(curr_state == 1 && d < thrd) {
      curr_state <- 0 # Breakdown to Neutral/Down
    }
    
    signal[i] <- curr_state
  }
  return(signal)
}