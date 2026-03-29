# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts_state_machine/sm_engine.R
# Purpose: Core Hysteresis-Aware State Machine Logic & Enrichment
# ==============================================================================


##################################################################################
#' Core Asymmetric State Logic
#' @description Implements Hysteresis: Must cross 'thru' to turn ON, 'thrd' to turn OFF.
calc_asymmetric_state <- function(dist_vector, thru = 0.04, thrd = -0.02) {
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

##################################################################################

#' Get Enriched xts_data for Plotting
#' 
#' @param xts_wlth_ticker Single column of the wealth index (e.g. xts_wlth[, "XLK"])
#' @param thru Upper threshold (default 0.04)
#' @param thrd Lower threshold (default -0.02)
#' @return Enriched xts with columns: Adjusted, SMA200, Dist200, Signal
get_enriched_xts_data <- function(xts_wlth_ticker, thru = 0.04, thrd = -0.02) {
  
  ticker_name <- colnames(xts_wlth_ticker)
  message(paste("⚙️ Running State Machine Enrichment (using Wealth Index) for:", ticker_name))
  
  # 1. Use existing Wealth Index as the "Adjusted" price
  # We rename the column to match the plotting script's expectations
  wealth <- xts_wlth_ticker
  colnames(wealth) <- "Adjusted"
  
  # 2. Calculate Technical Anchors
  # Note: rollmean from zoo is used for the 200D trend line
  sma_200  <- rollmean(wealth, k = 200, fill = NA, align = "right")
  dist_200 <- (wealth / sma_200) - 1
  
  # 3. Apply Asymmetric State Logic (Memory-Aware)
  states <- calc_asymmetric_state(as.numeric(dist_200), thru = thru, thrd = thrd)
  
  # 4. Assemble Enriched Object
  # Columns: Adjusted (Price), SMA200 (Line), Dist200 (Oscillator), Signal (Regime)
  xts_data <- cbind(wealth, sma_200, dist_200, states)
  colnames(xts_data) <- c("Adjusted", "SMA200", "Dist200", "Signal")
  
  return(xts_data)
}


##################################################################################