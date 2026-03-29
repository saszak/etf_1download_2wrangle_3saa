# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts_saa_taa/05_saa_portfolio.R
# Purpose: Strategic Asset Allocation (SAA) - Benchmark Synthesis
# ==============================================================================

library(PerformanceAnalytics)
library(tidyverse)
library(xts)

#' Generate SAA Benchmark Portfolio
#' @param xts_returns XTS of daily asset returns
#' @param weights_vector Named numeric vector (e.g., c(SPY=0.6, IEF=0.4))
#' @param rebalance_freq "months", "quarters", or "years"
#' @return A list with returns, drift-weights, and performance stats

calc_saa_portfolio <- function(xts_returns, weights_vector, rebalance_freq = "months") {
  
  # 1. Validation
  valid_freqs <- c("months", "quarters", "years")
  if(!(rebalance_freq %in% valid_freqs)) stop("Use: months, quarters, or years.")
  if(abs(sum(weights_vector) - 1) > 0.001) stop("Weights must sum to 1.0.")
  
  # 2. Execution
  tickers <- names(weights_vector)
  port_obj <- Return.portfolio(
    R = xts_returns[, tickers],
    weights = weights_vector,
    rebalance_on = rebalance_freq,
    verbose = TRUE
  )
  
  # 3. Packaging
  result <- list(
    returns     = port_obj$returns,
    weights_bop = port_obj$BOP.Weight, 
    stats       = table.AnnualizedReturns(port_obj$returns)
  )
  
  colnames(result$returns) <- paste0("SAA_", paste(tickers, collapse="_"))
  return(result)
}

# ==============================================================================
# EXAMPLE USE: THE 60/40 CASE STUDY
# (wrapped in if(FALSE) so source() only loads the function above)
# ==============================================================================
if (FALSE) {

  # 1. Define your Targets
  case_weights <- c("SPY" = 0.60, "IEF" = 0.40)

  # 2. Run different rebalancing scenarios to see the impact
  saa_monthly   <- calc_saa_portfolio(xts_ret, case_weights, "months")
  saa_quarterly <- calc_saa_portfolio(xts_ret, case_weights, "quarters")
  saa_yearly    <- calc_saa_portfolio(xts_ret, case_weights, "years")

  # 3. Combine for Comparison
  comparison <- cbind(saa_monthly$returns, saa_quarterly$returns, saa_yearly$returns)
  colnames(comparison) <- c("Monthly_Rebal", "Quarterly_Rebal", "Yearly_Rebal")

  # 4. Visualizing the "Rebalancing Bonus"
  charts.PerformanceSummary(comparison, main = "60/40 SAA: Rebalancing Frequency Comparison")

  # --- Case Study 2 ---
  case_weights <- c("SPY" = 0.60, "IEF" = 0.40)

  saa_q_output <- calc_saa_portfolio(
    xts_returns    = xts_ret,
    weights_vector = case_weights,
    rebalance_freq = "quarters"
  )

  comp_plot_data <- merge(xts_ret$SPY, xts_ret$IEF, saa_q_output$returns)
  colnames(comp_plot_data) <- c("SPY (Equity)", "IEF (Bonds)", "SAA 60/40 (Q-Rebal)")

  charts.PerformanceSummary(
    comp_plot_data,
    main     = "Case Study: SAA 60/40 vs. Components",
    colorset = c("#A8A8A8", "#D1D1D1", "#2E5A88"),
    lwd      = c(1, 1, 3),
    legend.loc = "topleft"
  )

}


