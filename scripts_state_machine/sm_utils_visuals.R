# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts_state_machine/sm_utils_visuals.R
# Purpose: Universal Operational Wrappers for State Machine Audits
# ==============================================================================

# Ensure the base engine is loaded
source("./scripts_state_machine/sm_visuals.R")

#' Explicitly plot a single ticker from a registry
plot_ticker_explicit <- function(registry, ticker, params) {
  
  xts_data <- registry[[ticker]]
  
  if (is.null(xts_data)) {
    stop(paste("❌ Ticker [", ticker, "] not found in the provided registry."))
  }
  
  # Call the base engine defined in sm_visuals.R
  p <- plot_signal_xts(
    xts_data    = xts_data, 
    ticker_name = ticker, 
    thru        = params$thru, 
    thrd        = params$thrd
  )
  
  return(p)
}

#' plot_master_comparison: Universal Multi-Ticker Stacker
#' Handles 1 to N tickers using the '...' argument
plot_master_comparison <- function(registry, params, ...) {
  
  # 1. Capture the tickers passed in
  tickers <- list(...)
  
  if (length(tickers) == 0) {
    stop("❌ Please provide at least one ticker symbol (e.g., 'SPY', 'GLD').")
  }
  
  message(paste("🛰️ Generating Master Comparison for:", paste(tickers, collapse = ", ")))
  
  # 2. Generate all plots using map
  # wrap_elements ensures each dual-panel plot is treated as one unit
  plots <- purrr::map(tickers, function(t) {
    p <- plot_ticker_explicit(registry, t, params)
    patchwork::wrap_elements(p)
  })
  
  # 3. Reduce the list into a single vertical stack
  combined_plot <- purrr::reduce(plots, `/`)
  
  return(combined_plot)
}

message("✅ Visual Utilities Loaded: plot_master_comparison() is now the primary interface.")