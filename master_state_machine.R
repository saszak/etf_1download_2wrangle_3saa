# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./master_state_machine.R
# Purpose: Central Command - Signal Monitoring, Regime Audits & Macro Comparison
# ==============================================================================

# 1. ENVIRONMENT & UTILITIES
# ------------------------------------------------------------------------------
source("./00_global_params.R")
source("./scripts_state_machine/sm_utils_visuals.R")

# 2. DATA INGESTION
# ------------------------------------------------------------------------------
signal_registry <- readRDS(project_tree$products$signal_registry)

# 3. GLOBAL STATUS MONITOR (Console Output)
# ------------------------------------------------------------------------------
message("\n================================================================")
message("🛰️  MASTER STATE MACHINE: LIVE REGIME STATUS")
message("================================================================")

purrr::walk(names(signal_registry), function(t) {
  df_xts <- signal_registry[[t]]
  last_row <- tail(df_xts, 1)
  
  # Calculate Regime Duration
  # We find the most recent change in the 'Signal' column
  signal_vec <- as.numeric(df_xts$Signal)
  last_signal <- tail(signal_vec, 1)
  changes <- which(diff(signal_vec) != 0)
  last_change_idx <- if(length(changes) > 0) max(changes) else 0
  days_in_regime <- nrow(df_xts) - last_change_idx
  
  # Determine Visual Label
  regime_label <- case_when(
    last_row$Signal == 1 ~ "BULL (Risk-On)",
    last_row$Signal == 0 & last_row$Dist200 >= sm_params$thrd ~ "NEUTRAL (Wait)",
    last_row$Signal == 0 & last_row$Dist200 < sm_params$thrd ~ "BEAR (Risk-Off)"
  )
  
  dist_pct <- round(as.numeric(last_row$Dist200) * 100, 2)
  
  cat(sprintf("Asset: %-5s | Regime: %-18s | Days: %-4d | Dist200: %6.2f%%\n", 
              t, regime_label, days_in_regime, dist_pct))
})
message("================================================================\n")

# 4. VISUAL AUDITS & COMPARISONS
# ------------------------------------------------------------------------------

# CHOICE A: Single Asset Deep-Dive
# plot_ticker_explicit(signal_registry, "XLK", sm_params)

# CHOICE B: Macro Divergence Check (Default)
compare_tickers_explicit(
  registry = signal_registry, 
  ticker1  = "XLK", 
  ticker2  = "GLD", 
  ticker3  = "SPY", 
  params   = sm_params
)