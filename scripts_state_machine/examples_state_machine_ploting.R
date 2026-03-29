
##################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts_state_machine/run_relative_audit.R
# Purpose: Execute Direct Relative Strength Audit using Global Registry
##################################################################################

# --- 1. SOURCE VISUALS ---
# Ensure the library is in memory
source("./scripts_state_machine/sm_visuals.R")

# --- 2. RUNNABLE CALL (DIRECT GLOBALS) ---
# We bypass the 'bridge' and use the Stage 01 output directly.
# xts_rel_wealth: The Wealth Index (Base 1.0)
# xts_rel:        The Daily Returns (for Drawdown panel)

p_xlk_audit <- plot_200DMA(
  ticker_symbol = "XLE", 
  xts_wealth    = xts_rel_wlth, # DIRECT GLOBAL USE
  xts_price     = xts_rel,        # DIRECT GLOBAL USE
  upper         = 0.04, 
  lower         = -0.02
)

# --- 3. VERIFY REGISTRY ---
# Check if the plot was cached in the 'key_plots' list
if(exists("key_plots") && "audit_xlk" %in% names(key_plots)) {
  message("📊 Sentinel Logic: XLK Audit registered in key_plots$audit_xlk")
}

##################################################################################
# SYSTEM RECAP:
# - NO BRIDGE: We use 'xts_rel_wealth' as the source of truth.
# - MATH: The Hysteresis oscillator is calculated on the 1.0-base Wealth Index.
# - VISUALS: The result is a 3-panel stack with synchronized yearly grids.
# - STORAGE: The ggplot object is ready for use in your Shiny UI.
##################################################################################

##################################################################################
# WHAT IS HAPPENING UNDER THE HOOD:
##################################################################################

# A. ENRICHMENT:
# get_enriched_xts_data(xts_wlth[, "XLK"]) creates:
# Adjusted (The Wealth Index starting at 1.0)
# SMA200   (The 200-day Trend of that index)
# Dist200  (The % distance from that trend)
# Signal   (The 1/0 Hysteresis state)

# B. VISUALIZATION:
# plot_signal_with_dd() then takes that data and:
# 1. Plots the Price/SMA/Regime colors.
# 2. Plots the Oscillator with +4%/-2% dashed lines.
# 3. Synchronizes with the daily 'xts_rel' to plot the Alpha Drawdown.
# 4. Forces the Left Margin to 60 (or 80) to align the Yearly Grids.

# C. STORAGE:
# The plot is rendered and the enriched XTS is returned to 'xlk_audit_results'
##################################################################################