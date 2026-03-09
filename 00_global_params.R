# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./00_global_params.R
# ==============================================================================

# 1. STRATEGY THRESHOLDS (3State Machine)
sm_params <- list(
  thru = 0.04, 
  thrd = -0.02,
  lookback_sma = 200
)

# 2. WATCHLIST DEFINITIONS
# Update in 00_global_params.R
my_watchlist <- c("SPY", "TLT", "GLD", "URTH", "XLK")
# 3. DATE WINDOWS (Aligned to your 2020 Data Start)
time_params <- list(
  analysis_start = "2020-01-01",  # Matches your Floor 1 availability
  report_start   = "2024-01-01"   # Focuses the "Institutional" plot on recent history
)

# 4. SAA REGIME MAPPING (For the next stage)
regime_labels <- list(
  "1" = "Risk-On",
  "0" = "Risk-Off"
)

source("./project_tree.R")
message("✅ Global Parameters Loaded: Data Window starts 2020-01-01.")