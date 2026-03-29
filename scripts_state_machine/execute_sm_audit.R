##################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts_state_machine/execute_sm_audit.R
# Purpose: Execute Audit using Project Data Dictionary (Relative Strength)
##################################################################################

library(tidyverse)
library(patchwork)
library(scales)
library(xts)

# --- 1. CONNECT TO INFRASTRUCTURE ---
# Source the visuals which internally contain the Engine Room
source("scripts_state_machine/sm_visuals.R") 

# --- 2. CONFIGURATION & TARGET ---
# Using the Global Variables defined in our SYSTEM_DATA_DICTIONARY.md
target_ticker <- "XLK" 
params <- list(thru = 0.04, thrd = -0.02)

# --- 3. VALIDATION CHECK ---
if(!exists("xts_rel_wlth") | !exists("xts_rel")) {
  stop("❌ Critical Error: Global variables 'xts_rel_wlth' or 'xts_rel' not found. 
       Please run 01_etf_wrangle.R first.")
}

# --- 4. EXECUTION VIA ORCHESTRATOR ---
message(paste("🛡️ Auditing Relative Structural Regime for:", target_ticker))

# We use the Orchestrator plot_200DMA because it:
#  A. Calls the math engine (get_enriched_xts_data)
#  B. Handles the 3-panel synchronization (plot_signal_with_dd)
#  C. Registers the result in key_plots
p_audit <- plot_200DMA(
  ticker_symbol = target_ticker,
  xts_wealth    = xts_rel_wlth, 
  xts_price     = xts_rel,
  upper         = params$thru,
  lower         = params$thrd
)

# --- 5. DISPLAY RESULT ---
# The orchestrator returns the plot, but also saves it to the global list
print(p_audit)

##################################################################################