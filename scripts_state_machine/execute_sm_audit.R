# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts_state_machine/execute_sm_audit.R
# Purpose: Run institutional-grade state audit for a specific ticker
# ==============================================================================

library(quantmod)
library(tidyverse)
library(patchwork)
library(scales)

# Load our new modules
source("scripts_state_machine/sm_engine.R")
source("scripts_state_machine/sm_visuals.R") # Ensure plot_sm_institutional is here

# --- CONFIGURATION ---
target_ticker <- "SPY"
params <- list(thru = 0.04, thrd = -0.02)

# --- EXECUTION ---
message(paste("🛡️ Auditing Structural Regime for:", target_ticker))

# 1. Get Data
getSymbols(target_ticker, from = "2018-01-01", auto.assign = TRUE)
df_raw <- get(target_ticker)

# 2. Wrangle & Calculate Distance
df_processed <- data.frame(Date = index(df_raw), Close = as.numeric(Cl(df_raw))) %>%
  mutate(
    SMA200  = TTR::SMA(Close, n = 200),
    Dist200 = (Close / SMA200) - 1
  ) %>%
  filter(!is.na(SMA200))

# 3. Apply State Machine
df_processed$Signal <- calc_asymmetric_state(
  df_processed$Dist200, 
  thru = params$thru, 
  thrd = params$thrd
)

# 4. Generate Institutional Plot
p <- plot_sm_institutional(
  df_processed, 
  ticker_name = target_ticker, 
  thru = params$thru, 
  thrd = params$thrd
)

# 5. Display Result
print(p)
