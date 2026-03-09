# ==============================================================================
# SCRIPT: xts_initialize.R
# Purpose: Load Raw ETF DB and create Synchronized XTS Objects (Length Matched)
# ==============================================================================
library(tidyverse)
library(timetk)
library(PerformanceAnalytics)

message("⏳ Initializing XTS Data Objects...")

# 1. Load Raw Database
raw_db <- read_rds(file.path(base_path, "data_raw/raw_p_d.rds"))

# 2. Create xts_p (Prices - Wide/Synchronized)
xts_p <- raw_db %>%
  select(date, symbol, adjusted) %>%
  pivot_wider(names_from = symbol, values_from = adjusted) %>%
  arrange(date) %>%
  tk_xts(date_var = date, silent = TRUE)

# 3. Create xts_r (Returns - Wide/Synchronized)
# Replace na.omit() logic: Calculate, then force first row to 0
xts_r <- Return.calculate(xts_p)
xts_r[1, ] <- 0

message("✅ 'xts_p' and 'xts_r' initialized. First return row set to 0.")