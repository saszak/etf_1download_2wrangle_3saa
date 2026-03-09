# ==============================================================================
# MODULE 01: 06_ADVANCED_ANALYTICS (THE BRAIN)
# ==============================================================================
library(tidyverse)
library(xts)

# 1. PATH ANCHORING
if(!exists("base_path")) {
  base_path <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
}

# 2. DEPENDENCY CHECK
if(!exists("calc_rolling_vol")) {
  source(file.path(base_path, "01_etf_wrangle", "04_core_analytics.R"))
}

# 3. DATA INGESTION
path_rel <- file.path(base_path, "01_etf_wrangle", "data_processed", "rel_ret_d.rds")
path_abs <- file.path(base_path, "00_etf_data_download", "data_raw", "abs_ret_d.rds")

xts_rel <- read_rds(path_rel)
xts_abs <- read_rds(path_abs)

message("🧠 Advanced Analytics: Processing ", ncol(xts_rel), " tickers...")

# 4. CALCULATIONS
# A. 3-Month Momentum (63 days)
mom_3m <- colSums(tail(xts_rel, 63), na.rm = TRUE) %>% 
  as.data.frame() %>% 
  rename(Mom_3M = ".") %>% 
  rownames_to_column("ticker") # Changed from 'symbol' to 'ticker'

# B. 20-Day Volatility
vol_20d <- tail(calc_rolling_vol(xts_abs, window = 20), 1) %>% 
  as.numeric()

# 5. CONSTRUCT SIGNAL TABLE
# Ensure etf_metadata is present and use 'ticker' column
if(!exists("etf_metadata")) {
  source(file.path(base_path, "00_etf_data_download", "01_init_universe.R"))
}

# Check if metadata uses 'symbol' or 'ticker' and adjust
if("symbol" %in% colnames(etf_metadata)) {
  etf_metadata <- etf_metadata %>% rename(ticker = symbol)
}

signal_table <- etf_metadata %>%
  left_join(mom_3m, by = "ticker") %>%
  mutate(
    Vol_20d = vol_20d[match(ticker, colnames(xts_abs))],
    Alpha_Score = round(Mom_3M / Vol_20d, 4)
  ) %>%
  filter(!is.na(Mom_3M)) %>%
  arrange(desc(Alpha_Score))

# 6. THE CRITICAL SAVE
save_path <- file.path(base_path, "01_etf_wrangle", "data_processed", "signal_table.rds")
write_rds(signal_table, save_path)

message("💾 SUCCESS: signal_table.rds saved with 'ticker' key.")
message("✅ 06_advanced_analytics.R: Complete.")