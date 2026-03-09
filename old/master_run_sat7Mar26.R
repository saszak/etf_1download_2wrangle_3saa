# ==============================================================================
# SOVEREIGN MASTER: 1-DOWNLOAD, 2-WRANGLE, 3-SAA (2026)
# ==============================================================================

library(tidyverse)
library(xts)
library(tidyquant)
library(rmarkdown)
library(here)

# 1. HARD ANCHOR
base_path <- "/Volumes/T7Red_Work/Rstudio_ssd/etf_1download_2wrangle_3saa"
setwd(base_path)

# Ensure 'data_processed' exists
if(!dir.exists("data_processed")) dir.create("data_processed")

# 2. LOAD MODULES
source(file.path(base_path, "00_etf_data_download", "01_init_universe.R"))
source(file.path(base_path, "00_etf_data_download", "02_data_loaders.R"))
source(file.path(base_path, "01_etf_wrangle", "04_core_analytics.R"))
source(file.path(base_path, "03_reporting_engine", "01_performance_report.R"))
source(file.path(base_path, "03_reporting_engine", "03_advanced_visuals.R"))
source(file.path(base_path, "03_reporting_engine", "05_shiny_builder.R")) # New Builder

# 3. DATA REFINERY
if(is_sync_required()) sync_etf_prices(all_tickers)
freqs <- c("d", "w", "m", "q")
abs_list <- setNames(lapply(freqs, function(f) refine_prices_to_returns(suffix = f)), freqs)

# --- 🧪 THE ATOMIC REPAIR: STRIP & REBUILD ---
raw_matrix  <- as.matrix(abs_list$d)
clean_dates <- as.Date(index(abs_list$d))
signal_table <- xts(raw_matrix, order.by = clean_dates)
tclass(signal_table) <- "Date"
tzone(signal_table)  <- "UTC"

# Save for the Dashboard
saveRDS(signal_table, file.path(base_path, "data_processed", "signal_table.rds"))
message("💾 Clean signal_table.rds parked in data_processed/")

# 4. EXECUTE SHINY ENGINE
# ------------------------------------------------------------------------------
# This generates the Rmd and opens the interactive Shiny session
build_and_launch_shiny(base_path)

message("🏁 MASTER EXECUTION COMPLETE.")