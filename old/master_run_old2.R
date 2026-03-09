# ==============================================================================
# SOVEREIGN MASTER: 1-DOWNLOAD, 2-WRANGLE, 3-SAA (2026)
# FILE PATH: ./master_run.R
# ==============================================================================

library(tidyverse)
library(xts)
library(tidyquant)

# 1. PROJECT ANCHOR
if (Sys.getenv("RSTUDIO") == "1") {
  base_path <- dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  base_path <- getwd()
}
setwd(base_path)

project_dirs <- c("data_raw", "data_processed", "output/plots", "output/reports")
walk(project_dirs, ~ if(!dir.exists(.x)) dir.create(.x, recursive = TRUE))

# 2. LOAD MODULES
source(file.path(base_path, "00_etf_data_download", "01_init_universe.R"))
source(file.path(base_path, "00_etf_data_download", "02_data_loaders.R"))
source(file.path(base_path, "01_etf_wrangle", "03_audit_sanity.R"))
source(file.path(base_path, "01_etf_wrangle", "04_core_analytics.R"))
source(file.path(base_path, "01_etf_wrangle", "05_visual_engine.R")) 
source(file.path(base_path, "xts_initialize.R"))
source(file.path(base_path, "03_reporting_engine", "01_performance_report.R"))
source(file.path(base_path, "03_reporting_engine", "03_advanced_visuals.R"))

# 3. STEP 1: DOWNLOAD
if(is_sync_required()) sync_etf_prices(all_tickers)

# 4. STEP 2: WRANGLE
freqs <- c("d", "w", "m", "q")
abs_list <- setNames(lapply(freqs, function(f) refine_prices_to_returns(suffix = f)), freqs)

# --- CRITICAL FIX: STRIP INTEGER INDEX ---
# We rebuild signal_table by extracting coredata and re-applying a Date-classed index
signal_table <- xts(coredata(abs_list$d), order.by = as.Date(index(abs_list$d)))
tclass(signal_table) <- "Date" 

source(file.path(base_path, "01_etf_wrangle", "06_advanced_analytics.R"))

# ==============================================================================
# SOVEREIGN MASTER: 1-DOWNLOAD, 2-WRANGLE, 3-SAA (2026)
# FILE PATH: ./master_run.R
# ==============================================================================

library(tidyverse)
library(xts)
library(tidyquant)

# 1. ANCHOR
if (Sys.getenv("RSTUDIO") == "1") {
  base_path <- dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  base_path <- getwd()
}
setwd(base_path)

# 2. LOAD MODULES
source(file.path(base_path, "00_etf_data_download", "01_init_universe.R"))
source(file.path(base_path, "00_etf_data_download", "02_data_loaders.R"))
source(file.path(base_path, "01_etf_wrangle", "04_core_analytics.R"))
source(file.path(base_path, "03_reporting_engine", "01_performance_report.R"))
source(file.path(base_path, "03_reporting_engine", "03_advanced_visuals.R"))

# 3. STEP 1 & 2: DATA PREP
if(is_sync_required()) sync_etf_prices(all_tickers)
freqs <- c("d", "w", "m", "q")
abs_list <- setNames(lapply(freqs, function(f) refine_prices_to_returns(suffix = f)), freqs)

# --- THE LAST WORKING RESTORATION: CONVERT TO DF TO KILL INTEGER ERROR ---
# This creates a clean dataframe 'signal_df' that has no xts "integer" baggage
signal_table <- abs_list$d
signal_df <- fortify.zoo(signal_table) %>% 
  rename(date = Index) %>% 
  mutate(date = as.Date(date))

# 4. STEP 3: REPORTING
message("📊 Executing Sovereign Intelligence Dossier...")

# A. Snapshot Logic - If your function requires XTS, we force the attribute here
tclass(signal_table) <- "Date"
snap_raw <- get_project_snapshot_from_xts(signal_table, etf_metadata)
render_project_snapshot(snap_raw)

# B. Export Visuals (Bar Plots with Outlier Management)
# ------------------------------------------------------------------------------

# 1. Analytical Sigma (Bar Plot + Text Labels)
ggsave(file.path(base_path, "output/plots/02_analytical_sigma.png"), 
       plot_sigma_analysis(signal_table, etf_metadata), width = 10, height = 10)

# 2. Precision YTD (15% Capped Bar Plot)
ggsave(file.path(base_path, "output/plots/03_precision_ytd.png"), 
       plot_precision_ytd(signal_table, etf_metadata), width = 10, height = 12)

message("🏁 RESTORED MASTER COMPLETE.")
# ------------------------------------------------------------------------------
message("📊 Executing Sovereign Intelligence Dossier...")

# A. Console Snapshot (The crash point)
# Now passing a signal_table with a verified Date index
snap_raw <- get_project_snapshot_from_xts(signal_table, etf_metadata)
if(!is.null(snap_raw)) render_project_snapshot(snap_raw)

# B. Export Visuals (Bar Plots & Heatmaps)
# ------------------------------------------------------------------------------

# 01 Overview
ggsave(file.path(base_path, "output/plots/01_overview_optical.png"), 
       plot_optical_overview(abs_list, etf_metadata), width = 12, height = 8)

# 02 Analytical Sigma (Outlier Management + Labels)
ggsave(file.path(base_path, "output/plots/02_analytical_sigma.png"), 
       plot_sigma_analysis(signal_table, etf_metadata), width = 10, height = 10)

# 03 Precision YTD (15% Cap + Labels)
ggsave(file.path(base_path, "output/plots/03_precision_ytd.png"), 
       plot_precision_ytd(signal_table, etf_metadata), width = 10, height = 12)

# 04 Synthetic Efficiency
ggsave(file.path(base_path, "output/plots/04_synthetic_efficiency.png"), 
       plot_synthetic_efficiency(signal_table, etf_metadata), width = 10, height = 8)

message("🏁 MASTER EXECUTION COMPLETE.")