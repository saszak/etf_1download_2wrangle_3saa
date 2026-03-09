# ==============================================================================
# PROJECT B: ETF_SAA_TAA - 00_BRIDGE_CONFIG.R (HARD-ANCHORED)
# Purpose: Map Warehouse (00) and Refinery (01) via Absolute Paths
# ==============================================================================

library(tidyverse)
library(xts)

# 1. PATH ANCHORING
# Detect if we are running via Master Orchestrator or Standalone
if (!exists("base_path")) {
  if (Sys.getenv("RSTUDIO") == "1") {
    # Standalone mode: We are inside 02_etf_saa_taa, so root is one level up
    base_path <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
  } else {
    base_path <- getwd()
  }
}

# Define Explicit Paths
warehouse_path <- file.path(base_path, "00_etf_data_download", "data_raw")
refinery_path  <- file.path(base_path, "01_etf_wrangle", "data_processed")

# 2. INJECT MASTER UNIVERSE MAP
# Ensuring the universe is loaded regardless of entry point
source(file.path(base_path, "00_etf_data_download", "01_init_universe.R"))

# 3. DATA INGESTION
# We check for the primary refinery file to validate the bridge
target_check <- file.path(refinery_path, "rel_ret_d.rds")

if (file.exists(target_check)) {
  
  # A. Load ABSOLUTE Data from Warehouse (00) 
  xts_d_abs <- read_rds(file.path(warehouse_path, "abs_ret_d.rds"))
  xts_w_abs <- read_rds(file.path(warehouse_path, "abs_ret_w.rds"))
  xts_m_abs <- read_rds(file.path(warehouse_path, "abs_ret_m.rds"))
  xts_q_abs <- read_rds(file.path(warehouse_path, "abs_ret_q.rds"))
  
  # B. Load RELATIVE Returns from Refinery (01)
  xts_d_rel <- read_rds(file.path(refinery_path, "rel_ret_d.rds"))
  xts_w_rel <- read_rds(file.path(refinery_path, "rel_ret_w.rds"))
  xts_m_rel <- read_rds(file.path(refinery_path, "rel_ret_m.rds"))
  xts_q_rel <- read_rds(file.path(refinery_path, "rel_ret_q.rds"))
  
  # C. Load Signal Table (Renamed per instruction)
  sig_path <- file.path(refinery_path, "signal_table.rds")
  if(file.exists(sig_path)) {
    signal_table <- read_rds(sig_path)
  }
  
  message("🚀 Bridge Operational | Anchored at: ", base_path)
  
} else {
  stop("❌ CRITICAL: Bridge failed. Refinery data not found at: ", refinery_path)
}