# ==============================================================================
# SOVEREIGN MASTER: 1-DOWNLOAD, 2-WRANGLE, 3-SAA (2026)
# ==============================================================================

# 1. HARD ANCHOR (Project North Star)
if (Sys.getenv("RSTUDIO") == "1") {
  base_path <- dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  base_path <- getwd()
}

setwd(base_path)
message("📍 Sovereign Root Anchored: ", base_path)

# 2. LOAD MODULES
source(file.path(base_path, "00_etf_data_download", "01_init_universe.R"))
source(file.path(base_path, "00_etf_data_download", "02_data_loaders.R"))
source(file.path(base_path, "01_etf_wrangle", "03_audit_sanity.R"))
source(file.path(base_path, "01_etf_wrangle", "04_core_analytics.R"))
source(file.path(base_path, "01_etf_wrangle", "05_visual_engine.R")) 
source(file.path(base_path, "xts_initialize.R"))

message("🚀 Sovereign Pipeline Synchronized.")

# 3. STEP 1: DOWNLOAD (WAREHOUSE)
if(is_sync_required()) {
  sync_etf_prices(all_tickers)
}

# 4. STEP 2: WRANGLE (REFINERY)
freqs <- c("d", "w", "m", "q")

# --- A. Warehouse Refinement (Absolute Returns) ---
abs_list <- setNames(lapply(freqs, function(f) {
  refine_prices_to_returns(suffix = f)
}), freqs)

# --- B. Refinery Refinement (Relative Spreads) ---
rel_list <- lapply(freqs, function(f) {
  calculate_relative_returns(abs_list[[f]], bmk = "SPY", suffix = f)
})

# --- C. SIGNAL GENERATION (Mapped to your filename) ---
# Updating this line to match your file: 06_advanced_analytics.R
source(file.path(base_path, "01_etf_wrangle", "06_advanced_analytics.R"))

# 5. STEP 3: STRATEGY BRIDGE (SAA/TAA)
source(file.path(base_path, "02_etf_saa_taa", "00_bridge_config.R"))

message("🏁 MASTER EXECUTION COMPLETE: 2026 Alpha Terminal is Hot.")