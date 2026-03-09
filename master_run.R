# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./master_run.R
# Purpose: Master Execution Script - Sentinel Intelligence Pipeline
# Architecture: Global Environment Orchestration with Floor 2 Integration
# ==============================================================================

# --- 0. INITIALIZE INFRASTRUCTURE ---
rm(list = ls()) # Clear environment for a clean sentinel run

if(!file.exists("project_tree.R")) stop("Critical Error: project_tree.R not found!")
source("project_tree.R")
source("00_global_params.R") # Load Signal-Project thresholds (+4%/-2%)

# --- 1. GLOBAL PARAMETERS (The Control Center) ---
OPTIONS <<- list(
  outlier_method        = "Interquartile", #
  sigma_label_suffix    = " (Analytical Sigma)", #
  show_outliers_in_plot = FALSE,
  force_fresh_sync      = FALSE,      # Smart-cache toggle
  run_external_audit    = TRUE        # Finnhub API Check
)

# --- 1.1 BOOTSTRAP ENVIRONMENT ---
source(project_tree$scripts$init)

# --- 1.5 CACHE BUSTER (Sentinel Guard) ---
if(OPTIONS$force_fresh_sync) {
  target_cache <- project_tree$products$raw_p_d
  if(file.exists(target_cache)) {
    message("🗑️ [WIPE] Removing old cache to force fresh baseline...")
    file.remove(target_cache)
  }
}

cat("\n--- STARTING SENTINEL ETF PIPELINE [", as.character(Sys.time()), "] ---\n")

# --- 2. STAGE 01: DATA ACQUISITION & WRANGLING ---
cat("\n[1/8] Stage 01: Wrangling Market Data...")
source(project_tree$scripts$wrangle) 

# --- 3. STAGE 01b: FLOOR 2 - SIGNAL ENGINE (Signal-Project) ---
# New: Generates the Hysteresis-aware 200D-MA Registry
cat("\n[2/8] Stage 01b: Building 3State Machine Signal Registry...")
source(project_tree$scripts$sm_batch) # Executes batch_enrich_registry()

# --- 4. STAGE 01c: TECHNICAL SIGNALS (LEGACY) ---
cat("\n[3/8] Stage 01c: Generating Standard Moving Averages...")
source(project_tree$scripts$signals)

# --- 5. STAGE 02: SAA & ANALYTICAL SIGMA ---
# Now connects signal_registry to 3SAA logic
cat("\n[4/8] Stage 02: SAA & Strategic Allocation...")
source(project_tree$scripts$saa)

# --- 6. STAGE 04: ADVANCED REPORTING ---
cat("\n[5/8] Stage 04: Merging Technical Intelligence...")
source(project_tree$scripts$advanced_reporting)

# --- 7. STAGE 06: INDEPENDENT SANITY GATE ---
if(OPTIONS$run_external_audit) {
  cat("\n[6/8] Stage 06: Executing Finnhub Independent Audit...")
  source(project_tree$scripts$sanity_check)
  
  if (exists("comparison") && "FAIL" %in% comparison$status) {
    stop("🚨 PIPELINE HALTED: Data drift detected.")
  }
}

# --- 8. STAGE 05: SHINY BUILDER & SIGNAL AUDIT ---
# Generates the Sentinel Plots for the UI
cat("\n[7/8] Stage 05: Building Sentinel Intelligence UI...")
source(project_tree$scripts$sm_visuals)
source(project_tree$scripts$shiny_builder)

# --- 9. EXECUTION ---
cat("\n[8/8] Invoking UI Renderer...")
build_and_launch_shiny()

cat("\n--- PIPELINE EXECUTION COMPLETE [", as.character(Sys.time()), "] ---\n")