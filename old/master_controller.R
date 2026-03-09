# ==============================================================================
# PROJECT: SOVEREIGN ETF ENGINE - MASTER EXECUTIVE (SEEKER EDITION)
# Purpose: Find and Run Scripts regardless of exact folder nesting
# ==============================================================================
library(readr)
library(dplyr)
library(tictoc)

# --- 1.0 GLOBAL CONFIGURATION ---
tic("Sovereign Engine Total Runtime")
message("🏛️  INITIATING SOVEREIGN EXECUTIVE...")

base_path <- "/Volumes/T7Red_Work/Rstudio_ssd/etf_1download_2wrangle_3saa"
setwd(base_path)

# --- 2.0 SCRIPT LOCATOR FUNCTION ---
# This finds the script anywhere in the project to prevent "No such file" errors
find_and_run <- function(script_name, step_name) {
  message("\n------------------------------------------------------------")
  message("🔍  SEARCHING FOR: ", script_name)
  
  # Recursive search for the file
  all_files <- list.files(path = base_path, pattern = paste0("^", script_name, "$"), 
                          recursive = TRUE, full.names = TRUE)
  
  # Filter out Rproj temp files
  valid_files <- all_files[!grepl(".Rproj.user", all_files)]
  
  if(length(valid_files) == 0) {
    message("❌  CRITICAL FAILURE: Could not find '", script_name, "' anywhere in project.")
    return(FALSE)
  }
  
  target_script <- valid_files[1]
  message("▶️  STEP: ", step_name)
  message("📂  EXECUTING: ", target_script)
  
  tryCatch({
    source(target_script, local = FALSE)
    message("✅  SUCCESS: ", step_name)
    return(TRUE)
  }, error = function(e) {
    message("❌  EXECUTION ERROR in ", step_name, ": ", e$message)
    return(FALSE)
  })
}

# --- 3.0 EXECUTION PIPELINE ---

# PHASE 1: Signals
s1 <- find_and_run("03_signal_generator.R", "Signal Generation")

# PHASE 2: Portfolio Architecture
if(s1) {
  s2 <- find_and_run("04_policy_builder.R", "Policy Construction")
  s3 <- find_and_run("05_visual_dashboard.R", "Visual Audit & DNA")
  s4 <- find_and_run("06_performance_attribution.R", "Alpha Attribution")
}

# PHASE 3: Selection & Trades
if(exists("s2") && s2) {
  s5 <- find_and_run("07_quantdb_selector.R", "Sovereign 5 Selection")
  s6 <- find_and_run("08_trade_generator.R", "Final Trade Orders")
}

# --- 4.0 FINAL SUMMARY ---
message("\n============================================================")
toc()
message("🏛️  SOVEREIGN ENGINE: PIPELINE COMPLETE.")
message("============================================================")