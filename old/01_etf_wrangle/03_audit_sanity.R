# ==============================================================================
# GEMINI ETF REFINERY: 03_AUDIT_SANITY (FEB 2026)
# ==============================================================================
library(tidyverse)
library(fs)
library(plotly)
library(xts)

# --- 3.1 STRUCTURAL PATH AUDIT ---
# Checks if the flat project files and folders are in place
run_full_audit <- function() {
  message("--- 🕵️ Starting Structural Audit ---")
  
  # Define required project components
  required_files <- c("01_init_universe.R", "02_data_loaders.R", "03_audit_sanity.R", 
                      "04_core_analytics.R", "05_visual_engine.R")
  required_dirs  <- c("data_raw", "data_processed", "plots")
  
  # Check Files
  file_status <- tibble(
    name = required_files,
    exists = file.exists(required_files)
  )
  
  # Check Dirs
  dir_status <- tibble(
    name = required_dirs,
    exists = dir.exists(required_dirs)
  )
  
  if(all(file_status$exists) & all(dir_status$exists)) {
    message("✅ Structure Verified: All core modules and folders found.")
  } else {
    missing <- c(file_status$name[!file_status$exists], dir_status$name[!dir_status$exists])
    warning("⚠️ Missing Components: ", paste(missing, collapse = ", "))
  }
  
  # Return file info for the Sunburst visual
  results <- dir_info(".", recurse = FALSE) %>%
    mutate(
      name = path_file(path),
      is_stale = modification_time < (Sys.time() - 86400),
      status_color = ifelse(is_stale, "#e74c3c", "#27ae60"),
      parent = "Root"
    ) %>%
    filter(type == "file")
  
  return(results)
}

# --- 3.2 DATA INTEGRITY AUDIT ---
# Checks the actual content of the returns (NAs and Freshness)
run_refinery_audit <- function(pillar_list) {
  message("--- 🧪 Starting Data Integrity Check ---")
  
  all_passed <- TRUE
  
  for (name in names(pillar_list)) {
    df <- pillar_list[[name]]
    
    # Check A: Ticker Count (Expected 34 based on your updated 01_init list)
    ticker_count <- ncol(df)
    
    # Check B: NA Density
    total_nas <- sum(is.na(df))
    
    # Check C: Date Continuity
    last_data_date <- index(xts::last(df))
    days_stale <- as.numeric(Sys.Date() - last_data_date)
    
    message(sprintf("[%s]: %d Assets | %d NAs | Days Stale: %d", 
                    name, ticker_count, total_nas, days_stale))
    
    # Failure Conditions
    if (total_nas > 0) {
      message("❌ FAIL: Pillar contains NAs. Check 02_data_loaders LOCF logic.")
      all_passed <- FALSE
    }
    
    if (days_stale > 4) {
      message("❌ FAIL: Data is stale (Last Date: ", last_data_date, ")")
      all_passed <- FALSE
    }
  }
  
  # Global Flag for the Master Run
  GEMINI_ETF_READY <<- all_passed
  return(all_passed)
}

# --- 3.3 HIERARCHY DISCOVERY (SUNBURST) ---
discover_hierarchy <- function(audit_data) {
  # Simplified for Flat Project: Root -> Files
  plot_labels <- c("Refinery_Root", audit_data$name)
  plot_parents <- c("", rep("Refinery_Root", nrow(audit_data)))
  plot_colors  <- c("#34495e", audit_data$status_color)
  
  plot_ly(
    labels = plot_labels,
    parents = plot_parents,
    type = 'sunburst',
    marker = list(colors = plot_colors)
  ) %>%
    layout(title = "Flat Project Freshness Map")
}

message("✅ 03_audit_sanity.R loaded.")