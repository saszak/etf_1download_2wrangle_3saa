# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts_state_machine/batch_enrich_registry.R
# Purpose: Tidy-to-XTS Batch Enrichment (Floor 2 Refinery)
# ==============================================================================

library(tidyverse)
library(quantmod)

# 1. LOAD INFRASTRUCTURE
# ------------------------------------------------------------------------------
source("./00_global_params.R")
source(project_tree$scripts$sm_engine)

batch_enrich_registry <- function() {
  
  message("🛰️ Starting Tidy-to-XTS Batch Enrichment...")
  
  # 2. LOAD FLOOR 1 (Tidy Dataframe)
  # ------------------------------------------------------------------------------
  if (!file.exists(project_tree$products$raw_p_d)) {
    stop("❌ Error: Raw data not found at ", project_tree$products$raw_p_d)
  }
  
  raw_df <- readRDS(project_tree$products$raw_p_d)
  
  # Ensure the watchlist is respected; if empty, process all available symbols
  target_symbols <- if(length(my_watchlist) > 0) {
    intersect(my_watchlist, unique(raw_df$symbol))
  } else {
    unique(raw_df$symbol)
  }
  
  message("🔍 Targeting: ", paste(target_symbols, collapse = ", "))
  
  # 3. PROCESSING LOOP (Split -> Enrich -> List)
  # ------------------------------------------------------------------------------
  enriched_registry <- map(target_symbols, function(t) {
    
    message("🛡️ Processing: ", t)
    
    # Filter for the specific ticker and convert to XTS
    ticker_data <- raw_df %>%
      filter(symbol == t) %>%
      arrange(date) %>%
      distinct(date, .keep_all = TRUE)   # guard against duplicate dates
    
    if(nrow(ticker_data) < sm_params$lookback_sma) {
      message("⚠️ Skip: ", t, " has insufficient data for SMA200.")
      return(NULL)
    }
    
    # Convert to XTS for technical analysis
    xts_obj <- as.xts(ticker_data$adjusted, order.by = ticker_data$date)
    colnames(xts_obj) <- "Adjusted"
    
    # Metric Extraction
    sma_vec  <- TTR::SMA(xts_obj, n = sm_params$lookback_sma)
    dist_vec <- (xts_obj / sma_vec) - 1
    
    # Hysteresis Logic (The 3State Machine)
    states <- calc_asymmetric_state(
      as.numeric(dist_vec), 
      thru = sm_params$thru, 
      thrd = sm_params$thrd
    )
    
    # Integration
    enriched <- cbind(xts_obj, sma_vec, dist_vec)
    colnames(enriched) <- c("Adjusted", "SMA200", "Dist200")
    enriched$Signal    <- states
    enriched$Action    <- c(0, diff(states))
    
    return(na.omit(enriched))
    
  }) %>% 
    set_names(target_symbols) %>% 
    compact()
  
  # 4. SAVE FLOOR 2 PRODUCT
  # ------------------------------------------------------------------------------
  saveRDS(enriched_registry, project_tree$products$signal_registry)
  message("✅ Signal Registry built with ", length(enriched_registry), " tickers.")
  message("💾 Saved to: ", project_tree$products$signal_registry)
  
  return(enriched_registry)
}

# --- AUTOMATIC EXECUTION (Optional) ---
# batch_enrich_registry()