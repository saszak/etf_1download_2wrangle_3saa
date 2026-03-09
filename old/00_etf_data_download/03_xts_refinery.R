# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./00_etf_data_download/03_xts_refinery.R
# Purpose: Functional XTS Processing - Winsorization, Sigma Scaling, & Outliers
# ==============================================================================

library(tidyverse)
library(xts)
library(zoo)

##############################################
# --- 1. CONNECT TO INFRASTRUCTURE ---
if(!exists("project_tree")) source("./project_tree.R")

# Only run Init if the metadata doesn't exist in the environment
if(!exists("etf_metadata")) {
  message("🧠 Brain not found. Initializing universe...")
  source(project_tree$scripts$init)
}
##############################################

# Load raw returns using the Project Tree path
abs_ret_d <- read_rds(project_tree$products$abs_ret_d)

message("🧪 Refining XTS Matrix for ", ncol(abs_ret_d), " tickers...")

# --- 2. FUNCTIONAL WINSORIZATION (Tail Management) ---
# Clips extreme outliers based on the 'winsor_pct' defined in etf_metadata
apply_functional_winsor <- function(return_matrix, metadata) {
  refined_matrix <- return_matrix
  
  for (tkt in colnames(return_matrix)) {
    # Lookup the specific clipping percentage for this ticker
    clip_val <- metadata %>% filter(ticker == tkt) %>% pull(winsor_pct)
    if (length(clip_val) == 0) clip_val <- 0.02 # Default fallback
    
    # Calculate adaptive quantiles
    q_limits <- quantile(return_matrix[, tkt], probs = c(clip_val, 1 - clip_val), na.rm = TRUE)
    
    # Apply clipping (Winsorizing)
    refined_matrix[, tkt] <- pmax(pmin(return_matrix[, tkt], q_limits[2]), q_limits[1])
  }
  return(refined_matrix)
}

# --- 3. ANALYST SIGMA CALCULATION (Z-Score Processing) ---
# Scales returns into Sigmas to identify "Extreme Regime" moves
calculate_analytical_sigma <- function(return_matrix, window = 252) {
  # Rolling Mean and SD over the specified lookback (1-year default)
  roll_m <- rollapply(return_matrix, width = window, FUN = mean, fill = NA, align = "right")
  roll_s <- rollapply(return_matrix, width = window, FUN = sd, fill = NA, align = "right")
  
  sigma_matrix <- (return_matrix - roll_m) / roll_s
  return(sigma_matrix)
}

# --- 4. EXECUTION: THE REFINERY PIPELINE ---

# Step A: Clip tails to stabilize the Hierarchical averages
xts_winsorized <- apply_functional_winsor(abs_ret_d, etf_metadata)

# Step B: Generate the Sigma Matrix for Outlier Management
xts_sigma <- calculate_analytical_sigma(xts_winsorized)

# Step C: Labeling Logic (Text Labels for the Bar Ends)
# We identify where Sigma > Ticker-Specific Limit defined in init
outlier_report <- xts_sigma[nrow(xts_sigma), ] %>%
  as.data.frame() %>%
  pivot_longer(everything(), names_to = "ticker", values_to = "current_sigma") %>%
  left_join(etf_metadata %>% select(ticker, sigma_limit, strat_func), by = "ticker") %>%
  mutate(label = case_when(
    current_sigma > sigma_limit  ~ "[EXHAUSTION-HIGH]",
    current_sigma < -sigma_limit ~ "[EXHAUSTION-LOW]",
    TRUE                         ~ "[STABLE]"
  ))

# --- 5. PERSISTENCE (Using Project Tree) ---
write_rds(xts_winsorized, project_tree$products$refined_ret)
write_rds(xts_sigma,      project_tree$products$sigma_mat)
write_rds(outlier_report,  project_tree$products$ref_report)

message("✅ 03_xts_refinery.R: Winsorized and Sigma matrices stabilized.")
message("💾 Files saved to: ", project_tree$dirs$data_ref)
message("📍 Refinery Report: Found ", sum(outlier_report$label != "[STABLE]"), " regime outliers.")