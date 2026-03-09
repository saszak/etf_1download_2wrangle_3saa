# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts/02_etf_saa_taa.R
# Purpose: Stage 02 - Cluster Aggregation & Hierarchical Weighting
# ==============================================================================

library(tidyverse)
library(xts)
library(PerformanceAnalytics)

# --- 1. CONNECT TO INFRASTRUCTURE ---
if(!exists("project_tree")) source("./project_tree.R")
if(!exists("etf_metadata")) source(project_tree$scripts$init)

# Load Refined Returns from Stage 01
xts_ret <- read_rds(project_tree$products$refined_ret)

message("⚖️ Stage 02: Executing SAA/TAA Strategy Logic...")

# --- 2. HIERARCHICAL CLUSTER AGGREGATION (SAA) ---
# FIX: Explicitly maintaining the XTS structure during aggregation
calculate_cluster_returns <- function(returns, clusters) {
  cluster_list <- map(clusters, function(tickers) {
    valid_tickers <- intersect(tickers, colnames(returns))
    
    if(length(valid_tickers) == 0) return(NULL)
    
    subset_ret <- returns[, valid_tickers, drop = FALSE]
    
    # Calculate row means but keep it as an XTS object
    # xts objects need a time index; we borrow it from the original 'returns'
    c_mean <- rowMeans(subset_ret, na.rm = TRUE)
    return(xts(c_mean, order.by = index(returns)))
  })
  
  # Remove any NULLs if a cluster had no valid tickers
  cluster_list <- compact(cluster_list)
  
  # Now do.call(merge, ...) will correctly identify these as xts objects
  cluster_xts <- do.call(merge, cluster_list)
  colnames(cluster_xts) <- names(clusters)
  
  return(cluster_xts)
}
# --- 3. RISK PARITY WEIGHTING (TAA) ---
# Lower volatility clusters receive higher relative weights
calculate_inv_vol_weights <- function(cluster_returns, window = 60) {
  # Calculate annualized rolling volatility
  vols <- tail(rollapply(cluster_returns, width = window, FUN = sd, fill = NA) * sqrt(252), 1)
  vols <- as.numeric(vols)
  names(vols) <- colnames(cluster_returns)
  
  # Inverse Vol: 1 / Vol
  inv_vol <- 1 / vols
  weights <- inv_vol / sum(inv_vol)
  return(weights)
}

# --- 4. EXECUTION PIPELINE ---

# Step A: Aggregate 61 Tickers -> 6 Strategy Clusters
xts_clusters <- calculate_cluster_returns(xts_ret, cat_tickers)

# Step B: Calculate Final Allocation Weights
final_weights <- calculate_inv_vol_weights(xts_clusters)

# Step C: Format for persistence and reporting
allocation_plan <- tibble(
  cluster = names(final_weights),
  weight  = final_weights
) %>%
  mutate(weight_pct = scales::percent(weight, accuracy = 0.1))

# --- 5. PERSISTENCE ---
write_rds(allocation_plan, project_tree$products$alloc_plan)

message("✅ Stage 02 Complete: Strategy Allocation Finalized.")
print(allocation_plan)
