# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./00_etf_data_download/04_haa_engine.R
# Purpose: Hierarchical Asset Allocation (HAA) - Cluster Momentum & Vol Scaling
# ==============================================================================

library(tidyverse)
library(xts)
library(PerformanceAnalytics)

# --- 1. CONNECT TO INFRASTRUCTURE ---
if(!exists("project_tree")) source("./project_tree.R")

# Only run Init if the metadata doesn't exist in the environment
if(!exists("etf_metadata")) {
  message("🧠 Brain not found. Initializing universe...")
  source(project_tree$scripts$init)
}



# Load Refined (Winsorized) Returns from the Refinery
xts_ret <- read_rds(project_tree$products$refined_ret)

message("🏗️ Building HAA Architecture for ", length(cat_tickers), " Strategy Clusters...")

# --- 2. CLUSTER PERFORMANCE AGGREGATION ---
# Logic: Treat each cluster as a single "Super-Asset" by averaging its members
calculate_cluster_returns <- function(returns, clusters) {
  cluster_list <- map(clusters, function(tickers) {
    # Ensure we only use tickers currently present in the return matrix
    valid_tickers <- intersect(tickers, colnames(returns))
    subset_ret <- returns[, valid_tickers, drop = FALSE]
    
    # Equal-weighted mean return across the cluster
    rowMeans(subset_ret, na.rm = TRUE)
  })
  
  # Merge list into a single wide XTS object
  cluster_xts <- do.call(merge, cluster_list)
  colnames(cluster_xts) <- names(clusters)
  return(cluster_xts)
}

# --- 3. RISK-ADJUSTED METRICS ---
# Logic: Calculate Momentum and Volatility for the HAA layers
calculate_haa_metrics <- function(cluster_xts, mom_window = 60, vol_window = 20) {
  # Rolling Cumulative Momentum (approx 3-month)
  mom <- rollapply(cluster_xts, width = mom_window, FUN = Return.cumulative, fill = NA, align = "right")
  
  # Rolling Volatility (approx 1-month) scaled to Annual
  vol <- rollapply(cluster_xts, width = vol_window, FUN = sd, fill = NA, align = "right") * sqrt(252)
  
  return(list(momentum = mom, volatility = vol))
}

# --- 4. EXECUTION PIPELINE ---

# Step A: Transform 61 Tickers -> 6 Strategy Clusters
xts_clusters <- calculate_cluster_returns(xts_ret, cat_tickers)

# Step B: Compute Cluster-level signals (The HAA Decision Fuel)
haa_metrics <- calculate_haa_metrics(xts_clusters)

# Step C: Combine and Label for Output
# We take the most recent state for the dashboard
latest_haa_state <- list(
  returns  = tail(xts_clusters, 1),
  momentum = tail(haa_metrics$momentum, 1),
  vol      = tail(haa_metrics$volatility, 1)
)

# --- 5. PERSISTENCE (Using Project Tree) ---
# We store this in the Processed Data directory
write_rds(xts_clusters, file.path(project_tree$dirs$data_ref, "xts_haa_clusters.rds"))
write_rds(haa_metrics,  file.path(project_tree$dirs$data_ref, "haa_cluster_metrics.rds"))

message("✅ 04_haa_engine.R: Hierarchical Architecture Ready.")
message("📊 Cluster Health: ", paste(names(cat_tickers), collapse = " | "))
