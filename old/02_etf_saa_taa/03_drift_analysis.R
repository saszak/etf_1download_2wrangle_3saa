# ==============================================================================
# PROJECT B: ETF_SAA_TAA - 03_DRIFT_ANALYSIS.R
# Purpose: Audit Book 3 (Total Netted Exposure) for Cluster/Sector Drift
# ==============================================================================
library(dplyr)
library(tidyr)

# --- 1. PATH ANCHORING ---
path_policy   <- "02_etf_saa_taa/data_processed/policy_list.rds"
path_metadata <- "02_etf_saa_taa/data_processed/selection_matrix.rds"
path_output   <- "02_etf_saa_taa/data_processed/drift_report.rds"

# --- 2. LOAD THE UNIFIED POLICY & METADATA ---
if(!file.exists(path_policy)) stop("❌ Drift Analysis: policy_list.rds not found.")
policy   <- readRDS(path_policy)
metadata <- readRDS(path_metadata) %>% 
  select(ticker, cluster, category)

# --- 3. SELECT THE TARGET BOOK (Book 3: TOTAL) ---
# Replacing NAs in cluster for the Core Anchors (SPY/AGG)
drift_report <- policy$TOTAL %>%
  left_join(metadata, by = "ticker") %>%
  mutate(cluster = replace_na(cluster, "CORE_BENCHMARK"))

# --- 4. GENERATE THE CROWDING SUMMARY ---
# This identifies if any cluster is getting "too heavy" due to TAA tilts
crowding_summary <- drift_report %>%
  group_by(cluster) %>%
  summarise(
    total_net_exposure = sum(net_weight, na.rm = TRUE),
    ticker_count = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    # Flagging "Crowded" clusters (e.g., if exposure exceeds 50%)
    status = if_else(total_net_exposure > 0.50, "⚠️ CROWDED", "✅ OK")
  ) %>%
  arrange(desc(total_net_exposure))

# --- PRINT AUDIT TO CONSOLE ---
message("📊 --- SOVEREIGN DRIFT AUDIT (BOOK 3: TOTAL) ---")
print(crowding_summary)

# --- 5. SAVE FOR DASHBOARD ---
saveRDS(drift_report, path_output)
message("✅ Drift Audit complete. Prepared for 04_master_allocator.R")