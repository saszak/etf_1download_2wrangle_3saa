# ==============================================================================
# PROJECT B: 09_STRESS_TEST.R (V2 - Sensitivity Optimized)
# Purpose: Differentiate TAA Resilience using Asset-Specific Beta
# ==============================================================================
library(dplyr)
library(readr)
library(tidyr)

message("📉 Refining Sovereign Stress Engine with Asset Beta...")

# --- 1.0 LOAD DATA ---
base_path <- "/Volumes/T7Red_Work/Rstudio_ssd/etf_1download_2wrangle_3saa"
path_policy <- file.path(base_path, "02_etf_saa_taa/data_processed/policy_list.rds")
policy <- read_rds(path_policy)

# --- 2.0 DEFINE ASSET SENSITIVITY (Beta to Crisis) ---
# Enhancers react more strongly to SPY moves (Beta > 1)
# Dampeners provide a "buffer" (Beta < 1)
asset_beta <- tribble(
  ~role,       ~equity_beta, ~bond_beta,
  "CORE",       1.0,          0.0,   # SPY behavior
  "ENHANCER",   1.25,         -0.1,  # High-conviction alpha
  "DAMPENER",   0.45,          0.7,  # Tail-risk protection
  "ANCHOR",     0.0,          1.0    # AGG behavior
)

# --- 3.0 CALCULATE WEIGHTED STRATEGY BETA ---
# Map roles to your current policy. 
# We assume SAA is pure CORE/ANCHOR, while TAA includes the Alpha roles.
taa_weights <- policy$TOTAL %>% 
  mutate(role = case_when(
    ticker == "SPY" ~ "CORE",
    ticker == "AGG" ~ "ANCHOR",
    TRUE            ~ "ENHANCER" # Assign tactical picks to Enhancer profile
  )) %>% 
  left_join(asset_beta, by = "role")

saa_weights <- policy$SAA %>% 
  mutate(role = ifelse(ticker == "SPY", "CORE", "ANCHOR")) %>%
  left_join(asset_beta, by = "role")

# --- 4.0 SCENARIO CALCULATION ---
scenarios <- tribble(
  ~Scenario,               ~SPY_Ret,  ~AGG_Ret,
  "2008 Financial Crisis", -0.370,     0.050,
  "2020 Covid Crash",      -0.200,     0.010,
  "2022 Rate Hike Year",   -0.180,    -0.130,
  "Tech Bubble Burst",     -0.120,     0.070,
  "VIX Spike (Flash)",     -0.050,     0.020
)

calculate_beta_ret <- function(w_df, s_row) {
  w_col <- grep("weight|net", names(w_df), value = TRUE)[1]
  
  w_df %>%
    mutate(proj_ret = (!!sym(w_col)) * ((equity_beta * s_row$SPY_Ret) + (bond_beta * s_row$AGG_Ret))) %>%
    summarise(total_ret = sum(proj_ret, na.rm = TRUE)) %>%
    pull(total_ret)
}

readable_results <- scenarios %>%
  rowwise() %>%
  mutate(
    SAA = calculate_beta_ret(saa_weights, pick(everything())),
    TAA = calculate_beta_ret(taa_weights, pick(everything()))
  ) %>%
  ungroup()

# --- 5.0 SAVE ---
saveRDS(readable_results, file.path(base_path, "02_etf_saa_taa/data_processed/stress_test_results.rds"))
message("✅ Differentiated Stress Results generated based on Portfolio Roles.")