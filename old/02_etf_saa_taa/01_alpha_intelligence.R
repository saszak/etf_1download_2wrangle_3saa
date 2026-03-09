# ==============================================================================
# PROJECT B: ETF_SAA_TAA - 01_ALPHA_INTELLIGENCE.R (REFINED)
# ==============================================================================
library(dplyr)
library(tidyr)
library(zoo)
library(xts)

# --- 1.0 DIRECTORY GUARD ---
# Ensure we are saving into the Module 02 data folder
if (!dir.exists("02_etf_saa_taa/data_processed")) {
  dir.create("02_etf_saa_taa/data_processed", recursive = TRUE)
}

# --- 1.1 COMPUTE ALPHA VELOCITY ---
message("🚀 Quantifying Alpha Velocity (Statistical Outlier Detection)...")

calc_alpha_velocity <- function(rel_xts) {
  # 20d Alpha Momentum / 252d Alpha Volatility
  # Using 252 for a full trading year of variance
  roll_alpha <- rollapply(rel_xts, width = 20, FUN = mean, fill = NA, align = "right")
  roll_sd    <- rollapply(rel_xts, width = 252, FUN = sd, fill = NA, align = "right")
  
  # Calculate Z-Score of the velocity
  velocity_xts <- roll_alpha / roll_sd
  
  z_scores <- tail(velocity_xts, 1) %>% 
    as.data.frame() %>%
    pivot_longer(cols = everything(), names_to = "ticker", values_to = "alpha_velocity_z")
  
  return(z_scores)
}

# Ensure xts_d_rel exists in your environment from Module 01
if(!exists("xts_d_rel")) stop("❌ Missing xts_d_rel. Run Module 01 Wrangle first.")

alpha_z_df <- calc_alpha_velocity(xts_d_rel)

# --- 1.2 THE SOVEREIGN SELECTION MATRIX ---
message("🔍 Filtering for Methodology Drift & Outliers...")

# NOTE: We assume range_percentile and dist_to_ma were calculated in Module 01.
# If they weren't, we'll need to add them to your signal_table first.

selection_matrix <- signal_table %>%
  left_join(alpha_z_df, by = "ticker") %>%
  mutate(
    # 1. Process Alpha Velocity (1.96 Z-Score = 95% confidence outlier)
    is_velocity_outlier = abs(alpha_velocity_z) > 1.96,
    
    # 2. Methodology Drift Labels (The "Spring-Load" Logic)
    # Using 'if_else' or 'case_when' to handle missing columns safely
    status = case_when(
      (Alpha_Score > 1.0) & (Vol_20d < 0.15) ~ "Spring-Load", # Example fallback logic
      (Alpha_Score < -1.0) ~ "Exhaustion",
      TRUE ~ "Neutral"
    ),
    
    # 3. Final Quadrant for 02_policy_weights
    quadrant = case_when(
      status == "Spring-Load" & alpha_velocity_z > 0 ~ "SPRING_LOAD (LONG)",
      status == "Exhaustion"  & alpha_velocity_z < 0 ~ "EXHAUSTION (SHORT)",
      TRUE ~ "NEUTRAL"
    )
  ) %>%
  arrange(desc(alpha_velocity_z))

# --- 1.3 SAVE ---
saveRDS(selection_matrix, "02_etf_saa_taa/data_processed/selection_matrix.rds")

# --- 1.4 STATUS REPORT ---
n_spring <- nrow(filter(selection_matrix, status == "Spring-Load"))
message("✅ Selection Matrix Saved: ", n_spring, " Spring-Loads found.")