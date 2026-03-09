# ==============================================================================
# PROJECT B: 07_quantdb_selector.R
# Purpose: Dynamic Selection of 5 ETFs to Optimize the SPY Core
# ==============================================================================

# 1. LOAD THE SOVEREIGN ENGINE
source("00_bridge_config.R")
quant_db <- readRDS("data/etf_quantdb.rds")

# 2. SELECT THE 3 "ALPHA ENHANCERS" 
# Goal: Maximum Alpha + Confirmed Momentum (is_confirmed == YES)
enhancers <- quant_db %>%
  filter(ticker != "SPY", is_confirmed == "YES", alpha_6y > 0) %>%
  # Priority: High Alpha weighted against Daily Momentum
  mutate(score = (alpha_6y * 0.7) + (mom_d * 0.3)) %>%
  slice_max(score, n = 3) %>%
  mutate(role = "ENHANCER", goal = "Return Expansion")

# 3. SELECT THE 2 "VOLATILITY DAMPENERS"
# Goal: Low Correlation to SPY + "Normal" Distribution (Stability)
dampeners <- quant_db %>%
  filter(ticker != "SPY", corr_to_bmk < 0.5) %>%
  # Stability Score: Inverting absolute Kurtosis/Skew (Lower = Better)
  mutate(stability_score = -(abs(kurtosis_val) + abs(skew_val))) %>%
  slice_max(stability_score, n = 2) %>%
  mutate(role = "DAMPENER", goal = "Tail-Risk Protection")

# 4. THE SOVEREIGN 5 ROSTER
sovereign_5_list <- bind_rows(enhancers, dampeners)

# 5. VISUAL VERIFICATION
message("🏛️ SELECTOR SUCCESS: The Sovereign 5 Roster is ready.")
print(sovereign_5_list %>% select(ticker, role, alpha_6y, corr_to_bmk, is_confirmed))

# --- DASHBOARD HOOK ---
# Immediately visualize their DNA against the SPY Anchor
# plot_etf_spider_vertical_large(c(sovereign_5_list$ticker, "SPY"))




