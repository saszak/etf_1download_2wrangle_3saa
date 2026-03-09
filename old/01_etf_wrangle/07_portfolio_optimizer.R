# ==============================================================================
# MODULE 01: 07_PORTFOLIO_OPTIMIZER
# Purpose: Convert Alpha Signals into Actionable Weights
# ==============================================================================

# 1. LOAD SIGNAL DATA
path_signal <- file.path(base_path, "01_etf_wrangle", "data_processed", "signal_table.rds")
signals <- read_rds(path_signal)

# 2. SELECT TOP SATELLITES (Top 1 per Cluster for diversification)
top_picks <- signals %>%
  group_by(cluster) %>%
  slice_max(Alpha_Score, n = 1) %>%
  ungroup() %>%
  slice_max(Alpha_Score, n = 3) # Take the top 3 clusters overall

# 3. GENERATE TARGET WEIGHTS
# Example logic: 60% Core Anchor, 40% Tactical Tilt
portfolio_targets <- top_picks %>%
  mutate(
    tilt_weight = (Alpha_Score / sum(Alpha_Score)) * 0.40,
    final_allocation = scales::percent(tilt_weight)
  ) %>%
  select(ticker, cluster, Alpha_Score, final_allocation)

print(portfolio_targets)