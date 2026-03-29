##################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./master_saa_pipeline.R
# Purpose: Final Hierarchical SAA (25% Anchors) + Audit + Scatter
##################################################################################

library(tidyverse)
library(plotly)
library(PerformanceAnalytics)

# 1. DEFINE HIERARCHICAL WEIGHTS
# ----------------------------------------------------------------------------
weights_df <- tribble(
  ~ticker, ~weight, ~branch, ~type,
  # --- FIXED INCOME BRANCH (50% Total) ---
  "AGG",   0.25,                "FI_Complex", "Core",
  "LQD",   0.25 * 0.60,         "FI_Complex", "Satellite",
  "IEF",   0.25 * 0.20,         "FI_Complex", "Satellite",
  "TLT",   0.25 * 0.05,         "FI_Complex", "Satellite",
  "TIP",   0.25 * 0.05,         "FI_Complex", "Satellite",
  "HYG",   0.25 * 0.05,         "FI_Complex", "Satellite",
  "SHY",   0.25 * 0.05,         "FI_Complex", "Satellite",
  
  # --- EQUITY BRANCH (50% Total) ---
  "URTH",  0.25,                "EQ_Complex", "Core",
  "IEFA",  0.25 * 0.25,         "EQ_Complex", "Satellite",
  "QQQ",   0.25 * 0.05,         "EQ_Complex", "Satellite",
  "SPY",   0.25 * 0.35 + (0.25 * 0.35 * 0.09), "EQ_Complex", "Satellite", 
  
  # --- SPY SECTORS ---
  "XLK",   0.25 * 0.35 * 0.32,  "EQ_Complex", "Sector",
  "XLV",   0.25 * 0.35 * 0.12,  "EQ_Complex", "Sector",
  "XLF",   0.25 * 0.35 * 0.13,  "EQ_Complex", "Sector",
  "XLY",   0.25 * 0.35 * 0.10,  "EQ_Complex", "Sector",
  "XLI",   0.25 * 0.35 * 0.09,  "EQ_Complex", "Sector",
  "XLP",   0.25 * 0.35 * 0.06,  "EQ_Complex", "Sector",
  "XLU",   0.25 * 0.35 * 0.03,  "EQ_Complex", "Sector",
  "XLE",   0.25 * 0.35 * 0.04,  "EQ_Complex", "Sector",
  "XLB",   0.25 * 0.35 * 0.02,  "EQ_Complex", "Sector"
)

message("✅ Total Portfolio Weight: ", sum(weights_df$weight) * 100, "%")

# 2. DATA PREPARATION (PERIOD: 2016-2026)
# ----------------------------------------------------------------------------
study_xts <- xts_ret["2016/"]

# 3. SUNBURST VISUALIZATION
# ----------------------------------------------------------------------------
sunburst_data <- bind_rows(
  tibble(ids = "Total Portfolio", labels = "<b>Total Portfolio</b>", parents = "", values = 1),
  tibble(ids = c("FI_Complex", "EQ_Complex"), labels = c("FI (50%)", "EQ (50%)"), parents = "Total Portfolio", values = 0.50),
  weights_df %>% transmute(ids = ticker, labels = paste0(ticker, "<br>", round(weight*100,1), "%"), parents = branch, values = weight)
)

plot_ly(
  data = sunburst_data, 
  ids = ~ids, 
  labels = ~labels, 
  parents = ~parents, 
  values = ~values, 
  type = 'sunburst', 
  branchvalues = 'total', 
  marker = list(colorscale = "Portland")
) %>%
  layout(title = "<b>Fractal SAA Structure</b><br>Hierarchical 25% Core Anchors")

# 4. PERFORMANCE AUDIT (VS 60/40)
# ----------------------------------------------------------------------------
w_vec <- setNames(weights_df$weight, weights_df$ticker)
avail_tickers <- intersect(names(w_vec), colnames(study_xts))
w_final <- w_vec[avail_tickers] / sum(w_vec[avail_tickers])

portfolio_ret <- xts(study_xts[, names(w_final)] %*% w_final, order.by = index(study_xts))
benchmark_6040 <- xts(study_xts$SPY * 0.6 + study_xts$AGG * 0.4, order.by = index(study_xts))
comparison_xts <- merge(portfolio_ret, benchmark_6040)
colnames(comparison_xts) <- c("Fractal_SAA", "Bench_60_40")

charts.PerformanceSummary(
  comparison_xts, 
  main = "Final Hierarchical Audit", 
  colorset = c("#2c3e50", "#e74c3c"),
  lwd = 2, 
  legend.loc = "topleft"
)

table.Stats(comparison_xts)

# 5. RISK-RETURN SCATTER
# ----------------------------------------------------------------------------
stats_list <- weights_df %>%
  filter(ticker %in% colnames(study_xts)) %>%
  mutate(
    ann_ret = map_dbl(ticker, ~Return.annualized(study_xts[, .x], scale = 252)),
    ann_sd  = map_dbl(ticker, ~StdDev.annualized(study_xts[, .x], scale = 252)),
    asset_class = case_when(
      ticker %in% c("URTH", "AGG") ~ "Core Anchor (25%)",
      branch == "FI_Complex"       ~ "FI Satellite",
      type == "Satellite"          ~ "EQ Satellite",
      TRUE                         ~ "EQ Sector"
    )
  )

ggplot(stats_list, aes(x = ann_sd, y = ann_ret, size = weight, color = asset_class)) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = ticker), vjust = -1.5, size = 3, fontface = "bold", show.legend = FALSE) +
  scale_size_continuous(range = c(3, 20), labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    title = "Fractal SAA: Component Risk-Return Map (2016+)",
    subtitle = "Anchors: 25% URTH / 25% AGG",
    x = "Annualized Volatility (Risk)", 
    y = "Annualized Return",
    size = "Portfolio Weight", 
    color = "Asset Role"
  )

##################################################################################

##################################################################################

##################################################################################

##################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./master_saa_pipeline.R
# Purpose: Final Hierarchical SAA (25% Anchors) + Audit + Scatter
##################################################################################

library(tidyverse)
library(plotly)
library(PerformanceAnalytics)
library(scales)

# 1. DEFINE HIERARCHICAL WEIGHTS
# ----------------------------------------------------------------------------
weights_df <- tribble(
  ~ticker, ~weight, ~branch, ~type,
  # --- FIXED INCOME BRANCH (50% Total) ---
  "AGG",   0.25,                "FI_Complex", "Core",
  "LQD",   0.25 * 0.60,         "FI_Complex", "Satellite",
  "IEF",   0.25 * 0.20,         "FI_Complex", "Satellite",
  "TLT",   0.25 * 0.05,         "FI_Complex", "Satellite",
  "TIP",   0.25 * 0.05,         "FI_Complex", "Satellite",
  "HYG",   0.25 * 0.05,         "FI_Complex", "Satellite",
  "SHY",   0.25 * 0.05,         "FI_Complex", "Satellite",
  
  # --- EQUITY BRANCH (50% Total) ---
  "URTH",  0.25,                "EQ_Complex", "Core",
  "IEFA",  0.25 * 0.25,         "EQ_Complex", "Satellite",
  "EQQQ",   0.25 * 0.05,         "EQ_Complex", "Satellite",
  "SPY",   0.25 * 0.35 + (0.25 * 0.35 * 0.09), "EQ_Complex", "Satellite", 
  
  # --- SPY SECTORS ---
  "XLK",   0.25 * 0.35 * 0.32,  "EQ_Complex", "Sector",
  "XLV",   0.25 * 0.35 * 0.12,  "EQ_Complex", "Sector",
  "XLF",   0.25 * 0.35 * 0.13,  "EQ_Complex", "Sector",
  "XLY",   0.25 * 0.35 * 0.10,  "EQ_Complex", "Sector",
  "XLI",   0.25 * 0.35 * 0.09,  "EQ_Complex", "Sector",
  "XLP",   0.25 * 0.35 * 0.06,  "EQ_Complex", "Sector",
  "XLU",   0.25 * 0.35 * 0.03,  "EQ_Complex", "Sector",
  "XLE",   0.25 * 0.35 * 0.04,  "EQ_Complex", "Sector",
  "XLB",   0.25 * 0.35 * 0.02,  "EQ_Complex", "Sector"
)

message("✅ Total Portfolio Weight: ", sum(weights_df$weight) * 100, "%")

# 2. DATA PREPARATION (PERIOD: 2016-PRESENT)
# ----------------------------------------------------------------------------
study_xts <- xts_ret["2016/"]

# 3. SUNBURST VISUALIZATION
# ----------------------------------------------------------------------------
sunburst_data <- bind_rows(
  tibble(ids = "Total Portfolio", labels = "<b>Total Portfolio</b>", parents = "", values = 1),
  tibble(ids = c("FI_Complex", "EQ_Complex"), labels = c("FI (50%)", "EQ (50%)"), parents = "Total Portfolio", values = 0.50),
  weights_df %>% transmute(ids = ticker, labels = paste0(ticker, "<br>", round(weight*100,1), "%"), parents = branch, values = weight)
)

plot_ly(
  data = sunburst_data, 
  ids = ~ids, 
  labels = ~labels, 
  parents = ~parents, 
  values = ~values, 
  type = 'sunburst', 
  branchvalues = 'total', 
  marker = list(colorscale = "Portland")
) %>%
  layout(title = "<b>Fractal SAA Structure</b><br>Hierarchical 25% Core Anchors")

# 4. PERFORMANCE AUDIT (VS 60/40)
# ----------------------------------------------------------------------------
w_vec <- setNames(weights_df$weight, weights_df$ticker)
avail_tickers <- intersect(names(w_vec), colnames(study_xts))
w_final <- w_vec[avail_tickers] / sum(w_vec[avail_tickers])

portfolio_ret <- xts(study_xts[, names(w_final)] %*% w_final, order.by = index(study_xts))
benchmark_6040 <- xts(study_xts$SPY * 0.6 + study_xts$AGG * 0.4, order.by = index(study_xts))
comparison_xts <- merge(portfolio_ret, benchmark_6040)
colnames(comparison_xts) <- c("Fractal_SAA", "Bench_60_40")

charts.PerformanceSummary(
  comparison_xts, 
  main = "Final Hierarchical Audit", 
  colorset = c("#2c3e50", "#e74c3c"),
  lwd = 2, 
  legend.loc = "topleft"
)

# 5. RISK-RETURN SCATTER
# ----------------------------------------------------------------------------
stats_list <- weights_df %>%
  filter(ticker %in% colnames(study_xts)) %>%
  mutate(
    ann_ret = map_dbl(ticker, ~Return.annualized(study_xts[, .x], scale = 252)),
    ann_sd  = map_dbl(ticker, ~StdDev.annualized(study_xts[, .x], scale = 252)),
    asset_class = case_when(
      ticker %in% c("URTH", "AGG") ~ "Core Anchor (25%)",
      branch == "FI_Complex"       ~ "FI Satellite",
      type == "Satellite"          ~ "EQ Satellite",
      TRUE                         ~ "EQ Sector"
    )
  )

ggplot(stats_list, aes(x = ann_sd, y = ann_ret, size = weight, color = asset_class)) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = ticker), vjust = -1.5, size = 3, fontface = "bold", show.legend = FALSE) +
  scale_size_continuous(range = c(3, 20), labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(labels = percent_format()) +
  theme_minimal() +
  labs(
    title = "Fractal SAA: Component Risk-Return Map",
    subtitle = "Period: 2016 - Present | Anchors: 25% URTH / 25% AGG",
    x = "Annualized Volatility", 
    y = "Annualized Return",
    size = "Portfolio Weight", 
    color = "asset_class"
  )

##################################################################################






##################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./03_saa_logic/07_equity_branch_audit.R
# Purpose: Strategic Equity Branch Audit (URTH 50% / SPY 25%) + Error Handling
##################################################################################

library(tidyverse)
library(plotly)
library(PerformanceAnalytics)
library(scales)

# 1. DEFINE STRATEGIC EQUITY WEIGHTS
# ----------------------------------------------------------------------------
weights_df_eq <- tribble(
  ~ticker, ~weight, ~branch,      ~type,             ~role,
  "URTH",  0.50,    "EQ_Complex", "Core",            "Global_Anchor", 
  "SPY",   0.25,    "EQ_Complex", "Core",            "US_Anchor",
  "IEFA",  0.10,    "EQ_Complex", "Satellite",       "Intl_Satellite",
  "EQQQ",   0.05,    "EQ_Complex", "Satellite",       "Growth_Satellite",
  "XLK",   0.05,    "EQ_Complex", "Sector",          "Sector_Driver",
  "XLI",   0.025,   "EQ_Complex", "Sector",          "Sector_Dragger",
  "XLF",   0.025,   "EQ_Complex", "Sector",          "Sector_Cyclical"
)

# 2. DATA PREPARATION & ERROR HANDLING
# ----------------------------------------------------------------------------
study_xts <- xts_ret["2018/"]

# Identify intersection of requested tickers and available data
w_vec_all <- setNames(weights_df_eq$weight, weights_df_eq$ticker)
avail_tickers <- intersect(names(w_vec_all), colnames(study_xts))

# Re-normalize weights for available tickers only to avoid "out of bounds"
w_vec_eq <- w_vec_all[avail_tickers]
w_vec_eq <- w_vec_eq / sum(w_vec_eq)

if(length(avail_tickers) < length(names(w_vec_all))) {
  missing <- setdiff(names(w_vec_all), avail_tickers)
  warning("⚠️ Missing tickers in data: ", paste(missing, collapse = ", "), ". Re-normalizing others.")
}

# 3. CALCULATE RETURNS
# ----------------------------------------------------------------------------
eq_portfolio_ret <- xts(study_xts[, avail_tickers] %*% w_vec_eq, 
                        order.by = index(study_xts))
colnames(eq_portfolio_ret) <- "Strategic_Equity_Core"

# Benchmark comparison
comparison_eq_xts <- merge(eq_portfolio_ret, study_xts[, c("SPY", "URTH") ] )
colnames(comparison_eq_xts) <- c("Strategic_Equity_Core", "SPY_Bmk", "URTH_Bmk")

# 4. PERFORMANCE AUDIT CHART
# ----------------------------------------------------------------------------
charts.PerformanceSummary(
  comparison_eq_xts, 
  main = "Equity Branch Audit: Global-US Hybrid vs SPY",
  colorset = c("#2c3e50", "#e74c3c"), 
  lwd = 2, 
  legend.loc = "topleft"
)

# 5. COMPONENT RISK-RETURN MAP
# ----------------------------------------------------------------------------
eq_stats <- weights_df_eq %>%
  filter(ticker %in% avail_tickers) %>%
  mutate(
    ann_ret = map_dbl(ticker, ~Return.annualized(study_xts[, .x], scale = 252)),
    ann_sd  = map_dbl(ticker, ~StdDev.annualized(study_xts[, .x], scale = 252))
  )

##################################################################################

ggplot(eq_stats, aes(x = ann_sd, y = ann_ret, size = weight, color = role)) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label = ticker), vjust = -1.8, size = 4, fontface = "bold", show.legend = FALSE) +
  scale_size_continuous(range = c(4, 25), labels = percent_format()) +
  # Set limits from 0 and remove padding at the lower bound
  scale_y_continuous(
    labels = percent_format(), 
    limits = c(0, NA), 
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_x_continuous(
    labels = percent_format(), 
    limits = c(0, NA), 
    expand = expansion(mult = c(0, 0.1))
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "grey80") # Adds visual structure to the origin
  ) +
  labs(
    title = "Equity Branch: Hybrid Anchor Strategy",
    subtitle = "Safe-guarded against missing ticker data | Origin-start (0,0)",
    x = "Annualized Volatility (Risk)", 
    y = "Annualized Return",
    size = "Branch Weight"
  )

##################################################################################

##################################################################################











