# ==============================================================================
# PROJECT B: ETF_SAA_TAA - 06_PERFORMANCE_ATTRIBUTION.R
# Purpose: Quantify Portfolio Risk/Return & Performance vs Relative Benchmarks
# ==============================================================================
library(dplyr)
library(xts)
library(ggplot2)
library(tidyr)
library(readr)
library(scales)

# --- 1.0 EXACT SSD PATH MAPPING ---
message("📍 Mapping SSD Paths...")
base_path <- "/Volumes/T7Red_Work/Rstudio_ssd/etf_1download_2wrangle_3saa"

path_policy    <- file.path(base_path, "02_etf_saa_taa/data_processed/policy_list.rds")
path_rel       <- file.path(base_path, "01_etf_wrangle/data_processed/rel_ret_d.rds")
path_out       <- file.path(base_path, "02_etf_saa_taa/reports")
path_stats_out <- file.path(base_path, "02_etf_saa_taa/data_processed/taa_stats.rds")

# --- 2.0 LOAD DATA ---
message("📂 Loading Sovereign Data Objects...")
policy    <- read_rds(path_policy)
xts_d_rel <- read_rds(path_rel)

# --- 3.0 IDENTIFY ACTIVE TICKERS (Total Portfolio Attribution) ---
message("🔍 Auditing policy for performance tracking...")

# Since BOOK_2 is missing, we pull all tickers with weight > 0 from the TOTAL policy
# In your case, this will target AGG and SPY
ls_pairs <- policy$TOTAL %>%
  filter(net_weight > 0) %>%
  pull(ticker) %>%
  unique()

if(length(ls_pairs) == 0) stop("❌ No tickers with net_weight > 0 found.")

message("✅ Tracking performance for: ", paste(ls_pairs, collapse = ", "))

# --- 4.0 CALCULATE CUMULATIVE RELATIVE RETURN ---
message("🧮 Calculating Spread Performance...")

available_tickers <- intersect(ls_pairs, colnames(xts_d_rel))
if(length(available_tickers) == 0) {
  stop("❌ None of your tickers (", paste(ls_pairs, collapse=" "), ") exist in the relative return data.")
}

# Extract relative returns (Ticker Return - Benchmark Return)
ls_returns <- xts_d_rel[, available_tickers]

# Weight the returns based on your policy weights
weights <- policy$TOTAL %>% 
  filter(ticker %in% available_tickers) %>%
  select(ticker, net_weight)

# Calculate weighted average relative return (Portfolio Alpha)
# We multiply each ticker's relative return by its weight in the portfolio
weighted_rets <- as.matrix(ls_returns) %*% weights$net_weight
basket_alpha_xts <- xts(weighted_rets, order.by = index(ls_returns))

# Cumulative Alpha
cum_alpha <- cumprod(1 + tidyr::replace_na(as.numeric(basket_alpha_xts), 0)) - 1
cum_alpha_xts <- xts(cum_alpha, order.by = index(ls_returns))

# --- 5.0 RISK/RETURN METRICS ---
ann_factor <- 252
avg_alpha  <- mean(basket_alpha_xts, na.rm = TRUE)
vol_alpha  <- sd(basket_alpha_xts, na.rm = TRUE)
max_dd     <- max(cummax(cum_alpha) - cum_alpha)

ls_stats <- tibble(
  Metric = c("Annualized Alpha", "Alpha Volatility", "Sharpe (Alpha)", "Max Drawdown (Alpha)"),
  Value = c(
    avg_alpha * ann_factor,
    vol_alpha * sqrt(ann_factor),
    if(vol_alpha > 0) (avg_alpha * ann_factor) / (vol_alpha * sqrt(ann_factor)) else 0,
    max_dd
  )
)

# --- 6.0 VISUALIZE ---


df_cum <- data.frame(
  Date = index(cum_alpha_xts), 
  CumReturn = as.numeric(cum_alpha_xts)
)

p_alpha <- ggplot(df_cum, aes(x = Date, y = CumReturn)) +
  geom_line(color = "#1B4F72", linewidth = 1) +
  geom_area(fill = "#1B4F72", alpha = 0.1) +
  scale_y_continuous(labels = percent) +
  labs(title = "Sovereign Portfolio: Cumulative Relative Return",
       subtitle = paste("Tracking:", paste(available_tickers, collapse = ", "), "vs Benchmarks"),
       y = "Cumulative Alpha", x = NULL,
       caption = "Calculated as: Σ (Ticker Weight * (Ticker Return - Benchmark Return))") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# --- 7.0 EXPORT ---
if(!dir.exists(path_out)) dir.create(path_out, recursive = TRUE)
ggsave(file.path(path_out, "portfolio_alpha_cum.png"), plot = p_alpha, width = 10, height = 5, dpi = 300)

message("📊 --- PORTFOLIO RISK/RETURN PROFILE ---")
print(ls_stats)

saveRDS(ls_stats, path_stats_out)
message("✅ Performance Attribution Complete.")