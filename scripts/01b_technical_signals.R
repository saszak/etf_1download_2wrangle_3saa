# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts/01b_technical_signals.R
# Purpose: Stage 01b - Technical Anchor Factory (MA10, 50, 200 + Multi-Horizon Momentum)
# Architecture: Logic-Separation (Signals vs. Wrangling)
# ==============================================================================

library(tidyverse)
library(TTR)
library(here)

message("📈 Stage 01b: Generating Technical Anchors...")

# --- 1. DATA PREPARATION ---
# Load Global Parameters to stay synced with project logic
if(!exists("project_tree")) source("project_tree.R")

# Ensure raw_data is available; if not, load from the tree's raw path
if(!exists("raw_data")) {
  message("📦 Loading raw_data from cache: ", project_tree$products$raw_p_d)
  raw_data <- read_rds(here(project_tree$products$raw_p_d))
}

# --- 2. CALCULATION ENGINE ---
# We handle both List-of-XTS and Tidy-Tibble formats
if (is.list(raw_data) && !is.data.frame(raw_data)) {
  # Convert List to Tidy format for consistent Stage 01b processing
  processing_df <- map_dfr(names(raw_data), function(t) {
    df <- as.data.frame(raw_data[[t]])
    df$date <- as.Date(rownames(df))
    df$symbol <- t
    # Find the Close/Adjusted column dynamically
    target_col <- grep("Close|Adjusted", names(df), value = TRUE)[1]
    df$adjusted <- df[[target_col]]
    return(df)
  })
} else {
  processing_df <- raw_data
}

# SMA wrapper: tolerates leading NAs (e.g. tickers that started mid-series)
safe_sma <- function(x, n) {
  out <- rep(NA_real_, length(x))
  first_valid <- which(!is.na(x))[1]
  if (is.na(first_valid)) return(out)
  valid_sma <- tryCatch(as.numeric(SMA(x[first_valid:length(x)], n = n)),
                        error = function(e) rep(NA_real_, length(x) - first_valid + 1))
  out[first_valid:length(x)] <- valid_sma
  out
}

ma_table <- processing_df %>%
  group_by(symbol) %>%
  arrange(date) %>%
  distinct(date, .keep_all = TRUE) %>%    # guard against duplicate dates (e.g. IBIT weekend rows)
  mutate(
    # Simple Moving Averages — safe_sma() handles leading NAs from late-inception tickers
    ma10  = safe_sma(adjusted, n = 10),
    ma50  = safe_sma(adjusted, n = 50),
    ma200 = safe_sma(adjusted, n = 200),

    # Multi-Horizon Momentum Distances (Price relative to Anchors)
    dist_10  = (adjusted / ma10) - 1,
    dist_50  = (adjusted / ma50) - 1,
    dist_200 = (adjusted / ma200) - 1
  ) %>%
  ungroup()

# --- 3. SIGNAL SNAPSHOT (LATEST REGIME) ---
technical_summary <- ma_table %>%
  group_by(symbol) %>%
  filter(date == max(date)) %>%
  select(ticker = symbol, date, adjusted, 
         ma10, ma50, ma200, 
         dist_10, dist_50, dist_200) %>%
  mutate(
    # Trend Regime Logic
    trend_regime = case_when(
      adjusted > ma200 & adjusted > ma50  ~ "Bullish",
      adjusted < ma200 & adjusted < ma50  ~ "Bearish",
      adjusted > ma200 & adjusted < ma50  ~ "Bullish-Correction",
      adjusted < ma200 & adjusted > ma50  ~ "Bearish-Relief",
      TRUE                                ~ "Neutral/Transition"
    ),
    momentum_status = case_when(
      dist_10 > 0.05  ~ "EXTREME-STRETCHED",
      dist_10 < -0.05 ~ "EXTREME-OVERSOLD",
      TRUE            ~ "NORMAL"
    ),
    cross_signal = if_else(ma10 > ma50, "Golden-Cross-Short", "Death-Cross-Short")
  ) %>%
  ungroup()

# --- 4. PERSISTENCE ---
message("💾 Persisting technical products to: ", project_tree$dirs$proc)

if(!dir.exists(here(project_tree$dirs$proc))) {
  dir.create(here(project_tree$dirs$proc), recursive = TRUE)
}

# Path 1: Full History
write_rds(ma_table, here(project_tree$products$ma_table))

# Path 2: Latest Summary
write_rds(technical_summary, here(project_tree$products$tech_summary))

message("✅ Stage 01b Complete: Multi-horizon signals stored successfully.")