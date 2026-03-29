################################################################################
# scripts_saa_execution/saa_alpha_intelligence.R
# Purpose : Compute alpha velocity z-scores and classify each ticker as
#           Spring-Load / Exhaustion / Neutral for use in policy weights.
#           Ported from etf_saa_taa/01_alpha_intelligence.R.
#
# Input   : saa_bridge_config.R  → xts_d_rel, tickers
#           technical_summary    → dist_200, trend_regime (from 01b)
#           raw_data             → adjusted prices (for range_percentile)
# Output  : selection_matrix (tibble, global env)
#           02_data_processed/saa_selection_matrix.rds
#
# Signals
#   alpha_velocity_z  : 20d rolling alpha mean / 252d rolling alpha sd
#   range_percentile  : (price − 52w_low) / (52w_high − 52w_low)
#   dist_200          : from technical_summary (price/MA200 − 1)
#
# Spring-Load  : range_percentile < 0.30 AND dist_200 > 0  (oversold but above MA200)
# Exhaustion   : range_percentile > 0.70 AND dist_200 < 0  (overbought but below MA200)
# Neutral      : everything else
#
# Quadrant (for Book 2 TAA)
#   SPRING_LOAD (LONG)   : Spring-Load AND alpha_velocity_z > 0
#   EXHAUSTION  (SHORT)  : Exhaustion  AND alpha_velocity_z < 0
#   NEUTRAL              : all others
################################################################################

library(dplyr)
library(tidyr)
library(zoo)
library(here)

if (!exists("xts_d_rel")) source(here::here("scripts_saa_execution/saa_bridge_config.R"))

# ── 1. ALPHA VELOCITY Z-SCORE ─────────────────────────────────────────────────
# 20-day rolling mean alpha / 252-day rolling alpha volatility
message("saa_alpha: computing alpha velocity z-scores ...")

calc_alpha_velocity <- function(rel_xts) {
  roll_alpha <- zoo::rollapply(rel_xts, width = 20,  FUN = mean, fill = NA, align = "right")
  roll_sd    <- zoo::rollapply(rel_xts, width = 252, FUN = sd,   fill = NA, align = "right")

  # Latest cross-sectional z-scores
  tail(roll_alpha / roll_sd, 1) %>%
    as.data.frame() %>%
    pivot_longer(cols = everything(),
                 names_to  = "ticker",
                 values_to = "alpha_velocity_z")
}

alpha_z_df <- calc_alpha_velocity(xts_d_rel[, tickers])

# ── 2. RANGE PERCENTILE (52-week) ─────────────────────────────────────────────
# Derived from cumulative wealth of xts_ret: proxy for price level relative to
# 52-week high/low when raw_data is not available.
message("saa_alpha: computing 52-week range percentiles ...")

calc_range_percentile <- function(ret_xts, window = 252) {
  # Reconstruct wealth index (log returns → exp(cumsum))
  wealth <- exp(apply(ret_xts, 2, cumsum))

  roll_high <- zoo::rollapply(wealth, width = window, FUN = max, fill = NA, align = "right")
  roll_low  <- zoo::rollapply(wealth, width = window, FUN = min, fill = NA, align = "right")

  latest       <- tail(wealth,     1)
  latest_high  <- tail(roll_high,  1)
  latest_low   <- tail(roll_low,   1)

  range_span <- latest_high - latest_low
  pct        <- (latest - latest_low) / ifelse(range_span == 0, 1, range_span)

  as.data.frame(t(pct)) %>%
    rownames_to_column("ticker") %>%
    rename(range_percentile = 2)
}

range_df <- calc_range_percentile(xts_d_abs[, tickers])

# ── 3. LOAD TECHNICAL SUMMARY (dist_200, trend_regime) ────────────────────────
# Prefer technical_summary.rds (latest snapshot, one row per ticker, has
# trend_regime).  Fall back to ma_technical_anchors.rds (full history,
# no trend_regime) if the summary file is not yet available.

ts_path   <- here::here("02_data_processed/technical_summary.rds")
ma_path   <- here::here("02_data_processed/ma_technical_anchors.rds")

if (file.exists(ts_path)) {
  tech_snap <- readRDS(ts_path) %>%
    select(ticker, dist_200, trend_regime) %>%
    filter(ticker %in% tickers)
} else if (file.exists(ma_path)) {
  warning("saa_alpha: technical_summary.rds not found — using ma_technical_anchors (no trend_regime).")
  tech_snap <- readRDS(ma_path) %>%
    group_by(symbol) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    select(ticker = symbol, dist_200) %>%
    mutate(trend_regime = NA_character_) %>%
    filter(ticker %in% tickers)
} else {
  warning("saa_alpha: no technical anchors file found — dist_200 and trend_regime set to NA.")
  tech_snap <- tibble(ticker = tickers, dist_200 = NA_real_, trend_regime = NA_character_)
}

# ── 4. BUILD SELECTION MATRIX ─────────────────────────────────────────────────
message("saa_alpha: building selection matrix ...")

selection_matrix <- saa_metadata %>%
  left_join(alpha_z_df, by = "ticker") %>%
  left_join(range_df,   by = "ticker") %>%
  left_join(tech_snap,  by = "ticker") %>%
  mutate(
    # Flag statistical alpha outliers (|z| > 1.96 ≈ 95% CI)
    is_velocity_outlier = abs(alpha_velocity_z) > 1.96,

    # Spring-Load / Exhaustion classification
    status = case_when(
      range_percentile < 0.30 & dist_200 > 0 ~ "Spring-Load",
      range_percentile > 0.70 & dist_200 < 0 ~ "Exhaustion",
      TRUE                                    ~ "Neutral"
    ),

    # TAA quadrant (fed into Book 2 policy weights)
    quadrant = case_when(
      status == "Spring-Load" & alpha_velocity_z > 0 ~ "SPRING_LOAD (LONG)",
      status == "Exhaustion"  & alpha_velocity_z < 0 ~ "EXHAUSTION (SHORT)",
      TRUE                                            ~ "NEUTRAL"
    )
  ) %>%
  arrange(desc(alpha_velocity_z))

# ── 5. SAVE ───────────────────────────────────────────────────────────────────
saveRDS(selection_matrix,
        here::here("02_data_processed/saa_selection_matrix.rds"))

message("saa_alpha: SAVED → 02_data_processed/saa_selection_matrix.rds")
message("  Spring-Loads : ", sum(selection_matrix$status == "Spring-Load", na.rm = TRUE))
message("  Exhaustions  : ", sum(selection_matrix$status == "Exhaustion",  na.rm = TRUE))
message("  LONG quadrant: ", sum(selection_matrix$quadrant == "SPRING_LOAD (LONG)", na.rm = TRUE))

# ── Auto-run guard ─────────────────────────────────────────────────────────────
if (!isTRUE(getOption("knitr.in.progress"))) {
  cat("\n── Selection Matrix (top alpha velocity) ───────────────\n")
  print(selection_matrix %>%
    select(ticker, cluster, status, quadrant,
           alpha_velocity_z, range_percentile, dist_200) %>%
    head(15))
}
