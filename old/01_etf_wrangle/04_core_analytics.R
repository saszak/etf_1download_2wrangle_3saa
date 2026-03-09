# ==============================================================================
# GEMINI ETF REFINERY: 04_CORE_ANALYTICS (MARCH 2026 UPDATE)
# ==============================================================================
library(tidyverse)
library(xts)
library(PerformanceAnalytics)
library(zoo)

# --- 4.1 THE MASTER SIGNAL TABLE ---
# Consolidates Momentum, Range, and Trend into a single "Source of Truth"
calculate_signal_table <- function(rel_returns_xts) {
  message("📊 Generating Unified Signal Table...")
  
  # 1. Momentum Windows (Short: 1m, Med: 3m, Long: 6m)
  mom_short <- colSums(xts::last(rel_returns_xts, 21))  
  mom_med   <- colSums(xts::last(rel_returns_xts, 63))  
  mom_long  <- colSums(xts::last(rel_returns_xts, 126))
  
  # 2. Cumulative Alpha (Relative performance over time)
  cum_alpha <- xts::as.xts(apply(rel_returns_xts, 2, cumsum))
  
  # 3. 50-Day Moving Average for Trend Velocity
  ma_50_series <- rollmean(cum_alpha, k = 50, fill = NA, align = "right")
  
  # 4. Map-Reduce to Final Table
  signal_table <- map_df(colnames(rel_returns_xts), function(tik) {
    # Momentum Score calculation
    m1 <- mom_short[tik]; m3 <- mom_med[tik]; m6 <- mom_long[tik]
    score <- (m1 * 0.5) + (m3 * 0.3) + (m6 * 0.2)
    
    # Range Percentile (Where is it in the last 5 years?)
    # 1260 days = ~5 years of trading data
    series  <- as.numeric(xts::last(cum_alpha[, tik], 1260))
    current <- last(series)
    mn      <- min(series, na.rm = TRUE)
    mx      <- max(series, na.rm = TRUE)
    range_p <- (current - mn) / (mx - mn)
    
    # Trend Velocity (Distance to 50d MA)
    ma_val  <- as.numeric(xts::last(ma_50_series[, tik]))
    dist_ma <- current - ma_val
    
    tibble(
      ticker           = tik,
      mom_score        = score,
      current_alpha    = current,
      range_percentile = range_p,
      dist_to_ma       = dist_ma,
      alpha_trend      = ifelse(dist_ma > 0, "UP", "DOWN")
    )
  }) %>%
    arrange(desc(mom_score)) %>%
    mutate(rank = row_number())
  
  # Cache for Visuals
  write_rds(signal_table, "data_processed/signal_table.rds")
  return(signal_table)
}

# --- 4.2 TACTICAL REBALANCING ---
# Recommends a Core-Satellite tilt based on the momentum scores
get_rebalance_recommendation <- function(signal_table, core_tik = "IVV", tilt_weight = 0.20) {
  
  # Filter for tickers with positive momentum, excluding the core anchor
  satellite_candidates <- signal_table %>%
    filter(ticker != core_tik & mom_score > 0) %>%
    head(3)
  
  if(nrow(satellite_candidates) == 0) {
    return(tibble(ticker = core_tik, final_weight = 1.0, note = "100% Core Defensive"))
  }
  
  # Pro-rata distribution of the 20% tilt across the top 3 satellites
  total_mom <- sum(satellite_candidates$mom_score)
  sat_rows <- satellite_candidates %>%
    mutate(
      final_weight = (mom_score / total_mom) * tilt_weight, 
      note         = "Satellite Momentum Tilt"
    )
  
  core_row <- tibble(
    ticker = core_tik, 
    final_weight = (1 - tilt_weight), 
    note = "Core Anchor"
  )
  
  result <- bind_rows(core_row, sat_rows) %>% 
    select(ticker, final_weight, note)
  
  return(result)
}

# --- 4.3 RISK & TREND UTILITIES ---
# Mandatory for 06_advanced_analytics.R signal generation
calc_rolling_vol <- function(xts_data, window = 20) {
  vol <- zoo::rollapply(xts_data, width = window, 
                        FUN = function(x) sd(x, na.rm = TRUE) * sqrt(252),
                        by.column = TRUE, align = "right", fill = NA)
  return(vol)
}

calc_trend_distance <- function(xts_prices, n = 200) {
  # Calculate 200-day Simple Moving Average
  sma <- zoo::rollapply(xts_prices, width = n, FUN = mean, fill = NA, align = "right")
  # Return ratio of Price to SMA
  dist <- xts_prices / sma
  return(dist)
}

message("✅ 04_core_analytics.R: Analytics Engine Ready (with Volatility Utils).")