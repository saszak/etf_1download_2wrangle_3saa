# ==============================================================================
# MODULE 01: 05_VISUAL_ENGINE (FORMAT-AWARE VERSION)
# ==============================================================================
library(tidyverse)
library(PerformanceAnalytics)
library(xts)
library(timetk)

# 1. LOAD DATA
path_abs <- file.path(base_path, "data_raw/raw_p_d.rds")
path_rel <- file.path(base_path, "01_etf_wrangle/data_processed/rel_ret_d.rds")

raw_data_abs <- read_rds(path_abs)
xts_rel      <- read_rds(path_rel)

# --- THE FORMAT CONVERTER ---
# If raw_data_abs has a column named 'symbol', it's a Long Tibble. 
# We must pivot it to Wide XTS so we can find "SPY".
if ("symbol" %in% colnames(raw_data_abs)) {
  message("🔄 Warehouse is in Long format. Converting to Wide XTS...")
  xts_abs <- raw_data_abs %>%
    pivot_wider(names_from = symbol, values_from = adjusted) %>%
    tk_xts(date_var = date, silent = TRUE)
} else {
  xts_abs <- raw_data_abs
}

# Final Sanitization (strips .Adjusted if any survived)
colnames(xts_abs) <- gsub("\\..*$", "", colnames(xts_abs))
colnames(xts_rel) <- gsub("\\..*$", "", colnames(xts_rel))

# --- 5.1 PLOT: REGIME-SYNC ALPHA TAPE ---
plot_regime_alpha_tape <- function(xts_prices, xts_alpha, target_tickers) {
  
  # A. Identify SPY Absolute Regimes
  # Now xts_prices DEFINITELY has a column named "SPY"
  spy_rets <- Return.calculate(xts_prices[, "SPY"]) %>% na.omit()
  
  # Filter for major drawdowns (>10%)
  dd_table <- table.Drawdowns(spy_rets, top = 5) %>% 
    filter(Depth <= -0.10) %>% 
    arrange(From)
  
  # B. Segment Timeline
  segments <- map_df(1:nrow(dd_table), function(i) {
    row <- dd_table[i,]
    data.frame(
      xmin = c(as.Date(row$From), as.Date(row$Trough)),
      xmax = c(as.Date(row$Trough), as.Date(row$To)),
      Regime = c("Crash", "Recovery")
    )
  })
  
  # C. Performance Mapping
  all_assets <- c("SPY", target_tickers)
  plot_df <- map_df(1:nrow(segments), function(j) {
    seg <- segments[j,]
    date_rng <- paste0(seg$xmin, "/", seg$xmax)
    
    perf_spy <- Return.cumulative(spy_rets[date_rng])
    valid_alpha_tickers <- intersect(colnames(xts_alpha), target_tickers)
    perf_rel <- Return.cumulative(xts_alpha[date_rng, valid_alpha_tickers])
    
    df_others <- as.data.frame(t(perf_rel)) %>% 
      rownames_to_column("ticker") %>% 
      rename(perf = 2)
    
    data.frame(ticker = "SPY", perf = as.numeric(perf_spy)) %>%
      bind_rows(df_others) %>%
      mutate(xmin = seg$xmin, xmax = seg$xmax, Regime = seg$Regime)
  })
  
  # D. THE HEATMAP GGPLOT
  ggplot(plot_df) +
    geom_rect(aes(xmin = xmin, xmax = xmax, 
                  ymin = as.numeric(factor(ticker, levels = rev(all_assets)))-0.4, 
                  ymax = as.numeric(factor(ticker, levels = rev(all_assets)))+0.4, 
                  fill = perf), color = "white") +
    geom_text(aes(x = xmin + (xmax-xmin)/2, 
                  y = as.numeric(factor(ticker, levels = rev(all_assets))), 
                  label = paste0(round(perf*100, 1), "%")), 
              color = "white", size = 3, fontface = "bold") +
    scale_fill_gradient2(low = "#BC4749", mid = "#2B2D42", high = "#52B788", midpoint = 0) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(breaks = 1:length(all_assets), labels = rev(all_assets)) +
    theme_minimal() +
    labs(title = "Regime-Sync Alpha Tape", subtitle = "SPY Absolute vs. Sector Relative Performance", x = NULL, y = NULL)
}

# --- 5.2 EXECUTION ---
# Using your Sovereign tickers
target_list <- c("XLK", "GLD", "SLV", "IEF")
p1 <- plot_regime_alpha_tape(xts_abs, xts_rel, target_list)

print(p1)