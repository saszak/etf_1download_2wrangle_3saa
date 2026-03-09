# ==============================================================================
# MODULE 01: 05a_VIS_REgime_AWARE (SOPHISTICATED REGIME VERSION)
# Purpose: Map Tickers against SPY Drawdown & Recovery Phases
# ==============================================================================
library(tidyverse)
library(PerformanceAnalytics)
library(xts)
library(timetk)
library(scales)

# 1. DATA INGESTION
xts_abs <- read_rds(file.path(base_path, "data_raw/raw_p_d.rds"))
xts_rel <- read_rds(file.path(base_path, "01_etf_wrangle/data_processed/rel_ret_d.rds"))

# Clean Names & Force Wide
if ("symbol" %in% colnames(xts_abs)) {
  xts_abs <- xts_abs %>% pivot_wider(names_from = symbol, values_from = adjusted) %>%
    tk_xts(date_var = date, silent = TRUE)
}
colnames(xts_abs) <- gsub("\\..*$", "", colnames(xts_abs))
colnames(xts_rel) <- gsub("\\..*$", "", colnames(xts_rel))

# 2. THE REGIME ENGINE: Extracting Drawdown & Recovery Windows
get_spy_regimes <- function(prices_xts, threshold = -0.10) {
  spy_rets <- Return.calculate(prices_xts[, "SPY"]) %>% na.omit()
  dd_table <- table.Drawdowns(spy_rets, top = 5) %>% 
    filter(Depth <= threshold) %>% 
    arrange(From)
  
  # Create segments for both "The Crash" and "The Recovery"
  regimes <- map_df(1:nrow(dd_table), function(i) {
    row <- dd_table[i,]
    bind_rows(
      data.frame(xmin = as.Date(row$From), xmax = as.Date(row$Trough), Phase = "DD"),
      data.frame(xmin = as.Date(row$Trough), xmax = as.Date(row$To), Phase = "Recovery")
    )
  })
  return(regimes)
}

# 3. PERFORMANCE MAPPING FUNCTION
plot_regime_sync_tape <- function(xts_p, xts_r, tickers, dd_threshold = -0.08) {
  
  regime_windows <- get_spy_regimes(xts_p, dd_threshold)
  spy_rets <- Return.calculate(xts_p[, "SPY"]) %>% na.omit()
  all_assets <- c(tickers, "SPY")
  
  plot_df <- map_df(1:nrow(regime_windows), function(i) {
    win <- regime_windows[i,]
    date_range <- paste0(win$xmin, "/", win$xmax)
    
    # Calculate Abs SPY and Rel Sectors for this specific window
    perf_spy <- sum(spy_rets[date_range], na.rm = TRUE)
    avail <- intersect(colnames(xts_r), tickers)
    perf_rel <- if(length(avail) > 0) colSums(xts_r[date_range, avail], na.rm = TRUE) else rep(NA, length(tickers))
    
    data.frame(ticker = names(perf_rel), perf = as.numeric(perf_rel)) %>%
      bind_rows(data.frame(ticker = "SPY", perf = perf_spy)) %>%
      mutate(xmin = win$xmin, xmax = win$xmax, Phase = win$Phase)
  })
  
  # 4. RENDER THE SUNDAY VISUAL
  ggplot(plot_df) +
    # Draw the performance tiles
    geom_rect(aes(xmin = xmin, xmax = xmax, 
                  ymin = as.numeric(factor(ticker, levels = rev(all_assets)))-0.45, 
                  ymax = as.numeric(factor(ticker, levels = rev(all_assets)))+0.45, 
                  fill = perf), color = "white", size = 0.3) +
    # Vertical Markers at every Phase Change
    geom_vline(aes(xintercept = xmin), linetype = "dotted", color = "white", alpha = 0.5) +
    geom_vline(aes(xintercept = xmax), linetype = "dotted", color = "white", alpha = 0.5) +
    # Labels
    geom_text(aes(x = xmin + (xmax-xmin)/2, y = as.numeric(factor(ticker, levels = rev(all_assets))), 
                  label = paste0(round(perf*100, 1), "%")), color = "white", size = 2.5, fontface = "bold") +
    # Sovereign Palette
    scale_fill_gradientn(colors = c("#8B0000", "#BC4749", "#2B2D42", "#52B788", "#006400"), 
                         values = rescale(c(-0.2, -0.05, 0, 0.05, 0.2)), na.value = "grey30") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(breaks = 1:length(all_assets), labels = rev(all_assets)) +
    theme_minimal() +
    theme(panel.grid = element_blank(), 
          plot.background = element_rect(fill = "#F8F9FA", color = NA),
          axis.title = element_blank()) +
    labs(title = "Regime-Sync Alpha Tape", 
         subtitle = "Sliced by SPY Drawdown (DD) and Recovery Phases")
}

# 5. EXECUTE & SAVE
target_list <- c("XLK", "XLY", "XLI", "XLF", "XLV", "XLP", "QUAL", "VLUE")
p_regime <- plot_regime_sync_tape(xts_abs, xts_rel, target_list)

ggsave(file.path(base_path, "key_plots/regime_alpha_tape.png"), p_regime, width = 18, height = 10, dpi = 300)