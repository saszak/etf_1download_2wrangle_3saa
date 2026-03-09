# ==============================================================================
# MODULE 01: 05a_VIS_ALPHA_TAPE (REGIME-DRIVEN)
# Purpose: Map Tickers against SPY Drawdown/Recovery Phases (>X%)
# ==============================================================================
library(tidyverse)
library(PerformanceAnalytics)
library(xts)
library(timetk)
library(scales)

# 1. DATA INGESTION & CLEANING
xts_abs <- read_rds(file.path(base_path, "data_raw/raw_p_d.rds"))
xts_rel <- read_rds(file.path(base_path, "01_etf_wrangle/data_processed/rel_ret_d.rds"))

# Clean Warehouse (Wide XTS)
if ("symbol" %in% colnames(xts_abs)) {
  xts_abs <- xts_abs %>% 
    pivot_wider(names_from = symbol, values_from = adjusted) %>%
    tk_xts(date_var = date, silent = TRUE)
}
colnames(xts_abs) <- gsub("\\..*$", "", colnames(xts_abs))
colnames(xts_rel) <- gsub("\\..*$", "", colnames(xts_rel))

# 2. SOPHISTICATED REGIME CALCULATOR
# This identifies the 'Pain' windows based on your % parameter
get_regime_data <- function(prices_xts, threshold = -0.10) {
  spy_rets <- Return.calculate(prices_xts[, "SPY"]) %>% na.omit()
  
  # Find top drawdowns exceeding the threshold
  dd_table <- table.Drawdowns(spy_rets, top = 10) %>% 
    filter(Depth <= threshold) %>% 
    arrange(From)
  
  # Create distinct DD (Peak-to-Trough) and Recovery (Trough-to-Peak) segments
  map_df(1:nrow(dd_table), function(i) {
    row <- dd_table[i,]
    bind_rows(
      data.frame(xmin = as.Date(row$From), xmax = as.Date(row$Trough), Phase = "DD"),
      data.frame(xmin = as.Date(row$Trough), xmax = as.Date(row$To), Phase = "Recovery")
    )
  })
}

# 3. PLOT ENGINE
plot_regime_tape <- function(xts_p, xts_r, target_tickers, trigger = -0.10) {
  
  regime_windows <- get_regime_data(xts_p, trigger)
  spy_rets <- Return.calculate(xts_p[, "SPY"]) %>% na.omit()
  all_assets <- c(target_tickers, "SPY") # Anchor SPY at the bottom
  
  # Process each segment
  plot_df <- map_df(1:nrow(regime_windows), function(i) {
    win <- regime_windows[i,]
    date_rng <- paste0(win$xmin, "/", win$xmax)
    
    # Calculate performance (Sum of log-rets for stability)
    perf_spy <- sum(spy_rets[date_rng], na.rm = TRUE)
    avail <- intersect(colnames(xts_r), target_tickers)
    perf_rel <- if(length(avail) > 0) colSums(xts_r[date_rng, avail], na.rm = TRUE) else rep(NA, length(target_tickers))
    
    data.frame(ticker = names(perf_rel), perf = as.numeric(perf_rel)) %>%
      bind_rows(data.frame(ticker = "SPY", perf = perf_spy)) %>%
      mutate(xmin = win$xmin, xmax = win$xmax, Phase = win$Phase)
  })
  
  # 4. RENDERING THE VISUAL
  ggplot(plot_df) +
    # Draw Tiles
    geom_rect(aes(xmin = xmin, xmax = xmax, 
                  ymin = as.numeric(factor(ticker, levels = rev(all_assets)))-0.4, 
                  ymax = as.numeric(factor(ticker, levels = rev(all_assets)))+0.4, 
                  fill = perf), color = "white", size = 0.3) +
    
    # VERTICAL DASHED LINES (Start of DD)
    geom_vline(data = filter(regime_windows, Phase == "DD"), 
               aes(xintercept = xmin), linetype = "dashed", color = "#1D3557", alpha = 0.8) +
    
    # DATE LABELS AT BOTTOM OF DASHED LINES
    geom_text(data = filter(regime_windows, Phase == "DD"),
              aes(x = xmin, y = 0.5, label = format(xmin, "%Y-%m")), 
              angle = 90, hjust = 1.1, size = 3, color = "#1D3557") +
    
    # PERF LABELS
    geom_text(aes(x = xmin + (xmax-xmin)/2, 
                  y = as.numeric(factor(ticker, levels = rev(all_assets))), 
                  label = paste0(round(perf*100, 1), "%")), 
              color = "white", size = 3, fontface = "bold") +
    
    # Sunday Palette
    scale_fill_gradientn(colors = c("#8B0000", "#BC4749", "#2B2D42", "#52B788", "#006400"), 
                         values = rescale(c(-0.25, -0.05, 0, 0.05, 0.25)), na.value = "grey30") +
    
    scale_x_date(expand = c(0.05, 0.05)) +
    scale_y_continuous(breaks = 1:length(all_assets), labels = rev(all_assets)) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(face = "bold", size = 18),
          plot.background = element_rect(fill = "white", color = NA)) +
    labs(title = "Regime-Sync Alpha Tape", 
         subtitle = paste0("Focus: SPY Drawdowns > ", abs(trigger)*100, "% | Vertical Dashes: Start Date of DD"))
}

# 5. EXECUTE & SAVE
# Use your target list
target_list <- c("XLK", "XLY", "XLI", "XLF", "XLV", "XLP", "QUAL", "VLUE")
p_regime <- plot_regime_tape(xts_abs, xts_rel, target_list, trigger = -0.10)

ggsave(file.path(base_path, "key_plots/spy_DD_regime.png"), p_regime, width = 18, height = 10, dpi = 300)