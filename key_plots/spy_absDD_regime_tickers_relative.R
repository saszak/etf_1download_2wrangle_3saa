# ==============================================================================
# MODULE 01: spy_absDD_regime_tickers_relative (MASTER BASELINE + DUAL MARKERS)
# Purpose: SPY-Driven X-Axis | Extended Segment Markers for Vertical Dates
# ==============================================================================
library(tidyverse)
library(PerformanceAnalytics)
library(xts)
library(timetk)
library(scales)

base_path=here()
# 1. LOAD & CLEAN
xts_abs <- read_rds( file.path(base_path, "01_data_raw/raw_data.rds") )
xts_rel <- read_rds(file.path(base_path, "02_data_processed/xts_rel.rds"))

if ("symbol" %in% colnames(xts_abs)) {
  xts_abs <- xts_abs %>% pivot_wider(names_from = symbol, values_from = adjusted) %>%
    tk_xts(date_var = date, silent = TRUE)
}
colnames(xts_abs) <- gsub("\\..*$", "", colnames(xts_abs))
colnames(xts_rel) <- gsub("\\..*$", "", colnames(xts_rel))

# 2. THE REGIME DEFINER
get_master_regimes <- function(prices_xts, trigger = -0.10) {
  spy_rets <- Return.calculate(prices_xts[, "SPY"]) %>% na.omit()
  
  dd_table <- table.Drawdowns(spy_rets, top = 10) %>% 
    filter(Depth <= trigger) %>% 
    arrange(From)
  
  regimes <- map_df(1:nrow(dd_table), function(i) {
    row <- dd_table[i,]
    bind_rows(
      data.frame(xmin = as.Date(row$From), xmax = as.Date(row$Trough), Phase = "DD"),
      data.frame(xmin = as.Date(row$Trough), xmax = as.Date(row$To), Phase = "Recovery")
    )
  })
  
  blue_skies <- data.frame(
    xmin = regimes$xmax[-nrow(regimes)], 
    xmax = regimes$xmin[-1], 
    Phase = "Blue Sky"
  ) %>% filter(xmin < xmax)
  
  bind_rows(regimes, blue_skies) %>% arrange(xmin)
}

# 3. MAPPING ENGINE
plot_master_baseline <- function(xts_p, xts_r, tickers, trigger_val = -0.10) {
  
  regimes <- get_master_regimes(xts_p, trigger_val)
  spy_rets <- Return.calculate(xts_p[, "SPY"]) %>% na.omit()
  
  plot_df <- map_df(1:nrow(regimes), function(i) {
    win <- regimes[i,]
    date_rng <- paste0(win$xmin, "/", win$xmax)
    
    perf_spy <- sum(spy_rets[date_rng], na.rm = TRUE)
    avail <- intersect(colnames(xts_r), tickers)
    perf_rel <- if(length(avail) > 0) colSums(xts_r[date_rng, avail], na.rm = TRUE) else rep(NA, length(tickers))
    
    data.frame(ticker = names(perf_rel), perf = as.numeric(perf_rel), type = "Alpha") %>%
      bind_rows(data.frame(ticker = "SPY", perf = perf_spy, type = "Baseline")) %>%
      mutate(xmin = win$xmin, xmax = win$xmax, Phase = win$Phase)
  })
  
  all_levels <- c(tickers, "SPY")
  plot_df$ticker <- factor(plot_df$ticker, levels = rev(all_levels))
  
  # 4. RENDER WITH EXTENDED DUAL MARKERS
  ggplot(plot_df) +
    # The Continuous Performance Ribbon
    geom_rect(aes(xmin = xmin, xmax = xmax, 
                  ymin = as.numeric(ticker)-0.45, ymax = as.numeric(ticker)+0.45, 
                  fill = perf)) +
    
    # DUAL VERTICAL MARKERS (USING GEOM_SEGMENT TO EXTEND DOWNWARDS)
    # y = Top of chart, yend = -0.5 (1cm below the SPY bar)
    geom_segment(aes(x = xmin, xend = xmin, 
                     y = length(all_levels) + 0.5, yend = -0.5, 
                     linetype = Phase), 
                 color = "white", alpha = 0.5, size = 0.6) +
    
    scale_linetype_manual(values = c("DD" = "dashed", "Recovery" = "dotted", "Blue Sky" = "solid"), guide = "none") +
    
    # VERTICAL DATES (Hanging off the extended markers)
    geom_text(aes(x = xmin, y = -0.6, label = format(xmin, "%Y-%m-%d")), 
              angle = 90, size = 3, fontface = "bold", color = "#1D3557", hjust = 1) +
    
    # PERCENTAGE LABELS
    geom_text(aes(x = xmin + (xmax-xmin)/2, y = as.numeric(ticker), 
                  label = paste0(round(perf*100, 1), "%")), 
              color = "white", size = 3.2, fontface = "bold") +
    
    # SUNDAY NIGHT PALETTE
    scale_fill_gradientn(colors = c("#660708", "#A4161A", "#161A1D", "#2D6A4F", "#081C15"), 
                         values = rescale(c(-0.25, -0.05, 0, 0.05, 0.25)), na.value = "grey20") +
    
    scale_x_date(expand = c(0,0)) +
    # Expand y-axis downwards to accommodate the extended lines and labels
    scale_y_continuous(breaks = 1:length(all_levels), labels = rev(all_levels), 
                       expand = expansion(add = c(1.5, 0.5))) +
    
    theme_minimal() +
    theme(panel.grid = element_blank(), 
          axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(face = "bold", color = "#1D3557"),
          plot.margin = margin(10, 10, 80, 10), 
          plot.background = element_rect(fill = "white", color = NA)) +
    labs(title = "Regime-Sync Alpha Tape: The Sovereign Baseline", 
         subtitle = "Dashed: DD Start | Dotted: Trough | Extended Markers with Vertical Date Anchors")
}

# 5. EXECUTE
target_list <- c("XLK", "XLY", "XLI", "XLF", "XLV", "XLP", "QUAL", "VLUE")
p_master <- plot_master_baseline(xts_abs, xts_rel, target_list, trigger_val = -0.10)
p_master

# Save to the T7 SSD structure
# ggsave(file.path(base_path, "key_plots/regime_alpha_tape.png"), p_master, width = 22, height = 11, dpi = 300)

message("🏆 Restoration Complete: Vertical lines now clear the SPY bar for the date labels.")