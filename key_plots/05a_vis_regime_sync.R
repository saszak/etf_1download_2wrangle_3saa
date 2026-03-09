# ==============================================================================
# MODULE 01: 05a_VIS_ALPHA_TAPE
# Purpose: Generate Detailed Regime-Sync Alpha Tape (Sunday Version)
# ==============================================================================
library(tidyverse)
library(PerformanceAnalytics)
library(xts)
library(timetk)
library(scales)

# 1. SETUP DIRECTORY
plot_dir <- file.path(base_path, "key_plots")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

# 2. DATA INGESTION & CLEANING
xts_abs <- read_rds(file.path(base_path, "data_raw/raw_p_d.rds"))
xts_rel <- read_rds(file.path(base_path, "01_etf_wrangle/data_processed/rel_ret_d.rds"))

# Format Check: Convert Warehouse Long to Wide if necessary
if ("symbol" %in% colnames(xts_abs)) {
  xts_abs <- xts_abs %>%
    pivot_wider(names_from = symbol, values_from = adjusted) %>%
    tk_xts(date_var = date, silent = TRUE)
}
colnames(xts_abs) <- gsub("\\..*$", "", colnames(xts_abs))
colnames(xts_rel) <- gsub("\\..*$", "", colnames(xts_rel))

# 3. PLOT LOGIC
plot_detailed_alpha_tape <- function(xts_p, xts_a, tickers) {
  q_breaks <- seq(floor_date(start(xts_a), "quarter"), 
                  ceiling_date(end(xts_a), "quarter"), by = "3 months")
  
  full_timeline <- data.frame(xmin = q_breaks[-length(q_breaks)], xmax = q_breaks[-1])
  spy_rets <- Return.calculate(xts_p[, "SPY"]) %>% na.omit()
  all_rows <- c(tickers, "SPY")
  
  plot_df <- map_df(1:nrow(full_timeline), function(i) {
    rng <- paste0(full_timeline$xmin[i], "/", full_timeline$xmax[i])
    perf_spy <- Return.cumulative(spy_rets[rng])
    perf_rel <- Return.cumulative(xts_a[rng, intersect(colnames(xts_a), tickers)])
    
    as.data.frame(t(perf_rel)) %>% 
      rownames_to_column("ticker") %>% 
      bind_rows(data.frame(ticker = "SPY", V1 = as.numeric(perf_spy))) %>%
      mutate(xmin = full_timeline$xmin[i], xmax = full_timeline$xmax[i])
  })

  ggplot(plot_df) +
    geom_rect(aes(xmin = xmin, xmax = xmax, 
                  ymin = as.numeric(factor(ticker, levels = rev(all_rows)))-0.45, 
                  ymax = as.numeric(factor(ticker, levels = rev(all_rows)))+0.45, 
                  fill = V1), color = "white", size = 0.2) +
    geom_text(aes(x = xmin + (xmax-xmin)/2, y = as.numeric(factor(ticker, levels = rev(all_rows))), 
                  label = paste0(round(V1*100, 1), "%")), color = "white", size = 2.5, fontface = "bold") +
    scale_fill_gradientn(colors = c("#8B0000", "#BC4749", "#2B2D42", "#52B788", "#006400"), 
                         values = rescale(c(-0.15, -0.05, 0, 0.05, 0.15))) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0,0)) +
    scale_y_continuous(breaks = 1:length(all_rows), labels = rev(all_rows)) +
    theme_minimal() + 
    labs(title = "Detailed Alpha Tape", subtitle = "Relative to SPY (Bottom Row)") +
    theme(panel.grid = element_blank(), axis.title = element_blank())
}

# 4. EXECUTE & SAVE
target_sectors <- c("XLK", "XLY", "XLI", "XLF", "XLV", "XLP", "QUAL", "VLUE")
p_tape <- plot_detailed_alpha_tape(xts_abs, xts_rel, target_sectors)

ggsave(file.path(plot_dir, "regime_alpha_tape.png"), p_tape, width = 16, height = 9, dpi = 300)
message("✅ Alpha Tape saved to: key_plots/regime_alpha_tape.png")