# ==============================================================================
# MODULE 01: 05a_VIS_TICKER_HEATMAP (NA-FIX VERSION)
# ==============================================================================
library(tidyverse)
library(PerformanceAnalytics)
library(xts)
library(timetk)
library(scales)

# 1. SETUP & DATA LOAD
plot_dir <- file.path(base_path, "key_plots")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

xts_abs <- read_rds(file.path(base_path, "data_raw/raw_p_d.rds"))
xts_rel <- read_rds(file.path(base_path, "01_etf_wrangle/data_processed/rel_ret_d.rds"))

# Force Wide XTS for Warehouse
if ("symbol" %in% colnames(xts_abs)) {
  xts_abs <- xts_abs %>% pivot_wider(names_from = symbol, values_from = adjusted) %>%
    tk_xts(date_var = date, silent = TRUE)
}

# AGGRESSIVE NAME CLEANING
# Ensures "XLK.Adjusted" becomes "XLK" in both files
colnames(xts_abs) <- gsub("\\..*$", "", colnames(xts_abs))
colnames(xts_rel) <- gsub("\\..*$", "", colnames(xts_rel))

# DIAGNOSTIC
message("🔎 Refinery Tickers Found: ", paste(colnames(xts_rel), collapse = ", "))

# 2. RENDER SUNDAY MASTERPIECE
q_breaks <- seq(floor_date(start(xts_rel), "quarter"), ceiling_date(end(xts_rel), "quarter"), by = "3 months")
timeline <- data.frame(xmin = q_breaks[-length(q_breaks)], xmax = q_breaks[-1])

# Match these exactly to what was in your Sunday image
target_sectors <- c("XLK", "XLY", "XLI", "XLF", "XLV", "XLP", "QUAL", "VLUE")
all_rows <- c(target_sectors, "SPY")

plot_df <- map_df(1:nrow(timeline), function(i) {
  rng <- paste0(timeline$xmin[i], "/", timeline$xmax[i])
  
  # Calculate Abs SPY
  spy_rets <- Return.calculate(xts_abs[, "SPY"]) %>% na.omit()
  perf_spy  <- as.numeric(Return.cumulative(spy_rets[rng]))
  
  # Calculate Rel Others - Using a safer subset
  avail_tickers <- intersect(colnames(xts_rel), target_sectors)
  
  if(length(avail_tickers) > 0) {
    # Pull relative returns directly
    rel_subset <- xts_rel[rng, avail_tickers]
    perf_rel <- colSums(rel_subset, na.rm = TRUE) # Cumulative log returns are additive
    
    df_others <- data.frame(
      ticker = names(perf_rel),
      V1 = as.numeric(perf_rel)
    )
  } else {
    df_others <- data.frame(ticker = target_sectors, V1 = NA)
  }
   
  # Combine
  bind_rows(df_others, data.frame(ticker = "SPY", V1 = perf_spy)) %>%
    mutate(xmin = timeline$xmin[i], xmax = timeline$xmax[i])
})

# 3. PLOT (Handling NAs visually just in case)
p_tape <- ggplot(plot_df) +
  geom_rect(aes(xmin = xmin, xmax = xmax, 
                ymin = as.numeric(factor(ticker, levels = rev(all_rows)))-0.45, 
                ymax = as.numeric(factor(ticker, levels = rev(all_rows)))+0.45, 
                fill = V1), color = "white") +
  geom_text(aes(x = xmin + (xmax-xmin)/2, 
                y = as.numeric(factor(ticker, levels = rev(all_rows))), 
                label = ifelse(is.na(V1), "err", paste0(round(V1*100, 1), "%"))), 
            color = "white", size = 2.8, fontface = "bold") +
  scale_fill_gradientn(colors = c("#8B0000", "#BC4749", "#2B2D42", "#52B788", "#006400"), 
                       values = rescale(c(-0.15, -0.05, 0, 0.05, 0.15)),
                       na.value = "grey50") + # Grey out if data is truly missing
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0,0)) +
  scale_y_continuous(breaks = 1:length(all_rows), labels = rev(all_rows)) +
  theme_minimal() + 
  theme(panel.grid = element_blank(), axis.title = element_blank(),
        plot.background = element_rect(fill = "white", color = NA)) +
  labs(title = "Relative to Abs SPY Returns", 
       subtitle = "X-Axis: Fixed Quarterly Timeline | Absolute SPY (Bottom Row)")

ggsave(file.path(plot_dir, "spy_rel_quarterly.png"), p_tape, width = 18, height = 10, dpi = 300)

message("🏁 Alpha Tape Update: Check key_plots/regime_alpha_tape.png")