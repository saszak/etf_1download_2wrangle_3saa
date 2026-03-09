# ==============================================================================
# MODULE 01: 05a_VIS_ALPHA_TAPE (SUNDAY RESTORATION)
# ==============================================================================
library(tidyverse)
library(PerformanceAnalytics)
library(xts)
library(timetk)
library(scales)

# 1. SETUP & DATA LOAD
plot_dir <- file.path(base_path, "key_plots")
xts_abs  <- read_rds(file.path(base_path, "data_raw/raw_p_d.rds"))
xts_rel  <- read_rds(file.path(base_path, "01_etf_wrangle/data_processed/rel_ret_d.rds"))

# Force Wide XTS & Clean Names
if ("symbol" %in% colnames(xts_abs)) {
  xts_abs <- xts_abs %>% pivot_wider(names_from = symbol, values_from = adjusted) %>%
    tk_xts(date_var = date, silent = TRUE)
}
colnames(xts_abs) <- gsub("\\..*$", "", colnames(xts_abs))
colnames(xts_rel) <- gsub("\\..*$", "", colnames(xts_rel))

# 2. IDENTIFY REGIME MARKERS (The Vertical Lines)
spy_rets <- Return.calculate(xts_abs[, "SPY"]) %>% na.omit()
dd_table <- table.Drawdowns(spy_rets, top = 5) %>% filter(Depth <= -0.10)
# These dates become your "Sunday" vertical lines
regime_markers <- unique(c(as.Date(dd_table$From), as.Date(dd_table$To)))

# 3. GENERATE PLOT DATA
q_breaks <- seq(floor_date(start(xts_rel), "quarter"), ceiling_date(end(xts_rel), "quarter"), by = "3 months")
timeline <- data.frame(xmin = q_breaks[-length(q_breaks)], xmax = q_breaks[-1])

target_sectors <- c("XLK", "XLY", "XLI", "XLF", "XLV", "XLP", "QUAL", "VLUE")
all_rows <- c(target_sectors, "SPY")

plot_df <- map_df(1:nrow(timeline), function(i) {
  rng <- paste0(timeline$xmin[i], "/", timeline$xmax[i])
  avail <- intersect(colnames(xts_rel), target_sectors)
  
  # Calculate returns (using colSums for log-return stability)
  perf_spy <- as.numeric(sum(spy_rets[rng], na.rm = TRUE))
  perf_rel <- if(length(avail) > 0) colSums(xts_rel[rng, avail], na.rm = TRUE) else rep(NA, length(target_sectors))
  
  data.frame(ticker = names(perf_rel), V1 = as.numeric(perf_rel)) %>%
    bind_rows(data.frame(ticker = "SPY", V1 = perf_spy)) %>%
    mutate(xmin = timeline$xmin[i], xmax = timeline$xmax[i])
})

# 4. RENDER WITH VERTICAL MARKERS
p_tape <- ggplot(plot_df) +
  geom_rect(aes(xmin = xmin, xmax = xmax, 
                ymin = as.numeric(factor(ticker, levels = rev(all_rows)))-0.45, 
                ymax = as.numeric(factor(ticker, levels = rev(all_rows)))+0.45, 
                fill = V1), color = "white") +
  # THE SUNDAY VERTICAL LINES
  geom_vline(xintercept = regime_markers, linetype = "dotted", color = "#1D3557", size = 0.8, alpha = 0.6) +
  geom_text(aes(x = xmin + (xmax-xmin)/2, y = as.numeric(factor(ticker, levels = rev(all_rows))), 
                label = paste0(round(V1*100, 1), "%")), color = "white", size = 2.8, fontface = "bold") +
  scale_fill_gradientn(colors = c("#8B0000", "#BC4749", "#2B2D42", "#52B788", "#006400"), 
                       values = rescale(c(-0.15, -0.05, 0, 0.05, 0.15)), na.value = "grey80") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0,0)) +
  scale_y_continuous(breaks = 1:length(all_rows), labels = rev(all_rows)) +
  theme_minimal() + 
  theme(panel.grid = element_blank(), axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 16)) +
  labs(title = "Relative to Abs SPY Returns", 
       subtitle = "Vertical Dotted Lines: SPY Regime Entry/Exit Markers")

ggsave(file.path(plot_dir, "spy_rel_sector_quarterly_no2.png"), p_tape, width = 18, height = 10, dpi = 300)
message("🏁 Sunday Restoration Complete: key_plots/regime_alpha_tape.png")