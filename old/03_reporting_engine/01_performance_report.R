# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./03_reporting_engine/01_performance_report.R
# Purpose: Generate Daily/Weekly/Monthly/YTD Performance Snapshots
# ==============================================================================

library(tidyverse)
library(tidyquant)
library(scales)
library(ggthemes)

message("📊 Reporting Engine: Generating Performance Snapshots...")

# ------------------------------------------------------------------------------
# 1. HELPER: CALCULATE MOMENTUM TABLE
# ------------------------------------------------------------------------------
# Creates a clean D/W/M/YTD summary table for all clusters
get_momentum_summary <- function(abs_list, metadata) {
  
  # Extract the last returns for each frequency
  d_ret <- abs_list$d %>% tail(1) %>% t() %>% as.data.frame() %>% rownames_to_column("ticker") %>% setNames(c("ticker", "Day"))
  w_ret <- abs_list$w %>% tail(1) %>% t() %>% as.data.frame() %>% rownames_to_column("ticker") %>% setNames(c("ticker", "Week"))
  m_ret <- abs_list$m %>% tail(1) %>% t() %>% as.data.frame() %>% rownames_to_column("ticker") %>% setNames(c("ticker", "Month"))
  
  # Calculate YTD from daily
  curr_year <- format(Sys.Date(), "%Y")
  ytd_ret <- abs_list$d %>%
    tk_tbl() %>%
    filter(format(index, "%Y") == curr_year) %>%
    pivot_longer(-index) %>%
    group_by(name) %>%
    summarise(YTD = prod(1 + value) - 1) %>%
    rename(ticker = name)
  
  # Join all and add metadata
  summary_table <- d_ret %>%
    left_join(w_ret, by = "ticker") %>%
    left_join(m_ret, by = "ticker") %>%
    left_join(ytd_ret, by = "ticker") %>%
    left_join(metadata, by = "ticker") %>%
    select(ticker, sub_block, cluster, Day, Week, Month, YTD) %>%
    arrange(sub_block, desc(YTD))
  
  return(summary_table)
}

# ------------------------------------------------------------------------------
# 2. PLOT: EUROPEAN REGIONAL STRENGTH
# ------------------------------------------------------------------------------
plot_regional_momentum <- function(summary_table) {
  
  # Filter specifically for Europe and Global vs US Anchors
  plot_df <- summary_table %>%
    filter(sub_block %in% c("Europe", "Global", "Anchor")) %>%
    pivot_longer(cols = c(Week, Month, YTD), names_to = "Horizon", values_to = "Return")
  
  p <- ggplot(plot_df, aes(x = reorder(ticker, Return), y = Return, fill = Horizon)) +
    geom_col(position = "dodge") +
    facet_wrap(~sub_block, scales = "free_y") +
    coord_flip() +
    scale_y_continuous(labels = percent) +
    theme_hc() +
    scale_fill_tq() +
    labs(
      title = "Regional Momentum: Europe vs. Global vs. US",
      subtitle = paste("Snapshot as of:", Sys.Date()),
      x = "", y = "Return %",
      fill = "Timeframe"
    )
  
  return(p)
}

# ------------------------------------------------------------------------------
# 3. EXPORT MODULE
# ------------------------------------------------------------------------------
run_full_reporting_suite <- function(abs_list, metadata) {
  
  # Create Output Folders if missing
  report_dir <- file.path(base_path, "output", "reports")
  plot_dir   <- file.path(base_path, "output", "plots")
  if (!dir.exists(report_dir)) dir.create(report_dir, recursive = TRUE)
  if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)
  
  # Generate Table
  summary_tab <- get_momentum_summary(abs_list, metadata)
  write_csv(summary_tab, file.path(report_dir, "momentum_summary.csv"))
  
  # Generate Regional Plot
  p_reg <- plot_regional_momentum(summary_tab)
  ggsave(file.path(plot_dir, "regional_momentum.png"), p_reg, width = 12, height = 7)
  
  message("✅ Reports exported to: ", report_dir)
  message("✅ Plots exported to: ", plot_dir)
  
  return(summary_tab)
}