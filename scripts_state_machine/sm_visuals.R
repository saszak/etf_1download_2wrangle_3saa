##################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts_state_machine/sm_visuals.R
# Style: Sentinel Institutional (Standard + Audit + Relative Extension)
# Purpose: Core Visualization Library for the 3State Hysteresis Machine
##################################################################################

library(tidyverse)
library(patchwork)
library(scales)
library(PerformanceAnalytics)

# Initialize Global Registry if missing
if(!exists("key_plots")) key_plots <- list()

# ------------------------------------------------------------------------------
# 1. plot_signal_xts()
# USE CASE: Core Dashboard Visualization
# - Purpose: Provides a high-density view of Price vs SMA and the Hysteresis Oscillator.
# - Best for: Shiny UI panels and identifying regime flips.
# - Logic: Maps the State Machine Signal to colors (Bull/Neutral/Bear).
# ------------------------------------------------------------------------------
plot_signal_xts <- function(xts_data, ticker_name = "Asset", thru = 0.04, thrd = -0.02) {
  
  # --- 1. DATA PREP ---
  df <- data.frame(Date = index(xts_data), coredata(xts_data)) %>%
    mutate(
      regime = case_when(
        Signal == 1 ~ "Bull",
        Signal == 0 & Dist200 >= thrd ~ "Neutral",
        Signal == 0 & Dist200 < thrd ~ "Bear"
      ),
      next_date = lead(Date, default = max(Date))
    )
  
  dyn_subtitle <- paste0("Basis: 200-Day SMA | Triggers: +", percent(thru), " / ", percent(thrd))
  palette <- c("Bull" = "#2A9D8F", "Neutral" = "#E9C46A", "Bear" = "#E76F51")
  
  # --- 2. TOP PANEL: PRICE & REGIME ---
  p1 <- ggplot(df, aes(x = Date)) +
    geom_rect(aes(xmin = Date, xmax = next_date, ymin = -Inf, ymax = Inf, fill = regime), 
              alpha = 0.1, show.legend = FALSE) +
    geom_line(aes(y = Adjusted), color = "#003049", linewidth = 0.5) +
    geom_line(aes(y = SMA200), color = "#D62828", linetype = "3131", linewidth = 0.4) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_fill_manual(values = palette) +
    scale_y_continuous(labels = dollar_format(), position = "right") +
    labs(title = paste(ticker_name, "STATE MACHINE"), subtitle = dyn_subtitle) +
    theme_minimal(base_family = "sans") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey80", linewidth = 0.3), 
      panel.grid.major.y = element_line(color = "#EDEDED", linewidth = 0.2),
      plot.title = element_text(face = "bold", size = 14, color = "#003049"),
      plot.subtitle = element_text(size = 10, color = "#333333", face = "italic"),
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      plot.margin = margin(l = 60, r = 5, t = 5, b = 0)
    )
  
  # --- 3. BOTTOM PANEL: OSCILLATOR & REGIME ---
  p2 <- ggplot(df, aes(x = Date, y = Dist200)) +
    geom_rect(aes(xmin = Date, xmax = next_date, ymin = -Inf, ymax = Inf, fill = regime), 
              alpha = 0.1, show.legend = FALSE) +
    geom_hline(yintercept = 0, color = "grey80", linewidth = 0.3) +
    geom_line(color = "#252525", linewidth = 0.4) +
    geom_hline(yintercept = thru, color = "#2A9D8F", linetype = "dashed", linewidth = 0.5) +
    geom_hline(yintercept = thrd, color = "#E76F51", linetype = "dashed", linewidth = 0.5) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_fill_manual(values = palette) +
    scale_y_continuous(labels = percent_format(accuracy = 1), position = "right") +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey80", linewidth = 0.3), 
      panel.grid.major.y = element_line(color = "#EDEDED", linewidth = 0.2),
      axis.title = element_blank(),
      axis.text.x = element_blank(), 
      plot.margin = margin(l = 60, r = 5, t = 0, b = 0)
    )
  
  return(p1 / p2 + plot_layout(heights = c(3, 1)))
}

# ------------------------------------------------------------------------------
# 2. plot_signal_with_dd()
# USE CASE: Strategy Stress-Testing & Performance Audit
# - Purpose: Syncs the 3State Machine with real-world Drawdown (Risk).
# - Best for: Validating if the "Bear" signal (Red) captures major drawdowns.
# ------------------------------------------------------------------------------
plot_signal_with_dd <- function(xts_data, returns_xts, ticker_name = "Asset", thru = 0.04, thrd = -0.02) {
  
  # 1. Generate standard Sentinel panels
  p_main <- plot_signal_xts(xts_data, ticker_name, thru, thrd) 
  
  # 2. Precision Date Alignment Logic
  common_dates <- index(xts_data)
  returns_sync <- returns_xts[common_dates]
  
  # 3. Calculate Drawdown
  dd_xts <- PerformanceAnalytics::Drawdowns(returns_sync)
  df_dd  <- data.frame(Date = index(dd_xts), DD = as.numeric(coredata(dd_xts)))
  
  # 4. Create Synchronized DD Panel
  p_dd <- ggplot(df_dd, aes(x = Date, y = DD)) +
    geom_area(fill = "#E76F51", alpha = 0.2) + 
    geom_line(color = "#E76F51", linewidth = 0.5) +
    scale_y_continuous(labels = scales::percent_format(), position = "right") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0), limits = range(common_dates)) + 
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey80", linewidth = 0.4, linetype = "solid"), 
      panel.grid.major.y = element_line(color = "#EDEDED", linewidth = 0.2),
      axis.title = element_blank(),
      axis.text.x = element_text(size = 9, color = "#333333", face = "bold"),
      plot.margin = margin(l = 60, r = 5, t = 0, b = 5) 
    )
  
  # 5. Assemble with Patchwork
  final_plot <- (patchwork::wrap_plots(p_main) / p_dd) + 
    patchwork::plot_layout(heights = c(4, 1))
  
  return(final_plot)
}

# ------------------------------------------------------------------------------
# 3. plot_200DMA()
# USE CASE: Automated Batch Research (The Workflow Engine)
# - Purpose: High-level wrapper that handles data enrichment + plotting.
# - Note: Automatically saves to global 'key_plots' list.
# ------------------------------------------------------------------------------
plot_200DMA <- function(ticker_symbol, xts_wealth, xts_price, upper = 0.04, lower = -0.02) {
  
  if (!ticker_symbol %in% colnames(xts_wealth)) {
    stop(paste("Error: Ticker", ticker_symbol, "not found."))
  }
  
  cat(paste0("⚙️ Running State Machine Enrichment: ", ticker_symbol, "\n"))
  
  # Calculation call
  xts_enriched <- get_enriched_xts_data(xts_wealth[, ticker_symbol], upper, lower)
  
  # Plot generation
  p <- plot_signal_with_dd(xts_enriched, xts_price[, ticker_symbol], ticker_symbol, thru = upper, thrd = lower)
  
  # Register in key_plots list
  if(!exists("key_plots")) key_plots <<- list()
  plot_key <- paste0("audit_", tolower(ticker_symbol))
  key_plots[[plot_key]] <<- p
  
  print(p)
  return(invisible(xts_enriched))
}

# ------------------------------------------------------------------------------
# 4. plot_relative_state_audit()
# USE CASE: Sector Rotation & Alpha Identification
# - Purpose: Applies the State Machine to Relative Strength (Ticker vs SPY).
# - Logic: Uses Base-1.0 Wealth Index for stable hysteresis math.
# ------------------------------------------------------------------------------
plot_relative_state_audit <- function(rel_ret_xts, ticker_name, thru = 0.04, thrd = -0.02) {
  
  if(!exists("key_plots")) key_plots <<- list()
  
  # Build stable Wealth Index
  rel_wealth <- cumprod(1 + rel_ret_xts)
  colnames(rel_wealth) <- "Adjusted"
  
  # Enrichment
  df_meta <- data.frame(Date = index(rel_wealth), coredata(rel_wealth)) %>%
    mutate(
      SMA200  = TTR::SMA(Adjusted, n = 200),
      Dist200 = (Adjusted / SMA200) - 1,
      Signal  = calc_asymmetric_state(Dist200, thru = thru, thrd = thrd)
    ) %>%
    filter(!is.na(SMA200))
  
  rel_state_xts <- xts(df_meta[,-1], order.by = df_meta$Date)
  
  # Generate Plot
  p_audit <- plot_signal_with_dd(
    xts_data = rel_state_xts, 
    returns_xts = rel_ret_xts, 
    ticker_name = paste(ticker_name, "Relative Strength (vs SPY)"),
    thru = thru, 
    thrd = thrd
  )
  
  # Auto-Registry
  plot_key <- paste0("rel_", tolower(ticker_name))
  key_plots[[plot_key]] <<- p_audit
  
  return(p_audit)
}

##################################################################################
# END OF FILE: sm_visuals.R
##################################################################################