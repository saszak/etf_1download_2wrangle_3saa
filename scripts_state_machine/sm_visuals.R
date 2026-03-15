# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts_state_machine/sm_visuals.R
# Style: Sentinel Institutional (Standard + Audit Extension)
# ==============================================================================
library(tidyverse)
library(patchwork)
library(scales)
library(PerformanceAnalytics)
# We name this plot_signal_xts so it "plugs in" to your master_run.R automatically
plot_signal_xts <- function(xts_data, ticker_name = "Asset", thru = 0.04, thrd = -0.02) {
  
  # --- 1. DATA PREP (Internal XTS to Tidy conversion) ---
  df <- data.frame(Date = index(xts_data), coredata(xts_data)) %>%
    mutate(
      # YOUR INSTITUTIONAL REGIME LOGIC
      regime = case_when(
        Signal == 1 ~ "Bull",
        Signal == 0 & Dist200 >= thrd ~ "Neutral",
        Signal == 0 & Dist200 < thrd ~ "Bear"
      ),
      next_date = lead(Date, default = max(Date))
    )
  
  # YOUR PALETTE & SUBTITLE
  dyn_subtitle <- paste0("Basis: 200-Day SMA | Triggers: +", percent(thru), " / ", percent(thrd))
  palette <- c("Bull" = "#2A9D8F", "Neutral" = "#E9C46A", "Bear" = "#E76F51")
  
  # --- 2. TOP PANEL: PRICE & REGIME ---
  p1 <- ggplot(df, aes(x = Date)) +
    geom_rect(aes(xmin = Date, xmax = next_date, ymin = -Inf, ymax = Inf, fill = regime), 
              alpha = 0.1, show.legend = FALSE) +
    geom_line(aes(y = Adjusted), color = "#003049", linewidth = 0.5) +
    geom_line(aes(y = SMA200), color = "#D62828", linetype = "3131", linewidth = 0.4) +
    
    # YOUR ANNUAL ANCHOR LOGIC
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
      axis.text.x = element_blank()
    )
  
  # --- 3. BOTTOM PANEL: OSCILLATOR & REGIME ---
  p2 <- ggplot(df, aes(x = Date, y = Dist200)) +
    geom_rect(aes(xmin = Date, xmax = next_date, ymin = -Inf, ymax = Inf, fill = regime), 
              alpha = 0.1, show.legend = FALSE) +
    geom_hline(yintercept = 0, color = "grey80", linewidth = 0.3) +
    geom_line(color = "#252525", linewidth = 0.4) +
    
    # YOUR THRESHOLD DASHES
    geom_hline(yintercept = thru, color = "#2A9D8F", linetype = "dashed", linewidth = 0.5) +
    geom_hline(yintercept = thrd, color = "#E76F51", linetype = "dashed", linewidth = 0.5) +
    
    # FORMATTING
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_fill_manual(values = palette) +
    scale_y_continuous(labels = percent_format(accuracy = 1), position = "right") +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey80", linewidth = 0.3), 
      panel.grid.major.y = element_line(color = "#EDEDED", linewidth = 0.2),
      axis.title = element_blank(),
      axis.text.x = element_text(size = 9, color = "#333333", face = "bold")
    )
  
  # --- 4. COMBINE ---
  p1 / p2 + plot_layout(heights = c(3, 1))
}



# ---AAA-------1.Price Chart + 200MA Dist Rversal  + DD Chart --------------------------------------------------------------------
# AUDIT EXTENSION: plot_signal_with_dd
# ------------------------------------------------------------------------------
#' @description Aligns the State Machine logic with a synchronized Drawdown panel.
#' @param xts_data The Plot-Specific Metadata (Price, SMA, Signal)
#' @param returns_xts The raw daily returns xts (e.g., from your SAA/TAA output)

plot_signal_with_dd <- function(xts_data, returns_xts, ticker_name = "Asset", thru = 0.04, thrd = -0.02) {
  
  # 1. Generate standard Sentinel panels (Inherits Right-side Y-Axis)
  # We suppress the x-axis text on p2 because the DD panel will provide it
  p_main <- plot_signal_xts(xts_data, ticker_name, thru, thrd) 
  
  # 2. Precision Date Alignment Logic
  common_dates <- index(xts_data)
  returns_sync <- returns_xts[common_dates]
  
  # 3. Calculate Drawdown
  dd_xts <- PerformanceAnalytics::Drawdowns(returns_sync)
  df_dd  <- data.frame(Date = index(dd_xts), DD = as.numeric(coredata(dd_xts)))
  
  # 4. Create Synchronized DD Panel (Sentinel Style)
  p_dd <- ggplot(df_dd, aes(x = Date, y = DD)) +
    geom_area(fill = "#E76F51", alpha = 0.2) + # Bearish Orange/Red
    geom_line(color = "#E76F51", linewidth = 0.5) +
    
    # Matching your Institutional Scale/Grid Logic
    scale_y_continuous(labels = percent_format(), position = "right") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0), limits = range(common_dates)) + 
    
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey80", linewidth = 0.3), 
      panel.grid.major.y = element_line(color = "#EDEDED", linewidth = 0.2),
      axis.title = element_blank(),
      axis.text.x = element_text(size = 9, color = "#333333", face = "bold"),
      # Critical: Lock the Left Margin to align with p_main's "clean" left edge
      plot.margin = margin(l = 60, r = 5, t = 0, b = 5) 
    )
  
  # 5. Assemble with Patchwork
  # Heights: Price (3), Oscillator (1), Drawdown (1)
  final_plot <- (p_main / p_dd) + plot_layout(heights = c(3, 1, 1))
  
  return(final_plot)
}