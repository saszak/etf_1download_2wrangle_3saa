# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./03_reporting_engine/02_snapshot_visuals.R
# Purpose: High-Precision (1-decimal) Snapshots and Categorized Bar Charts
# ==============================================================================

library(tidyverse)
library(tidyquant)
library(scales)
library(knitr)
library(lubridate)

# --- 1. CORE ENGINE: SNAPSHOT GENERATOR ---
# Generates high-precision performance data across 4 viewports
get_project_snapshot_from_xts <- function(xts_r, etf_metadata) {
  
  # Setup Date Viewports
  latest_date <- max(index(xts_r))
  date_ytd    <- floor_date(latest_date, "year")
  date_mtd    <- floor_date(latest_date, "month")
  date_wk     <- latest_date - days(7)
  
  # Calculation Core
  perf_data <- map_df(etf_metadata$ticker, function(t_sym) {
    if (t_sym %in% colnames(xts_r)) {
      r_vec <- xts_r[, t_sym]
      
      # Safety: Handle short histories or NAs in specific periods
      safe_cum_ret <- function(vec, start_date) {
        sub_vec <- vec[paste0(start_date, "::")]
        if(length(sub_vec) == 0) return(0)
        return(as.numeric(Return.cumulative(sub_vec)))
      }
      
      tibble(
        ticker = t_sym,
        Day    = as.numeric(last(r_vec)),
        Week   = safe_cum_ret(r_vec, date_wk),
        MTD    = safe_cum_ret(r_vec, date_mtd),
        YTD    = safe_cum_ret(r_vec, date_ytd)
      )
    }
  })
  
  # Structured Merge with Metadata
  final_snapshot <- perf_data %>%
    inner_join(etf_metadata, by = "ticker") %>%
    select(category, sub_block, ticker, purpose, Day, Week, MTD, YTD) %>%
    group_by(category) %>%
    arrange(category, desc(YTD)) %>%
    ungroup()
  
  return(final_snapshot)
}

# --- 2. RENDER: CONSOLE TABLE (1-Decimal Precision) ---
render_project_snapshot <- function(snap_df) {
  message("📋 Rendering Precision Table (1-Decimal)...")
  snap_df %>%
    mutate(across(c(Day, Week, MTD, YTD), ~ percent(.x, accuracy = 0.1))) %>%
    knitr::kable()
}

# --- 3. PLOT: CATEGORIZED SNAPSHOT (Plain Steelblue Style) ---
plot_project_snapshot <- function(snap_df, period = "YTD") {
  
  # Clean up dataframe for plotting
  plt_df <- snap_df %>% filter(!is.na(.data[[period]]))
  
  ggplot(plt_df, aes(x = reorder(ticker, .data[[period]]), 
                     y = .data[[period]])) +
    
    # Plain Bars
    geom_col(fill = "steelblue", width = 0.7) +
    
    # Precision Labels
    geom_text(aes(label = percent(.data[[period]], accuracy = 0.1)), 
              hjust = ifelse(plt_df[[period]] >= 0, -0.2, 1.2), 
              size = 3) +
    
    facet_grid(category ~ ., scales = "free_y", space = "free_y") +
    coord_flip() +
    
    scale_y_continuous(labels = percent_format(accuracy = 0.1), 
                       expand = expansion(mult = c(0.15, 0.15))) +
    theme_minimal() +
    labs(
      title = paste("Portfolio Performance:", period),
      subtitle = paste("As of:", Sys.Date()),
      x = NULL, y = NULL
    ) +
    theme(
      strip.text.y = element_text(angle = 0, size = 10, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text = element_text(size = 9)
    )
}