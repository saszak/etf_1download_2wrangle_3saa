# ==============================================================================
# SOVEREIGN VISUAL ENGINE (XTS-NATIVE & PERFORMANCE ANALYTICS)
# Path: ./03_reporting_engine/03_advanced_visuals.R
# ==============================================================================
library(tidyverse); library(PerformanceAnalytics); library(ggrepel); library(scales); library(xts)

# --- THEME ---
theme_sovereign_soft <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.2), color = "#2C3E50"),
      panel.background = element_rect(fill = "#FFF1E0", color = NA),
      plot.background = element_rect(fill = "#FFF1E0", color = NA),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.key.size = unit(0.3, "cm"),
      legend.text = element_text(size = 8)
    )
}

# --- SIGMA DATA ENGINE (NATIVE XTS) ---
get_sigma_scores <- function(xts_r, metadata, window = 252) {
  # 1. Verification: Ensure input is xts and contains returns
  if(!is.xts(xts_r)) return(NULL)
  
  # 2. PerformanceAnalytics/TTR math on the full matrix
  # runSD and runMean handle the rolling window natively across columns
  sigma_list <- map_df(colnames(xts_r), function(t) {
    series <- na.omit(xts_r[, t])
    if(nrow(series) < 10) return(NULL)
    
    # We use the returns directly since signal_table is already Tier 2
    mu <- mean(tail(series, window), na.rm = TRUE)
    sv <- sd(tail(series, window), na.rm = TRUE)
    curr <- as.numeric(last(series))
    
    z_val <- if(!is.na(sv) && sv > 0.000001) (curr - mu) / sv else 0
    
    tibble(
      ticker = t, 
      z_score = z_val,
      ret_move = curr,   # Current Daily Return
      risk_vol = sv      # Historical Rolling Vol
    )
  })
  
  sigma_list %>% 
    left_join(metadata, by = "ticker") %>% 
    arrange(desc(abs(z_score)))
}

# --- SIGMA PLOT ---
plot_sigma_analysis <- function(sigma_df) {
  if(is.null(sigma_df) || nrow(sigma_df) == 0) return(NULL)
  
  # Outlier Management: Focus on top 25 disruptions
  plot_data <- sigma_df %>% slice(1:25)
  
  ggplot(plot_data, aes(x = reorder(ticker, z_score), y = z_score, fill = category)) +
    geom_bar(stat = "identity", width = 0.7, alpha = 0.8) +
    # Text label for the Analytical Sigma bar
    geom_text(aes(label = round(z_score, 2)), 
              hjust = ifelse(plot_data$z_score >= 0, -0.3, 1.3), 
              fontface = "bold", size = 3.5) +
    coord_flip() + 
    scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
    theme_sovereign_soft(base_size = 14) + 
    labs(title = "Sigma Disruption (Shock)", 
         subtitle = "Z-Score of current return vs trailing volatility",
         x = NULL, y = "Z-Score")
}

# --- RISK-REWARD PLOT ---
plot_synthetic_efficiency <- function(xts_r, metadata, limits = list(risk = c(0, 1), ret = c(-1, 1))) {
  # Native PerformanceAnalytics annualized stats
  # Assuming 252 trading days for daily return data
  eff_df <- map_df(colnames(xts_r), function(t) {
    r_series <- na.omit(xts_r[, t])
    if(nrow(r_series) < 10) return(NULL)
    
    tibble(
      ticker = t, 
      risk = as.numeric(PerformanceAnalytics::StdDev.annualized(r_series, scale = 252)),
      reward = as.numeric(PerformanceAnalytics::Return.annualized(r_series, scale = 252)),
      raw_ret = as.numeric(PerformanceAnalytics::Return.cumulative(r_series))
    )
  }) %>% 
    left_join(metadata, by = "ticker") %>% 
    filter(risk >= limits$risk[1], risk <= limits$risk[2], 
           reward >= limits$ret[1], reward <= limits$ret[2])
  
  ggplot(eff_df, aes(x = risk, y = reward, color = category)) +
    geom_point(aes(size = abs(raw_ret)), alpha = 0.6) +
    geom_text_repel(aes(label = ticker), size = 3.8, fontface = "bold") +
    scale_x_continuous(labels = percent) + 
    scale_y_continuous(labels = percent) +
    theme_sovereign_soft(base_size = 14) + 
    guides(size = "none") +
    labs(title = "Annualized Efficiency Frontier", 
         subtitle = "Risk (Annual Vol) vs Reward (Annual Return)",
         x = "Annualized Vol", y = "Annualized Return")
}