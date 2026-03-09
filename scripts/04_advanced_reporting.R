# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts/04_advanced_reporting.R
# Purpose: Stage 04 - Sentinel "Full Map" Intelligence (HARDENED SCOPE)
# ==============================================================================

# --- PHASE 0: INFRASTRUCTURE & GLOBAL SCOPE ---
library(here)
library(tidyverse)
library(PerformanceAnalytics)
library(ggrepel)
library(scales)

if(!exists("project_tree")) source(here("project_tree.R"))

# Force-reload metadata to ensure it's in GLOBAL scope
if(!exists("etf_metadata")) source(here(project_tree$scripts$init))

# 🛡️ GLOBAL METADATA PINNING
# Using <<- ensures 'standard_metadata' is available for the Step 1.3 Join
standard_metadata <<- etf_metadata %>%
  mutate(category = coalesce(
    if ("Strategy_Function" %in% names(.)) Strategy_Function else NULL,
    if ("strat_func" %in% names(.)) strat_func else NULL,
    "Uncategorized"
  ))

# Load data products
xts_ret      <- read_rds(here(project_tree$products$refined_ret))
raw_data     <- read_rds(here(project_tree$products$raw_p_d))
tech_summary <- read_rds(here(project_tree$products$tech_summary))

# ==============================================================================
# [PHASE 1] THE ENGINE (Sequence: Sigma -> Nominal -> Join)
# ==============================================================================

# 1.1 Calculate Statistical Sigma
message("🔍 Step 1.1: Calculating Sigma...")
sigma_base_df <- map_df(colnames(xts_ret), function(t) {
  series <- na.omit(xts_ret[, t])
  if(nrow(series) < 252) return(NULL)
  r_today <- as.numeric(last(series))
  vol_daily <- as.numeric(StdDev.annualized(tail(series, 252))) / sqrt(252)
  tibble(ticker = t, z_score = round(r_today / vol_daily, 2))
})

# 1.2 Calculate Nominal Reality
message("📈 Step 1.2: Calculating Nominal YTD...")
ytd_nominal_df <- raw_data %>%
  group_by(symbol) %>%
  reframe(
    p_start = adjusted[date == max(date[format(date, "%Y") == "2025"])],
    p_now   = last(adjusted),
    ytd_nominal = (p_now / p_start) - 1
  ) %>%
  rename(ticker = symbol)

# 1.3 THE UNIFIED JOIN
# Now standard_metadata is guaranteed to be in the Global Env
message("🔗 Step 1.3: Creating unified_mom_df...")
unified_mom_df <<- sigma_base_df %>%
  left_join(ytd_nominal_df, by = "ticker") %>%
  left_join(standard_metadata, by = "ticker") %>% 
  left_join(tech_summary %>% select(-any_of(c("date", "adjusted"))), by = "ticker") %>%
  mutate(
    momentum_cluster = if_else(dist_10 > 0 & dist_50 > 0 & dist_200 > 0, 
                               "🔥 FULL MOMENTUM", "---"),
    action_signal = case_when(
      dist_10 > 0.03 & z_score > 2.0   ~ "TRIM/HEDGE", 
      dist_10 < -0.02 & z_score < -1.5 & trend_regime == "Bullish" ~ "STRENGTH BUY",
      dist_10 < 0 & z_score > 1.5 & trend_regime == "Bearish" ~ "RELIEF RALLY",
      dist_10 > 0 & z_score < -1.0 & trend_regime == "Bullish" ~ "COOLING/ADD",
      TRUE ~ "HOLD/OBSERVE"
    )
  )

# ==============================================================================
# [PHASE 2] THE VISUALS
# ==============================================================================
message("🎨 Step 2.0: Generating Plot Objects...")

theme_sovereign_bold <- function() {
  theme_minimal() + 
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "#E0E0E0"),
      legend.position = "bottom",
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = rel(1.2))
    )
}

plot_technical_exhaustion_quadrant <<- function(df) {
  x_max <- max(abs(df$dist_10), na.rm=T) * 1.2
  y_max <- max(abs(df$z_score), na.rm=T) * 1.2
  
  ggplot(df, aes(x = dist_10, y = z_score, color = trend_regime)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
    annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf, fill = "red", alpha = 0.05) +
    annotate("text", x = x_max*0.6, y = y_max*0.8, label = "EXHAUSTED", color = "red", fontface="bold", alpha=0.4) +
    annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0, fill = "green", alpha = 0.05) +
    annotate("text", x = -x_max*0.6, y = -y_max*0.8, label = "DISCOUNT", color = "darkgreen", fontface="bold", alpha=0.4) +
    geom_point(aes(size = abs(dist_200)), alpha = 0.7) +
    geom_text_repel(aes(label = ticker), fontface = "bold") +
    scale_x_continuous(labels = percent) +
    scale_color_manual(values = c("Bullish" = "#003049", "Bearish" = "#D62828", "Neutral" = "grey50")) +
    theme_sovereign_bold() +
    labs(title = "Technical Exhaustion Map", x = "10-Day Stretch (%)", y = "Daily Sigma (Z)")
}

# Export plots globally for Shiny
p_sigma <<- ggplot(unified_mom_df, aes(x = reorder(ticker, z_score), y = z_score, fill = z_score)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.3) +
  coord_flip() + theme_sovereign_bold() +
  scale_fill_gradient2(low = "#D62828", mid = "#F7F7F7", high = "#003049") +
  labs(title = "Daily Sigma Shock Analysis", y = "Z-Score", x = "")

p_quad_nominal <<- ggplot(unified_mom_df, aes(x = ytd_nominal, y = z_score, color = category)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_text_repel(aes(label = ticker), fontface = "bold") +
  scale_x_continuous(labels = percent) +
  theme_sovereign_bold() +
  labs(title = "Nominal Reality Quadrant", x = "Actual YTD Return (%)", y = "Daily Shock (Z)")

p_eff <<- ggplot(unified_mom_df, aes(x = dist_200, y = ytd_nominal, color = category)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_text_repel(aes(label = ticker), fontface = "bold") +
  scale_x_continuous(labels = percent) + 
  scale_y_continuous(labels = percent) +
  theme_sovereign_bold() + 
  labs(title = "Trend Strength vs Return", x = "200-Day Stretch (%)", y = "YTD Return (%)")

message("✅ Stage 04: Objects Merged and Exported Globally.")