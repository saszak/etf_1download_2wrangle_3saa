# ==============================================================================
# PROJECT B: ETF_SAA_TAA - 05_VISUAL_DASHBOARD (SOVEREIGN EDITION)
# Purpose: DNA Spiders + Exposure Audit (Color & Cluster Fix)
# ==============================================================================
library(ggplot2)
library(dplyr)
library(scales)
library(fmsb)
library(PerformanceAnalytics)
library(tidyr)
library(readr)

# --- 1.0 DYNAMIC FILE DISCOVERY ---
message("📍 Mapping SSD Paths...")
base_path <- "/Volumes/T7Red_Work/Rstudio_ssd/etf_1download_2wrangle_3saa"

force_find <- function(pattern) {
  files <- list.files(path = base_path, pattern = pattern, recursive = TRUE, full.names = TRUE)
  files <- files[!grepl(".Rproj.user", files)]
  if(length(files) > 0) return(files[1])
  return(NULL)
}

path_db     <- file.path(base_path, "02_etf_saa_taa/data/etf_quantdb.rds")
path_matrix <- force_find("selection_matrix\\.rds$")
path_policy <- force_find("policy_list\\.rds$")
path_out    <- file.path(base_path, "02_etf_saa_taa/reports")

# --- 2.0 LOAD DATA ---
message("📂 Loading Data...")
etf_quantdb <- read_rds(path_db) %>% ungroup() 
metadata    <- read_rds(path_matrix) %>% select(ticker, cluster) %>% distinct()
policy_list <- read_rds(path_policy)

# --- 3.0 PREPARE DNA MATRIX ---
message("🧬 Calibrating Sovereign DNA Matrix...")

safe_rescale <- function(x) {
  x <- as.numeric(x)
  if(all(is.na(x)) || diff(range(x, na.rm = TRUE)) == 0) return(rep(0.5, length(x)))
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

dna_matrix <- etf_quantdb %>%
  select(ticker, alpha_6y, ann_ret, mom_d, vol_scalar, kurtosis_val, skew_val) %>%
  mutate(
    Alpha      = safe_rescale(alpha_6y),
    Return     = safe_rescale(ann_ret),
    Momentum   = safe_rescale(mom_d),
    RiskParity = safe_rescale(vol_scalar),
    Stability  = safe_rescale(-(abs(kurtosis_val) + abs(skew_val)))
  ) %>%
  select(ticker, Alpha, Return, Momentum, RiskParity, Stability)

# --- 4.0 VISUAL COMPONENT: EXPOSURE BAR CHART ---
create_exposure_plot <- function() {
  # Fix: Ensure cluster names are cleaned and predictable
  plot_df <- policy_list$TOTAL %>%
    left_join(metadata, by = "ticker") %>%
    mutate(cluster = case_when(
      is.na(cluster) ~ "CORE_ANCHOR",
      grepl("Alpha", cluster, ignore.case = TRUE) ~ "Multipolar Alpha",
      grepl("Pipes", cluster, ignore.case = TRUE) ~ "Pipes & Power",
      grepl("Cycle", cluster, ignore.case = TRUE) ~ "Cycle & Breadth",
      grepl("Bulwark|Defensive", cluster, ignore.case = TRUE) ~ "Defensive Bulwark",
      TRUE ~ cluster
    )) %>%
    arrange(net_weight)
  
  ggplot(plot_df, aes(x = reorder(ticker, net_weight), y = net_weight, fill = cluster)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_text(aes(label = percent(net_weight, accuracy = 0.1)), 
              hjust = -0.2, size = 3.5, fontface = "bold") +
    coord_flip() +
    scale_y_continuous(labels = percent, expand = expansion(mult = c(0, .15))) +
    # Sovereign Color Palette
    scale_fill_manual(values = c(
      "CORE_ANCHOR"       = "#343a40", 
      "Multipolar Alpha"  = "#2a9d8f",
      "Pipes & Power"     = "#264653",
      "Cycle & Breadth"   = "#e9c46a",
      "Defensive Bulwark" = "#e76f51"
    )) +
    labs(title = "Sovereign Book 3: Total Combined Exposure",
         subtitle = paste("Audit Date:", Sys.Date()),
         x = NULL, y = "Net Portfolio Weight (%)") +
    theme_minimal() +
    theme(legend.position = "bottom", 
          plot.title = element_text(face="bold", size = 14),
          panel.grid.major.y = element_blank())
}

# --- 5.0 VISUAL COMPONENT: SPIDER GRID ---

plot_etf_spider_grid <- function(target_tickers, cols = 3) {
  if(!"SPY" %in% target_tickers) target_tickers <- c("SPY", target_tickers)
  available_tickers <- intersect(target_tickers, dna_matrix$ticker)
  assets_to_plot <- available_tickers[available_tickers != "SPY"]
  
  if(length(assets_to_plot) == 0) return(message("ℹ️ No tactical tickers found."))
  
  rows <- ceiling(length(assets_to_plot) / cols)
  par(mfrow = c(rows, cols), mar = c(2, 2, 4, 2))
  
  for(t in assets_to_plot) {
    plot_data <- dna_matrix %>% filter(ticker %in% c("SPY", t))
    # Standardize Row Names for radarchart
    plot_df_radar <- as.data.frame(plot_data %>% select(-ticker))
    rownames(plot_df_radar) <- plot_data$ticker
    
    # fmsb format: Row 1 = Max, Row 2 = Min
    df_radar_final <- rbind(rep(1, 5), rep(0, 5), plot_df_radar)
    
    radarchart(df_radar_final, 
               pcol = c("#80808088", "#E63946"), pfcol = c("#80808011", "#E6394633"), 
               plwd = c(1, 4), plty = c(2, 1), cglcol = "grey90", vlcex = 0.8, 
               title = paste(t, "vs SPY"))
  }
  par(mfrow = c(1, 1)) 
}

# --- 6.0 EXECUTION ---
message("📊 Rendering Sovereign Dashboard...")

p_final <- create_exposure_plot()
print(p_final)

tactical_tickers <- policy_list$TAA$ticker %>% unique()
if(length(tactical_tickers) > 0) {
  plot_etf_spider_grid(tactical_tickers)
}

# Export
if(!dir.exists(path_out)) dir.create(path_out, recursive = TRUE)
ggsave(file.path(path_out, "exposure_audit.png"), p_final, width = 10, height = 6, dpi = 300)

message("✅ Dashboard Finalized. Check /reports/exposure_audit.png")