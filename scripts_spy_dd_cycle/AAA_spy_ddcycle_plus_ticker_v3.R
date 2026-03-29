# ==============================================================================
# MODULE: 05d_VIS_DUAL_TICKER_OVERLAY (v12 SOVEREIGN)
# Purpose: SPY Master Cycles + XLK Absolute Performance (All Labels Included)
# ==============================================================================
library(tidyverse)
library(PerformanceAnalytics)
library(xts)
library(timetk)
library(scales)

# 1. DATA INGESTION & ALIGNMENT
# ------------------------------------------------------------------------------
raw_db <- read_rds(file.path(base_path, "data_raw/raw_p_d.rds"))

data_wide <- raw_db %>%
  filter(symbol %in% c("SPY", "XLK")) %>%
  select(date, symbol, adjusted) %>%
  pivot_wider(names_from = symbol, values_from = adjusted) %>%
  arrange(date) %>%
  drop_na(SPY, XLK)

combined_xts <- data_wide %>% tk_xts(date_var = date, silent = TRUE)
combined_rets <- Return.calculate(combined_xts) %>% na.omit()

spy_rets <- combined_rets[, "SPY"]
xlk_rets <- combined_rets[, "XLK"]

# 2. DUAL-TICKER ENGINE
# ------------------------------------------------------------------------------
calculate_dual_regime <- function(m_r, o_r, thresh = 0.10) {
  dd_table <- table.Drawdowns(m_r, top = 25) %>% 
    filter(Depth <= -thresh) %>% 
    arrange(From)
  
  rects <- map_df(1:nrow(dd_table), function(i) {
    row <- dd_table[i,]
    bind_rows(
      tibble(xmin = as.Date(row$From), xmax = as.Date(row$Trough), type = "Fall",     color = "#D90429"),
      tibble(xmin = as.Date(row$Trough), xmax = as.Date(row$To),     type = "Recovery", color = "#F77F00")
    )
  })
  
  expansion_rects <- tibble(
    xmin = rects$xmax[seq(2, nrow(rects), 2)],
    xmax = lead(rects$xmin[seq(1, nrow(rects), 2)]),
    type = "Expansion", color = "#2D6A4F"
  ) %>% filter(!is.na(xmax))
  
  last_rect <- tibble(xmin = max(rects$xmax), xmax = max(index(m_r)), type = "Expansion", color = "#2D6A4F")
  
  all_windows <- bind_rows(rects, expansion_rects, last_rect) %>% 
    arrange(xmin) %>%
    rowwise() %>%
    mutate(
      days = as.numeric(xmax - xmin),
      is_thin = days < 60,
      # SPY Perf
      m_perf = as.numeric(Return.cumulative(m_r[paste0(xmin, "/", xmax)])),
      m_label = percent(m_perf, accuracy = 0.1),
      # XLK Perf
      o_perf = as.numeric(Return.cumulative(o_r[paste0(xmin, "/", xmax)])),
      o_label = percent(o_perf, accuracy = 0.1),
      txt_color = color
    ) %>%
    ungroup()
  
  return(all_windows)
}

# 3. SCALING & GEOMETRY
# ------------------------------------------------------------------------------
regime_df <- calculate_dual_regime(spy_rets, xlk_rets, thresh = 0.10)

cum_df <- tibble(date = index(spy_rets), cumret = as.numeric(cumprod(1 + spy_rets) - 1))
year_markers <- cum_df %>% mutate(year = format(date, "%Y")) %>% group_by(year) %>% slice(1) %>% ungroup()

max_v <- max(cum_df$cumret)
min_v <- min(cum_df$cumret)
tr    <- max_v - min_v

h_spy <- tr * 0.08
h_xlk <- h_spy / 2
gap   <- tr * 0.015

y_spy <- min_v - (tr * 0.18)
y_xlk <- y_spy - h_xlk - gap
y_low <- y_xlk - (tr * 0.35) # Expanded for floating labels

# 4. THE RENDER
# ------------------------------------------------------------------------------
ggplot() +
  geom_vline(data = year_markers, aes(xintercept = date), color = "gray92", linewidth = 1.2) +
  
  # BARS
  geom_rect(data = regime_df, aes(xmin = xmin, xmax = xmax, ymin = y_spy, ymax = y_spy + h_spy, fill = color), color = "white", linewidth = 0.2) +
  geom_rect(data = regime_df, aes(xmin = xmin, xmax = xmax, ymin = y_xlk, ymax = y_xlk + h_xlk, fill = color), color = "white", linewidth = 0.2, alpha = 0.7) +
  
  # WIDE LABELS (Inside Bars)
  geom_text(data = regime_df %>% filter(!is_thin), aes(x = xmin + (xmax-xmin)/2, y = y_spy + h_spy/2, label = m_label), color = "white", size = 2.5, fontface = "bold") +
  geom_text(data = regime_df %>% filter(!is_thin), aes(x = xmin + (xmax-xmin)/2, y = y_xlk + h_xlk/2, label = o_label), color = "white", size = 2.1, fontface = "bold") +
  
  # THIN LABELS - SPY (Floating Above)
  geom_text(data = regime_df %>% filter(is_thin),
            aes(x = xmin + (xmax-xmin)/2, y = y_spy + h_spy + (tr * 0.02), label = m_label, color = txt_color),
            angle = 45, size = 2.4, fontface = "bold", hjust = 0) +
  
  # THIN LABELS - XLK (Floating Below)
  geom_text(data = regime_df %>% filter(is_thin),
            aes(x = xmin + (xmax-xmin)/2, y = y_xlk - (tr * 0.02), label = o_label, color = txt_color),
            angle = 45, size = 2.0, fontface = "bold", hjust = 1) +
  
  geom_line(data = cum_df, aes(x = date, y = cumret), color = "darkblue", linewidth = 0.5) +
  
  # LABELS & GRID
  annotate("text", x = min(cum_df$date), y = y_spy + h_spy/2, label = "SPY", hjust = 1.3, size = 3.5, fontface = "bold", color = "#1D3557") +
  annotate("text", x = min(cum_df$date), y = y_xlk + h_xlk/2, label = "XLK", hjust = 1.3, size = 3.0, fontface = "bold", color = "grey40") +
  geom_text(data = year_markers, aes(x = date, y = y_low + (tr * 0.05), label = year), size = 3.5, color = "#2B2D42", fontface = "bold", vjust = 1) +
  
  scale_fill_identity() + scale_color_identity() +
  scale_y_continuous(labels = percent_format(), limits = c(y_low, max_v * 1.05)) +
  scale_x_date(expand = expansion(mult = c(0.12, 0.02))) +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.title = element_blank(), axis.text.x = element_blank())



# ==============================================================================
# FUNCTION: plot_sovereign_dual_overlay
# Purpose: Generate a Cumulative Return + Dual Regime Bar Chart
# ==============================================================================
plot_sovereign_dual_overlay <- function(db, master_tkr = "SPY", overlay_tkr = "XLK", thresh = 0.10) {
  
  # 1. DATA ALIGNMENT & RETURNS
  # ----------------------------------------------------------------------------
  data_wide <- db %>%
    filter(symbol %in% c(master_tkr, overlay_tkr)) %>%
    select(date, symbol, adjusted) %>%
    pivot_wider(names_from = symbol, values_from = adjusted) %>%
    arrange(date) %>%
    drop_na(all_of(c(master_tkr, overlay_tkr)))
  
  combined_xts <- data_wide %>% tk_xts(date_var = date, silent = TRUE)
  combined_rets <- Return.calculate(combined_xts) %>% na.omit()
  
  m_r <- combined_rets[, master_tkr]
  o_r <- combined_rets[, overlay_tkr]
  
  # 2. MASTER CYCLE ENGINE
  # ----------------------------------------------------------------------------
  dd_table <- table.Drawdowns(m_r, top = 25) %>% 
    filter(Depth <= -thresh) %>% 
    arrange(From)
  
  rects <- map_df(1:nrow(dd_table), function(i) {
    row <- dd_table[i,]
    bind_rows(
      tibble(xmin = as.Date(row$From), xmax = as.Date(row$Trough), type = "Fall",     color = "#D90429"),
      tibble(xmin = as.Date(row$Trough), xmax = as.Date(row$To),     type = "Recovery", color = "#F77F00")
    )
  })
  
  exp_rects <- tibble(
    xmin = rects$xmax[seq(2, nrow(rects), 2)],
    xmax = lead(rects$xmin[seq(1, nrow(rects), 2)]),
    type = "Expansion", color = "#2D6A4F"
  ) %>% filter(!is.na(xmax))
  
  last_rect <- tibble(xmin = max(rects$xmax), xmax = max(index(m_r)), type = "Expansion", color = "#2D6A4F")
  
  regime_df <- bind_rows(rects, exp_rects, last_rect) %>% 
    arrange(xmin) %>%
    rowwise() %>%
    mutate(
      days = as.numeric(xmax - xmin),
      is_thin = days < 60,
      m_perf = as.numeric(Return.cumulative(m_r[paste0(xmin, "/", xmax)])),
      m_label = percent(m_perf, accuracy = 0.1),
      o_perf = as.numeric(Return.cumulative(o_r[paste0(xmin, "/", xmax)])),
      o_label = percent(o_perf, accuracy = 0.1),
      txt_color = color
    ) %>% ungroup()
  
  # 3. SCALING & COORDINATES
  # ----------------------------------------------------------------------------
  cum_df <- tibble(date = index(m_r), cumret = as.numeric(cumprod(1 + m_r) - 1))
  year_markers <- cum_df %>% mutate(year = format(date, "%Y")) %>% group_by(year) %>% slice(1) %>% ungroup()
  
  max_v <- max(cum_df$cumret); min_v <- min(cum_df$cumret); tr <- max_v - min_v
  h_m <- tr * 0.08; h_o <- h_m / 2; gap <- tr * 0.015
  y_m <- min_v - (tr * 0.18); y_o <- y_m - h_o - gap; y_low <- y_o - (tr * 0.35)
  
  # 4. RENDER
  # ----------------------------------------------------------------------------
  ggplot() +
    geom_vline(data = year_markers, aes(xintercept = date), color = "gray92", linewidth = 1.2) +
    
    # BARS
    geom_rect(data = regime_df, aes(xmin = xmin, xmax = xmax, ymin = y_m, ymax = y_m + h_m, fill = color), color = "white", linewidth = 0.2) +
    geom_rect(data = regime_df, aes(xmin = xmin, xmax = xmax, ymin = y_o, ymax = y_o + h_o, fill = color), color = "white", linewidth = 0.2, alpha = 0.7) +
    
    # WIDE LABELS
    geom_text(data = regime_df %>% filter(!is_thin), aes(x = xmin + (xmax-xmin)/2, y = y_m + h_m/2, label = m_label), color = "white", size = 2.5, fontface = "bold") +
    geom_text(data = regime_df %>% filter(!is_thin), aes(x = xmin + (xmax-xmin)/2, y = y_o + h_o/2, label = o_label), color = "white", size = 2.1, fontface = "bold") +
    
    # THIN LABELS (Dual Floating)
    geom_text(data = regime_df %>% filter(is_thin), aes(x = xmin + (xmax-xmin)/2, y = y_m + h_m + (tr * 0.02), label = m_label, color = txt_color), angle = 45, size = 2.4, fontface = "bold", hjust = 0) +
    geom_text(data = regime_df %>% filter(is_thin), aes(x = xmin + (xmax-xmin)/2, y = y_o - (tr * 0.02), label = o_label, color = txt_color), angle = 45, size = 2.0, fontface = "bold", hjust = 1) +
    
    geom_line(data = cum_df, aes(x = date, y = cumret), color = "darkblue", linewidth = 0.5) +
    
    # ANNOTATIONS
    annotate("text", x = min(cum_df$date), y = y_m + h_m/2, label = master_tkr, hjust = 1.3, size = 3.5, fontface = "bold", color = "#1D3557") +
    annotate("text", x = min(cum_df$date), y = y_o + h_o/2, label = overlay_tkr, hjust = 1.3, size = 3.0, fontface = "bold", color = "grey40") +
    geom_text(data = year_markers, aes(x = date, y = y_low + (tr * 0.05), label = year), size = 3.5, color = "#2B2D42", fontface = "bold", vjust = 1) +
    
    scale_fill_identity() + scale_color_identity() +
    scale_y_continuous(labels = percent_format(), limits = c(y_low, max_v * 1.05)) +
    scale_x_date(expand = expansion(mult = c(0.12, 0.02))) +
    labs(title = paste(master_tkr, "vs", overlay_tkr, "Sovereign Overlay"), 
         subtitle = paste("Master Cycle:", master_tkr, "| Overlay Perf:", overlay_tkr)) +
    theme_minimal() + theme(panel.grid = element_blank(), axis.title = element_blank(), axis.text.x = element_blank())
}

# 5. EXECUTION EXAMPLE
# ------------------------------------------------------------------------------
# plot_sovereign_dual_overlay(spy_raw, "SPY", "XLK", thresh = 0.10)
