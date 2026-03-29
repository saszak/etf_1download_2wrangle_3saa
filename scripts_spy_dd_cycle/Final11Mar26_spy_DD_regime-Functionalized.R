# ==============================================================================
# FUNCTION: plot_dd_cycle (v14 - Zero-Start Compatible)
# ==============================================================================

# ==============================================================================
# FUNCTION: plot_dd_cycle_centered
# Purpose: 3X Bars + Phase Verticals (Dashed/Dotted) + Restored Year Grid
# ==============================================================================

# plot_dd_cycle_centered(master_tkr="SPY", overlay_tkr= "XLK", relative = T)
# plot_dd_cycle_centered(master_tkr="SPY", overlay_tkr= "IEF", relative = F)
# plot_dd_cycle_centered(master_tkr="SPY", overlay_tkr= "XLF", relative = F)
plot_dd_cycle_centered <- function(master_tkr = "SPY", overlay_tkr = "XLK", 
                                   thresh = 0.10, relative = FALSE) {
  
  # 1. PULL FROM GLOBAL ENV
  # ----------------------------------------------------------------------------
  m_r <- xts_r[, master_tkr]
  o_r <- xts_r[, overlay_tkr]
  
  # 2. CYCLE ENGINE
  # ----------------------------------------------------------------------------
  dd_table <- table.Drawdowns(m_r, top = 25) %>% 
    filter(Depth <= -thresh) %>% 
    arrange(From)
  
  # Base Regime Logic
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
  
  regime_bar <- bind_rows(rects, exp_rects, last_rect) %>% 
    arrange(xmin) %>%
    rowwise() %>%
    mutate(
      days = as.numeric(xmax - xmin),
      is_thin = days < 60,
      m_p = as.numeric(Return.cumulative(m_r[paste0(xmin, "/", xmax)])),
      o_p = as.numeric(Return.cumulative(o_r[paste0(xmin, "/", xmax)])),
      m_label = percent(m_p, accuracy = 0.1),
      o_val   = if(relative) (o_p - m_p) else o_p,
      o_label = if(relative) sprintf("%s%.1f%%", ifelse(o_val > 0, "+", ""), o_val * 100) else percent(o_val, accuracy = 0.1),
      txt_color = color
    ) %>% ungroup()
  
  # 3. PHASE MARKER LOGIC (Dashed = Fall, Dotted = Expansion)
  # ----------------------------------------------------------------------------
  phase_markers <- regime_bar %>%
    filter(type %in% c("Fall", "Expansion")) %>%
    mutate(lty = ifelse(type == "Fall", "dashed", "dotted"),
           line_col = ifelse(type == "Fall", "#D90429", "#2D6A4F"))
  
  # 4. COORDINATES & TIME MARKERS
  # ----------------------------------------------------------------------------
  cum_df <- tibble(date = index(m_r), cumret = as.numeric(cumprod(1 + m_r) - 1))
  year_markers <- cum_df %>% 
    mutate(year = format(date, "%Y")) %>% 
    group_by(year) %>% slice(1) %>% ungroup()
  
  max_abs_val <- max(abs(cum_df$cumret)) * 1.1 
  h_bar <- max_abs_val * 0.36
  gap   <- max_abs_val * 0.02
  
  y_m   <- gap
  y_o   <- -gap - h_bar
  y_floor <- -max_abs_val * 1.8 
  
  # 5. RENDER
  # ----------------------------------------------------------------------------
  ggplot() +
    # A. YEAR GRID (Light Grey Background)
    geom_vline(data = year_markers, aes(xintercept = date), 
               color = "gray92", linewidth = 0.8) +
    
    # B. PHASE VERTICALS (Aligned to Start Dates)
    # Dashed Red for Fall Start
    geom_vline(data = phase_markers %>% filter(lty == "dashed"),
               aes(xintercept = xmin), color = "#D90429", linetype = "dashed", alpha = 0.4) +
    # Dotted Green for Expansion Start
    geom_vline(data = phase_markers %>% filter(lty == "dotted"),
               aes(xintercept = xmin), color = "#2D6A4F", linetype = "dotted", alpha = 0.4) +
    
    # Horizontal Waterline
    geom_hline(yintercept = 0, color = "black", linewidth = 1.0) +
    
    # C. BARS (3X Width)
    geom_rect(data = regime_bar, aes(xmin = xmin, xmax = xmax, ymin = y_m, ymax = y_m + h_bar, fill = color), color = "white", linewidth = 0.4) +
    geom_rect(data = regime_bar, aes(xmin = xmin, xmax = xmax, ymin = y_o, ymax = y_o + h_bar, fill = color), color = "white", linewidth = 0.4, alpha = 0.9) +
    
    # D. LABELS
    geom_text(data = regime_bar %>% filter(!is_thin), aes(x = xmin + (xmax - xmin)/2, y = y_m + h_bar/2, label = m_label), color = "white", size = 3.5, fontface = "bold") +
    geom_text(data = regime_bar %>% filter(!is_thin), aes(x = xmin + (xmax - xmin)/2, y = y_o + h_bar/2, label = o_label), color = "white", size = 3.5, fontface = "bold") +
    
    # FLOATING LABELS
    geom_text(data = regime_bar %>% filter(is_thin), aes(x = xmin + (xmax - xmin)/2, y = y_m + h_bar + (max_abs_val * 0.08), label = m_label, color = txt_color), angle = 45, size = 2.8, fontface = "bold", hjust = 0) +
    geom_text(data = regime_bar %>% filter(is_thin), aes(x = xmin + (xmax - xmin)/2, y = y_o - (max_abs_val * 0.08), label = o_label, color = txt_color), angle = 45, size = 2.8, fontface = "bold", hjust = 1) +
    
    # YEAR FOOTER
    geom_text(data = year_markers, aes(x = date, y = y_floor + (max_abs_val * 0.1), label = year), 
              size = 3.2, fontface = "bold", color = "gray40") +
    
    # E. PERFORMANCE LINE (0.2 Width)
    geom_line(data = cum_df, aes(x = date, y = cumret), color = "darkgrey", linewidth = 0.5 ) +
    
    # SIDE ANNOTATIONS
    annotate("text", x = min(cum_df$date), y = y_m + h_bar/2, label = master_tkr, hjust = 1.3, size = 4.5, fontface = "bold", color = "#1D3557") +
    annotate("text", x = min(cum_df$date), y = y_o + h_bar/2, label = overlay_tkr, hjust = 1.3, size = 4.5, fontface = "bold", color = "grey30") +
    
    scale_fill_identity() + scale_color_identity() +
    scale_y_continuous(labels = percent_format(), 
                       breaks = seq(-5, 15, by = 0.20),
                       limits = c(y_floor, max_abs_val * 1.8)) + 
    scale_x_date(expand = expansion(mult = c(0.12, 0.10))) +
    
    theme_minimal() + 
    theme(
      panel.grid.major.y = element_line(color = "gray90", linewidth = 1),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title = element_blank(),
      axis.text.x = element_blank()
    )
}












#plot_dd_cycle(master_tkr="SPY", overlay_tkr= "XLK", relative = T)

# ==============================================================================
# FUNCTION: plot_dd_cycle
# Purpose: Dual-ticker overlay + 20% Interval Grid + width=1
# ==============================================================================
##################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts_state_machine/sm_cycle_visuals.R
# Style: Structural Dash Edition (No Bands)
# Purpose: Dual-Ticker Cycle Attribution with Phase Transition Markers
##################################################################################

library(tidyverse)
library(patchwork)
library(scales)
library(PerformanceAnalytics)
library(xts)

# ------------------------------------------------------------------------------
# 1. plot_dd_cycle()
# USE CASE: Structural Phase Audit via Phase Line Markers
# ------------------------------------------------------------------------------
##################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts_state_machine/sm_cycle_visuals.R
# Style: Structural Dash + Visible Flipped Dates
# Purpose: Dual-Ticker Cycle Attribution with Guaranteed Date Rendering
##################################################################################

library(tidyverse)
library(patchwork)
library(scales)
library(PerformanceAnalytics)
library(xts)

# ------------------------------------------------------------------------------
# 1. plot_dd_cycle()
# USE CASE: Structural Phase Audit with Event-Specific Date Callouts
# ------------------------------------------------------------------------------
##################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts_state_machine/sm_cycle_visuals.R
# Style: Structural Dash + Red Drawdown Swap
# Purpose: Finalized Dual-Ticker Cycle Audit with Red Fall Phase
##################################################################################

##################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts_state_machine/sm_cycle_visuals.R
# Style: Structural Dash + Red Drawdown + X2 Bar Height
# Purpose: Finalized Dual-Ticker Cycle Audit with Robust Visual Bars
##################################################################################

plot_dd_cycle <- function(master_tkr = "SPY", overlay_tkr = "XLK", 
                          thresh = 0.10, relative = FALSE) {
  
  # 1. PULL FROM GLOBAL ENV
  # ----------------------------------------------------------------------------
  m_r <- xts_r[, master_tkr]
  o_r <- xts_r[, overlay_tkr]
  
  # 2. CYCLE ENGINE
  # ----------------------------------------------------------------------------
  dd_table <- table.Drawdowns(m_r, top = 25) %>% 
    filter(Depth <= -thresh) %>% 
    arrange(From)
  
  # A. PHASE MARKER LINES
  markers <- map_df(1:nrow(dd_table), function(i) {
    row <- dd_table[i,]
    bind_rows(
      tibble(x = as.Date(row$From),   type = "Start_DD",   lty = "dashed", col = "#D90429"), # Red
      tibble(x = as.Date(row$Trough), type = "Start_Rec",  lty = "dotted", col = "#F77F00"),
      tibble(x = as.Date(row$To),     type = "End_Rec",    lty = "solid",  col = "grey70")
    )
  })
  
  # B. PHASE SEGMENTS (Bars)
  rects <- map_df(1:nrow(dd_table), function(i) {
    row <- dd_table[i,]
    bind_rows(
      tibble(xmin = as.Date(row$From), xmax = as.Date(row$Trough), type = "Fall",     color = "#D90429"), # Red
      tibble(xmin = as.Date(row$Trough), xmax = as.Date(row$To),     type = "Recovery", color = "#F77F00") 
    )
  })
  
  exp_rects <- tibble(
    xmin = rects$xmax[seq(2, nrow(rects), 2)],
    xmax = lead(rects$xmin[seq(1, nrow(rects), 2)]),
    type = "Expansion", color = "#2D6A4F"
  ) %>% filter(!is.na(xmax))
  
  last_rect <- tibble(xmin = max(rects$xmax), xmax = max(index(m_r)), type = "Expansion", color = "#2D6A4F")
  
  regime_bar <- bind_rows(rects, exp_rects, last_rect) %>% 
    arrange(xmin) %>%
    rowwise() %>%
    mutate(
      days = as.numeric(xmax - xmin),
      is_thin = days < 60,
      m_p = as.numeric(Return.cumulative(m_r[paste0(xmin, "/", xmax)])),
      o_p = as.numeric(Return.cumulative(o_r[paste0(xmin, "/", xmax)])),
      m_label = percent(m_p, accuracy = 0.1),
      o_val   = if(relative) (o_p - m_p) else o_p,
      o_label = if(relative) sprintf("%s%.1f%%", ifelse(o_val > 0, "+", ""), o_val * 100) else percent(o_val, accuracy = 0.1),
      txt_color = color
    ) %>% ungroup()
  
  # 3. SCALING & COORDINATES (X2 Height Adjustments)
  # ----------------------------------------------------------------------------
  cum_df <- tibble(date = index(m_r), cumret = as.numeric(cumprod(1 + m_r) - 1))
  year_markers <- cum_df %>% mutate(year = format(date, "%Y")) %>% group_by(year) %>% slice(1) %>% ungroup()
  
  max_val <- max(cum_df$cumret); min_val <- min(cum_df$cumret); tr <- max_val - min_val
  
  # DOUBLE HEIGHT HERE
  h_bar <- tr * 0.16 
  gap   <- tr * 0.02
  
  # Push Y positions lower to accommodate thicker bars
  y_m   <- min_val - (tr * 0.35)
  y_o   <- y_m - h_bar - gap
  
  y_label_pos <- y_o - (tr * 0.05)
  y_low       <- y_o - (tr * 0.55) # Extended for flipped labels
  
  dd_start_labels <- dd_table %>% 
    mutate(x = as.Date(From), label = format(x, "%Y-%m-%d"))
  
  # 4. RENDER
  # ----------------------------------------------------------------------------
  ggplot() +
    # A. STRUCTURAL PHASE MARKERS
    geom_vline(data = markers, aes(xintercept = x, linetype = lty, color = col), 
               linewidth = 0.6, alpha = 0.5, show.legend = FALSE) +
    
    # B. YEAR GRID
    geom_vline(data = year_markers, aes(xintercept = date), color = "gray92", linewidth = 1.2) +
    
    # C. PERFORMANCE BARS (X2 Height)
    geom_rect(data = regime_bar, aes(xmin = xmin, xmax = xmax, ymin = y_m, ymax = y_m + h_bar, fill = color), color = "white", linewidth = 0.2) +
    geom_rect(data = regime_bar, aes(xmin = xmin, xmax = xmax, ymin = y_o, ymax = y_o + h_bar, fill = color), color = "white", linewidth = 0.2, alpha = 0.8) +
    
    # D. LABELS (Centered)
    geom_text(data = regime_bar %>% filter(!is_thin), aes(x = xmin + (xmax - xmin)/2, y = y_m + h_bar/2, label = m_label), color = "white", size = 2.8, fontface = "bold") +
    geom_text(data = regime_bar %>% filter(!is_thin), aes(x = xmin + (xmax - xmin)/2, y = y_o + h_bar/2, label = o_label), color = "white", size = 2.8, fontface = "bold") +
    
    # E. FLIPPED DATE LABELS
    geom_text(data = dd_start_labels, aes(x = x, y = y_label_pos, label = label), 
              angle = 90, hjust = 1, size = 2.5, fontface = "bold", color = "grey40") +
    
    # F. CUMULATIVE LINE
    geom_line(data = cum_df, aes(x = date, y = cumret), color = "#003049", linewidth = 0.8) +
    
    # G. ANNOTATIONS
    annotate("text", x = min(cum_df$date), y = y_m + h_bar/2, label = master_tkr, hjust = 1.3, size = 3.5, fontface = "bold", color = "#1D3557") +
    annotate("text", x = min(cum_df$date), y = y_o + h_bar/2, label = overlay_tkr, hjust = 1.3, size = 3.5, fontface = "bold", color = "grey30") +
    
    scale_fill_identity() + scale_color_identity() + scale_linetype_identity() +
    
    coord_cartesian(ylim = c(y_low, max_val * 1.05), clip = "off") + 
    
    scale_y_continuous(labels = percent_format(), 
                       breaks = seq(-1, max_val + 0.2, by = 0.20)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = expansion(mult = c(0.12, 0.02))) +
    
    labs(title = paste(master_tkr, "vs", overlay_tkr, "Structural Cycle Audit"),
         subtitle = "Dashed Red: Beg DD | Dotted Orange: Beg Recovery | Solid Grey: Peak Restored") +
    
    theme_minimal() + 
    theme(
      panel.grid.major.y = element_line(color = "gray90", linewidth = 1),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title = element_blank(), 
      axis.text.x = element_text(size = 9, color = "gray30", face = "bold", vjust = -2),
      plot.margin = margin(l = 10, r = 10, t = 10, b = 60) 
    )
}

##################################################################################


##################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts_state_machine/sm_cycle_visuals_multi.R
# Style: Dynamic Multi-Stack + Red Drawdown + X2 Bar Height + Vectorized Relative
# Purpose: Multi-Ticker Structural Cycle Audit - FULL FILE
##################################################################################

library(tidyverse)
library(patchwork)
library(scales)
library(PerformanceAnalytics)
library(xts)

plot_multi_dd_cycle <- function(master_tkr = "SPY", 
                                overlay_tkrs = c("XLK", "XLF", "XLE"), 
                                thresh = 0.10,
                                relative = FALSE) {
  
  # 1. DATA PREP
  # ----------------------------------------------------------------------------
  all_tkrs <- c(master_tkr, overlay_tkrs)
  m_r <- xts_r[, master_tkr]
  
  # 2. CYCLE ENGINE (Based on Master Ticker)
  # ----------------------------------------------------------------------------
  dd_table <- table.Drawdowns(m_r, top = 25) %>% 
    filter(Depth <= -thresh) %>% 
    arrange(From)
  
  # A. PHASE MARKER LINES
  markers <- map_df(1:nrow(dd_table), function(i) {
    row <- dd_table[i,]
    bind_rows(
      tibble(x = as.Date(row$From),   lty = "dashed", col = "#D90429"), # RED
      tibble(x = as.Date(row$Trough), lty = "dotted", col = "#F77F00"), # ORANGE
      tibble(x = as.Date(row$To),     lty = "solid",  col = "grey70")   # GREY
    )
  })
  
  # B. REGIME PERIOD DEFINITION
  rect_base <- map_df(1:nrow(dd_table), function(i) {
    row <- dd_table[i,]
    bind_rows(
      tibble(xmin = as.Date(row$From), xmax = as.Date(row$Trough), type = "Fall",     color = "#D90429"),
      tibble(xmin = as.Date(row$Trough), xmax = as.Date(row$To),     type = "Recovery", color = "#F77F00") 
    )
  })
  
  exp_rects <- tibble(
    xmin = rect_base$xmax[seq(2, nrow(rect_base), 2)],
    xmax = lead(rect_base$xmin[seq(1, nrow(rect_base), 2)]),
    type = "Expansion", color = "#2D6A4F"
  ) %>% filter(!is.na(xmax))
  
  last_rect <- tibble(xmin = max(rect_base$xmax), xmax = max(index(m_r)), type = "Expansion", color = "#2D6A4F")
  regime_periods <- bind_rows(rect_base, exp_rects, last_rect) %>% arrange(xmin)
  
  # C. MULTI-TICKER CALCULATION
  multi_regime_bar <- map_df(all_tkrs, function(tkr) {
    tkr_r <- xts_r[, tkr]
    regime_periods %>%
      rowwise() %>%
      mutate(
        ticker = tkr,
        days = as.numeric(xmax - xmin),
        is_thin = days < 60,
        raw_ret = as.numeric(Return.cumulative(tkr_r[paste0(xmin, "/", xmax)])),
        m_ret   = as.numeric(Return.cumulative(m_r[paste0(xmin, "/", xmax)]))
      ) %>% ungroup()
  }) %>%
    mutate(
      # Vectorized fix for the condition error
      display_ret = ifelse(relative & ticker != master_tkr, (raw_ret - m_ret), raw_ret),
      label = ifelse(relative & ticker != master_tkr,
                     sprintf("%s%.1f%%", ifelse(display_ret > 0, "+", ""), display_ret * 100),
                     percent(display_ret, accuracy = 0.1))
    )
  
  # 3. DYNAMIC SCALING & COORDINATES
  # ----------------------------------------------------------------------------
  cum_df <- tibble(date = index(m_r), cumret = as.numeric(cumprod(1 + m_r) - 1))
  max_val <- max(cum_df$cumret); min_val <- min(cum_df$cumret); tr <- max_val - min_val
  
  h_bar <- tr * 0.16 
  gap   <- tr * 0.02
  
  ticker_map <- tibble(
    ticker = all_tkrs,
    y_start = min_val - (1:length(all_tkrs) * (h_bar + gap)) - (tr * 0.15)
  )
  
  multi_regime_bar <- multi_regime_bar %>% left_join(ticker_map, by = "ticker")
  
  y_low <- min(ticker_map$y_start) - (tr * 0.45)
  y_label_pos <- min(ticker_map$y_start) - (tr * 0.05)
  
  dd_start_labels <- dd_table %>% 
    mutate(x = as.Date(From), label = format(x, "%Y-%m-%d"))
  
  # 4. RENDER
  # ----------------------------------------------------------------------------
  ggplot() +
    # A. VERTICAL MARKERS
    geom_vline(data = markers, aes(xintercept = x, linetype = lty, color = col), 
               linewidth = 0.6, alpha = 0.5, show.legend = FALSE) +
    
    # B. BARS & LABELS
    geom_rect(data = multi_regime_bar, 
              aes(xmin = xmin, xmax = xmax, ymin = y_start, ymax = y_start + h_bar, fill = color), 
              color = "white", linewidth = 0.2) +
    
    geom_text(data = multi_regime_bar %>% filter(!is_thin), 
              aes(x = xmin + (xmax - xmin)/2, y = y_start + h_bar/2, label = label), 
              color = "white", size = 2.5, fontface = "bold") +
    
    # C. TICKER NAMES
    geom_text(data = ticker_map, aes(x = min(cum_df$date), y = y_start + h_bar/2, label = ticker), 
              hjust = 1.3, size = 3.5, fontface = "bold", color = "grey30") +
    
    # D. FLIPPED DATE LABELS
    geom_text(data = dd_start_labels, aes(x = x, y = y_label_pos, label = label), 
              angle = 90, hjust = 1, size = 2.5, fontface = "bold", color = "grey40") +
    
    # E. MASTER CUMULATIVE LINE
    geom_line(data = cum_df, aes(x = date, y = cumret), color = "#003049", linewidth = 0.8) +
    
    scale_fill_identity() + scale_color_identity() + scale_linetype_identity() +
    
    coord_cartesian(ylim = c(y_low, max_val * 1.05), clip = "off") + 
    
    scale_y_continuous(labels = percent_format(), breaks = seq(-1, max_val + 0.2, by = 0.20)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = expansion(mult = c(0.15, 0.02))) +
    
    labs(title = paste("Structural Cycle Audit: Master [", master_tkr, "]"),
         subtitle = paste("Overlay Tickers Stacked Below | Mode:", ifelse(relative, "Relative Strength", "Absolute Return"))) +
    
    theme_minimal() + 
    theme(
      panel.grid.major.y = element_line(color = "gray90", linewidth = 1),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title = element_blank(), 
      axis.text.x = element_text(size = 9, color = "gray30", face = "bold", vjust = -2),
      plot.margin = margin(l = 10, r = 10, t = 10, b = 60) 
    )
}

##################################################################################

##################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts_state_machine/sm_cycle_visuals_multi.R
# Style: Dynamic Multi-Stack + Red DD + Lighter Green Expansion
# Purpose: Multi-Ticker Structural Cycle Audit - Final Lighter Version
##################################################################################

library(tidyverse)
library(patchwork)
library(scales)
library(PerformanceAnalytics)
library(xts)

plot_multi_dd_cycle <- function(master_tkr = "SPY", 
                                overlay_tkrs = c("XLK", "XLF", "XLE", "XLV", "XLY", "XLI"), 
                                thresh = 0.10,
                                relative = FALSE) {
  
  # 1. DATA PREP
  # ----------------------------------------------------------------------------
  all_tkrs <- c(master_tkr, overlay_tkrs)
  m_r <- xts_r[, master_tkr]
  
  # 2. CYCLE ENGINE (Based on Master Ticker)
  # ----------------------------------------------------------------------------
  dd_table <- table.Drawdowns(m_r, top = 25) %>% 
    filter(Depth <= -thresh) %>% 
    arrange(From)
  
  # A. PHASE MARKER LINES
  markers <- map_df(1:nrow(dd_table), function(i) {
    row <- dd_table[i,]
    bind_rows(
      tibble(x = as.Date(row$From),   lty = "dashed", col = "#D90429"), # RED
      tibble(x = as.Date(row$Trough), lty = "dotted", col = "#F77F00"), # ORANGE
      tibble(x = as.Date(row$To),     lty = "solid",  col = "grey70")   # GREY
    )
  })
  
  # B. REGIME PERIOD DEFINITION
  rect_base <- map_df(1:nrow(dd_table), function(i) {
    row <- dd_table[i,]
    bind_rows(
      tibble(xmin = as.Date(row$From), xmax = as.Date(row$Trough), type = "Fall",     color = "#D90429"),
      tibble(xmin = as.Date(row$Trough), xmax = as.Date(row$To),     type = "Recovery", color = "#F77F00") 
    )
  })
  
  exp_rects <- tibble(
    xmin = rect_base$xmax[seq(2, nrow(rect_base), 2)],
    xmax = lead(rect_base$xmin[seq(1, nrow(rect_base), 2)]),
    # CHANGED TO LIGHTER GREEN: "#40916C" from "#2D6A4F"
    type = "Expansion", color = "#40916C"
  ) %>% filter(!is.na(xmax))
  
  # CHANGED TO LIGHTER GREEN: "#40916C"
  last_rect <- tibble(xmin = max(rect_base$xmax), xmax = max(index(m_r)), type = "Expansion", color = "#40916C")
  regime_periods <- bind_rows(rect_base, exp_rects, last_rect) %>% arrange(xmin)
  
  # C. MULTI-TICKER CALCULATION
  multi_regime_bar <- map_df(all_tkrs, function(tkr) {
    tkr_r <- xts_r[, tkr]
    regime_periods %>%
      rowwise() %>%
      mutate(
        ticker = tkr,
        days = as.numeric(xmax - xmin),
        is_thin = days < 60,
        raw_ret = as.numeric(Return.cumulative(tkr_r[paste0(xmin, "/", xmax)])),
        m_ret   = as.numeric(Return.cumulative(m_r[paste0(xmin, "/", xmax)]))
      ) %>% ungroup()
  }) %>%
    mutate(
      display_ret = ifelse(relative & ticker != master_tkr, (raw_ret - m_ret), raw_ret),
      label = ifelse(relative & ticker != master_tkr,
                     sprintf("%s%.1f%%", ifelse(display_ret > 0, "+", ""), display_ret * 100),
                     percent(display_ret, accuracy = 0.1)),
      # CONTRAST LOGIC
      txt_color = case_when(
        ticker == master_tkr ~ "white",
        !relative ~ "white",
        relative & display_ret >= 0 ~ "white", # POSITIVE = WHITE
        relative & display_ret < 0  ~ "black", # NEGATIVE = BLACK
        TRUE ~ "white"
      )
    )
  
  # 3. SCALING & COORDINATES
  # ----------------------------------------------------------------------------
  cum_df <- tibble(date = index(m_r), cumret = as.numeric(cumprod(1 + m_r) - 1))
  max_val <- max(cum_df$cumret); min_val <- min(cum_df$cumret); tr <- max_val - min_val
  
  h_bar <- tr * 0.16 
  gap   <- tr * 0.02
  
  ticker_map <- tibble(
    ticker = all_tkrs,
    y_start = min_val - (1:length(all_tkrs) * (h_bar + gap)) - (tr * 0.15)
  )
  
  multi_regime_bar <- multi_regime_bar %>% left_join(ticker_map, by = "ticker")
  
  y_low <- min(ticker_map$y_start) - (tr * 0.45)
  y_label_pos <- min(ticker_map$y_start) - (tr * 0.05)
  
  dd_start_labels <- dd_table %>% 
    mutate(x = as.Date(From), label = format(x, "%Y-%m-%d"))
  
  # 4. RENDER
  # ----------------------------------------------------------------------------
  ggplot() +
    # A. VERTICAL MARKERS
    geom_vline(data = markers, aes(xintercept = x, linetype = lty, color = col), 
               linewidth = 0.6, alpha = 0.5, show.legend = FALSE) +
    
    # B. BARS & LABELS
    geom_rect(data = multi_regime_bar, 
              aes(xmin = xmin, xmax = xmax, ymin = y_start, ymax = y_start + h_bar, fill = color), 
              color = "white", linewidth = 0.2) +
    
    geom_text(data = multi_regime_bar %>% filter(!is_thin), 
              aes(x = xmin + (xmax - xmin)/2, y = y_start + h_bar/2, 
                  label = label, color = txt_color), 
              size = 2.5, fontface = "bold") +
    
    # C. TICKER NAMES
    geom_text(data = ticker_map, aes(x = min(cum_df$date), y = y_start + h_bar/2, label = ticker), 
              hjust = 1.3, size = 3.5, fontface = "bold", color = "grey30") +
    
    # D. FLIPPED DATE LABELS
    geom_text(data = dd_start_labels, aes(x = x, y = y_label_pos, label = label), 
              angle = 90, hjust = 1, size = 2.5, fontface = "bold", color = "grey40") +
    
    # E. MASTER CUMULATIVE LINE
    geom_line(data = cum_df, aes(x = date, y = cumret), color = "#003049", linewidth = 0.8) +
    
    scale_fill_identity() + scale_color_identity() + scale_linetype_identity() +
    
    coord_cartesian(ylim = c(y_low, max_val * 1.05), clip = "off") + 
    
    scale_y_continuous(labels = percent_format(), breaks = seq(-1, max_val + 0.2, by = 0.20)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = expansion(mult = c(0.15, 0.02))) +
    
    labs(title = paste("Structural Cycle Audit: ", master_tkr),
         subtitle = paste0("Mode: ", ifelse(relative, "Relative Alpha (W:Outperf, B:Underperf)", "Absolute Return"), 
                           " | Drawdown Threshold: ", percent(thresh))) +
    
    theme_minimal() + 
    theme(
      panel.grid.major.y = element_line(color = "gray90", linewidth = 1),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title = element_blank(), 
      axis.text.x = element_text(size = 9, color = "gray30", face = "bold", vjust = -2),
      plot.margin = margin(l = 10, r = 10, t = 10, b = 60) 
    )
}

##################################################################################

##################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts_state_machine/sm_cycle_table.R
# Style: Vectorized Phase Performance Table
# Purpose: Extract Raw Data from Structural Cycle Audit
##################################################################################

get_multi_dd_table <- function(master_tkr = "SPY", 
                               overlay_tkrs = c("XLK", "XLF", "XLE"), 
                               thresh = 0.10,
                               relative = FALSE) {
  
  # 1. DATA PREP
  # ----------------------------------------------------------------------------
  all_tkrs <- c(master_tkr, overlay_tkrs)
  m_r <- xts_r[, master_tkr]
  
  # 2. CYCLE ENGINE (Identical to Visuals)
  # ----------------------------------------------------------------------------
  dd_table <- table.Drawdowns(m_r, top = 25) %>% 
    filter(Depth <= -thresh) %>% 
    arrange(From)
  
  # Define the specific Date Windows (Fall, Recovery, Expansion)
  rect_base <- map_df(1:nrow(dd_table), function(i) {
    row <- dd_table[i,]
    bind_rows(
      tibble(xmin = as.Date(row$From), xmax = as.Date(row$Trough), phase = "Fall"),
      tibble(xmin = as.Date(row$Trough), xmax = as.Date(row$To),     phase = "Recovery") 
    )
  })
  
  exp_rects <- tibble(
    xmin = rect_base$xmax[seq(2, nrow(rect_base), 2)],
    xmax = lead(rect_base$xmin[seq(1, nrow(rect_base), 2)]),
    phase = "Expansion"
  ) %>% filter(!is.na(xmax))
  
  last_rect <- tibble(xmin = max(rect_base$xmax), xmax = max(index(m_r)), phase = "Expansion")
  regime_periods <- bind_rows(rect_base, exp_rects, last_rect) %>% arrange(xmin)
  
  # 3. TABULAR CALCULATION
  # ----------------------------------------------------------------------------
  perf_table <- map_df(all_tkrs, function(tkr) {
    tkr_r <- xts_r[, tkr]
    regime_periods %>%
      rowwise() %>%
      mutate(
        ticker = tkr,
        days = as.numeric(xmax - xmin),
        raw_ret = as.numeric(Return.cumulative(tkr_r[paste0(xmin, "/", xmax)])),
        m_ret   = as.numeric(Return.cumulative(m_r[paste0(xmin, "/", xmax)]))
      ) %>% ungroup()
  }) %>%
    mutate(
      # Apply relative logic if flag is TRUE
      final_ret = ifelse(relative & ticker != master_tkr, (raw_ret - m_ret), raw_ret),
      # Add metadata for clarity
      metric_type = ifelse(relative & ticker != master_tkr, "Relative_Alpha", "Absolute_Return")
    ) %>%
    select(ticker, phase, xmin, xmax, days, final_ret, metric_type) %>%
    rename(start_date = xmin, end_date = xmax, period_return = final_ret)
  
  return(perf_table)
}

##################################################################################


##################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts_state_machine/sm_cycle_table.R
# Style: Vectorized Phase Performance Table
# Purpose: Extract Raw Data from Structural Cycle Audit
##################################################################################

get_multi_dd_table <- function(master_tkr = "SPY", 
                               overlay_tkrs = c("XLK", "XLF", "XLE"), 
                               thresh = 0.10,
                               relative = FALSE) {
  
  # 1. DATA PREP
  # ----------------------------------------------------------------------------
  all_tkrs <- c(master_tkr, overlay_tkrs)
  m_r <- xts_r[, master_tkr]
  
  # 2. CYCLE ENGINE (Identical to Visuals)
  # ----------------------------------------------------------------------------
  dd_table <- table.Drawdowns(m_r, top = 25) %>% 
    filter(Depth <= -thresh) %>% 
    arrange(From)
  
  # Define the specific Date Windows (Fall, Recovery, Expansion)
  rect_base <- map_df(1:nrow(dd_table), function(i) {
    row <- dd_table[i,]
    bind_rows(
      tibble(xmin = as.Date(row$From), xmax = as.Date(row$Trough), phase = "Fall"),
      tibble(xmin = as.Date(row$Trough), xmax = as.Date(row$To),     phase = "Recovery") 
    )
  })
  
  exp_rects <- tibble(
    xmin = rect_base$xmax[seq(2, nrow(rect_base), 2)],
    xmax = lead(rect_base$xmin[seq(1, nrow(rect_base), 2)]),
    phase = "Expansion"
  ) %>% filter(!is.na(xmax))
  
  last_rect <- tibble(xmin = max(rect_base$xmax), xmax = max(index(m_r)), phase = "Expansion")
  regime_periods <- bind_rows(rect_base, exp_rects, last_rect) %>% arrange(xmin)
  
  # 3. TABULAR CALCULATION
  # ----------------------------------------------------------------------------
  perf_table <- map_df(all_tkrs, function(tkr) {
    tkr_r <- xts_r[, tkr]
    regime_periods %>%
      rowwise() %>%
      mutate(
        ticker = tkr,
        days = as.numeric(xmax - xmin),
        raw_ret = as.numeric(Return.cumulative(tkr_r[paste0(xmin, "/", xmax)])),
        m_ret   = as.numeric(Return.cumulative(m_r[paste0(xmin, "/", xmax)]))
      ) %>% ungroup()
  }) %>%
    mutate(
      # Apply relative logic if flag is TRUE
      final_ret = ifelse(relative & ticker != master_tkr, (raw_ret - m_ret), raw_ret),
      # Add metadata for clarity
      metric_type = ifelse(relative & ticker != master_tkr, "Relative_Alpha", "Absolute_Return")
    ) %>%
    select(ticker, phase, xmin, xmax, days, final_ret, metric_type) %>%
    rename(start_date = xmin, end_date = xmax, period_return = final_ret)
  
  return(perf_table)
}

##################################################################################

##################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts_state_machine/sm_cycle_summary_plot.R
# Style: Intelligent Phase-Grouped Barplot
# Purpose: Summarize Multi-Ticker Performance across Master Regimes
##################################################################################

plot_cycle_summary <- function(perf_df, master_tkr = "SPY") {
  
  # 1. PREP DATA FOR PLOTTING
  # ----------------------------------------------------------------------------
  plot_data <- perf_df %>%
    mutate(
      # Ensure Phase is an ordered factor for logical flow
      phase = factor(phase, levels = c("Fall", "Recovery", "Expansion")),
      # Determine bar color based on Phase (matching the main chart)
      bar_color = case_when(
        phase == "Fall"      ~ "#D90429", # Red
        phase == "Recovery"  ~ "#F77F00", # Orange
        phase == "Expansion" ~ "#40916C", # Lighter Green
        TRUE ~ "grey50"
      ),
      # Contrast logic for text: White if positive, Black if negative
      txt_color = ifelse(period_return >= 0, "white", "black"),
      label = percent(period_return, accuracy = 0.1)
    )
  
  # 2. RENDER
  # ----------------------------------------------------------------------------
  ggplot(plot_data, aes(x = ticker, y = period_return, fill = bar_color)) +
    # A. BARS
    geom_col(color = "white", linewidth = 0.3) +
    
    # B. DYNAMIC LABELS
    geom_text(aes(label = label, color = txt_color), 
              position = position_stack(vjust = 0.5), 
              size = 3, fontface = "bold") +
    
    # C. PHASE FACETING (The "Intelligence")
    facet_wrap(~phase, scales = "free_y") +
    
    # D. STYLING
    scale_fill_identity() +
    scale_color_identity() +
    scale_y_continuous(labels = percent_format()) +
    
    labs(
      title = paste("Regime Performance Summary (Master:", master_tkr, ")"),
      subtitle = "Grouped by Market Phase | Comparison of Absolute or Relative Returns",
      x = NULL,
      y = "Period Return"
    ) +
    
    theme_minimal() +
    theme(
      strip.background = element_rect(fill = "gray95", color = NA),
      strip.text = element_text(face = "bold", size = 11, color = "gray30"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
      plot.title = element_text(face = "bold", size = 14),
      plot.margin = margin(10, 10, 10, 10)
    )
}

##################################################################################


##################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts_state_machine/sm_cycle_summary_stacked.R
# Style: Stacked Regime Performance Bars
# Purpose: Vertical Attribution of Phases by Ticker
##################################################################################

plot_cycle_summary_stacked <- function(perf_df, master_tkr = "SPY") {
  
  # 1. PREP DATA FOR PLOTTING
  # ----------------------------------------------------------------------------
  plot_data <- perf_df %>%
    mutate(
      # Ensure Phase is an ordered factor to control the stacking order
      # Reversing levels can change if Fall is at bottom or top
      phase = factor(phase, levels = c("Expansion", "Recovery", "Fall")),
      # Match existing color palette
      bar_color = case_when(
        phase == "Fall"      ~ "#D90429", # Red
        phase == "Recovery"  ~ "#F77F00", # Orange
        phase == "Expansion" ~ "#40916C", # Lighter Green
        TRUE ~ "grey50"
      ),
      # Contrast logic: White if positive, Black if negative
      txt_color = ifelse(period_return >= 0, "white", "black"),
      label = percent(period_return, accuracy = 0.1)
    )
  
  # 2. RENDER
  # ----------------------------------------------------------------------------
  ggplot(plot_data, aes(x = ticker, y = period_return, fill = bar_color)) +
    
    # A. STACKED BARS
    # position_stack() handles the vertical alignment automatically
    geom_col(color = "white", linewidth = 0.4, position = "stack") +
    
    # B. DYNAMIC LABELS (Inside Segments)
    geom_text(aes(label = label, color = txt_color), 
              position = position_stack(vjust = 0.5), 
              size = 2.8, fontface = "bold") +
    
    # C. STYLING & COLOR IDENTITY
    scale_fill_identity() +
    scale_color_identity() +
    scale_y_continuous(labels = percent_format()) +
    
    # D. ZERO LINE (Critical for seeing drag vs. gain)
    geom_hline(yintercept = 0, color = "black", linewidth = 0.8, alpha = 0.6) +
    
    labs(
      title = paste("Regime Contribution: ", master_tkr, "Benchmark"),
      subtitle = "Stacked Performance by Market Phase (Fall / Recovery / Expansion)",
      x = NULL,
      y = "Cumulative Period Return"
    ) +
    
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 10),
      axis.text.y = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 14),
      plot.margin = margin(10, 10, 10, 10)
    )
}

##### AAA #############################################################################

##################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts_state_machine/sm_cycle_timeline_stack.R
# Style: Chronological Performance Waterfall
# Purpose: Show XLK "Pieces" across the actual Project Timeline
##################################################################################

plot_xlk_timeline_attribution <- function(perf_df, target_tkr = "XLK") {
  
  # 1. PREP DATA: ORDER BY TIME
  # ----------------------------------------------------------------------------
  plot_data <- perf_df %>%
    filter(ticker == target_tkr) %>%
    arrange(start_date) %>%
    mutate(
      # Create a unique ID for each specific period to keep them chronological
      period_id = factor(paste0(format(start_date, "%b %y"), " (", phase, ")"), 
                         levels = paste0(format(start_date, "%b %y"), " (", phase, ")")),
      
      # Cumulative coordinates
      y_end = cumsum(period_return),
      y_start = lag(y_end, default = 0),
      
      bar_color = case_when(
        phase == "Fall"      ~ "#D90429", 
        phase == "Recovery"  ~ "#F77F00", 
        phase == "Expansion" ~ "#40916C", 
        TRUE ~ "grey50"
      ),
      
      txt_color = ifelse(period_return >= 0, "white", "black"),
      label = percent(period_return, accuracy = 0.1)
    )
  
  # 2. RENDER CHRONOLOGICAL STACK
  # ----------------------------------------------------------------------------
  ggplot(plot_data) +
    # A. ZERO LINE
    geom_hline(yintercept = 0, color = "black", linewidth = 0.8, alpha = 0.4) +
    
    # B. THE CHRONOLOGICAL PIECES
    # We use period_id on the X-axis to show the timeline progression
    geom_rect(aes(xmin = as.numeric(period_id) - 0.4, 
                  xmax = as.numeric(period_id) + 0.4, 
                  ymin = y_start, ymax = y_end, fill = bar_color),
              color = "white", linewidth = 0.5) +
    
    # C. "CONNECTOR" LINES (Optional - shows the path between phases)
    geom_segment(aes(x = as.numeric(period_id) - 0.4, xend = as.numeric(period_id) + 0.4, 
                     y = y_start, yend = y_start), linetype = "dotted", color = "grey50") +
    
    # D. LABELS
    geom_text(aes(x = as.numeric(period_id), y = y_start + (y_end - y_start)/2, 
                  label = label, color = txt_color), 
              size = 3, fontface = "bold") +
    
    # E. STYLING
    scale_fill_identity() +
    scale_color_identity() +
    scale_y_continuous(labels = percent_format()) +
    scale_x_continuous(breaks = 1:nrow(plot_data), labels = plot_data$period_id) +
    
    labs(
      title = paste(target_tkr, "Performance Timeline Attribution"),
      subtitle = "Each bar shows the return of that phase, stacked on the previous period's result",
      x = "Market Regime Timeline",
      y = "Cumulative Performance Path"
    ) +
    
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 9),
      plot.title = element_text(face = "bold", size = 14),
      plot.margin = margin(10, 10, 10, 10)
    )
}

##################################################################################

