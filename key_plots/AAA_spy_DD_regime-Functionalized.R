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
  
  # 3. EQUAL HEIGHT SCALING & COORDINATES
  # ----------------------------------------------------------------------------
  cum_df <- tibble(date = index(m_r), cumret = as.numeric(cumprod(1 + m_r) - 1))
  year_markers <- cum_df %>% mutate(year = format(date, "%Y")) %>% group_by(year) %>% slice(1) %>% ungroup()
  
  max_val <- max(cum_df$cumret); min_val <- min(cum_df$cumret); tr <- max_val - min_val
  
  h_bar <- tr * 0.08 
  gap   <- tr * 0.02
  y_m <- min_val - (tr * 0.22)
  y_o <- y_m - h_bar - gap
  y_low <- y_o - (tr * 0.35)
  
  # 4. RENDER
  # ----------------------------------------------------------------------------
  ggplot() +
    # Background Year Markers
    geom_vline(data = year_markers, aes(xintercept = date), color = "gray92", linewidth = 1.2) +
    
    # BARS
    geom_rect(data = regime_bar, aes(xmin = xmin, xmax = xmax, ymin = y_m, ymax = y_m + h_bar, fill = color), color = "white", linewidth = 0.2) +
    geom_rect(data = regime_bar, aes(xmin = xmin, xmax = xmax, ymin = y_o, ymax = y_o + h_bar, fill = color), color = "white", linewidth = 0.2, alpha = 0.8) +
    
    # LABELS
    geom_text(data = regime_bar %>% filter(!is_thin), aes(x = xmin + (xmax - xmin)/2, y = y_m + h_bar/2, label = m_label), color = "white", size = 2.6, fontface = "bold") +
    geom_text(data = regime_bar %>% filter(!is_thin), aes(x = xmin + (xmax - xmin)/2, y = y_o + h_bar/2, label = o_label), color = "white", size = 2.6, fontface = "bold") +
    geom_text(data = regime_bar %>% filter(is_thin), aes(x = xmin + (xmax - xmin)/2, y = y_m + h_bar + (tr * 0.025), label = m_label, color = txt_color), angle = 45, size = 2.4, fontface = "bold", hjust = 0) +
    geom_text(data = regime_bar %>% filter(is_thin), aes(x = xmin + (xmax - xmin)/2, y = y_o - (tr * 0.025), label = o_label, color = txt_color), angle = 45, size = 2.4, fontface = "bold", hjust = 1) +
    
    geom_line(data = cum_df, aes(x = date, y = cumret), color = "darkblue", linewidth = 0.6) +
    
    # ANNOTATIONS
    annotate("text", x = min(cum_df$date), y = y_m + h_bar/2, label = master_tkr, hjust = 1.3, size = 3.5, fontface = "bold", color = "#1D3557") +
    annotate("text", x = min(cum_df$date), y = y_o + h_bar/2, label = overlay_tkr, hjust = 1.3, size = 3.5, fontface = "bold", color = "grey30") +
    
    scale_fill_identity() + scale_color_identity() +
    # Grid Logic: 20% intervals
    scale_y_continuous(labels = percent_format(), 
                       breaks = seq(-1, max_val + 0.2, by = 0.20),
                       limits = c(y_low, max_val * 1.05)) +
    scale_x_date(expand = expansion(mult = c(0.12, 0.02))) +
    
    labs(title = paste(master_tkr, "vs", overlay_tkr, "Dual Cycle Analysis"),
         subtitle = paste("Threshold:", percent(thresh), "| Overlay:", ifelse(relative, "Relative Alpha", "Absolute Performance"))) +
    
    theme_minimal() + 
    theme(
      panel.grid.major.y = element_line(color = "gray90", linewidth = 1), # <--- width=1
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title = element_blank(), 
      axis.text.x = element_blank()
    )
}
