# ==============================================================================
# FUNCTION: plot_dual_regime_overlay
# Purpose: Top Bar Labeled | Centered 0-Decimal Labels | Dark Grey Summary
# Fix: Summary stats color changed to darkgrey; labels strictly centered.
# ==============================================================================


plot_dual_regime_overlay <- function(r_xts, t_macro = 0.10, t_micro = 0.05, 
                                     asset_name = "SPY", xwidth = 1.5,
                                     label_size = 2.5) {
  
  # 0. DATA INTEGRITY
  if (!is.xts(r_xts)) r_xts <- tk_xts(as.data.frame(r_xts), silent = TRUE)
  colnames(r_xts) <- "Returns"
  
  # 1. SUMMARY STATS (1-decimal for precision)
  total_ret <- percent(as.numeric(Return.cumulative(r_xts)), accuracy = 0.1)
  ann_ret   <- percent(as.numeric(Return.annualized(r_xts)), accuracy = 0.1)
  max_dd    <- percent(as.numeric(maxDrawdown(r_xts)), accuracy = 0.1)
  vol       <- percent(as.numeric(StdDev.annualized(r_xts)), accuracy = 0.1)
  
  perf_text <- paste0(
    "Total Return: ", total_ret, "\n",
    "Annualized: ", ann_ret, "\n",
    "Max Drawdown: ", max_dd, "\n",
    "Volatility: ", vol
  )
  
  # 2. CALCULATION ENGINE
  calculate_regime_cycle <- function(r_xts_in, thresh) {
    dd_table <- table.Drawdowns(r_xts_in, top = 50) %>% 
      filter(Depth <= -thresh) %>% arrange(From)
    
    if(nrow(dd_table) == 0) return(NULL)
    
    get_period_perf <- function(start, end) {
      window_rets <- r_xts_in[paste0(as.Date(start), "::", as.Date(end))]
      if(length(window_rets) == 0) return(0)
      as.numeric(Return.cumulative(window_rets))
    }
    
    rects <- map_df(1:nrow(dd_table), function(i) {
      row <- dd_table[i,]; bind_rows(
        tibble(xmin = as.Date(row$From), xmax = as.Date(row$Trough), type = "Fall",     color = "#D90429"),
        tibble(xmin = as.Date(row$Trough), xmax = as.Date(row$To),     type = "Recovery", color = "#F77F00")
      )
    })
    
    expansion_rects <- tibble(
      xmin = rects$xmax[seq(2, nrow(rects), 2)],
      xmax = lead(rects$xmin[seq(1, nrow(rects), 2)]),
      type = "Expansion", color = "#2D6A4F"
    ) %>% filter(!is.na(xmax))
    
    last_rect <- tibble(xmin = max(rects$xmax, na.rm = TRUE), 
                        xmax = as.Date(max(index(r_xts_in))), 
                        type = "Expansion", color = "#2D6A4F")
    
    bind_rows(rects, expansion_rects, last_rect) %>% 
      arrange(xmin) %>% rowwise() %>%
      mutate(days = as.numeric(xmax - xmin), 
             perf = get_period_perf(xmin, xmax),
             # 0-Decimal Rounding
             label = percent(perf, accuracy = 1), 
             is_thin = days < 40) %>% ungroup()
  }
  
  # 3. PREP DATA
  regime_macro <- calculate_regime_cycle(r_xts, t_macro)
  regime_micro <- calculate_regime_cycle(r_xts, t_micro)
  cum_df <- tibble(date = index(r_xts), cumret = as.numeric(cumprod(1 + r_xts) - 1))
  
  # 4. COORDINATE CALCULATION
  max_val     <- max(cum_df$cumret, na.rm = TRUE)
  min_val     <- min(cum_df$cumret, na.rm = TRUE)
  total_range <- max_val - min_val
  
  bar_h_macro <- total_range * 0.05 * xwidth
  bar_h_micro <- bar_h_macro / 2
  gap         <- total_range * 0.01
  
  y_base_micro <- min_val - (total_range * 0.20)
  y_base_macro <- y_base_micro + bar_h_micro + gap
  y_limit_low  <- y_base_micro - (total_range * 0.05)
  
  # 5. RENDER
  ggplot() +
    # BARS
    geom_rect(data = regime_micro, aes(xmin=xmin, xmax=xmax, ymin=y_base_micro, ymax=y_base_micro+bar_h_micro, fill=color), color="white", linewidth=0.2) +
    geom_rect(data = regime_macro, aes(xmin=xmin, xmax=xmax, ymin=y_base_macro, ymax=y_base_macro+bar_h_macro, fill=color), color="white", linewidth=0.2) +
    
    # PRICE LINE
    geom_line(data = cum_df, aes(x = date, y = cumret), color = "darkblue", linewidth = 0.5) +
    
    # PERFORMANCE ANNOTATION (Top Left - DARKGREY)
    annotate("label", x = min(cum_df$date), y = max_val, 
             label = perf_text, hjust = 0, vjust = 1, size = 3, 
             fontface = "bold", color = "darkgrey", fill = "white", alpha = 0.8, label.size = 0) +
    
    # TEXT: MACRO (Centered 0-Decimals)
    geom_text(data = regime_macro %>% filter(!is_thin),
              aes(x = xmin + (xmax-xmin)/2, y = y_base_macro + bar_h_macro/2, label = label),
              color = "white", size = label_size, fontface = "bold") +
    
    # TEXT: MACRO (Nudge for Thin Red - Centered 0-Decimals)
    geom_text(data = regime_macro %>% filter(is_thin & type == "Fall"),
              aes(x = xmin + (xmax-xmin)/2, y = y_base_macro + bar_h_macro + (total_range * 0.02), label = label),
              color = "#D90429", size = label_size * 0.9, fontface = "bold") +
    
    # MARGIN THRESHOLDS
    annotate("text", x = min(cum_df$date), y = y_base_macro + bar_h_macro/2, label = percent(t_macro), 
             hjust = 1.3, size = 3.2, fontface = "bold", color = "#1D3557") +
    
    labs(title = asset_name, subtitle = paste("Macro Threshold:", percent(t_macro))) +
    
    scale_fill_identity() +
    scale_y_continuous(labels = percent_format(), name = NULL, limits = c(y_limit_low, max_val * 1.05)) +
    scale_x_date(expand = expansion(mult = c(0.12, 0.08))) + 
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 14), panel.grid.minor = element_blank())
}

# ------------------------------------------------------------------------------
# EXAMPLE USAGE
# ------------------------------------------------------------------------------
spy_ret=xts_ret[, "SPY"]
plot_dual_regime_overlay(
  r_xts = spy_ret, 
  asset_name = "SPY: DarkGrey Summary Overlay",
  xwidth = 10.0, 
  label_size = 3.2
)