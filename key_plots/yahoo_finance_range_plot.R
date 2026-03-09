# ==============================================================================
# FUNCTION: plot_range_matrix
# Purpose: Technical range matrix with dynamic lookback & beige ledger
# Parameters: 
#   - data: xts object (xts_r)
#   - lookback: integer (e.g., 252 for 52-week, 21 for 1-month)
# ==============================================================================
plot_range_matrix <- function(data, lookback = 252) {
  
  # 1. WINDOW FILTERING
  # ----------------------------------------------------------------------------
  # Take the last 'n' observations based on lookback
  data_window <- tail(data, lookback)
  
  # Convert returns to price levels (Base 100) for the period
  price_matrix <- apply(data_window, 2, function(x) cumprod(1 + x) * 100)
  
  df_metrics <- map_df(colnames(price_matrix), function(tkr) {
    prices <- na.omit(price_matrix[, tkr])
    curr   <- as.numeric(tail(prices, 1))
    hi     <- as.numeric(max(prices))
    lo     <- as.numeric(min(prices))
    
    tibble(
      symbol  = tkr,
      low     = lo,
      high    = hi,
      current = curr
    )
  })
  
  # 2. CALCULATION & BEIGE LOGIC
  # ----------------------------------------------------------------------------
  df_plot <- df_metrics %>%
    mutate(
      pos = (current - low) / (high - low),
      status_tag = ifelse(pos >= 0.5, "bull", "bear"),
      row_id = row_number(),
      fill_group = ifelse(row_id %% 2 == 0, "beige_row", "white_row")
    )
  
  whisker_data <- expand_grid(
    symbol = df_plot$symbol,
    pct = seq(0, 1, by = 0.1)
  )
  
  # 3. RENDER
  # ----------------------------------------------------------------------------
  ggplot(df_plot, aes(y = reorder(symbol, row_id))) +
    
    # A. BEIGE CHECKERED ROWS
    geom_rect(aes(xmin = -Inf, xmax = Inf, 
                  ymin = row_id - 0.5, ymax = row_id + 0.5, 
                  fill = fill_group), alpha = 1.0) +
    
    # B. STRUCTURAL GRID (10%)
    geom_vline(xintercept = seq(0, 1, by = 0.1), 
               color = "white", linewidth = 0.8) +
    
    # C. RANGE TRACK
    geom_segment(aes(x = 0, xend = 1, yend = symbol), 
                 color = "gray88", linewidth = 4, lineend = "butt") +
    
    # D. 10% WHISKERS
    geom_segment(data = whisker_data, 
                 aes(x = pct, xend = pct, 
                     y = as.numeric(factor(symbol)) - 0.25, 
                     yend = as.numeric(factor(symbol)) + 0.25),
                 color = "gray65", linewidth = 0.5) +
    
    # E. 50% ANCHOR
    geom_segment(aes(x = 0.5, xend = 0.5, 
                     y = as.numeric(factor(symbol)) - 0.4, 
                     yend = as.numeric(factor(symbol)) + 0.4),
                 color = "#1D3557", linewidth = 1.2) +
    
    # F. INVERTED TRIANGLE PIN
    geom_point(aes(x = pos, fill = status_tag), 
               shape = 25, size = 5, color = "white", stroke = 0.8) +
    
    # G. PRICE LABELS
    geom_text(aes(x = 0, label = round(low, 1)), 
              hjust = 1.5, size = 3, color = "gray30", fontface = "bold") +
    geom_text(aes(x = 1, label = round(high, 1)), 
              hjust = -0.5, size = 3, color = "gray30", fontface = "bold") +
    geom_text(aes(x = pos, label = round(current, 1)), 
              vjust = -2.5, size = 3.5, fontface = "bold") +
    
    # SCALING
    scale_fill_manual(values = c(
      "beige_row" = "#F5F5DC", "white_row" = "white",
      "bull"      = "#2D6A4F", "bear"      = "#D90429"
    ), guide = "none") +
    
    scale_x_continuous(limits = c(-0.2, 1.2), 
                       breaks = seq(0, 1, by = 0.1), 
                       labels = percent_format()) +
    
    labs(title = "Market Asset Range Matrix",
         subtitle = paste("Lookback Period:", lookback, "Days | 10% Grid | Beige Ledger"),
         x = "Position Within Range", y = NULL) +
    
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text.y = element_text(face = "bold", size = 11, color = "#1D3557"),
      axis.text.x = element_text(color = "gray40", size = 9, face = "bold"),
      plot.title = element_text(face = "bold", size = 16),
      plot.margin = margin(20, 50, 20, 50)
    )
}