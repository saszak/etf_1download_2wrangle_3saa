# ==============================================================================
# MODULE: 05b_VIS_CUM_REGIME_OVERLAY (v12 SOVEREIGN - CHROMATIC)
# Purpose: Cumulative Returns + Cycle Bar + Color-Matched Floating Labels
# ==============================================================================
library(tidyverse)
library(PerformanceAnalytics)
library(xts)
library(timetk)
library(scales)

# 1. DATABASE INGESTION
# ------------------------------------------------------------------------------
spy_raw <- read_rds(file.path(base_path, "data_raw/raw_p_d.rds"))

spy_xts <- spy_raw %>%
  filter(symbol == "SPY") %>%
  select(date, adjusted) %>%
  tk_xts(date_var = date, silent = TRUE)

spy_rets <- Return.calculate(spy_xts) %>% na.omit()

# 2. THE CYCLE STATE MACHINE (Chromatic Logic)
# ------------------------------------------------------------------------------
calculate_regime_cycle <- function(p_xts, r_xts, thresh = 0.10) {
  dd_table <- table.Drawdowns(r_xts, top = 25) %>%
    filter(Depth <= -thresh) %>%
    arrange(From)
  
  get_period_perf <- function(start, end) {
    window_rets <- r_xts[paste0(as.Date(start), "/", as.Date(end))]
    if(length(window_rets) == 0) return(0)
    as.numeric(Return.cumulative(window_rets))
  }
  
  # Build Rectangles
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
    type = "Expansion",
    color = "#2D6A4F"
  ) %>% filter(!is.na(xmax))
  
  last_rect <- tibble(xmin = max(rects$xmax), xmax = as.Date(max(index(p_xts))), type = "Expansion", color = "#2D6A4F")
  
  all_rects <- bind_rows(rects, expansion_rects, last_rect) %>% arrange(xmin)
  
  # --- CHROMATIC TEXT LOGIC ---
  all_rects <- all_rects %>%
    rowwise() %>%
    mutate(
      days = as.numeric(xmax - xmin),
      perf = get_period_perf(xmin, xmax),
      label = percent(perf, accuracy = 0.1),
      is_thin = days < 60,
      # Store the same color for the text as the bar
      txt_color = color 
    ) %>%
    ungroup()
  
  markers <- all_rects %>%
    filter(type %in% c("Fall", "Expansion")) %>%
    mutate(Date = xmin, lty = ifelse(type == "Fall", "dashed", "dotted"))
  
  list(bar = all_rects, markers = markers)
}

# 3. PREP DATA
# ------------------------------------------------------------------------------
cycle_data <- calculate_regime_cycle(spy_xts, spy_rets, thresh = 0.10)
regime_bar <- cycle_data$bar
regime_markers <- cycle_data$markers

cum_df <- tibble(
  date = index(spy_rets),
  cumret = as.numeric(cumprod(1 + spy_rets) - 1)
)

year_markers <- tibble(date = index(spy_xts)) %>%
  mutate(year = format(date, "%Y")) %>%
  group_by(year) %>% slice(1) %>% ungroup()

# Scaling
max_val <- max(cum_df$cumret, na.rm = TRUE)
min_val <- min(cum_df$cumret, na.rm = TRUE)
total_range <- max_val - min_val
bar_height <- total_range * 0.08 
y_min_bar <- min_val - (total_range * 0.22)
y_limit_low <- y_min_bar - (total_range * 0.20)

# 4. THE RENDER
# ------------------------------------------------------------------------------
ggplot() +
  # YEAR GRID
  geom_vline(data = year_markers, aes(xintercept = date), color = "gray92", linewidth = 1.2 )  +
  
  # PHASE VERTICALS
  geom_vline(data = regime_markers %>% filter(lty == "dashed"),
             aes(xintercept = Date), color = "#D90429", linetype = "dashed", alpha = 0.4) +
  geom_vline(data = regime_markers %>% filter(lty == "dotted"),
             aes(xintercept = Date), color = "#2D6A4F", linetype = "dotted", alpha = 0.5) +
  
  # THE CYCLE BAR
  geom_rect(data = regime_bar,
            aes(xmin = xmin, xmax = xmax, 
                ymin = y_min_bar, ymax = y_min_bar + bar_height, 
                fill = color),
            color = "white", linewidth = 0.2) +
  
  # LAYER A: Centered White Labels (Wide Bars Only)
  geom_text(data = regime_bar %>% filter(!is_thin),
            aes(x = xmin + (xmax - xmin)/2, 
                y = y_min_bar + (bar_height/2), 
                label = label),
            color = "white", size = 2.8, fontface = "bold") +
  
  # LAYER B: Floating Chromatic Labels (Thin Bars Only - Color Matched)
  geom_text(data = regime_bar %>% filter(is_thin),
            aes(x = xmin + (xmax - xmin)/2, 
                y = y_min_bar + bar_height + (total_range * 0.015), 
                label = label,
                color = txt_color), # <--- MATCHING BAR COLOR
            angle = 45, size = 2.6, fontface = "bold", hjust = 0) +
  
  geom_line(data = cum_df, aes(x = date, y = cumret), color = "darkblue", linewidth = 0.2) +
  
  # YEAR LABELS
  geom_text(data = year_markers,
            aes(x = date, y = y_min_bar - (total_range * 0.08), label = year),
            size = 3.5, color = "#2B2D42", fontface = "bold", vjust = 1) +
  
  scale_fill_identity() +
  scale_color_identity() + # Required to use the txt_color hex codes
  scale_y_continuous(labels = percent_format(), name = "Cumulative Growth (SPY)", limits = c(y_limit_low, max_val * 1.05)) +
  scale_x_date(expand = c(0,0)) +
  
  labs(title = "SPY Sovereign Cycle Overlay",
       subtitle = "Floating labels are color-matched to their respective market regime",
       caption = "Database: T7 RDS | Chromatic Text Mode Active") +
  
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(face = "bold", size = 18),
        plot.margin = margin(10, 20, 80, 20))