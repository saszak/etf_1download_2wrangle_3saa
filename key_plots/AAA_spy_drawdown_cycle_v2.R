# ==============================================================================
# MODULE: 05b_VIS_CUM_REGIME_OVERLAY (v12 DUAL-THRESHOLD)
# Purpose: Cumulative Returns + 10% Macro Bar + 5% Micro Bar (Half-Thickness)
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

# 2. THE DUAL-CYCLE ENGINE
# ------------------------------------------------------------------------------
calculate_regime_cycle <- function(p_xts, r_xts, thresh = 0.10, label_suffix = "") {
  dd_table <- table.Drawdowns(r_xts, top = 40) %>%
    filter(Depth <= -thresh) %>%
    arrange(From)
  
  get_period_perf <- function(start, end) {
    window_rets <- r_xts[paste0(as.Date(start), "::", as.Date(end))]
    if(length(window_rets) == 0) return(0)
    as.numeric(Return.cumulative(window_rets))
  }
  
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
  
  all_rects <- bind_rows(rects, expansion_rects, last_rect) %>% 
    arrange(xmin) %>%
    rowwise() %>%
    mutate(
      days = as.numeric(xmax - xmin),
      perf = get_period_perf(xmin, xmax),
      label = percent(perf, accuracy = 0.1),
      is_thin = days < 60,
      thresh_tag = label_suffix
    ) %>%
    ungroup()
  
  return(all_rects)
}

# 3. EXECUTE DATA PREP
# ------------------------------------------------------------------------------
regime_10 <- calculate_regime_cycle(spy_xts, spy_rets, thresh = 0.10, label_suffix = "10%")
regime_05 <- calculate_regime_cycle(spy_xts, spy_rets, thresh = 0.05, label_suffix = "5%")

markers_10 <- regime_10 %>%
  filter(type %in% c("Fall", "Expansion")) %>%
  mutate(Date = xmin, lty = ifelse(type == "Fall", "dashed", "dotted"))

cum_df <- tibble(
  date = index(spy_rets),
  cumret = as.numeric(cumprod(1 + spy_rets) - 1)
)

year_markers <- tibble(date = index(spy_xts)) %>%
  mutate(year = format(date, "%Y")) %>%
  group_by(year) %>% slice(1) %>% ungroup()

# 4. Y-AXIS STACKING LOGIC (ASymmetric Thickness)
# ------------------------------------------------------------------------------
max_val <- max(cum_df$cumret, na.rm = TRUE)
min_val <- min(cum_df$cumret, na.rm = TRUE)
total_range <- max_val - min_val

bar_h_10 <- total_range * 0.06      # Full Thickness
bar_h_05 <- bar_h_10 / 2            # HALF THICKNESS
gap      <- total_range * 0.01      

y_top_10 <- min_val - (total_range * 0.15)
y_top_05 <- y_top_10 - bar_h_05 - gap

y_limit_low <- y_top_05 - (total_range * 0.25) 

# 5. THE DUAL-BAR RENDER
# ------------------------------------------------------------------------------
ggplot() +
  # YEAR GRID
  geom_vline(data = year_markers, aes(xintercept = date), color = "gray92", linewidth = 1.2) +
  
  # PHASE VERTICALS (Aligned to 10%)
  geom_vline(data = markers_10 %>% filter(lty == "dashed"),
             aes(xintercept = Date), color = "#D90429", linetype = "dashed", alpha = 0.3) +
  geom_vline(data = markers_10 %>% filter(lty == "dotted"),
             aes(xintercept = Date), color = "#2D6A4F", linetype = "dotted", alpha = 0.3) +
  
  # BAR 1: 10% MACRO (Thick)
  geom_rect(data = regime_10,
            aes(xmin = xmin, xmax = xmax, ymin = y_top_10, ymax = y_top_10 + bar_h_10, fill = color),
            color = "white", linewidth = 0.2) +
  
  # BAR 2: 5% MICRO (HALF AS THICK)
  geom_rect(data = regime_05,
            aes(xmin = xmin, xmax = xmax, ymin = y_top_05, ymax = y_top_05 + bar_h_05, fill = color),
            color = "white", linewidth = 0.2) +
  
  # TEXT LABELS: 10% Bar
  geom_text(data = regime_10 %>% filter(!is_thin),
            aes(x = xmin + (xmax-xmin)/2, y = y_top_10 + bar_h_10/2, label = label),
            color = "white", size = 2.5, fontface = "bold") +
  
  geom_text(data = regime_10 %>% filter(is_thin),
            aes(x = xmin + (xmax-xmin)/2, y = y_top_10 + bar_h_10 + (total_range * 0.015), 
                label = label, color = color),
            angle = 45, size = 2.3, fontface = "bold", hjust = 0) +
  
  # CUMULATIVE LINE
  geom_line(data = cum_df, aes(x = date, y = cumret), color = "darkblue", linewidth = 0.2) +
  
  # YEAR LABELS
  geom_text(data = year_markers,
            aes(x = date, y = y_top_05 - (total_range * 0.12), label = year),
            size = 3.5, color = "#2B2D42", fontface = "bold", vjust = 1) +
  
  # CLEAN THRESHOLD LABELS
  annotate("text", x = min(cum_df$date), y = y_top_10 + bar_h_10/2, label = "10%", 
           hjust = 1.3, size = 3.5, fontface = "bold", color = "#1D3557") +
  annotate("text", x = min(cum_df$date), y = y_top_05 + bar_h_05/2, label = "5%", 
           hjust = 1.3, size = 3, fontface = "bold", color = "#1D3557") +
  
  scale_fill_identity() +
  scale_color_identity() +
  scale_y_continuous(labels = percent_format(), name = "Cumulative Growth (SPY)", 
                     limits = c(y_limit_low, max_val * 1.05)) +
  scale_x_date(expand = expansion(mult = c(0.08, 0.02))) +
  
  labs(title = "SPY Sovereign Dual-Threshold Overlay",
       subtitle = "Top: 10% Macro Regime | Bottom: 5% Micro Sensitivity (Half-Thickness)",
       caption = "Database: T7 RDS | Thresholds: 10% vs 5% Drawdown Sensitivity") +
  
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(face = "bold", size = 18),
        plot.margin = margin(10, 40, 80, 60))