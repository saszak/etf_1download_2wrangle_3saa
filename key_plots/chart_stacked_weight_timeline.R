################################################################################
# CHART TEMPLATE : stacked_weight_timeline
# NAME           : Stacked Portfolio Weight Timeline
# FILE           : key_plots/chart_stacked_weight_timeline.R
#
# WHAT IT SHOWS
#   Portfolio weight composition as a stacked area over time.
#   Each ticker = one coloured band. Bands expand/shrink as TAA tilts activate.
#   Fall regime episodes shaded in pink behind the stack.
#
# DESIGN PRINCIPLES
#   • stacked geom_area (not geom_bar) — continuity readable at a glance
#   • Thin white separator lines between bands (linewidth = 0.1)
#   • Regime shading sits *behind* the stack (first layer)
#   • Legend: right side, small keys (0.35 cm), tickers ordered by avg weight desc
#
# INVOKE
#   source(here("key_plots/chart_stacked_weight_timeline.R"))
#   plot_stacked_weight_timeline(weight_history_tbl, saa_tbl, rt)
#
# INPUTS
#   weight_history_tbl  tibble  — cols: date, ticker, taa_weight (or any weight col)
#   saa_tbl             tibble  — cols: ticker, weight, saa_bucket  (for ordering)
#   rt                  tibble  — regime table from build_regime_table()
#   weight_col          chr     — column name to plot (default "taa_weight")
#   title / subtitle    chr     — optional overrides
#
# USED IN
#   scripts_daa_saa_taa/03_taa_rules.R  →  plot_taa_composition()
#   executive_summary.Rmd  Section 13 (DAA Framework)
################################################################################

library(tidyverse)
library(scales)

plot_stacked_weight_timeline <- function(
    weight_history_tbl,
    saa_tbl,
    rt,
    weight_col = "taa_weight",
    title      = "Portfolio Composition Over Time",
    subtitle   = "Stacked area — each band = one ticker  |  Pink = Fall regime"
) {

  # ── Ticker order: SAA bucket then weight descending ──────────────────────────
  bucket_map <- saa_tbl %>%
    arrange(saa_bucket, desc(weight)) %>%
    select(ticker, saa_bucket)

  df <- weight_history_tbl %>%
    rename(w = !!weight_col) %>%
    left_join(bucket_map, by = "ticker") %>%
    mutate(ticker = factor(ticker, levels = bucket_map$ticker))

  regime_rect <- rt %>%
    filter(regime == "Fall") %>%
    mutate(xmin = as.Date(xmin), xmax = as.Date(xmax))

  ggplot(df, aes(date, w, fill = ticker)) +
    # ── Regime shading (behind everything) ──────────────────────────────────────
    geom_rect(data        = regime_rect,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE,
              fill        = "#fca5a5", alpha = 0.22) +
    # ── Stacked area ────────────────────────────────────────────────────────────
    geom_area(position  = "stack",
              colour    = "white",
              linewidth = 0.10,
              alpha     = 0.88) +
    # ── Scales ──────────────────────────────────────────────────────────────────
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.01))) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    # ── Labels & theme ───────────────────────────────────────────────────────────
    labs(title    = title,
         subtitle = subtitle,
         x = NULL, y = "Weight", fill = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(colour = "grey50", size = 9),
      legend.position  = "right",
      legend.key.size  = unit(0.35, "cm")
    )
}

# ── Quick-run example (guarded) ───────────────────────────────────────────────
if (FALSE) {
  library(here)
  source(here("project_tree.R"))
  source(here("scripts_daa_saa_taa/01_saa_baseline.R"))
  taa_hist <- readRDS(here("02_data_processed/taa_weights_history.rds"))
  plot_stacked_weight_timeline(taa_hist, SAA_60_40, rt)
}
