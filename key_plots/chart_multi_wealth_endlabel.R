################################################################################
# CHART TEMPLATE : multi_wealth_endlabel
# NAME           : Multi-Series Wealth Chart with End-of-Line Labels
# FILE           : key_plots/chart_multi_wealth_endlabel.R
#
# WHAT IT SHOWS
#   Cumulative wealth ($1 invested) for 2–5 portfolios on the same axis.
#   Fall/Recovery regime shading behind the lines.
#   Each line labelled directly at its right endpoint (no legend needed).
#   Annualised return appended to label (e.g. "DAA\n+8.4% p.a.").
#
# DESIGN PRINCIPLES
#   • Direct end-of-line labels via geom_text + nudge_x  (no legend)
#   • coord_cartesian(clip="off") + plot.margin right padding for label space
#   • Y-axis lower limit = 0.7 (clips extreme early periods, not the main story)
#   • Regime shading: red (#fca5a5 α=0.18) = Fall  |  green (#bbf7d0 α=0.18) = Recovery
#   • Ann return computed as wealth_final ^ (252/n_days) − 1 at last point
#   • Colour palette: purple (#7c3aed) | blue (#3b82f6) | grey (#9ca3af)
#     → use for: active strategy | SAA baseline | passive benchmark
#
# INVOKE — MINIMAL (colours and labels default from ret_list)
#   source(here("key_plots/chart_multi_wealth_endlabel.R"))
#   rt <- build_regime_table(xts_ret[, "SPY"])
#   plot_multi_wealth_endlabel(
#     ret_list = list(SPY = xts_ret[,"SPY"], GLD = xts_ret[,"GLD"], IEF = xts_ret[,"IEF"]),
#     rt       = rt
#   )
#
# INVOKE — FULL CONTROL
#   plot_multi_wealth_endlabel(
#     ret_list = list(DAA = daa_ret, SAA = saa_ret, Passive = bmk_ret),
#     rt       = rt,
#     colours  = c("#7c3aed", "#3b82f6", "#9ca3af"),
#     labels   = c("DAA — 200DMA filter", "SAA 60/40", "Passive SPY/IEF")
#   )
#
# INPUTS
#   ret_list   named list of xts daily return series  ← names become default labels
#   rt         tibble — regime table from build_regime_table()
#   colours    chr vector — optional; defaults: purple/blue/grey/amber/green/red
#              then hue_pal() for >6 series
#   labels     chr vector — optional; defaults to names(ret_list)
#              supports \n for multi-line end-of-line annotation
#   y_floor    numeric — lower y-axis limit (default 0.7)
#   right_pad  numeric — right plot.margin in pts for label space (default 160)
#
# DEFAULTING CONVENTION (reuse in other templates)
#   colours = NULL → filled from fixed palette or hue_pal(n)
#   labels  = NULL → filled from names(ret_list)
#   This pattern lets callers pass just data + rt for a working plot,
#   and override only what they need.
#
# USED IN
#   scripts_daa_saa_taa/04_daa_backtest.R  →  plot_backtest_wealth()
#   executive_summary.Rmd  Section 13 (DAA Framework)
################################################################################


#  rt <- build_regime_table(xts_ret[, "SPY"])
#  plot_multi_wealth_endlabel(                                                                                                                            
#   ret_list = list(SPY = xts_ret[,"SPY"], IEF = xts_ret[,"IEF"]),
#   rt       = rt                                                                                                                                        
# )

library(tidyverse)
library(xts)
library(scales)

plot_multi_wealth_endlabel <- function(
    ret_list,
    rt,
    colours   = NULL,   # default: evenly-spaced hue palette
    labels    = NULL,   # default: names(ret_list)
    y_floor   = 0.70,
    right_pad = 160
) {
  n <- length(ret_list)

  # Default labels = list names
  if (is.null(labels))  labels  <- names(ret_list)
  if (is.null(labels) || any(is.na(labels)))
    labels <- paste0("S", seq_len(n))

  # Default colours: up to 3 use the canonical palette, else hue_pal
  default_palette <- c("#7c3aed", "#3b82f6", "#9ca3af",
                       "#f59e0b", "#22c55e", "#ef4444")
  if (is.null(colours))
    colours <- if (n <= length(default_palette)) default_palette[seq_len(n)]
               else scales::hue_pal()(n)

  # ── Align all series to common date range ─────────────────────────────────────
  combined <- Reduce(function(a, b) merge(a, b, join = "inner"), ret_list)
  if (is.null(names(ret_list))) names(ret_list) <- paste0("S", seq_along(ret_list))

  port_names <- names(ret_list)
  colour_map <- setNames(colours[seq_along(port_names)], port_names)
  label_map  <- setNames(labels[seq_along(port_names)],  port_names)

  # ── Wealth tibble ──────────────────────────────────────────────────────────────
  wealth_df <- as.data.frame(combined) %>%
    rownames_to_column("date") %>%
    mutate(date = as.Date(date)) %>%
    pivot_longer(-date, names_to = "portfolio", values_to = "ret") %>%
    group_by(portfolio) %>%
    arrange(date) %>%
    mutate(wealth = cumprod(1 + ret)) %>%
    ungroup()

  # ── End-of-line labels with ann return ────────────────────────────────────────
  last_points <- wealth_df %>%
    group_by(portfolio) %>%
    slice_max(date, n = 1) %>%
    left_join(
      wealth_df %>%
        group_by(portfolio) %>%
        summarise(n_days  = n(),
                  ann_ret = last(wealth)^(252 / n()) - 1,
                  .groups = "drop"),
      by = "portfolio"
    ) %>%
    mutate(end_label = sprintf("%s\n%+.1f%% p.a.",
                               recode(portfolio, !!!label_map),
                               ann_ret * 100)) %>%
    ungroup()

  # ── Regime shading ─────────────────────────────────────────────────────────────
  regime_rect <- rt %>%
    filter(regime %in% c("Fall", "Recovery")) %>%
    mutate(
      fill  = if_else(regime == "Fall", "#fca5a5", "#bbf7d0"),
      xmin  = as.Date(xmin),
      xmax  = as.Date(xmax)
    )

  # ── Plot ───────────────────────────────────────────────────────────────────────
  ggplot(wealth_df, aes(date, wealth, colour = portfolio)) +
    geom_rect(data        = regime_rect,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf,
                  fill = fill),
              inherit.aes = FALSE, alpha = 0.18) +
    scale_fill_identity() +
    geom_line(linewidth = 0.9) +
    geom_text(
      data      = last_points,
      aes(label = end_label),
      hjust     = 0,
      nudge_x   = 60,
      size      = 3,
      fontface  = "bold",
      lineheight = 0.85
    ) +
    scale_colour_manual(values = colour_map, guide = "none") +
    scale_y_continuous(
      labels = dollar_format(prefix = "$"),
      limits = c(y_floor, NA),
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    coord_cartesian(clip = "off") +
    labs(
      title    = "Cumulative Wealth — $1 Invested",
      subtitle = "Pink = Fall regime  |  Green = Recovery  |  Ann return at line end",
      x = NULL, y = "Wealth ($)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.margin      = margin(5, right_pad, 5, 5),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(colour = "grey50", size = 9)
    )
}

# ── Quick-run example (guarded) ───────────────────────────────────────────────
if (FALSE) {
  library(here)
  source(here("project_tree.R"))
  source(here("scripts_daa_saa_taa/01_saa_baseline.R"))
  daa  <- readRDS(here("02_data_processed/daa_returns.rds"))
  ret_list <- list(DAA = daa, SAA_60_40 = saa_returns, Passive_60_40 = benchmark_returns)
  plot_multi_wealth_endlabel(
    ret_list,
    rt,
    colours = c("#7c3aed", "#3b82f6", "#9ca3af"),
    labels  = c("DAA — trend filter", "SAA 60/40", "Passive SPY/IEF")
  )
}
