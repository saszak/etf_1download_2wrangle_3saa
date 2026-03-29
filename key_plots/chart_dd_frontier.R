################################################################################
# CHART TEMPLATE : dd_frontier
# NAME           : DD Frontier — Trade-off Curve per Stabilizer
# FILE           : key_plots/chart_dd_frontier.R
#
# WHAT IT SHOWS
#   For each candidate stabilizer instrument, trace the (MaxDD, AnnRet) path
#   as the overlay weight increases from 0% → 25%.  Arrow direction = increasing
#   weight.  The passive 60/40 anchor (diamond) is the shared starting point.
#   Ideal path: moves LEFT (less DD) with minimal downward drift (low drag).
#
# DESIGN PRINCIPLES
#   • x-axis = MaxDD % (not reversed — lower = further left = better)
#   • y-axis = Annual Return %
#   • Arrowed geom_path per ticker — direction = increasing overlay weight
#   • Black diamond = passive 60/40 reference (derived from w=0 rows)
#   • Filled circles at w=10% and w=20% to anchor the scale
#   • Colour palette: RColorBrewer "Dark2" (up to 8 instruments)
#
# INVOKE
#   source(here("key_plots/chart_dd_frontier.R"))
#   plot_dd_frontier(dd_frontier_data)
#
# INPUTS
#   dd_frontier_data  tibble — cols: ticker, weight, maxdd, ann_ret
#                     produced by scripts_daa_saa_taa/05_dd_stabilizer.R
#                     (the weight==0 row is the passive baseline — same for all)
#
# OPTIONAL PARAMETERS
#   weight_marks  numeric vector — overlay weights to mark with points (default c(0.10, 0.20))
#   title         chr — plot title override
#   subtitle      chr — plot subtitle override
#
# USED IN
#   scripts_daa_saa_taa/05_dd_stabilizer.R
#   executive_summary.Rmd  Section 13 (DAA Framework)
################################################################################

library(tidyverse)
library(scales)

plot_dd_frontier <- function(
    dd_frontier_data,
    weight_marks = c(0.10, 0.20),
    title    = "DD Frontier \u2014 Trade-off Curve per Stabilizer (0% \u2192 25% overlay)",
    subtitle = "Arrow direction = increasing overlay weight  |  Diamond = Passive 60/40 starting point\nIdeal: path moves left (less DD) with minimal downward drift (little return drag)"
) {

  # Passive reference: w=0 is identical for every ticker — take first occurrence
  passive_ref <- dd_frontier_data %>%
    filter(weight == 0) %>%
    slice(1)

  passive_x <- passive_ref$maxdd   * 100
  passive_y <- passive_ref$ann_ret * 100

  # Weight-mark labels (build shape/label maps dynamically from weight_marks)
  wm_chr    <- as.character(weight_marks)
  wm_shapes <- setNames(c(16L, 17L, 15L, 18L)[seq_along(weight_marks)], wm_chr)
  wm_labels <- setNames(sprintf("w = %.0f%%", weight_marks * 100), wm_chr)

  ggplot(dd_frontier_data,
         aes(x = maxdd * 100, y = ann_ret * 100,
             colour = ticker, group = ticker)) +

    # ── Passive anchor ────────────────────────────────────────────────────────
    geom_point(data        = passive_ref,
               aes(x = maxdd * 100, y = ann_ret * 100),
               inherit.aes = FALSE,
               colour = "#111827", size = 5, shape = 18) +
    annotate("text",
             x     = passive_x + 0.3,
             y     = passive_y,
             label = "Passive\n60/40",
             hjust = 0, size = 3, fontface = "bold", colour = "#111827") +

    # ── Frontier paths ────────────────────────────────────────────────────────
    geom_path(linewidth = 0.8, alpha = 0.85,
              arrow = arrow(length = unit(0.18, "cm"), type = "closed")) +

    # ── Weight markers ────────────────────────────────────────────────────────
    geom_point(data = dd_frontier_data %>%
                 filter(weight %in% weight_marks),
               aes(shape = factor(weight)),
               size = 3) +
    scale_shape_manual(values = wm_shapes,
                       labels = wm_labels,
                       name   = "Weight") +

    # ── Scales ────────────────────────────────────────────────────────────────
    scale_colour_brewer(palette = "Dark2", name = "Instrument") +
    scale_x_continuous(labels = function(x) paste0(x, "%"),
                       name   = "MaxDD  (lower = better, move left) \u2192") +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       name   = "Annual Return  (higher = better) \u2191") +

    labs(title = title, subtitle = subtitle) +

    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(colour = "grey50", size = 9),
      legend.position  = "right"
    )
}

# ── Quick-run example (guarded) ───────────────────────────────────────────────
if (FALSE) {
  library(here)
  source(here("project_tree.R"))
  source(here("scripts_daa_saa_taa/05_dd_stabilizer.R"))
  # dd_frontier_data is produced by 05_dd_stabilizer.R
  plot_dd_frontier(dd_frontier_data)

  # Or load from disk
  dd_frontier_data <- readRDS(here("02_data_processed/dd_frontier_data.rds"))
  plot_dd_frontier(dd_frontier_data)
}
