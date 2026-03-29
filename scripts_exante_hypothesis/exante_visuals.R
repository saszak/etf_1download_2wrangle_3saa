################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE:    scripts_exante_hypothesis/exante_visuals.R
# Purpose: Exante Hypothesis — Visualization Library
#
# ── DEPENDENCIES ──────────────────────────────────────────────────────────────
#   source("scripts_exante_hypothesis/exante_engine.R") first
#   exante_tbl : output of build_exante_metrics(), optionally enriched with
#                enrich_with_metadata()
#
# ── PLOT INVENTORY ────────────────────────────────────────────────────────────
#   P1  plot_exante_quadrant()        2×2 Enhancer/Stabilizer scatter (main)
#   P2  plot_corr_dd()                Anti-diversification: corr vs DD cost
#   P3  plot_exante_heatmap()         Ticker × Horizon role heatmap
#   P4  plot_role_counts()            Role distribution by horizon (bar chart)
#   P5  plot_vol_dd_tradeoff()        Vol reduction vs DD reduction scatter
#   P6  plot_horizon_stability()      Role consistency: % windows in dominant role
################################################################################

library(tidyverse)
library(ggrepel)
library(scales)

if (!exists("project_tree")) source(here::here("project_tree.R"))

# ── SHARED PALETTE & CONSTANTS ─────────────────────────────────────────────────

ROLE_PAL <- c(
  Dominant   = "#27ae60",   # green
  Enhancer   = "#2980b9",   # blue
  Stabilizer = "#f39c12",   # orange/amber
  Detractor  = "#c0392b"    # red
)

ROLE_SHAPES <- c(
  Dominant   = 16,   # filled circle
  Enhancer   = 17,   # filled triangle up
  Stabilizer = 15,   # filled square
  Detractor  = 4     # cross
)

.exante_theme <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor  = element_blank(),
      legend.position   = "bottom",
      plot.title        = element_text(face = "bold"),
      plot.subtitle     = element_text(colour = "grey40", size = base_size * 0.85),
      strip.text        = element_text(face = "bold")
    )
}


# ==============================================================================
# P1: plot_exante_quadrant()
# ==============================================================================
#
# 2×2 scatter: x = delta_ret, y = delta_dd
# Each point = one ticker; color = role; label selected tickers.
#
# This is the PRIMARY classification plot.
# The four quadrants show the Enhancer/Stabilizer framework in action.
# Tickers near the origin are "neutral" — neither helping nor hurting.
#
# INPUTS:
#   exante_tbl   : output of build_exante_metrics()
#   horizon_sel  : horizon to display, default "full"
#   label_roles  : which roles to label, default all
#   label_n      : max labels per role
#   color_by     : "role" (default) or a metadata column, e.g. "asset_class"
# ==============================================================================

plot_exante_quadrant <- function(exante_tbl,
                                  horizon_sel  = "full",
                                  label_roles  = c("Dominant", "Enhancer", "Stabilizer"),
                                  label_n      = 6,
                                  color_by     = "role",
                                  ret_eps      = 0.005,
                                  risk_eps     = 0.005) {

  tbl <- exante_tbl %>%
    filter(horizon == horizon_sel) %>%
    mutate(role = factor(role, levels = names(ROLE_PAL)))

  bmk_label <- unique(tbl$bmk)
  blend_pct <- unique(tbl$w_blend) * 100

  # Tickers to label
  label_tbl <- tbl %>%
    filter(role %in% label_roles) %>%
    group_by(role) %>%
    slice_max(order_by = abs(delta_ret) + abs(delta_dd), n = label_n) %>%
    ungroup()

  # Map color aesthetic
  if (color_by == "role") {
    p <- ggplot(tbl, aes(x = delta_ret, y = delta_dd,
                          colour = role, shape = role))
  } else {
    p <- ggplot(tbl, aes(x = delta_ret, y = delta_dd,
                          colour = .data[[color_by]], shape = role))
  }

  p +
    # Quadrant grid lines
    geom_hline(yintercept = 0,        colour = "grey50", linetype = "dashed", linewidth = 0.5) +
    geom_hline(yintercept =  risk_eps, colour = "grey80", linetype = "dotted", linewidth = 0.4) +
    geom_hline(yintercept = -risk_eps, colour = "grey80", linetype = "dotted", linewidth = 0.4) +
    geom_vline(xintercept = 0,        colour = "grey50", linetype = "dashed", linewidth = 0.5) +
    geom_vline(xintercept =  ret_eps,  colour = "grey80", linetype = "dotted", linewidth = 0.4) +
    geom_vline(xintercept = -ret_eps,  colour = "grey80", linetype = "dotted", linewidth = 0.4) +
    # Quadrant background shading (subtle)
    annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf,
             fill = "#27ae60", alpha = 0.04) +   # Dominant  — top-right
    annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = 0,
             fill = "#2980b9", alpha = 0.04) +   # Enhancer  — bottom-right
    annotate("rect", xmin = -Inf, xmax = 0, ymin = 0, ymax = Inf,
             fill = "#f39c12", alpha = 0.04) +   # Stabilizer — top-left
    annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0,
             fill = "#c0392b", alpha = 0.04) +   # Detractor  — bottom-left
    # Quadrant labels (corners)
    annotate("text", x = Inf, y = Inf,   label = "DOMINANT",   hjust = 1.1, vjust = 1.4,
             colour = "#27ae60", fontface = "bold", size = 3.5) +
    annotate("text", x = Inf, y = -Inf,  label = "ENHANCER",   hjust = 1.1, vjust = -0.4,
             colour = "#2980b9", fontface = "bold", size = 3.5) +
    annotate("text", x = -Inf, y = Inf,  label = "STABILIZER", hjust = -0.1, vjust = 1.4,
             colour = "#f39c12", fontface = "bold", size = 3.5) +
    annotate("text", x = -Inf, y = -Inf, label = "DETRACTOR",  hjust = -0.1, vjust = -0.4,
             colour = "#c0392b", fontface = "bold", size = 3.5) +
    # Points
    geom_point(aes(size = abs(ret_tick)), alpha = 0.75) +
    # Labels for notable tickers
    geom_text_repel(
      data         = label_tbl,
      aes(label = ticker),
      size         = 3.0,
      fontface     = "bold",
      max.overlaps = 20,
      segment.color = "grey60",
      segment.size  = 0.3,
      show.legend  = FALSE
    ) +
    # Scales
    {if (color_by == "role")
      scale_colour_manual(values = ROLE_PAL, name = NULL)
     else
      scale_colour_viridis_d(name = color_by, option = "D")
    } +
    scale_shape_manual(values = ROLE_SHAPES, name = NULL) +
    scale_size_continuous(range = c(1.5, 6), guide = "none") +
    scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    .exante_theme() +
    labs(
      title    = sprintf("P1: Exante Classification — %s Benchmark | Horizon: %s",
                         bmk_label, horizon_sel),
      subtitle = sprintf(
        "x = Ann Return improvement vs %s alone (%.0f%% blend) | y = MaxDD improvement (+ = less DD)\nPoint size = ticker standalone return",
        bmk_label, blend_pct
      ),
      x = sprintf("Delta Return vs %s (annualised)", bmk_label),
      y = "Delta MaxDD vs Benchmark (+ = less severe DD)"
    )
}


# ==============================================================================
# P2: plot_corr_dd()
# ==============================================================================
#
# ANTI-DIVERSIFICATION CORE PLOT
#
# x = corr_full (full-sample correlation to benchmark)
# y = delta_dd  (positive = blend has less severe DD than bmk alone)
# size = abs(dd_tick) — standalone DD severity of the ticker
# color = role
#
# WHAT TO LOOK FOR:
#   - If diversification dogma were correct: negative slope (lower corr → better DD)
#   - The anti-diversification finding: near-zero corr has POOR delta_dd because
#     these tickers have their own large standalone DD events (independent timing)
#   - High corr (≈1) tickers: DD ≈ bmk's DD (no improvement but no worsening)
#   - The story is told by colour: standalone DD drives the y-axis, not just corr
#
# Optional: `color_by = "dd_tick"` shows the mechanism — high standalone DD tickers
# cluster in the bottom half regardless of their correlation to the bmk.
# ==============================================================================

plot_corr_dd <- function(exante_tbl,
                          horizon_sel = "full",
                          label_n     = 8,
                          show_loess  = TRUE) {

  tbl <- exante_tbl %>%
    filter(horizon == horizon_sel) %>%
    mutate(role = factor(role, levels = names(ROLE_PAL)))

  bmk_label <- unique(tbl$bmk)
  blend_pct <- unique(tbl$w_blend) * 100

  # Label tickers at the extremes (most interesting stories)
  label_tbl <- bind_rows(
    tbl %>% arrange(delta_dd)       %>% head(label_n %/% 2),  # worst DD cost
    tbl %>% arrange(desc(delta_dd)) %>% head(label_n %/% 2)   # best DD benefit
  ) %>% distinct(ticker, .keep_all = TRUE)

  p <- ggplot(tbl, aes(x = corr_full, y = delta_dd)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
    geom_vline(xintercept = 0, linetype = "dotted", colour = "grey70", linewidth = 0.4) +
    geom_vline(xintercept = 0.5, linetype = "dotted", colour = "grey70", linewidth = 0.4) +
    # Annotation: expected direction per diversification dogma
    annotate("segment",
             x = -0.5, xend = 0.5, y = 0.05, yend = -0.02,
             arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
             colour = "grey60", linetype = "dotted", linewidth = 0.5) +
    annotate("text", x = 0.0, y = 0.06, label = "Diversification dogma (lower corr → less DD)",
             colour = "grey50", size = 2.8, hjust = 0.5) +
    # Points
    geom_point(aes(colour = role, shape = role, size = abs(dd_tick)), alpha = 0.75) +
    # Loess trend line
    {if (show_loess)
      geom_smooth(method = "loess", span = 0.8, se = TRUE,
                  colour = "#2c3e50", fill = "#2c3e50", alpha = 0.1, linewidth = 1)
    } +
    # Labels
    geom_text_repel(
      data         = label_tbl,
      aes(label = ticker, colour = role),
      size         = 3.0,
      fontface     = "bold",
      max.overlaps = 20,
      segment.color = "grey60",
      segment.size  = 0.3,
      show.legend  = FALSE
    ) +
    scale_colour_manual(values = ROLE_PAL, name = "Role") +
    scale_shape_manual(values = ROLE_SHAPES, name = "Role") +
    scale_size_continuous(range = c(1.5, 7),
                          name  = "Standalone MaxDD severity",
                          labels = percent_format(accuracy = 1)) +
    scale_x_continuous(limits = c(-1, 1),
                       breaks = seq(-1, 1, 0.25),
                       labels = number_format(accuracy = 0.1)) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    .exante_theme() +
    guides(size = guide_legend(override.aes = list(alpha = 0.6))) +
    labs(
      title    = sprintf("P2: Anti-Diversification Test — %s Benchmark | Horizon: %s",
                         bmk_label, horizon_sel),
      subtitle = sprintf(
        paste0(
          "x = Full-sample correlation to %s  |  y = MaxDD change (+ = less DD in %.0f/%0.f blend)\n",
          "Point size = ticker's standalone MaxDD severity  |  Loess trend tests the dogma"
        ),
        bmk_label, (1 - unique(tbl$w_blend)) * 100, unique(tbl$w_blend) * 100
      ),
      x = sprintf("Correlation to %s (full sample)", bmk_label),
      y = "Delta MaxDD vs Benchmark Alone (+ = blend reduces DD)"
    )
}


# ==============================================================================
# P3: plot_exante_heatmap()
# ==============================================================================
#
# Ticker × Horizon heatmap — shows whether a ticker's role is stable across
# investment horizons or horizon-sensitive.
#
# Color = role (4-way classification).
# Optional: sort by pf_function or asset_class if metadata is enriched.
# ==============================================================================

plot_exante_heatmap <- function(exante_tbl,
                                 sort_by = "role_full") {

  bmk_label <- unique(exante_tbl$bmk)

  # Order tickers by their "full" horizon role, then within role by delta_dd
  if (sort_by == "role_full") {
    role_order <- exante_tbl %>%
      filter(horizon == "full") %>%
      mutate(role = factor(role, levels = names(ROLE_PAL))) %>%
      arrange(role, desc(delta_dd)) %>%
      pull(ticker)
  } else if (sort_by %in% colnames(exante_tbl)) {
    role_order <- exante_tbl %>%
      filter(horizon == "full") %>%
      arrange(.data[[sort_by]], desc(delta_dd)) %>%
      pull(ticker)
  } else {
    role_order <- sort(unique(exante_tbl$ticker))
  }

  # Only include tickers present in all horizons
  shared_tickers <- exante_tbl %>%
    group_by(ticker) %>%
    summarise(n_horizons = n_distinct(horizon), .groups = "drop") %>%
    filter(n_horizons == n_distinct(exante_tbl$horizon)) %>%
    pull(ticker)

  tbl <- exante_tbl %>%
    filter(ticker %in% shared_tickers) %>%
    mutate(
      ticker  = factor(ticker, levels = intersect(role_order, shared_tickers)),
      horizon = factor(horizon, levels = c("1Q", "1Y", "full")),
      role    = factor(role, levels = names(ROLE_PAL))
    )

  ggplot(tbl, aes(x = horizon, y = ticker, fill = role)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    # Consistency overlay: fade tiles where consistency is low
    geom_tile(aes(alpha = consistency), fill = "white", colour = NA) +
    scale_fill_manual(values = ROLE_PAL, name = "Role", drop = FALSE) +
    scale_alpha_continuous(range = c(0, 0.5), guide = "none") +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    .exante_theme(base_size = 10) +
    theme(
      axis.text.y  = element_text(size = 7),
      panel.border = element_rect(colour = "grey80", fill = NA, linewidth = 0.5)
    ) +
    labs(
      title    = sprintf("P3: Role Stability Across Horizons — %s Benchmark", bmk_label),
      subtitle = "Each cell = dominant role in that horizon | Faded = lower consistency across sub-periods",
      x        = "Investment Horizon",
      y        = NULL
    )
}


# ==============================================================================
# P4: plot_role_counts()
# ==============================================================================
#
# Stacked bar: number (and %) of tickers in each role per horizon.
# Answers: "How many tickers are Enhancers vs Stabilizers, and does it depend
# on the horizon?"
# ==============================================================================

plot_role_counts <- function(exante_tbl) {

  bmk_label <- unique(exante_tbl$bmk)

  count_tbl <- exante_tbl %>%
    mutate(
      role    = factor(role, levels = names(ROLE_PAL)),
      horizon = factor(horizon, levels = c("1Q", "1Y", "full"))
    ) %>%
    count(horizon, role) %>%
    group_by(horizon) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup()

  ggplot(count_tbl, aes(x = horizon, y = n, fill = role)) +
    geom_col(position = "stack", width = 0.65, colour = "white", linewidth = 0.4) +
    geom_text(
      aes(label = ifelse(n >= 2, sprintf("%d\n(%.0f%%)", n, pct * 100), "")),
      position = position_stack(vjust = 0.5),
      size     = 3.0,
      colour   = "white",
      fontface = "bold"
    ) +
    scale_fill_manual(values = ROLE_PAL, name = NULL, drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    .exante_theme() +
    labs(
      title    = sprintf("P4: Role Distribution by Horizon — %s Benchmark", bmk_label),
      subtitle = "How many tickers are classified Dominant/Enhancer/Stabilizer/Detractor per horizon",
      x        = "Investment Horizon",
      y        = "Number of Tickers"
    )
}


# ==============================================================================
# P5: plot_vol_dd_tradeoff()
# ==============================================================================
#
# Vol reduction vs DD reduction scatter.
# Questions: Do tickers that reduce vol also reduce DD? Or can you get one
# without the other? This reveals the vol-DD dissociation — a ticker that
# reduces vol via "diversification" may not reduce the MaxDD at all.
#
# x = delta_vol  (negative = blend has lower vol — conventional diversification)
# y = delta_dd   (positive = blend has less severe DD — what investors feel)
# ==============================================================================

plot_vol_dd_tradeoff <- function(exante_tbl,
                                  horizon_sel = "full",
                                  label_n     = 6) {

  tbl <- exante_tbl %>%
    filter(horizon == horizon_sel) %>%
    mutate(role = factor(role, levels = names(ROLE_PAL)))

  bmk_label <- unique(tbl$bmk)

  label_tbl <- bind_rows(
    tbl %>% filter(role == "Dominant")   %>% slice_max(delta_dd + (-delta_vol), n = label_n %/% 2),
    tbl %>% filter(role == "Stabilizer") %>% slice_max(delta_dd, n = label_n %/% 2),
    tbl %>% filter(role == "Detractor")  %>% slice_min(delta_dd, n = 2)
  ) %>% distinct(ticker, .keep_all = TRUE)

  ggplot(tbl, aes(x = delta_vol, y = delta_dd)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
    # Diagonal reference: if DD scaled perfectly with vol
    geom_abline(slope = 1, intercept = 0, colour = "grey70", linetype = "dotted",
                linewidth = 0.5) +
    annotate("text", x = Inf, y = -Inf,
             label = "← Vol reduces but DD worsens\n(diversification illusion zone)",
             hjust = 1.05, vjust = -0.2, size = 2.8, colour = "grey50") +
    geom_point(aes(colour = role, shape = role, size = abs(corr_full)), alpha = 0.75) +
    geom_text_repel(
      data         = label_tbl,
      aes(label = ticker, colour = role),
      size         = 3.0,
      fontface     = "bold",
      max.overlaps = 20,
      segment.color = "grey60",
      segment.size  = 0.3,
      show.legend  = FALSE
    ) +
    scale_colour_manual(values = ROLE_PAL, name = "Role") +
    scale_shape_manual(values = ROLE_SHAPES, name = "Role") +
    scale_size_continuous(range = c(1.5, 6),
                          name  = "Correlation to BMK",
                          labels = number_format(accuracy = 0.01)) +
    scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    .exante_theme() +
    labs(
      title    = sprintf("P5: Vol Reduction vs DD Reduction — %s Benchmark | %s",
                         bmk_label, horizon_sel),
      subtitle = paste0(
        "x = Ann Vol change in blended portfolio (negative = less volatile, conventional 'diversification')\n",
        "y = MaxDD change (positive = less severe — what investors actually experience)\n",
        "Diagonal = perfect scaling; points below diagonal = vol reduction fails to protect DD"
      ),
      x = "Delta Annualised Volatility (negative = blend less volatile)",
      y = "Delta MaxDD (+ = blend has less severe drawdown)"
    )
}


# ==============================================================================
# P6: plot_horizon_stability()
# ==============================================================================
#
# For each ticker, shows the fraction of sub-period windows in which it
# acted as its "full-sample role". High consistency = robust classification.
# Low consistency = regime-dependent ticker (helpful in some periods, harmful others).
#
# Displayed as a horizontal bar sorted by consistency DESC within role group.
# ==============================================================================

plot_horizon_stability <- function(exante_tbl,
                                    horizon_sel = "1Q",
                                    top_n       = 30) {

  tbl <- exante_tbl %>%
    filter(horizon == horizon_sel) %>%
    mutate(role = factor(role, levels = names(ROLE_PAL)))

  bmk_label <- unique(tbl$bmk)

  # Take top_n tickers by consistency (per role, to balance display)
  tbl_top <- tbl %>%
    group_by(role) %>%
    slice_max(consistency, n = ceiling(top_n / 4)) %>%
    ungroup() %>%
    arrange(role, desc(consistency)) %>%
    mutate(ticker = factor(ticker, levels = rev(unique(ticker))))

  ggplot(tbl_top, aes(x = consistency, y = ticker, fill = role)) +
    geom_col(width = 0.75, alpha = 0.85) +
    geom_vline(xintercept = 0.5, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
    scale_fill_manual(values = ROLE_PAL, name = NULL, drop = FALSE) +
    scale_x_continuous(labels = percent_format(accuracy = 1),
                       limits = c(0, 1),
                       expand = expansion(mult = c(0, 0.02))) +
    facet_wrap(~role, scales = "free_y", ncol = 2) +
    .exante_theme(base_size = 11) +
    theme(
      axis.text.y  = element_text(size = 8),
      legend.position = "none"
    ) +
    labs(
      title    = sprintf("P6: Role Consistency — %s Benchmark | %s Windows",
                         bmk_label, horizon_sel),
      subtitle = sprintf(
        "Bar = fraction of %s sub-periods where ticker acted in its full-sample role | Dashed = 50%%",
        horizon_sel
      ),
      x = "Fraction of Sub-Periods with Consistent Role",
      y = NULL
    )
}


# ==============================================================================
# RENDER: render_exante_report()
# ==============================================================================
#
# Renders exante_hypothesis_report.Rmd for any benchmark ticker.
# Output is saved to 03_reports/ as exante_{BMK}_w{blend_pct}.html
#
# USAGE:
#   render_exante_report("SPY")
#   render_exante_report("AGG")
#   render_exante_report("QQQ", w_blend = 0.3)
#   render_exante_report("GLD", ret_eps = 0.01, risk_eps = 0.01)
#
# INPUTS:
#   bmk        : benchmark ticker (must be a column in xts_ret)
#   w_blend    : benchmark weight in blend (default 0.5)
#   ret_eps    : return threshold for role boundary (default 0.005)
#   risk_eps   : DD threshold for role boundary (default 0.005)
#   output_dir : directory for the rendered HTML (default 03_reports/)
# ==============================================================================

render_exante_report <- function(bmk        = "SPY",
                                  w_blend    = 0.5,
                                  ret_eps    = 0.005,
                                  risk_eps   = 0.005,
                                  output_dir = here::here("03_reports")) {

  rmd_path <- here::here("Rmd/exante_hypothesis_report.Rmd")

  if (!file.exists(rmd_path))
    stop("Report template not found: ", rmd_path)

  output_file <- sprintf("exante_%s_w%d.html", bmk, round(w_blend * 100))

  cat(sprintf("\n── Rendering Exante Report | bmk=%s | blend=%d/%d | output=%s\n",
              bmk, round(w_blend * 100), round((1 - w_blend) * 100),
              file.path(output_dir, output_file)))

  rmarkdown::render(
    input       = rmd_path,
    output_file = output_file,
    output_dir  = output_dir,
    params      = list(
      bmk      = bmk,
      w_blend  = w_blend,
      ret_eps  = ret_eps,
      risk_eps = risk_eps
    ),
    envir = new.env(parent = globalenv()),
    quiet = FALSE
  )

  cat(sprintf("── Done: %s\n", file.path(output_dir, output_file)))
  invisible(file.path(output_dir, output_file))
}


# ==============================================================================
# CONVENIENCE: run_all_exante_plots()
# ==============================================================================
#
# Runs all 6 plots in sequence for a given exante_tbl + horizon.
# Useful for exploratory sessions.
# ==============================================================================

run_all_exante_plots <- function(exante_tbl, horizon_sel = "full") {
  cat(sprintf("\n── Running all Exante plots | horizon=%s ──\n", horizon_sel))

  print(plot_exante_quadrant(exante_tbl, horizon_sel = horizon_sel))
  print(plot_corr_dd(exante_tbl,         horizon_sel = horizon_sel))
  print(plot_exante_heatmap(exante_tbl))
  print(plot_role_counts(exante_tbl))
  print(plot_vol_dd_tradeoff(exante_tbl, horizon_sel = horizon_sel))
  print(plot_horizon_stability(exante_tbl, horizon_sel = "1Q"))

  invisible(exante_tbl)
}
