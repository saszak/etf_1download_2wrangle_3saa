################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : utility/plot_xts_pair.R
# Purpose : Utility — compare two xts daily-return series visually
#
# FUNCTION: plot_xts_pair(xts1, xts2, ...)
#
# THREE PANELS
#   Panel 1 : Cumulative wealth of xts1 AND xts2 on the same axis
#   Panel 2 : Cumulative wealth of the spread  (xts1 - xts2)
#              i.e. the Long xts1 / Short xts2 portfolio return
#   Panel 3 : Running drawdown from peak for both series + spread
#
# COMBINED : patchwork stack of all three (returned invisibly; also printed)
#
# USAGE
#   source(here("utility/plot_xts_pair.R"))
#   plot_xts_pair(xts_ret[, "GLD"], xts_ret[, "XLK"],
#                 label1 = "GLD", label2 = "XLK")
#
# PARAMETERS
#   xts1, xts2   xts objects of daily log or arithmetic returns
#   label1/2     display names (default: column name of each xts)
#   col1/2       line colours for xts1 / xts2
#   col_spread   line colour for the spread
#   title_prefix optional string prepended to all panel titles
################################################################################

library(tidyverse)
library(xts)
library(scales)
library(patchwork)

plot_xts_pair <- function(
    xts1,
    xts2,
    label1       = colnames(xts1)[1],
    label2       = colnames(xts2)[1],
    col1         = "#1D3557",
    col2         = "#E63946",
    col_spread   = "#F4A261",
    roll_cor_win = 250,     # rolling correlation window in days
    roll_ir_win  = 250,     # rolling IR window in days
    regime_tbl   = NULL,   # optional: regime table from build_regime_table()
                           #   if supplied, adds regime shading to Panel 5 + Panel 6
    title_prefix = NULL
) {

  # ── Input validation ─────────────────────────────────────────────────────────
  stopifnot(is.xts(xts1), is.xts(xts2))
  xts1 <- xts1[, 1]   # enforce single column
  xts2 <- xts2[, 1]

  # ── Align to common date range ───────────────────────────────────────────────
  common <- merge(xts1, xts2, join = "inner")
  colnames(common) <- c("s1", "s2")

  dates       <- as.Date(index(common))
  r1          <- as.numeric(common[, "s1"])
  r2          <- as.numeric(common[, "s2"])
  r_spread    <- r1 - r2

  # ── Cumulative wealth (base 1.0) ─────────────────────────────────────────────
  cum_wealth <- function(r) cumprod(1 + r)

  w1      <- cum_wealth(r1)
  w2      <- cum_wealth(r2)
  w_spread <- cum_wealth(r_spread)

  # ── Running drawdown from peak: (wealth - cummax) / cummax ──────────────────
  running_dd <- function(w) (w - cummax(w)) / cummax(w)

  dd1     <- running_dd(w1)
  dd2     <- running_dd(w2)
  dd_spr  <- running_dd(w_spread)

  # ── Rolling correlation ───────────────────────────────────────────────────────
  roll_cor <- as.numeric(rollapply(
    merge(xts(r1, order.by = dates), xts(r2, order.by = dates)),
    width     = roll_cor_win,
    FUN       = function(m) cor(m[, 1], m[, 2], use = "complete.obs"),
    by.column = FALSE,
    align     = "right",
    fill      = NA
  ))

  full_cor  <- cor(r1, r2, use = "complete.obs")   # full-period scalar

  cor_df <- tibble(date = dates, roll_cor = roll_cor)

  # ── Tidy tibbles ─────────────────────────────────────────────────────────────
  wealth_df <- tibble(
    date   = dates,
    !!label1   := w1,
    !!label2   := w2
  ) %>%
    pivot_longer(-date, names_to = "series", values_to = "wealth") %>%
    mutate(series = factor(series, levels = c(label1, label2)))

  spread_df <- tibble(
    date   = dates,
    wealth = w_spread
  )

  dd_df <- tibble(
    date              = dates,
    !!label1          := dd1,
    !!label2          := dd2,
    `Spread (L/S)`    := dd_spr
  ) %>%
    pivot_longer(-date, names_to = "series", values_to = "dd") %>%
    mutate(series = factor(series, levels = c(label1, label2, "Spread (L/S)")))

  # ── Shared theme ──────────────────────────────────────────────────────────────
  base_theme <- theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(face = "bold", size = 12),
      plot.subtitle     = element_text(colour = "grey50", size = 8),
      axis.title.x      = element_blank(),
      legend.position   = "bottom",
      legend.key.height = unit(0.4, "cm")
    )

  pfx <- if (!is.null(title_prefix)) paste0(title_prefix, " — ") else ""

  # ── Panel 1: cumulative wealth of both series ─────────────────────────────────
  p1 <- ggplot(wealth_df, aes(x = date, y = wealth, colour = series)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50",
               linewidth = 0.4) +
    geom_line(linewidth = 0.8) +
    scale_colour_manual(values = setNames(c(col1, col2), c(label1, label2)),
                        name = NULL) +
    scale_y_continuous(labels = number_format(accuracy = 0.01)) +
    scale_x_date(expand = expansion(mult = 0.01)) +
    labs(
      title    = paste0(pfx, "Cumulative Wealth: ", label1, " vs ", label2),
      subtitle = "Base = 1.0 at first common date"
    ) +
    base_theme

  # ── Panel 2: cumulative wealth of spread (Long xts1 / Short xts2) ────────────
  # Pre-compute slices to avoid lazy-eval / namespace issues inside geom data=
  spread_above <- spread_df[spread_df$wealth >= 1, ]
  spread_below <- spread_df[spread_df$wealth <  1, ]

  p2 <- ggplot(spread_df, aes(x = date, y = wealth)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50",
               linewidth = 0.4) +
    geom_ribbon(
      data = spread_above,
      aes(ymin = 1, ymax = wealth),
      fill = col_spread, alpha = 0.25
    ) +
    geom_ribbon(
      data = spread_below,
      aes(ymin = wealth, ymax = 1),
      fill = col2, alpha = 0.18
    ) +
    geom_line(colour = col_spread, linewidth = 0.8) +
    scale_y_continuous(labels = number_format(accuracy = 0.01)) +
    scale_x_date(expand = expansion(mult = 0.01)) +
    labs(
      title    = paste0(pfx, "Spread Wealth: Long ", label1, " / Short ", label2),
      subtitle = "Orange = positive spread  |  Red = negative spread  |  Base = 1.0"
    ) +
    base_theme

  # ── Panel 3: drawdown ─────────────────────────────────────────────────────────
  dd_colours <- setNames(
    c(col1, col2, col_spread),
    c(label1, label2, "Spread (L/S)")
  )

  # Max drawdown labels for subtitle
  mdd <- function(d) min(d, na.rm = TRUE)
  mdd_label <- sprintf("Max DD — %s: %s  |  %s: %s  |  Spread: %s",
                        label1,        percent(mdd(dd1),    accuracy = 0.1),
                        label2,        percent(mdd(dd2),    accuracy = 0.1),
                        percent(mdd(dd_spr), accuracy = 0.1))

  dd_s1  <- dd_df[dd_df$series == label1,          ]
  dd_s2  <- dd_df[dd_df$series == label2,          ]
  dd_spr_df <- dd_df[dd_df$series == "Spread (L/S)", ]

  p3 <- ggplot(dd_df, aes(x = date, y = dd, colour = series)) +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
    geom_ribbon(
      data = dd_s1,
      aes(ymin = dd, ymax = 0), fill = col1, alpha = 0.10, colour = NA
    ) +
    geom_ribbon(
      data = dd_s2,
      aes(ymin = dd, ymax = 0), fill = col2, alpha = 0.10, colour = NA
    ) +
    geom_ribbon(
      data = dd_spr_df,
      aes(ymin = dd, ymax = 0), fill = col_spread, alpha = 0.12, colour = NA
    ) +
    geom_line(linewidth = 0.7, alpha = 0.85) +
    scale_colour_manual(values = dd_colours, name = NULL) +
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       limits = c(min(c(dd1, dd2, dd_spr), na.rm = TRUE) * 1.05, 0.02)) +
    scale_x_date(expand = expansion(mult = 0.01)) +
    labs(
      title    = paste0(pfx, "Running Drawdown from Peak"),
      subtitle = mdd_label
    ) +
    base_theme

  # ── Summary stats ─────────────────────────────────────────────────────────────
  ann_ret <- function(r) (prod(1 + r))^(252 / length(r)) - 1
  ann_vol <- function(r) sd(r, na.rm = TRUE) * sqrt(252)
  sharpe  <- function(r) ann_ret(r) / ann_vol(r)
  max_dd  <- function(w) min(running_dd(w), na.rm = TRUE)

  stats <- tibble(
    Series        = c(label1, label2, paste0(label1, " − ", label2)),
    `Ann Return`  = percent(c(ann_ret(r1),  ann_ret(r2),  ann_ret(r_spread)),  accuracy = 0.1),
    `Ann Vol`     = percent(c(ann_vol(r1),  ann_vol(r2),  ann_vol(r_spread)),  accuracy = 0.1),
    `Sharpe`      = round(c(sharpe(r1),    sharpe(r2),    sharpe(r_spread)),   2),
    `Max DD`      = percent(c(max_dd(w1),  max_dd(w2),   max_dd(w_spread)),    accuracy = 0.1),
    `Total Return`= percent(c(last(w1)-1,  last(w2)-1,   last(w_spread)-1),   accuracy = 0.1)
  )

  cat("\n── Summary Statistics ──────────────────────────────────────\n")
  print(stats)
  cat(sprintf("Common period : %s → %s  (%d days)\n",
              format(min(dates)), format(max(dates)), length(dates)))

  # ── Panel 4: return distribution boxplot ─────────────────────────────────────
  box_df <- tibble(
    !!label1          := r1,
    !!label2          := r2,
    `Spread (L/S)`    := r_spread
  ) %>%
    pivot_longer(everything(), names_to = "series", values_to = "ret") %>%
    mutate(series = factor(series, levels = c(label1, label2, "Spread (L/S)")))

  box_colours <- setNames(c(col1, col2, col_spread),
                           c(label1, label2, "Spread (L/S)"))

  # Annotation: vol and skew per series
  box_stats <- box_df %>%
    group_by(series) %>%
    summarise(
      vol  = sd(ret, na.rm = TRUE) * sqrt(252),
      skew = moments::skewness(ret, na.rm = TRUE),
      kurt = moments::kurtosis(ret, na.rm = TRUE) - 3,   # excess kurtosis
      .groups = "drop"
    )

  # Fallback if moments package not available
  if (!requireNamespace("moments", quietly = TRUE)) {
    box_stats <- box_df %>%
      group_by(series) %>%
      summarise(
        vol  = sd(ret, na.rm = TRUE) * sqrt(252),
        skew = NA_real_,
        kurt = NA_real_,
        .groups = "drop"
      )
  }

  ann_label <- box_stats %>%
    mutate(label = if (!any(is.na(skew)))
             sprintf("σ=%s\nskew=%.2f\nkurt=%.2f",
                     percent(vol, accuracy = 0.1), skew, kurt)
           else
             sprintf("σ=%s", percent(vol, accuracy = 0.1)))

  p4 <- ggplot(box_df, aes(x = series, y = ret, fill = series)) +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
    geom_boxplot(width = 0.55, outlier.size = 0.6, outlier.alpha = 0.4,
                 alpha = 0.75) +
    geom_text(data = ann_label,
              aes(x = series, y = max(box_df$ret, na.rm = TRUE) * 1.05,
                  label = label),
              inherit.aes = FALSE, size = 3, vjust = 1, lineheight = 0.9) +
    scale_fill_manual(values = box_colours, guide = "none") +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(
      title    = paste0(pfx, "Daily Return Distributions"),
      subtitle = "Box = IQR  |  Line = median  |  Whiskers = 1.5×IQR  |  Dots = outliers"
    ) +
    base_theme +
    theme(axis.title.y = element_blank())

  # ── Troika: three-view distribution (standalone, also returned separately) ────
  # p4a: violin + boxplot
  p4a <- ggplot(box_df, aes(x = series, y = ret, fill = series)) +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
    geom_violin(alpha = 0.25, colour = NA, trim = TRUE) +
    geom_boxplot(width = 0.20, outlier.size = 0.5, outlier.alpha = 0.3,
                 alpha = 0.85) +
    geom_text(data = ann_label,
              aes(x = series, y = max(box_df$ret, na.rm = TRUE) * 1.05,
                  label = label),
              inherit.aes = FALSE, size = 2.6, vjust = 1, lineheight = 0.85) +
    scale_fill_manual(values = box_colours, guide = "none") +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(title = "Violin + Box",
         subtitle = "Full density  |  Box = IQR  |  Dots = outliers") +
    base_theme + theme(axis.title.y = element_blank())

  # p4b: zoomed boxplot
  iqr_max   <- box_df %>%
    group_by(series) %>%
    summarise(iqr = IQR(ret, na.rm = TRUE), .groups = "drop") %>%
    pull(iqr) %>% max()

  p4b <- ggplot(box_df, aes(x = series, y = ret, fill = series)) +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
    geom_boxplot(width = 0.55, outlier.size = 0.5, outlier.alpha = 0.3,
                 alpha = 0.80) +
    coord_cartesian(ylim = c(-iqr_max * 2.5, iqr_max * 2.5)) +
    scale_fill_manual(values = box_colours, guide = "none") +
    scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
    labs(title = "Zoomed Axis",
         subtitle = "\u00b12.5\u00d7IQR view  |  outliers exist beyond frame") +
    base_theme + theme(axis.title.y = element_blank())

  # p4c: horizontal ridgeline density
  if (!requireNamespace("ggridges", quietly = TRUE))
    install.packages("ggridges", repos = "https://cloud.r-project.org")

  p4c <- ggplot(box_df, aes(x = ret, y = series, fill = series)) +
    geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.4) +
    ggridges::geom_density_ridges(
      alpha = 0.65, scale = 1.4,
      quantile_lines = TRUE, quantiles = c(0.25, 0.5, 0.75),
      colour = "white", linewidth = 0.4
    ) +
    scale_fill_manual(values = box_colours, guide = "none") +
    scale_x_continuous(labels = percent_format(accuracy = 0.1),
                       limits = c(-0.08, 0.08)) +
    labs(title = "Density Ridgeline",
         subtitle = "White lines = Q1 / median / Q3") +
    base_theme + theme(axis.title.y = element_blank(),
                       axis.title.x = element_blank())

  p_dist_troika <- (p4a | p4b | p4c) +
    plot_annotation(
      title    = paste0(pfx, "Daily Return Distributions — three views"),
      subtitle = paste0(
        "Violin+Box: density shape + stats  |  ",
        "Zoomed: IQR detail  |  ",
        "Ridgeline: Spread \u03c3 < either leg confirms diversification benefit"
      ),
      theme = theme(plot.title    = element_text(face = "bold", size = 12),
                    plot.subtitle = element_text(colour = "grey50", size = 8))
    )

  # ── Regime helpers (only when rt supplied) ───────────────────────────────────
  REGIME_COLORS <- c(Fall = "#D90429", Recovery = "#F77F00",
                     Consolidation = "#2D6A4F")

  has_regime <- !is.null(regime_tbl)

  if (has_regime) {
    shade_fall <- regime_tbl[regime_tbl$regime == "Fall", ]
    shade_rec  <- regime_tbl[regime_tbl$regime == "Recovery", ]

    # Join rolling corr to daily regime label
    regime_daily <- regime_tbl %>%
      rowwise() %>%
      mutate(date = list(seq(as.Date(xmin), as.Date(xmax), by = "day"))) %>%
      unnest(date) %>%
      dplyr::select(date, regime) %>%
      ungroup()

    cor_regime_df <- cor_df %>%
      left_join(regime_daily, by = "date") %>%
      mutate(regime = factor(regime,
                              levels = c("Fall", "Recovery", "Consolidation")))

    # Regime-conditional mean correlation (for annotation)
    regime_means <- cor_regime_df %>%
      group_by(regime) %>%
      summarise(mean_cor = mean(roll_cor, na.rm = TRUE), .groups = "drop")
  }

  # ── Panel 5: rolling correlation + optional regime shading ───────────────────
  p5 <- ggplot(cor_df[!is.na(cor_df$roll_cor), ], aes(x = date, y = roll_cor)) +

    geom_hline(yintercept = c(-0.5, 0, 0.5),
               linetype = "dashed", colour = "grey75", linewidth = 0.3) +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.5)

  # Regime shading goes behind everything else
  if (has_regime) {
    p5 <- p5 +
      geom_rect(data = shade_fall,
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                fill = "#D90429", alpha = 0.10, inherit.aes = FALSE) +
      geom_rect(data = shade_rec,
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                fill = "#F77F00", alpha = 0.08, inherit.aes = FALSE)
  }

  cor_clean   <- cor_df[!is.na(cor_df$roll_cor), ]
  cor_pos     <- cor_clean[cor_clean$roll_cor >= 0, ]
  cor_neg     <- cor_clean[cor_clean$roll_cor <  0, ]

  p5 <- p5 +
    geom_ribbon(
      data = cor_pos,
      aes(ymin = 0, ymax = roll_cor), fill = col1, alpha = 0.20
    ) +
    geom_ribbon(
      data = cor_neg,
      aes(ymin = roll_cor, ymax = 0), fill = col2, alpha = 0.20
    ) +
    geom_line(colour = "grey30", linewidth = 0.7) +
    geom_hline(yintercept = full_cor, linetype = "solid",
               colour = col_spread, linewidth = 0.8) +
    annotate("text",
             x = min(cor_df$date, na.rm = TRUE),
             y = full_cor + 0.05,
             label = sprintf("Full-period ρ = %.2f", full_cor),
             hjust = 0, size = 3, colour = col_spread) +
    scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.25),
                       labels = number_format(accuracy = 0.1)) +
    scale_x_date(expand = expansion(mult = 0.01)) +
    labs(
      title    = paste0(pfx, sprintf("%d-Day Rolling Correlation: %s vs %s",
                                     roll_cor_win, label1, label2)),
      subtitle = if (has_regime)
        "Red shading = SPY Fall  |  Orange = Recovery  |  Blue/Red ribbon = ρ sign  |  Orange line = full-period ρ"
      else
        "Blue ribbon = positive ρ  |  Red ribbon = negative ρ  |  Orange line = full-period ρ"
    ) +
    base_theme

  # ── Panel 6: regime-conditional correlation boxplot (only if rt supplied) ────
  p6 <- NULL

  if (has_regime) {
    p6 <- ggplot(cor_regime_df, aes(x = regime, y = roll_cor, fill = regime)) +
      geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.5) +
      geom_hline(yintercept = full_cor, linetype = "dashed",
                 colour = col_spread, linewidth = 0.7) +
      geom_violin(alpha = 0.30, colour = NA, trim = TRUE) +
      geom_boxplot(width = 0.25, outlier.size = 0.6, outlier.alpha = 0.4,
                   alpha = 0.80) +
      geom_text(data = regime_means,
                aes(x = regime, y = mean_cor,
                    label = sprintf("μ=%.2f", mean_cor)),
                vjust = -0.7, size = 3, fontface = "bold",
                inherit.aes = FALSE) +
      annotate("text",
               x = 0.55, y = full_cor + 0.04,
               label = sprintf("Full-period ρ=%.2f", full_cor),
               hjust = 0, size = 2.8, colour = col_spread) +
      scale_fill_manual(values = REGIME_COLORS, guide = "none") +
      scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.25),
                         labels = number_format(accuracy = 0.1)) +
      labs(
        title    = paste0(pfx, "Regime-Conditional Correlation: ",
                          label1, " vs ", label2),
        subtitle = paste0(
          "Does ρ shift across SPY regimes?  ",
          "ρ↓ in Fall = genuine hedge  |  ρ↑ in Fall = correlated selloff"
        ),
        x = NULL, y = sprintf("%d-day rolling ρ", roll_cor_win)
      ) +
      base_theme +
      theme(axis.title.x = element_blank())

    # Print regime-conditional means to console
    cat("\n── Regime-Conditional Correlation ──────────────────────────\n")
    print(regime_means)
  }

  # ── Rolling IR computation ────────────────────────────────────────────────────
  # IR = (rolling mean of spread × 252) / (rolling sd of spread × √252)
  #    = (rolling mean / rolling sd) × √252
  roll_ir <- {
    mu_r  <- as.numeric(rollapply(r_spread, roll_ir_win, mean,
                                   na.rm = TRUE, align = "right", fill = NA))
    sd_r  <- as.numeric(rollapply(r_spread, roll_ir_win, sd,
                                   na.rm = TRUE, align = "right", fill = NA))
    (mu_r / sd_r) * sqrt(252)
  }

  full_ir  <- mean(r_spread, na.rm = TRUE) /
              sd(r_spread,   na.rm = TRUE) * sqrt(252)

  ir_df <- tibble(date = dates, ir = roll_ir)

  # ── Panel 7: rolling IR ───────────────────────────────────────────────────────
  # Reference levels: 0 (neutral), ±0.5 (top-quartile threshold)
  p7 <- ggplot(ir_df[!is.na(ir_df$ir), ], aes(x = date, y = ir))

  if (has_regime) {
    p7 <- p7 +
      geom_rect(data = shade_fall,
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                fill = "#D90429", alpha = 0.10, inherit.aes = FALSE) +
      geom_rect(data = shade_rec,
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                fill = "#F77F00", alpha = 0.08, inherit.aes = FALSE)
  }

  p7 <- p7 +
    geom_hline(yintercept = 0,    colour = "grey40", linewidth = 0.5) +
    geom_hline(yintercept =  0.5, linetype = "dashed",
               colour = "#2D6A4F", linewidth = 0.5) +
    geom_hline(yintercept = -0.5, linetype = "dashed",
               colour = "#D90429", linewidth = 0.5) +
    annotate("text", x = min(ir_df$date, na.rm = TRUE),
             y =  0.53, label = "IR = +0.5 (top quartile)",
             hjust = 0, size = 2.8, colour = "#2D6A4F") +
    annotate("text", x = min(ir_df$date, na.rm = TRUE),
             y = -0.53, label = "IR = -0.5",
             hjust = 0, size = 2.8, colour = "#D90429") +

    # Ribbon: positive IR = green, negative = red
    geom_ribbon(
      data = ir_df[!is.na(ir_df$ir) & ir_df$ir >= 0, ],
      aes(ymin = 0, ymax = ir), fill = "#2D6A4F", alpha = 0.20
    ) +
    geom_ribbon(
      data = ir_df[!is.na(ir_df$ir) & ir_df$ir <  0, ],
      aes(ymin = ir, ymax = 0), fill = "#D90429", alpha = 0.20
    ) +
    geom_line(colour = "grey30", linewidth = 0.7) +

    # Full-period IR reference
    geom_hline(yintercept = full_ir, linetype = "solid",
               colour = col_spread, linewidth = 0.8) +
    annotate("text",
             x = min(ir_df$date, na.rm = TRUE),
             y = full_ir + 0.08,
             label = sprintf("Full-period IR = %.2f", full_ir),
             hjust = 0, size = 3, colour = col_spread) +

    scale_x_date(expand = expansion(mult = 0.01)) +
    labs(
      title    = paste0(pfx, sprintf("%d-Day Rolling IR: Long %s / Short %s",
                                     roll_ir_win, label1, label2)),
      subtitle = paste0(
        "IR = annualised E[spread] / σ[spread]  |  ",
        "Green = IR > 0 (spread earns)  |  Red = IR < 0  |  ",
        "Dashed = ±0.5 top-quartile threshold"
      )
    ) +
    base_theme

  # Print full-period IR to console
  cat(sprintf("\n── Rolling IR (window=%d days) ─────────────────────────────\n",
              roll_ir_win))
  cat(sprintf("Full-period IR : %.2f\n", full_ir))
  if (has_regime) {
    ir_by_regime <- ir_df %>%
      left_join(regime_daily, by = "date") %>%
      group_by(regime) %>%
      summarise(mean_ir = round(mean(ir, na.rm = TRUE), 2),
                pct_positive = round(mean(ir > 0, na.rm = TRUE) * 100, 1),
                .groups = "drop")
    cat("IR by SPY regime:\n")
    print(ir_by_regime)
  }

  # ── Panel 8: 250-day rolling return for both series + spread ─────────────────
  roll_ret1 <- as.numeric(rollapply(r1,       roll_ir_win,
                                    function(x) prod(1 + x) - 1,
                                    align = "right", fill = NA))
  roll_ret2 <- as.numeric(rollapply(r2,       roll_ir_win,
                                    function(x) prod(1 + x) - 1,
                                    align = "right", fill = NA))
  roll_ret_spr <- as.numeric(rollapply(r_spread, roll_ir_win,
                                       function(x) prod(1 + x) - 1,
                                       align = "right", fill = NA))

  rret_df <- tibble(
    date              = dates,
    !!label1          := roll_ret1,
    !!label2          := roll_ret2,
    `Spread (L/S)`    := roll_ret_spr
  ) %>%
    pivot_longer(-date, names_to = "series", values_to = "rret") %>%
    mutate(series = factor(series, levels = c(label1, label2, "Spread (L/S)"))) %>%
    filter(!is.na(rret))

  rret_colours <- setNames(c(col1, col2, col_spread),
                            c(label1, label2, "Spread (L/S)"))

  p8 <- ggplot(rret_df, aes(x = date, y = rret, colour = series))

  if (has_regime) {
    p8 <- p8 +
      geom_rect(data = shade_fall,
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                fill = "#D90429", alpha = 0.10, inherit.aes = FALSE) +
      geom_rect(data = shade_rec,
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                fill = "#F77F00", alpha = 0.08, inherit.aes = FALSE)
  }

  p8 <- p8 +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.5) +
    geom_line(linewidth = 0.7, alpha = 0.9) +
    scale_colour_manual(values = rret_colours, name = NULL) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_x_date(expand = expansion(mult = 0.01)) +
    labs(
      title    = paste0(pfx, sprintf("%d-Day Rolling Return: %s  |  %s  |  Spread",
                                     roll_ir_win, label1, label2)),
      subtitle = "Cumulative return over trailing 250 trading days  |  Above 0 = positive period return"
    ) +
    base_theme

  # ── Combined plot ─────────────────────────────────────────────────────────────
  stack <- if (has_regime) (p1 / p2 / p3 / p8 / p4 / p5 / p6 / p7)
           else             (p1 / p2 / p3 / p8 / p4 / p5 / p7)

  sub <- if (has_regime)
    "P1: wealth  |  P2: L/S spread  |  P3: DD  |  P8: 250d return  |  P4: dist  |  P5: rolling ρ  |  P6: regime ρ  |  P7: rolling IR"
  else
    "P1: wealth  |  P2: L/S spread  |  P3: DD  |  P8: 250d return  |  P4: dist  |  P5: rolling ρ  |  P7: rolling IR"

  combined <- stack +
    plot_annotation(
      title    = paste0(pfx, label1, " vs ", label2, " — Pair Analysis"),
      subtitle = sub,
      theme = theme(
        plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(colour = "grey50", size = 9)
      )
    )

  print(combined)
  invisible(list(p_both = p1, p_spread = p2, p_dd = p3, p_roll_ret = p8,
                 p_box = p4, p_dist_troika = p_dist_troika,
                 p_cor = p5, p_regime_cor = p6, p_ir = p7,
                 combined = combined,
                 full_cor = full_cor, full_ir = full_ir, stats = stats))
}
