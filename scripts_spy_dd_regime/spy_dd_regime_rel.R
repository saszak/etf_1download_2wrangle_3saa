################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : scripts_spy_dd_regime/spy_dd_regime_rel.R
# Purpose : SPY Regime Chart — Mixed Mode
#             SPY row    → Absolute return per regime period
#             All others → Relative return vs SPY  (ticker − SPY)
#
# DEPENDS ON: spy_dd_regime.R + regime_multi_ticker.R
#
# FUNCTIONS
#   plot_regime_rel_overlay()    Main chart (mixed absolute/relative mode)
#   plot_regime_rel_heatmap()    Heatmap  (SPY col absolute, others relative)
#   plot_regime_rel_summary()    Faceted avg alpha per regime, sorted
#   plot_abs_alpha_regime()      Grouped bars: avg abs return + avg alpha by regime
#   plot_fall_fingerprint()      Strip plot: per-episode alpha in Fall only
#   plot_regime_fingerprint()    Strip plot: per-episode alpha across full cycle (faceted)
#   plot_regime_fingerprint_box() Boxplot version — green/red by median alpha sign
#   get_regime_fingerprint()      1-row tibble: mean alpha per regime (ticker × bmk)
#   run_regime_rel_analysis()    Wrapper — all three plots
################################################################################

library(tidyverse)
library(xts)
library(PerformanceAnalytics)
library(scales)
library(patchwork)

if (!exists("build_regime_table"))
  source(here::here("scripts_spy_dd_regime/spy_dd_regime.R"))

if (!exists("plot_regime_multi_overlay"))
  source(here::here("scripts_spy_dd_regime/regime_multi_ticker.R"))

# ==============================================================================
# HELPER: mixed-mode per-period display return
#   SPY (master)  → raw absolute return
#   All others    → ticker return − SPY return  (relative alpha)
# ==============================================================================
.disp_ret_mixed <- function(tk, raw_ret, spy_ret, master) {
  if (tk == master) raw_ret else raw_ret - spy_ret
}

.label_mixed <- function(tk, disp, master) {
  if (tk == master)
    scales::percent(disp, accuracy = 1)
  else
    sprintf("%s%.1f%%", ifelse(disp >= 0, "+", ""), disp * 100)
}

# Classify the TYPE of out/underperformance for a non-master ticker
#
# ┌─────────┬──────────────────────────────┬─────────────────────────────────┐
# │  Type   │  Condition                   │  Meaning                        │
# ├─────────┼──────────────────────────────┼─────────────────────────────────┤
# │ HEDGE   │ α>0, abs>0, spy<0            │ Ticker rose while SPY fell      │
# │ STABLE  │ α>0, abs<0, spy<0            │ Both fell — ticker fell less    │
# │ ALPHA   │ α>0, spy>0                   │ Both rose — ticker rose more    │
# │ LAG     │ α<0, spy>0                   │ Underperformed rising SPY       │
# │ LOSS    │ α<0, spy<0                   │ Both fell — ticker fell worse   │
# └─────────┴──────────────────────────────┴─────────────────────────────────┘
#
# Cell display (non-SPY rows):
#   Large bold   → α (relative, fill basis)
#   Small plain  → abs return
#   Small italic → type tag
#
.outperf_type <- function(raw_ret, spy_ret, alpha) {
  dplyr::case_when(
    alpha > 0 & raw_ret > 0 & spy_ret < 0 ~ "HEDGE",   # rose while SPY fell
    alpha > 0 & raw_ret < 0 & spy_ret < 0 ~ "STABLE",  # both fell, ticker less
    alpha > 0                              ~ "ALPHA",   # both rose, ticker more
    alpha < 0 & spy_ret > 0               ~ "LAG",     # underperformed rising SPY
    TRUE                                   ~ "LOSS"     # both fell, ticker worse
  )
}

# ==============================================================================
# 1. plot_regime_rel_overlay()
# ==============================================================================
# Identical layout to plot_regime_multi_overlay() but:
#   - SPY bar row  : absolute return  (same label style as before)
#   - Overlay rows : relative return  (+/- prefix, green/red contrast)
#   - SPY row label colour: always white
#   - Overlay label colour: white when |alpha| > 5%, dark otherwise
#   - Overlay bars: SAME regime colour fill as SPY row — the fill shows the
#     regime phase, not the direction. Direction is communicated by the label.
# ==============================================================================

plot_regime_rel_overlay <- function(xts_ret,
                                     tickers,
                                     master      = "SPY",
                                     t_fall      = 0.10,
                                     t_cruise    = 0.05,
                                     label_size  = 2.4,
                                     min_bar_days = 45) {

  missing_tk <- setdiff(c(master, tickers), colnames(xts_ret))
  if (length(missing_tk) > 0)
    stop("Tickers not in xts_ret: ", paste(missing_tk, collapse = ", "))

  all_tkrs <- c(master, tickers)
  master_r <- xts_ret[, master]
  rt       <- build_regime_table(master_r, t_fall, t_cruise)

  cum_df <- tibble(
    date   = index(master_r),
    cumret = as.numeric(cumprod(1 + master_r) - 1)
  )

  # ── Per-ticker, per-period display values ─────────────────────────────────
  bars_df <- map_dfr(all_tkrs, function(tk) {
    tk_r <- xts_ret[, tk]
    rt %>%
      rowwise() %>%
      mutate(
        ticker    = tk,
        raw_ret   = .period_ret(tk_r, xmin, xmax),
        spy_ret   = period_return,
        disp_ret  = .disp_ret_mixed(tk, raw_ret, spy_ret, master),
        label_str = .label_mixed(tk, disp_ret, master),
        # SPY row: always white label; overlays: contrast on magnitude of alpha
        txt_color = case_when(
          tk == master             ~ "white",
          abs(disp_ret) >= 0.05   ~ "white",
          TRUE                     ~ "grey15"
        )
      ) %>%
      ungroup()
  }) %>%
    mutate(ticker = factor(ticker, levels = rev(all_tkrs)))

  # ── Coordinate system ──────────────────────────────────────────────────────
  max_val     <- max(cum_df$cumret, na.rm = TRUE)
  min_val     <- min(cum_df$cumret, na.rm = TRUE)
  total_range <- max_val - min_val
  n_tkrs      <- length(all_tkrs)

  bar_h <- max(total_range * 0.32 / n_tkrs, total_range * 0.03)
  gap   <- bar_h * 0.15

  ticker_y <- tibble(
    ticker  = factor(all_tkrs, levels = rev(all_tkrs)),
    y_start = min_val - total_range * 0.12 -
              (seq_along(all_tkrs) - 1) * (bar_h + gap)
  )

  bars_df <- bars_df %>% left_join(ticker_y, by = "ticker")
  y_floor <- min(ticker_y$y_start) - total_range * 0.30

  year_markers <- cum_df %>%
    mutate(year = format(date, "%Y")) %>%
    group_by(year) %>% slice(1) %>% ungroup()

  current_regime <- as.character(tail(rt$regime, 1))
  current_since  <- format(tail(rt$xmin, 1), "%d %b %Y")

  ann_r  <- scales::percent(as.numeric(Return.annualized(master_r)),  accuracy = 0.1)
  mdd_r  <- scales::percent(as.numeric(maxDrawdown(master_r)),        accuracy = 0.1)
  sharpe <- round(as.numeric(SharpeRatio.annualized(master_r, Rf = 0)), 2)

  # ── Build plot ─────────────────────────────────────────────────────────────
  ggplot() +

    # Regime background shading
    geom_rect(
      data = rt %>% filter(regime == "Consolidation"),
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf,
          fill = as.character(regime)),
      alpha = 0.05, inherit.aes = FALSE
    ) +
    scale_fill_manual(
      values = c(Consolidation = "#2D6A4F",
                 Fall = "#D90429", Recovery = "#F77F00"),
      breaks = c("Fall", "Recovery", "Consolidation"),
      name   = "Regime",
      guide  = guide_legend(override.aes = list(alpha = 0.9, size = 5))
    ) +

    # Year grid
    geom_vline(data = year_markers, aes(xintercept = date),
               color = "grey91", linewidth = 0.9) +

    # Phase transition markers
    geom_vline(data = rt %>% filter(regime == "Fall"),
               aes(xintercept = xmin),
               color = REGIME_PAL["Fall"], linetype = "dashed",
               alpha = 0.4, linewidth = 0.5) +
    geom_vline(data = rt %>% filter(regime == "Recovery"),
               aes(xintercept = xmax),
               color = REGIME_PAL["Recovery"], linetype = "dotted",
               alpha = 0.4, linewidth = 0.5) +

    geom_hline(yintercept = 0, color = "black", linewidth = 0.9) +

    # SPY cumulative line
    geom_line(data = cum_df, aes(x = date, y = cumret),
              color = "#1D3557", linewidth = 0.75) +

    # Stacked bars — fill driven by regime colour
    geom_rect(
      data = bars_df,
      aes(xmin = xmin, xmax = xmax,
          ymin = y_start, ymax = y_start + bar_h,
          fill = as.character(regime)),
      color = "white", linewidth = 0.25,
      show.legend = TRUE
    ) +

    # Labels inside bars
    geom_text(
      data = bars_df %>% filter(!is_thin),
      aes(x     = xmin + (xmax - xmin) / 2,
          y     = y_start + bar_h / 2,
          label = label_str,
          color = txt_color),
      size = label_size, fontface = "bold",
      show.legend = FALSE
    ) +
    scale_color_identity() +

    # Floating angled labels for thin periods (SPY only)
    geom_text(
      data = bars_df %>% filter(is_thin, ticker == master),
      aes(x     = xmin + (xmax - xmin) / 2,
          y     = y_start + bar_h + total_range * 0.025,
          label = label_str,
          color = as.character(color)),
      angle = 45, hjust = 0,
      size = label_size * 0.8, fontface = "bold",
      show.legend = FALSE
    ) +

    # Ticker name labels — SPY in navy, overlays in grey
    geom_text(
      data = ticker_y,
      aes(x     = min(cum_df$date),
          y     = y_start + bar_h / 2,
          label = as.character(ticker),
          color = if_else(as.character(ticker) == master, "#1D3557", "grey40")),
      hjust = 1.35, size = 3.6, fontface = "bold",
      show.legend = FALSE
    ) +

    # Mode legend annotation (top left)
    annotate("text",
             x = min(cum_df$date), y = max_val,
             label = paste0(master, " = Absolute  |  Others = Relative (α vs SPY)"),
             hjust = 0, vjust = 1, size = 2.9,
             fontface = "italic", color = "grey45") +

    # Year footer
    geom_text(
      data = year_markers,
      aes(x = date, y = y_floor + total_range * 0.08, label = year),
      size = 3.0, fontface = "bold", color = "grey45"
    ) +

    # Stats footer
    annotate("text",
             x = min(cum_df$date) + (max(cum_df$date) - min(cum_df$date)) / 2,
             y = y_floor + total_range * 0.02,
             label = sprintf("%s  |  Ann: %s  |  MaxDD: %s  |  Sharpe: %s",
                             master, ann_r, mdd_r, sharpe),
             hjust = 0.5, size = 2.9, fontface = "bold", color = "grey40") +

    scale_x_date(expand = expansion(mult = c(0.10, 0.04))) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      breaks = seq(-2, 20, by = 0.25),
      limits = c(y_floor, max_val * 1.10)
    ) +

    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(color = "grey93", linewidth = 0.5),
      axis.title         = element_blank(),
      axis.text.x        = element_blank(),
      legend.position    = "top",
      legend.title       = element_text(face = "bold", size = 10),
      plot.title         = element_text(face = "bold", size = 14),
      plot.subtitle      = element_text(color = "grey50", size = 9),
      plot.margin        = margin(t = 8, r = 15, b = 55, l = 75)
    ) +

    labs(
      title    = sprintf("%s Regime Chart — Absolute vs Relative Mode  (%d tickers)",
                         master, length(tickers)),
      subtitle = sprintf(
        "Fall ≥%.0f%%  |  Cruise <%.0f%%  |  %s: Absolute  |  All others: α vs %s  |  %s – %s",
        t_fall * 100, t_cruise * 100, master, master,
        format(min(cum_df$date), "%b %Y"), format(max(cum_df$date), "%b %Y")
      )
    )
}


# ==============================================================================
# 2. plot_regime_rel_heatmap()
# ==============================================================================
# Heatmap: SPY column uses absolute return colour scale
#          All other columns use diverging red/green centred at 0 (alpha)
# ==============================================================================

plot_regime_rel_heatmap <- function(xts_ret,
                                     tickers,
                                     master   = "SPY",
                                     t_fall   = 0.10,
                                     t_cruise = 0.05) {

  missing_tk <- setdiff(c(master, tickers), colnames(xts_ret))
  if (length(missing_tk) > 0)
    stop("Tickers not in xts_ret: ", paste(missing_tk, collapse = ", "))

  master_r <- xts_ret[, master]
  rt       <- build_regime_table(master_r, t_fall, t_cruise) %>%
    mutate(period_label = paste0(regime, "\n", format(xmin, "%b%y")))

  all_tkrs <- c(master, tickers)

  heat_df <- map_dfr(all_tkrs, function(tk) {
    tk_r <- xts_ret[, tk]
    rt %>%
      rowwise() %>%
      mutate(
        ticker    = tk,
        raw_ret   = .period_ret(tk_r, xmin, xmax),
        spy_ret   = period_return,
        alpha     = raw_ret - spy_ret,
        # fill ALWAYS based on alpha (SPY alpha = 0 → neutral)
        disp_ret  = alpha,
        lbl_alpha = if_else(
          tk == master,
          sprintf("%.1f%%", raw_ret * 100),          # SPY: show abs at large size
          sprintf("%s%.1f%%", if_else(alpha >= 0, "+", ""), alpha * 100)
        ),
        lbl_abs   = if_else(
          tk == master, "",                           # SPY: nothing at small size
          sprintf("%.1f%%", raw_ret * 100)
        ),
        lbl_type  = if_else(
          tk == master, "",
          .outperf_type(raw_ret, spy_ret, alpha)
        )
      ) %>%
      ungroup() %>%
      select(ticker, period_label, regime, disp_ret,
             lbl_alpha, lbl_abs, lbl_type, xmin)
  }) %>%
    mutate(
      ticker       = factor(ticker, levels = rev(all_tkrs)),
      period_label = factor(period_label,
                            levels = unique(period_label[order(xmin)])),
      is_master    = ticker == master,
      txt_color    = if_else(disp_ret >= 0, "#1B3A6B", "#7B1010")
    )

  fill_limit <- max(abs(heat_df$disp_ret[heat_df$ticker != master]),
                    na.rm = TRUE)

  ggplot(heat_df, aes(x = period_label, y = ticker, fill = disp_ret)) +
    geom_tile(color = "white", linewidth = 0.8) +

    # Alpha label — primary, larger, upper portion of tile
    geom_text(aes(label = lbl_alpha, color = txt_color),
              size = 3.4, fontface = "bold", vjust = -0.6,
              show.legend = FALSE) +

    # Abs label — secondary, smaller, middle
    geom_text(aes(label = lbl_abs, color = txt_color),
              size = 2.5, fontface = "plain", vjust = 0.8,
              show.legend = FALSE) +

    # Type tag — tertiary, smallest, italic, bottom of tile
    geom_text(aes(label = lbl_type, color = txt_color),
              size = 2.0, fontface = "italic", vjust = 2.5,
              show.legend = FALSE) +

    # Thick border on SPY row
    geom_tile(
      data = heat_df %>% filter(is_master),
      aes(x = period_label, y = ticker),
      fill = NA, color = "#1D3557", linewidth = 1.5,
      show.legend = FALSE
    ) +
    scale_fill_gradient2(
      low      = "#F4CCCC",
      mid      = "#F0F0F0",
      high     = "#C8E6C9",
      midpoint = 0,
      limits   = c(-fill_limit, fill_limit),
      name     = "α vs SPY\n(fill)",
      labels   = scales::percent_format(accuracy = 1)
    ) +
    scale_color_identity() +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid       = element_blank(),
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 9,
                                      face = "bold", color = "grey25"),
      axis.text.y      = element_text(face = "bold", size = 11, color = "grey15"),
      axis.title       = element_blank(),
      legend.position  = "right",
      legend.title     = element_text(size = 9, face = "bold"),
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(color = "grey45", size = 9,
                                      margin = margin(b = 6))
    ) +
    labs(
      title    = sprintf("Regime Heatmap — α vs %s  |  Abs return & Type for reference", master),
      subtitle = sprintf(
        "Fill = α vs %s  (blue +, red −)  |  Large = α  |  Small = abs  |  Italic = type  |  HEDGE: ticker↑ SPY↓  |  STABLE: both↓ ticker less  |  ALPHA: both↑ ticker more  |  LAG: underperformed rising SPY  |  LOSS: both↓ ticker worse  |  Fall ≥%.0f%%",
        master, t_fall * 100
      )
    )
}


# ==============================================================================
# 3. plot_regime_rel_summary()
# ==============================================================================
# Faceted by regime type. SPY bar = absolute avg return.
# All other bars = avg alpha vs SPY. Sorted within each facet.
# SPY bar visually distinguished with a navy border.
# ==============================================================================

plot_regime_rel_summary <- function(xts_ret,
                                     tickers,
                                     master   = "SPY",
                                     t_fall   = 0.10,
                                     t_cruise = 0.05) {

  missing_tk <- setdiff(c(master, tickers), colnames(xts_ret))
  if (length(missing_tk) > 0)
    stop("Tickers not in xts_ret: ", paste(missing_tk, collapse = ", "))

  master_r <- xts_ret[, master]
  rt       <- build_regime_table(master_r, t_fall, t_cruise)
  all_tkrs <- c(master, tickers)

  perf_df <- map_dfr(all_tkrs, function(tk) {
    tk_r <- xts_ret[, tk]
    rt %>%
      rowwise() %>%
      mutate(
        ticker   = tk,
        raw_ret  = .period_ret(tk_r, xmin, xmax),
        spy_ret  = period_return,
        disp_ret = .disp_ret_mixed(tk, raw_ret, spy_ret, master)
      ) %>%
      ungroup()
  })

  avg_df <- perf_df %>%
    group_by(ticker, regime) %>%
    summarise(avg_ret = mean(disp_ret, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(
      label_str  = if_else(
        ticker == master,
        scales::percent(avg_ret, accuracy = 0.1),
        sprintf("%s%.1f%%", ifelse(avg_ret >= 0, "+", ""), avg_ret * 100)
      ),
      bar_color  = REGIME_PAL[as.character(regime)],
      is_master  = ticker == master,
      txt_color  = if_else(abs(avg_ret) > 0.05, "white", "grey20"),
      bar_border = if_else(is_master, "#1D3557", "white")
    ) %>%
    group_by(regime) %>%
    mutate(ticker = reorder(ticker, avg_ret)) %>%
    ungroup()

  ggplot(avg_df, aes(x = ticker, y = avg_ret, fill = bar_color)) +
    geom_col(aes(color = bar_border),
             width = 0.75, linewidth = 0.6,
             show.legend = FALSE) +
    geom_text(
      aes(label = label_str, color = txt_color,
          vjust = if_else(avg_ret >= 0, -0.35, 1.35)),
      size = 2.7, fontface = "bold", show.legend = FALSE
    ) +
    geom_hline(yintercept = 0, linewidth = 0.7, color = "grey30") +
    facet_wrap(~regime, scales = "free_y", nrow = 1) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    coord_flip() +
    theme_minimal(base_size = 10) +
    theme(
      strip.background   = element_rect(fill = "grey95", color = NA),
      strip.text         = element_text(face = "bold", size = 10),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.y        = element_text(face = "bold", size = 9),
      axis.title         = element_blank(),
      plot.title         = element_text(face = "bold", size = 13)
    ) +
    labs(
      title    = sprintf("Avg Return per Regime  —  %s: Absolute  |  Others: α vs %s",
                         master, master),
      subtitle = "Navy border = SPY (absolute)  |  All others sorted by avg alpha  |  + prefix = outperformed SPY"
    )
}


# ==============================================================================
# 4. run_regime_rel_analysis()   Wrapper
# ==============================================================================

run_regime_rel_analysis <- function(xts_ret,
                                     tickers,
                                     master     = "SPY",
                                     t_fall     = 0.10,
                                     t_cruise   = 0.05,
                                     label_size = 2.4) {

  cat(sprintf(
    "\n── Regime Relative Analysis: %s Absolute + %d Relative Tickers | Fall ≥%.0f%% ──\n",
    master, length(tickers), t_fall * 100
  ))
  cat(sprintf("   Tickers : %s\n", paste(tickers, collapse = ", ")))
  cat(sprintf("   %s row  : Absolute return per regime period\n", master))
  cat(sprintf("   Others  : Alpha vs %s per regime period\n", master))
  cat("─────────────────────────────────────────────────────────────────────────\n")

  print(plot_regime_rel_overlay(xts_ret, tickers, master, t_fall, t_cruise,
                                 label_size = label_size))
  print(plot_regime_rel_heatmap(xts_ret, tickers, master, t_fall, t_cruise))
  print(plot_regime_rel_summary(xts_ret, tickers, master, t_fall, t_cruise))

  invisible(build_regime_table(xts_ret[, master], t_fall, t_cruise))
}

# ==============================================================================
# plot_abs_alpha_regime()
# PURPOSE : Grouped vertical bar chart — Avg Abs Return vs Avg Alpha vs master
#           per regime type (Fall / Recovery / Consolidation).
#           master bar = SPY absolute; all others = alpha vs master.
# ARGS
#   xts_ret  : xts of daily returns (all tickers must be columns)
#   tickers  : character vector — tickers to display (master excluded; added auto)
#   rt       : regime table from build_regime_table() — must be pre-built
#   master   : benchmark ticker (default "SPY")
#   title    : optional plot title override
# ==============================================================================
plot_abs_alpha_regime <- function(xts_ret,
                                  tickers,
                                  rt,
                                  master  = "SPY",
                                  title   = NULL) {

  tickers      <- tickers[tickers != master]
  tickers      <- tickers[tickers %in% colnames(xts_ret)]
  all_tickers  <- c(master, tickers)

  df <- map_dfr(all_tickers, function(tk) {
    tk_r <- xts_ret[, tk]
    rt %>% rowwise() %>%
      mutate(
        ticker  = tk,
        abs_ret = {
          w <- tk_r[paste0(xmin, "/", xmax)]
          if (length(w) == 0) NA_real_
          else as.numeric(Return.cumulative(w))
        },
        spy_abs = period_return,
        alpha   = abs_ret - spy_abs
      ) %>%
      ungroup() %>%
      select(ticker, regime, abs_ret, alpha)
  }) %>%
    mutate(regime = as.character(regime)) %>%
    group_by(ticker, regime) %>%
    summarise(
      avg_abs   = mean(abs_ret, na.rm = TRUE),
      avg_alpha = mean(alpha,   na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    pivot_longer(c(avg_abs, avg_alpha),
                 names_to  = "metric",
                 values_to = "value") %>%
    mutate(
      metric  = if_else(metric == "avg_abs", "Abs Return", paste0("\u03b1 vs ", master)),
      regime  = factor(regime, levels = c("Fall", "Recovery", "Consolidation")),
      ticker  = factor(ticker, levels = all_tickers),
      bar_col = case_when(
        metric != "Abs Return" & value >= 0 ~ "#1B3A6B",
        metric != "Abs Return" & value <  0 ~ "#7B1010",
        metric == "Abs Return" & value >= 0 ~ "#5B8DB8",
        TRUE                               ~ "#C97070"
      )
    )

  plot_title <- title %||% paste0(
    "Abs Return vs \u03b1 vs ", master, " \u2014 by Regime Type"
  )

  ggplot(df, aes(x = ticker, y = value, fill = bar_col)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.65,
             show.legend = FALSE) +
    geom_hline(yintercept = 0, linewidth = 0.6, color = "grey40") +
    geom_text(aes(
      label = sprintf("%s%.1f%%", if_else(value >= 0, "+", ""), value * 100),
      vjust = if_else(value >= 0, -0.35, 1.25),
      color = bar_col),
      position = position_dodge(width = 0.7),
      size = 2.8, fontface = "bold", show.legend = FALSE) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    facet_wrap(~regime, nrow = 1) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text         = element_text(face = "bold", size = 11),
      axis.title         = element_blank(),
      axis.text.x        = element_text(face = "bold", size = 9,
                                        angle = 30, hjust = 1),
      plot.title         = element_text(face = "bold", size = 13),
      plot.subtitle      = element_text(color = "grey45", size = 9)
    ) +
    labs(
      title    = plot_title,
      subtitle = paste0("Dark = \u03b1 vs ", master,
                        "  |  Light = Abs return  |  n = ",
                        length(tickers), " tickers + ", master)
    )
}

# ==============================================================================
# .build_episode_alpha()  [internal helper]
# Computes per-episode alpha vs master for each ticker × regime combination
# ==============================================================================
.build_episode_alpha <- function(xts_ret, tickers, rt, master = "SPY") {
  tickers <- tickers[tickers != master]
  tickers <- tickers[tickers %in% colnames(xts_ret)]

  map_dfr(tickers, function(tk) {
    tk_r  <- xts_ret[, tk]
    spy_r <- xts_ret[, master]
    rt %>%
      mutate(episode = row_number()) %>%
      rowwise() %>%
      mutate(
        ticker  = tk,
        abs_ret = {
          w <- tk_r[paste0(xmin, "/", xmax)]
          if (length(w) == 0) NA_real_ else as.numeric(Return.cumulative(w))
        },
        spy_ret = {
          w <- spy_r[paste0(xmin, "/", xmax)]
          if (length(w) == 0) NA_real_ else as.numeric(Return.cumulative(w))
        },
        alpha = abs_ret - spy_ret
      ) %>%
      ungroup() %>%
      select(ticker, regime, episode, xmin, abs_ret, spy_ret, alpha)
  })
}

# ==============================================================================
# plot_fall_fingerprint()
# PURPOSE : Strip plot — per-episode alpha in Fall regime only.
#           Tickers sorted by mean Fall alpha (best hedges at top).
#           Dot = one episode | Diamond = mean | ±1 SD bar | zero reference.
# ==============================================================================
plot_fall_fingerprint <- function(xts_ret,
                                  tickers,
                                  rt,
                                  master = "SPY",
                                  title  = NULL) {

  ep <- .build_episode_alpha(xts_ret, tickers, rt, master) %>%
    filter(regime == "Fall", !is.na(alpha))

  # ticker order: best mean hedge (most negative alpha) at top
  ticker_order <- ep %>%
    group_by(ticker) %>%
    summarise(mean_alpha = mean(alpha), .groups = "drop") %>%
    arrange(mean_alpha) %>%
    pull(ticker)

  stats <- ep %>%
    group_by(ticker) %>%
    summarise(
      mean_a = mean(alpha),
      sd_a   = sd(alpha),
      n      = n(),
      .groups = "drop"
    ) %>%
    mutate(
      lo = mean_a - sd_a,
      hi = mean_a + sd_a
    )

  ep <- ep %>% mutate(ticker = factor(ticker, levels = ticker_order))
  stats <- stats %>% mutate(ticker = factor(ticker, levels = ticker_order))

  plot_title <- title %||% paste0("Fall Fingerprint — \u03b1 vs ", master,
                                   " per Episode")

  ggplot() +
    # ±1 SD bar
    geom_segment(data = stats,
                 aes(x = lo, xend = hi, y = ticker, yend = ticker),
                 color = "#94a3b8", linewidth = 1.2, alpha = 0.5) +
    # individual episodes
    geom_jitter(data = ep,
                aes(x = alpha, y = ticker,
                    color = if_else(alpha >= 0, "#22c55e", "#ef4444")),
                height = 0.18, size = 2.2, alpha = 0.85) +
    # mean diamond
    geom_point(data = stats,
               aes(x = mean_a, y = ticker,
                   color = if_else(mean_a >= 0, "#16a34a", "#dc2626")),
               shape = 18, size = 4.5) +
    # zero reference
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "grey50", linewidth = 0.6) +
    scale_color_identity() +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.y = element_line(color = "#e5e7eb", linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      axis.title.y       = element_blank(),
      axis.text.y        = element_text(face = "bold", size = 9),
      plot.title         = element_text(face = "bold", size = 13),
      plot.subtitle      = element_text(color = "grey45", size = 9)
    ) +
    labs(
      title    = plot_title,
      subtitle = paste0("Dot = one Fall episode  |  \u25c6 = mean  |  bar = \u00b11 SD  |  ",
                        length(unique(ep$episode)), " Fall episodes  |  ",
                        "sorted by mean \u03b1"),
      x = paste0("\u03b1 vs ", master)
    )
}

# ==============================================================================
# plot_regime_fingerprint()
# PURPOSE : Full-cycle fingerprint — same strip layout faceted across
#           Fall | Recovery | Consolidation.
#           Ticker order fixed by Fall mean alpha (worst hedge at bottom).
# ==============================================================================
plot_regime_fingerprint <- function(xts_ret,
                                    tickers,
                                    rt,
                                    master = "SPY",
                                    title  = NULL) {

  ep <- .build_episode_alpha(xts_ret, tickers, rt, master) %>%
    filter(!is.na(alpha))

  # ticker order driven by Fall alpha
  ticker_order <- ep %>%
    filter(regime == "Fall") %>%
    group_by(ticker) %>%
    summarise(mean_alpha = mean(alpha), .groups = "drop") %>%
    arrange(mean_alpha) %>%
    pull(ticker)

  # include any ticker missing from Fall (append at bottom)
  ticker_order <- c(ticker_order,
                    setdiff(unique(ep$ticker), ticker_order))

  stats <- ep %>%
    group_by(ticker, regime) %>%
    summarise(
      mean_a = mean(alpha),
      sd_a   = sd(alpha),
      n      = n(),
      .groups = "drop"
    ) %>%
    mutate(lo = mean_a - sd_a, hi = mean_a + sd_a)

  ep    <- ep    %>% mutate(ticker = factor(ticker, levels = ticker_order),
                             regime = factor(regime, levels = c("Fall","Recovery","Consolidation")))
  stats <- stats %>% mutate(ticker = factor(ticker, levels = ticker_order),
                             regime = factor(regime, levels = c("Fall","Recovery","Consolidation")))

  regime_colors <- c(Fall = "#fca5a5", Recovery = "#bbf7d0", Consolidation = "#bfdbfe")

  plot_title <- title %||% paste0("Regime Fingerprint — \u03b1 vs ", master,
                                   " across Full Cycle")

  ggplot() +
    geom_rect(data = data.frame(regime = factor(c("Fall","Recovery","Consolidation"),
                                                levels = c("Fall","Recovery","Consolidation")),
                                col = unname(regime_colors)),
              aes(fill = col), xmin = -Inf, xmax = Inf,
              ymin = -Inf, ymax = Inf, alpha = 0.08) +
    geom_segment(data = stats,
                 aes(x = lo, xend = hi, y = ticker, yend = ticker),
                 color = "#94a3b8", linewidth = 1.0, alpha = 0.5) +
    geom_jitter(data = ep,
                aes(x = alpha, y = ticker,
                    color = if_else(alpha >= 0, "#22c55e", "#ef4444")),
                height = 0.18, size = 1.8, alpha = 0.80) +
    geom_point(data = stats,
               aes(x = mean_a, y = ticker,
                   color = if_else(mean_a >= 0, "#16a34a", "#dc2626")),
               shape = 18, size = 4.0) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "grey50", linewidth = 0.5) +
    scale_color_identity() +
    scale_fill_identity() +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    facet_wrap(~regime, nrow = 1, scales = "free_x") +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_line(color = "#e5e7eb", linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      axis.title.y       = element_blank(),
      axis.text.y        = element_text(face = "bold", size = 8),
      strip.text         = element_text(face = "bold", size = 11),
      plot.title         = element_text(face = "bold", size = 13),
      plot.subtitle      = element_text(color = "grey45", size = 9)
    ) +
    labs(
      title    = plot_title,
      subtitle = paste0("Dot = one episode  |  \u25c6 = mean  |  bar = \u00b11 SD  |  ",
                        "ticker order = Fall \u03b1 rank"),
      x = paste0("\u03b1 vs ", master)
    )
}

# ==============================================================================
# plot_regime_fingerprint_box()
# PURPOSE : Boxplot version of plot_regime_fingerprint().
#           Fill = green when median alpha >= 0, red otherwise.
#           Ticker order fixed by Fall mean alpha (best hedges at top).
# ==============================================================================
plot_regime_fingerprint_box <- function(xts_ret,
                                        tickers,
                                        rt,
                                        master = "SPY",
                                        title  = NULL) {

  ep <- .build_episode_alpha(xts_ret, tickers, rt, master) %>%
    filter(!is.na(alpha)) %>%
    mutate(regime = factor(regime, levels = c("Fall", "Recovery", "Consolidation")))

  ticker_order <- ep %>%
    filter(regime == "Fall") %>%
    group_by(ticker) %>%
    summarise(m = mean(alpha), .groups = "drop") %>%
    arrange(m) %>%
    pull(ticker)

  ticker_order <- c(ticker_order, setdiff(unique(ep$ticker), ticker_order))

  ep <- ep %>% mutate(ticker = factor(ticker, levels = ticker_order))

  medians <- ep %>%
    group_by(ticker, regime) %>%
    summarise(med = median(alpha), .groups = "drop") %>%
    mutate(fill_col = if_else(med >= 0, "#bbf7d0", "#fca5a5"))

  ep <- ep %>%
    left_join(medians %>% select(ticker, regime, fill_col),
              by = c("ticker", "regime"))

  plot_title <- title %||% paste0(
    "Regime Fingerprint (Boxplot) \u2014 \u03b1 vs ", master, " across Full Cycle"
  )

  ggplot(ep, aes(x = alpha, y = ticker, fill = fill_col)) +
    geom_boxplot(outlier.size = 1.5, outlier.alpha = 0.6,
                 linewidth = 0.4, alpha = 0.8, width = 0.6) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "grey50", linewidth = 0.6) +
    scale_fill_identity() +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    facet_wrap(~regime, nrow = 1, scales = "free_x") +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_line(color = "#e5e7eb", linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      axis.title.y       = element_blank(),
      axis.text.y        = element_text(face = "bold", size = 9),
      strip.text         = element_text(face = "bold", size = 11),
      legend.position    = "none",
      plot.title         = element_text(face = "bold", size = 13),
      plot.subtitle      = element_text(color = "grey45", size = 9)
    ) +
    labs(
      title    = plot_title,
      subtitle = paste0("Green = median \u03b1 \u2265 0  |  Red = median \u03b1 < 0  |  ",
                        "Box = IQR  |  ticker order = Fall \u03b1 rank"),
      x = paste0("\u03b1 vs ", master)
    )
}

# ==============================================================================
# get_regime_fingerprint()
# PURPOSE : Returns a 1-row tibble with mean alpha vs bmk per regime type.
#           Bind rows across tickers for a universe-wide fingerprint table.
# ARGS
#   xts_ret : xts of daily returns
#   ticker  : single ticker string
#   rt      : regime table from build_regime_table()
#   bmk     : benchmark ticker (default "SPY")
# RETURNS
#   tibble: ticker | Fall | Recovery | Consolidation
# ==============================================================================
get_regime_fingerprint <- function(xts_ret, ticker, rt = NULL, bmk = "SPY") {
  if (is.null(rt)) rt <- suppressWarnings(build_regime_table(xts_ret[, bmk]))
  ep <- .build_episode_alpha(xts_ret, tickers = ticker, rt = rt, master = bmk)

  ep %>%
    filter(!is.na(alpha)) %>%
    group_by(regime) %>%
    summarise(mean_alpha = mean(alpha), .groups = "drop") %>%
    pivot_wider(names_from = regime, values_from = mean_alpha) %>%
    mutate(ticker = ticker, bmk = bmk, .before = 1)
}

# ==============================================================================
# MAIN
# ==============================================================================

if (!exists("xts_ret"))
  xts_ret <- readRDS(here::here("02_data_processed/xts_ret_returns.rds"))

core_universe <- c(
  "IEF", "HYG",
  "IEFA",
  "XLK", "XLF", "XLI",
  "GLD"
)

run_regime_rel_analysis(
  xts_ret    = xts_ret,
  tickers    = core_universe,
  master     = "SPY",
  t_fall     = 0.10,
  t_cruise   = 0.05,
  label_size = 2.4
)

################################################################################
