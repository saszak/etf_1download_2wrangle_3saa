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
      data = rt %>% filter(regime %in% c("Cruise", "Consolidation")),
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf,
          fill = as.character(regime)),
      alpha = 0.05, inherit.aes = FALSE
    ) +
    scale_fill_manual(
      values = c(Cruise = "#2D6A4F", Consolidation = "#457B9D",
                 Fall = "#D90429", Recovery = "#F77F00"),
      breaks = c("Fall", "Recovery", "Consolidation", "Cruise"),
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

    # Current regime badge
    annotate("label",
             x = max(cum_df$date), y = max_val,
             label = sprintf("NOW: %s  (since %s)", current_regime, current_since),
             hjust = 1, vjust = 1, size = 3.2, fontface = "bold",
             color = REGIME_PAL[current_regime],
             fill = "white", label.size = 0.5,
             label.padding = unit(0.3, "lines")) +

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
        disp_ret  = .disp_ret_mixed(tk, raw_ret, spy_ret, master),
        label_str = .label_mixed(tk, disp_ret, master)
      ) %>%
      ungroup() %>%
      select(ticker, period_label, regime, disp_ret, label_str, xmin)
  }) %>%
    mutate(
      ticker       = factor(ticker, levels = rev(all_tkrs)),
      period_label = factor(period_label,
                            levels = unique(period_label[order(xmin)])),
      is_master    = ticker == master,
      txt_color    = if_else(abs(disp_ret) > 0.07, "white", "grey20")
    )

  ggplot(heat_df, aes(x = period_label, y = ticker, fill = disp_ret)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = label_str, color = txt_color),
              size = 2.5, fontface = "bold", show.legend = FALSE) +
    # Thick border on SPY row to visually separate it
    geom_tile(
      data = heat_df %>% filter(is_master),
      aes(x = period_label, y = ticker),
      fill = NA, color = "#1D3557", linewidth = 1.2,
      show.legend = FALSE
    ) +
    scale_fill_gradient2(
      low      = "#D90429", mid = "white", high = "#2D6A4F",
      midpoint = 0,
      name     = "Return / Alpha",
      labels   = scales::percent_format(accuracy = 1)
    ) +
    scale_color_identity() +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid    = element_blank(),
      axis.text.x   = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y   = element_text(face = "bold", size = 9),
      axis.title    = element_blank(),
      legend.position = "right"
    ) +
    labs(
      title    = sprintf("Regime Heatmap — %s Absolute  |  Others Relative  (α vs %s)",
                         master, master),
      subtitle = "Navy border = SPY (absolute)  |  All other rows = alpha vs SPY  |  Columns = regime periods"
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
