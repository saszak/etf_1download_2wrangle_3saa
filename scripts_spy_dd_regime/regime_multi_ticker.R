################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : scripts_spy_dd_regime/regime_multi_ticker.R
# Purpose : Multi-Ticker SPY Regime Chart — ticker vector as input
#
# DEPENDS ON: scripts_spy_dd_regime/spy_dd_regime.R
#   (loads build_regime_table(), REGIME_PAL, etc.)
#
# FUNCTIONS
#   plot_regime_multi_overlay()   Main chart: SPY line + N stacked ticker rows
#   plot_regime_ticker_heatmap()  Heatmap: tickers × regime periods (returns)
#   plot_regime_ticker_summary()  Faceted bars: avg return per ticker per regime
#   run_multi_ticker_regime()     Wrapper — all three plots from one call
#
# KEY DESIGN CHOICES
#   - SPY regime periods (Fall/Recovery/Consolidation/Cruise) are the backbone
#   - Each ticker row shows that ticker's absolute OR relative return per period
#   - Relative = ticker_return - SPY_return (alpha per regime phase)
#   - Text contrast: white on dark bars, dark on light bars
#   - Thin periods (< 45 days): angled floating label instead of inside bar
#   - Adaptive bar height: shrinks gracefully with more tickers
################################################################################

library(tidyverse)
library(xts)
library(PerformanceAnalytics)
library(scales)
library(patchwork)
library(ggrepel)

# Source the regime engine if not already loaded
if (!exists("build_regime_table")) {
  source(here::here("scripts_spy_dd_regime/spy_dd_regime.R"))
}

# ==============================================================================
# HELPER: compute per-period return for one ticker
# ==============================================================================
.period_ret <- function(xts_col, xmin, xmax) {
  w <- xts_col[paste0(as.Date(xmin), "/", as.Date(xmax))]
  if (length(w) == 0) return(0)
  as.numeric(Return.cumulative(w))
}

# ==============================================================================
# 1. plot_regime_multi_overlay()
# ==============================================================================
# Produces the SPY Drawdown Regime Chart with all tickers stacked below.
#
# INPUTS
#   xts_ret       : multi-column xts of daily returns (must contain master + tickers)
#   tickers       : character vector of overlay tickers
#   master        : master ticker for regime definition (default "SPY")
#   t_fall        : Fall threshold (default 0.10)
#   t_cruise      : Cruise threshold (default 0.05)
#   relative      : if TRUE, show (ticker - master) return per period
#   label_size    : text size inside bars (default 2.4)
#   min_bar_days  : periods shorter than this get floating angled labels (default 45)
# ==============================================================================

plot_regime_multi_overlay <- function(xts_ret,
                                       tickers,
                                       master       = "SPY",
                                       t_fall       = 0.10,
                                       t_cruise     = 0.05,
                                       relative     = FALSE,
                                       label_size   = 2.4,
                                       min_bar_days = 45) {

  # ── Validate inputs ────────────────────────────────────────────────────────
  missing_tk <- setdiff(c(master, tickers), colnames(xts_ret))
  if (length(missing_tk) > 0)
    stop("Tickers not in xts_ret: ", paste(missing_tk, collapse = ", "))

  all_tkrs  <- c(master, tickers)
  master_r  <- xts_ret[, master]

  # ── Build regime periods from master ──────────────────────────────────────
  rt <- build_regime_table(master_r, t_fall, t_cruise)

  # ── Cumulative return line for master ─────────────────────────────────────
  cum_df <- tibble(
    date   = index(master_r),
    cumret = as.numeric(cumprod(1 + master_r) - 1)
  )

  # ── Compute per-ticker, per-period returns ─────────────────────────────────
  master_per_period <- rt$period_return   # pre-computed in build_regime_table

  bars_df <- map_dfr(all_tkrs, function(tk) {
    tk_r <- xts_ret[, tk]
    rt %>%
      rowwise() %>%
      mutate(
        ticker  = tk,
        raw_ret = .period_ret(tk_r, xmin, xmax),
        spy_ret = period_return,          # master return for that period
        disp_ret = if (relative && tk != master) raw_ret - spy_ret else raw_ret,
        label_str = if (relative && tk != master)
          sprintf("%s%.1f%%", ifelse(disp_ret >= 0, "+", ""), disp_ret * 100)
        else
          percent(disp_ret, accuracy = 1),
        txt_color = case_when(
          abs(disp_ret) > 0.06 ~ "white",
          TRUE                 ~ "grey15"
        )
      ) %>%
      ungroup()
  }) %>%
    mutate(ticker = factor(ticker, levels = rev(all_tkrs)))  # SPY on top row

  # ── Coordinate system ──────────────────────────────────────────────────────
  max_val     <- max(cum_df$cumret, na.rm = TRUE)
  min_val     <- min(cum_df$cumret, na.rm = TRUE)
  total_range <- max_val - min_val
  n_tkrs      <- length(all_tkrs)

  # Bar height shrinks with more tickers; floor at 0.03 * range
  bar_h <- max(total_range * 0.32 / n_tkrs, total_range * 0.03)
  gap   <- bar_h * 0.15

  # Map each ticker to a y-position (SPY = top row, others below)
  ticker_y <- tibble(
    ticker  = factor(all_tkrs, levels = rev(all_tkrs)),
    y_start = min_val - total_range * 0.12 -
              (seq_along(all_tkrs) - 1) * (bar_h + gap)
  )

  bars_df <- bars_df %>% left_join(ticker_y, by = "ticker")
  y_floor <- min(ticker_y$y_start) - total_range * 0.30

  # ── Year markers ──────────────────────────────────────────────────────────
  year_markers <- cum_df %>%
    mutate(year = format(date, "%Y")) %>%
    group_by(year) %>% slice(1) %>% ungroup()

  # # ── Current regime badge ──────────────────────────────────────────────────
  # current_regime <- as.character(tail(rt$regime, 1))
  # current_since  <- format(tail(rt$xmin, 1), "%d %b %Y")
  # current_label  <- sprintf("NOW: %s  (since %s)", current_regime, current_since)

  # ── Summary stats ─────────────────────────────────────────────────────────
  total_r <- percent(as.numeric(Return.cumulative(master_r)), accuracy = 0.1)
  ann_r   <- percent(as.numeric(Return.annualized(master_r)),  accuracy = 0.1)
  mdd_r   <- percent(as.numeric(maxDrawdown(master_r)),        accuracy = 0.1)
  sharpe  <- round(as.numeric(SharpeRatio.annualized(master_r, Rf = 0)), 2)

  # ── Plot ───────────────────────────────────────────────────────────────────
  p <- ggplot() +

    # Background shading for Consolidation periods
    geom_rect(
      data = rt %>% filter(regime == "Consolidation"),
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = as.character(regime)),
      alpha = 0.05, inherit.aes = FALSE
    ) +
    scale_fill_manual(
      values = c(Consolidation = "#2D6A4F"),
      guide  = "none"
    ) +

    # Year grid
    geom_vline(data = year_markers, aes(xintercept = date),
               color = "grey91", linewidth = 0.9) +

    # Fall start markers (dashed red)
    geom_vline(
      data = rt %>% filter(regime == "Fall"),
      aes(xintercept = xmin),
      color = REGIME_PAL["Fall"], linetype = "dashed",
      alpha = 0.4, linewidth = 0.5
    ) +
    # Recovery end markers (dotted orange)
    geom_vline(
      data = rt %>% filter(regime == "Recovery"),
      aes(xintercept = xmax),
      color = REGIME_PAL["Recovery"], linetype = "dotted",
      alpha = 0.4, linewidth = 0.5
    ) +

    # Waterline
    geom_hline(yintercept = 0, color = "black", linewidth = 0.9) +

    # Master cumulative line
    geom_line(data = cum_df, aes(x = date, y = cumret),
              color = "#1D3557", linewidth = 0.75) +

    # ── Stacked ticker bars ────────────────────────────────────────────────
    geom_rect(
      data = bars_df,
      aes(xmin = xmin, xmax = xmax,
          ymin = y_start, ymax = y_start + bar_h,
          fill = as.character(regime)),
      color = "white", linewidth = 0.25,
      show.legend = TRUE
    ) +
    scale_fill_manual(
      name   = "Regime",
      values = REGIME_PAL,
      breaks = names(REGIME_PAL),
      guide  = guide_legend(override.aes = list(size = 5))
    ) +

    # Labels inside bars (non-thin periods)
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

    # Angled floating labels for thin periods (master ticker only to avoid clutter)
    geom_text(
      data = bars_df %>% filter(is_thin, ticker == master),
      aes(x     = xmin + (xmax - xmin) / 2,
          y     = y_start + bar_h + total_range * 0.025,
          label = label_str,
          color = as.character(color)),
      angle = 45, hjust = 0, size = label_size * 0.8,
      fontface = "bold", show.legend = FALSE
    ) +

    # Ticker name labels (left margin)
    geom_text(
      data = ticker_y,
      aes(x     = min(cum_df$date),
          y     = y_start + bar_h / 2,
          label = as.character(ticker),
          color = if_else(as.character(ticker) == master, "#1D3557", "grey35")),
      hjust = 1.35, size = 3.6, fontface = "bold",
      show.legend = FALSE
    ) +

    # Year footer labels
    geom_text(
      data = year_markers,
      aes(x = date, y = y_floor + total_range * 0.08, label = year),
      size = 3.0, fontface = "bold", color = "grey45"
    ) +

    # Summary stats footer
    annotate("text",
             x = min(cum_df$date) + (max(cum_df$date) - min(cum_df$date)) / 2,
             y = y_floor + total_range * 0.02,
             label = sprintf(
               "%s  |  Ann: %s  |  MaxDD: %s  |  Sharpe: %s",
               master, ann_r, mdd_r, sharpe
             ),
             hjust = 0.5, size = 2.9, fontface = "bold", color = "grey40") +

    scale_x_date(expand = expansion(mult = c(0.10, 0.04))) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
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
      legend.text        = element_text(size = 9),
      plot.title         = element_text(face = "bold", size = 14),
      plot.subtitle      = element_text(color = "grey50", size = 9),
      plot.margin        = margin(t = 8, r = 15, b = 55, l = 75)
    ) +

    labs(
      title    = sprintf("%s Drawdown Regime Chart — %d Tickers", master, length(tickers)),
      subtitle = sprintf(
        "Fall ≥%.0f%%  |  Cruise <%.0f%%  |  Mode: %s  |  %s to %s",
        t_fall * 100, t_cruise * 100,
        if (relative) "Relative (ticker − SPY)" else "Absolute Return",
        format(min(cum_df$date), "%b %Y"),
        format(max(cum_df$date), "%b %Y")
      )
    )

  p
}


# ==============================================================================
# 2. plot_regime_ticker_heatmap()
# ==============================================================================
# Heatmap: rows = tickers, columns = regime periods (labelled by date + type)
# Fill = period return (or relative return if relative = TRUE)
# Useful for reading at-a-glance which tickers shine / suffer per regime period
# ==============================================================================

plot_regime_ticker_heatmap <- function(xts_ret,
                                        tickers,
                                        master   = "SPY",
                                        t_fall   = 0.10,
                                        t_cruise = 0.05,
                                        relative = FALSE) {

  missing_tk <- setdiff(c(master, tickers), colnames(xts_ret))
  if (length(missing_tk) > 0)
    stop("Tickers not in xts_ret: ", paste(missing_tk, collapse = ", "))

  master_r <- xts_ret[, master]
  rt       <- build_regime_table(master_r, t_fall, t_cruise)

  # Period label: "Fall\nOct18" style
  rt <- rt %>%
    mutate(period_label = paste0(regime, "\n", format(xmin, "%b%y")))

  all_tkrs <- c(master, tickers)

  heat_df <- map_dfr(all_tkrs, function(tk) {
    tk_r <- xts_ret[, tk]
    rt %>%
      rowwise() %>%
      mutate(
        ticker   = tk,
        raw_ret  = .period_ret(tk_r, xmin, xmax),
        spy_ret  = period_return,
        disp_ret = if (relative && tk != master) raw_ret - spy_ret else raw_ret
      ) %>%
      ungroup() %>%
      select(ticker, period_label, regime, disp_ret, xmin)
  }) %>%
    mutate(
      ticker       = factor(ticker, levels = rev(all_tkrs)),
      period_label = factor(period_label,
                            levels = unique(period_label[order(xmin)]))
    )

  # Color scale midpoint = 0 for relative, median for absolute
  mid_pt <- if (relative) 0 else median(heat_df$disp_ret, na.rm = TRUE)

  ggplot(heat_df, aes(x = period_label, y = ticker, fill = disp_ret)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(
      aes(label = if (relative)
            sprintf("%s%.1f%%", ifelse(disp_ret >= 0, "+", ""), disp_ret * 100)
          else
            percent(disp_ret, accuracy = 1),
          color = if_else(abs(disp_ret) > 0.08, "white", "grey20")),
      size = 2.5, fontface = "bold",
      show.legend = FALSE
    ) +
    # Regime type border — top strip colored by regime
    geom_tile(
      data = heat_df %>% distinct(period_label, regime, xmin) %>%
             mutate(ticker = all_tkrs[1], y_dummy = length(all_tkrs) + 0.5),
      aes(x = period_label, y = factor(all_tkrs[1], levels = rev(all_tkrs)),
          color = as.character(regime)),
      fill = NA, linewidth = 1.5,
      show.legend = FALSE
    ) +
    scale_fill_gradient2(
      low      = "#D90429", mid = "white", high = "#2D6A4F",
      midpoint = mid_pt,
      name     = if (relative) "Relative\nReturn" else "Period\nReturn",
      labels   = percent_format(accuracy = 1)
    ) +
    scale_color_manual(
      values = c(as.character(REGIME_PAL), "white" = "white", "grey20" = "grey20"),
      guide  = "none"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid    = element_blank(),
      axis.text.x   = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y   = element_text(face = "bold", size = 9),
      axis.title    = element_blank(),
      legend.position = "right"
    ) +
    labs(
      title    = sprintf("Ticker × Regime Period Heatmap  (master: %s)", master),
      subtitle = sprintf(
        "Mode: %s  |  Columns ordered chronologically  |  Border = regime type",
        if (relative) "Relative (ticker − SPY)" else "Absolute Return"
      )
    )
}


# ==============================================================================
# 3. plot_regime_ticker_summary()
# ==============================================================================
# Faceted bar chart: for each regime type, show avg return across all tickers.
# Sorted by avg return within each facet — immediately shows who leads / lags.
# ==============================================================================

plot_regime_ticker_summary <- function(xts_ret,
                                        tickers,
                                        master   = "SPY",
                                        t_fall   = 0.10,
                                        t_cruise = 0.05,
                                        relative = FALSE) {

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
        disp_ret = if (relative && tk != master) raw_ret - spy_ret else raw_ret
      ) %>%
      ungroup()
  })

  # Average per ticker per regime
  avg_df <- perf_df %>%
    group_by(ticker, regime) %>%
    summarise(
      avg_ret = mean(disp_ret, na.rm = TRUE),
      n       = n(),
      .groups = "drop"
    ) %>%
    mutate(
      label     = if (relative)
        sprintf("%s%.1f%%", ifelse(avg_ret >= 0, "+", ""), avg_ret * 100)
      else
        percent(avg_ret, accuracy = 0.1),
      bar_color = REGIME_PAL[as.character(regime)],
      is_master = ticker == master,
      txt_color = if_else(abs(avg_ret) > 0.05, "darkblue", "grey20")
    )

  # Within each regime facet, sort by avg_ret descending
  avg_df <- avg_df %>%
    group_by(regime) %>%
    mutate(ticker = reorder(ticker, avg_ret)) %>%
    ungroup()

  ggplot(avg_df, aes(x = ticker, y = avg_ret, fill = bar_color)) +
    geom_col(
      aes(alpha = if_else(is_master, 1, 0.85)),
      width = 0.75, show.legend = FALSE
    ) +
    geom_text(
      aes(label = label, color = txt_color,
          vjust = if_else(avg_ret >= 0, -0.35, 1.35)),
      size = 2.6, fontface = "bold", show.legend = FALSE
    ) +
    geom_hline(yintercept = 0, linewidth = 0.7, color = "grey30") +
    facet_wrap(~regime, scales = "free_y", nrow = 1) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_alpha_identity() +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    coord_flip() +
    theme_minimal(base_size = 10) +
    theme(
      strip.background  = element_rect(fill = "grey95", color = NA),
      strip.text        = element_text(face = "bold", size = 10),
      panel.grid.minor  = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.y       = element_text(face = "bold", size = 8),
      axis.title        = element_blank(),
      plot.title        = element_text(face = "bold", size = 13)
    ) +
    labs(
      title    = sprintf("Average Return per Regime Type  (master: %s)", master),
      subtitle = sprintf(
        "Mode: %s  |  Sorted by avg return within each regime  |  Bold outline = %s",
        if (relative) "Relative (ticker − SPY)" else "Absolute Return", master
      )
    )
}


# ==============================================================================
# 4. run_multi_ticker_regime()   Convenience wrapper
# ==============================================================================

run_multi_ticker_regime <- function(xts_ret,
                                     tickers,
                                     master       = "SPY",
                                     t_fall       = 0.10,
                                     t_cruise     = 0.05,
                                     relative     = FALSE,
                                     label_size   = 2.4) {

  cat(sprintf(
    "\n── Multi-Ticker Regime Analysis: %s + %d tickers | Fall ≥%.0f%% | Cruise <%.0f%% ──\n",
    master, length(tickers), t_fall * 100, t_cruise * 100
  ))
  cat(sprintf("   Tickers: %s\n", paste(tickers, collapse = ", ")))
  cat(sprintf("   Mode   : %s\n",
              if (relative) "Relative (ticker − SPY)" else "Absolute Return"))
  cat("─────────────────────────────────────────────────────────────────────────\n")

  print(plot_regime_multi_overlay(xts_ret, tickers, master,
                                   t_fall, t_cruise, relative, label_size))
  print(plot_regime_ticker_heatmap(xts_ret, tickers, master,
                                    t_fall, t_cruise, relative))
  print(plot_regime_ticker_summary(xts_ret, tickers, master,
                                    t_fall, t_cruise, relative))

  invisible(build_regime_table(xts_ret[, master], t_fall, t_cruise))
}


# ==============================================================================
# MAIN: Run with core 15-ETF universe
# ==============================================================================

if (!exists("xts_ret")) {
  xts_ret <- readRDS(here::here("02_data_processed/xts_ret_returns.rds"))
}

# ── Core 15 universe (from PCA eigenvector analysis) ─────────────────────────
core_universe <- c(
  "AGG", "IEF", "TLT", "HYG", "TIP",
  "IEFA", "FEZ", "VWO",
  "QQQ", "XLK", "XLF", "XLI", "XLE",
  "GLD", "VIXY"
)

core_universe <- c(
  "XLV", "IHI", "IBB"
)



# ── Absolute returns per regime ───────────────────────────────────────────────
run_multi_ticker_regime(
  xts_ret  = xts_ret,
  tickers  = core_universe,
  master   = "SPY",
  t_fall   = 0.10,
  t_cruise = 0.05,
  relative = FALSE
)

# ── Relative returns (alpha per regime) ───────────────────────────────────────
run_multi_ticker_regime(
  xts_ret  = xts_ret,
  tickers  = core_universe,
  master   = "SPY",
  t_fall   = 0.10,
  t_cruise = 0.05,
  relative = TRUE
)

################################################################################
