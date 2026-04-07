################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : scripts_daa_saa_taa/09_overlay_backtest.R
# Purpose : Phase 2.2 — Backtest all three enhancer trigger types for a set of
#           tickers against the 6040Classic baseline.
#
# FRAMEWORK
#   Baseline: 6040Classic = 60% SPY + 40% IEF
#   Overlay at weight w:  (1 − w) × 6040Classic  +  w × TICKER
#
#   Type A — Unconditional
#     Blend held always. No switching signal.
#     Best for: tickers with persistent IR advantage (QQQ, SMH historically).
#     Risk: no Fall protection.
#
#   Type B — SPY Regime-conditional
#     Blend held in Consolidation + Recovery; pure baseline in Fall.
#     Trigger: build_regime_table() → regime label per day.
#     Risk: regime detection lag (~10% drop already absorbed before Fall fires).
#
#   Type C — 200DMA signal-conditional
#     Blend held when TICKER is above its own 200DMA; pure baseline otherwise.
#     Trigger: price > MA200 (computed internally from cumulative returns).
#     Risk: whipsaw cost in choppy markets.
#
# OUTPUTS
#   run_overlay_backtest()   — master wrapper; runs all three types for a
#                              vector of tickers and returns results + plots
#
#   plot_overlay_grid()      — wealth comparison chart per ticker
#                              4 lines: Baseline | Type A | Type B | Type C
#
#   plot_overlay_scoreboard()— heatmap table: rows = tickers,
#                              columns = AnnRet + MaxDD for each type
#                              colour-coded relative to baseline
#
# PARAMETERS (top of script)
#   W_EQ / W_FI    6040Classic weights (default 0.60 / 0.40)
#   OVERLAY_W      overlay weight for each ticker (default 0.10)
#   MA_WIN         moving average window for Type C signal (default 200)
################################################################################

library(tidyverse)
library(xts)
library(zoo)
library(patchwork)
library(scales)
library(here)

if (!exists("project_tree"))       source(here("project_tree.R"))
if (!exists("etf_metadata"))       source(here(project_tree$scripts$init))
if (!exists("xts_ret"))            source(here(project_tree$scripts$wrangle))
if (!exists("build_regime_table")) source(here("scripts_spy_dd_regime/spy_dd_regime.R"))
if (!exists("rt") || !is.data.frame(rt))
  rt <- build_regime_table(xts_ret[, "SPY"])
if (!exists(".build_baseline"))    source(here("scripts_daa_saa_taa/08_enhancer_screen.R"))

# ── Parameters ────────────────────────────────────────────────────────────────
W_EQ           <- 0.60
W_FI           <- 0.40
BASELINE_LABEL <- "6040Classic"
OVERLAY_W      <- 0.10   # overlay weight applied to each ticker
MA_WIN         <- 200L   # moving average window for Type C signal

# ── Palette ───────────────────────────────────────────────────────────────────
COL_BASELINE <- "#1d3461"   # navy
COL_A        <- "#7c3aed"   # purple
COL_B        <- "#f59e0b"   # amber
COL_C        <- "#16a34a"   # green

# ==============================================================================
# HELPERS (local — safe to call even if 08_ not sourced)
# ==============================================================================

.ob_ann     <- function(r) mean(as.numeric(r), na.rm = TRUE) * 252
.ob_vol     <- function(r) sd(as.numeric(r),   na.rm = TRUE) * sqrt(252)
.ob_ann_ret <- function(r) {
  r <- as.numeric(r[!is.na(r)])
  prod(1 + r)^(252 / length(r)) - 1
}
.ob_max_dd  <- function(r) {
  w <- cumprod(1 + as.numeric(r[!is.na(r)]))
  min((w - cummax(w)) / cummax(w))
}
.ob_ir <- function(r) {
  v <- .ob_vol(r)
  if (is.na(v) || v == 0) NA_real_ else .ob_ann(r) / v
}

# Align baseline and ticker; return merged 2-column xts or NULL.
# Using the merged xts directly for arithmetic avoids xts() constructor mismatches.
.ob_align <- function(baseline_xts, tk, xts_ret) {
  if (!(tk %in% colnames(xts_ret))) return(NULL)
  common <- merge(baseline_xts, xts_ret[, tk], join = "inner")
  if (nrow(common) < 252) return(NULL)
  common   # return full merged xts; callers use [,1] and [,2]
}

# ==============================================================================
# TYPE A — UNCONDITIONAL
# ==============================================================================
# Returns xts of daily overlay returns: always blended.
# Uses xts arithmetic directly — no xts() constructor, no as.numeric().

build_type_a_returns <- function(ticker, baseline_xts, xts_ret,
                                  w = OVERLAY_W) {
  common <- .ob_align(baseline_xts, ticker, xts_ret)
  if (is.null(common)) return(NULL)
  ret <- (1 - w) * common[, 1] + w * common[, 2]
  colnames(ret) <- sprintf("TypeA_%s", ticker)
  ret
}

# ==============================================================================
# TYPE B — SPY REGIME-CONDITIONAL
# ==============================================================================
# Returns xts of daily overlay returns:
#   in Fall      → pure baseline
#   otherwise    → blended
# Signal built as xts using index(common) — guaranteed same length.

build_type_b_returns <- function(ticker, baseline_xts, xts_ret, rt,
                                  w = OVERLAY_W) {
  common <- .ob_align(baseline_xts, ticker, xts_ret)
  if (is.null(common)) return(NULL)

  regime_lkup <- rt %>%
    rowwise() %>%
    mutate(date = list(seq(as.Date(xmin), as.Date(xmax), by = "day"))) %>%
    unnest(date) %>%
    select(date, regime) %>%
    ungroup() %>%
    distinct(date, .keep_all = TRUE)   # deduplicate boundary dates shared between episodes

  dates   <- as.Date(index(common))
  regimes <- tibble(date = dates) %>%
    left_join(regime_lkup, by = "date") %>%
    mutate(regime = replace_na(regime, "Consolidation"))

  # signal = 1 → blend, 0 → pure baseline (in Fall)
  signal <- xts(as.integer(regimes$regime != "Fall"), order.by = index(common))

  ret <- common[, 1] + signal * w * (common[, 2] - common[, 1])
  colnames(ret) <- sprintf("TypeB_%s", ticker)
  ret
}

# ==============================================================================
# TYPE C — 200DMA SIGNAL-CONDITIONAL
# ==============================================================================
# Returns xts of daily overlay returns:
#   ticker above 200DMA → blended
#   ticker below 200DMA → pure baseline
#
# 200DMA computed from cumulative log-price (sum of log returns).
# Signal built as xts using index(common) — guaranteed same length.

build_type_c_returns <- function(ticker, baseline_xts, xts_ret,
                                  w      = OVERLAY_W,
                                  ma_win = MA_WIN,
                                  trend_signals = NULL) {
  common <- .ob_align(baseline_xts, ticker, xts_ret)
  if (is.null(common)) return(NULL)

  # Determine signal: 1 = above MA (blend), 0 = below MA (baseline)
  if (!is.null(trend_signals) && ticker %in% trend_signals$symbol) {
    sig_tbl <- trend_signals %>%
      filter(symbol == ticker) %>%
      select(date, signal)
    signal_vals <- tibble(date = as.Date(index(common))) %>%
      left_join(sig_tbl, by = "date") %>%
      tidyr::fill(signal, .direction = "down") %>%
      mutate(signal = replace_na(signal, 1L)) %>%
      pull(signal)
  } else {
    # Cumulative log-price proxy for the ticker
    cum_px <- cumsum(as.numeric(coredata(common[, 2])))
    ma_vec <- zoo::rollapply(cum_px, width = ma_win, FUN = mean,
                              fill = NA, align = "right")
    signal_vals <- as.integer(is.na(ma_vec) | cum_px >= ma_vec)
  }

  signal <- xts(signal_vals, order.by = index(common))
  ret    <- common[, 1] + signal * w * (common[, 2] - common[, 1])
  colnames(ret) <- sprintf("TypeC_%s", ticker)
  ret
}

# ==============================================================================
# COMPUTE ALL THREE TYPES FOR A VECTOR OF TICKERS
# ==============================================================================

build_overlay_comparison <- function(
    tickers,
    xts_ret,
    rt,
    w_eq           = W_EQ,
    w_fi           = W_FI,
    baseline_label = BASELINE_LABEL,
    overlay_w      = OVERLAY_W,
    ma_win         = MA_WIN,
    trend_signals  = NULL
) {
  baseline_xts <- .build_baseline(xts_ret, w_eq, w_fi, baseline_label)

  message(sprintf("▶ Overlay backtest: %d tickers × 3 types at w=%.0f%%  [%s baseline]",
                  length(tickers), overlay_w * 100, baseline_label))

  results <- purrr::map(tickers, function(tk) {
    if (!(tk %in% colnames(xts_ret))) {
      message(sprintf("  ⚠ %s not in xts_ret — skipped", tk))
      return(NULL)
    }

    ra <- build_type_a_returns(tk, baseline_xts, xts_ret, overlay_w)
    rb <- build_type_b_returns(tk, baseline_xts, xts_ret, rt, overlay_w)
    rc <- build_type_c_returns(tk, baseline_xts, xts_ret, overlay_w, ma_win, trend_signals)

    # Align all to common dates (use Type A as reference — widest)
    if (is.null(ra)) return(NULL)
    combined <- merge(baseline_xts, ra, join = "inner")
    if (!is.null(rb)) combined <- merge(combined, rb, join = "inner")
    if (!is.null(rc)) combined <- merge(combined, rc, join = "inner")

    # Fix digit-prefix mangling on baseline column
    mangled <- paste0("X", baseline_label)
    if (mangled %in% colnames(combined) && !baseline_label %in% colnames(combined))
      colnames(combined)[colnames(combined) == mangled] <- baseline_label

    # Per-type statistics
    stats <- purrr::map_dfr(colnames(combined), function(col) {
      r <- as.numeric(combined[, col])
      tibble(
        series  = col,
        ann_ret = .ob_ann_ret(r),
        max_dd  = .ob_max_dd(r),
        ann_vol = .ob_vol(r),
        ir      = .ob_ir(r),
        n_days  = sum(!is.na(r))
      )
    }) %>%
      mutate(
        ticker = tk,
        type   = case_when(
          series == baseline_label            ~ "Baseline",
          startsWith(series, "TypeA")         ~ "A",
          startsWith(series, "TypeB")         ~ "B",
          startsWith(series, "TypeC")         ~ "C",
          TRUE                                ~ series
        )
      )

    list(ticker = tk, returns = combined, stats = stats)
  })

  names(results) <- tickers
  results <- Filter(Negate(is.null), results)

  message(sprintf("✅ Overlay comparison built for %d tickers.", length(results)))
  list(
    results        = results,
    baseline_xts   = baseline_xts,
    baseline_label = baseline_label,
    overlay_w      = overlay_w,
    params         = list(w_eq = w_eq, w_fi = w_fi, ma_win = ma_win)
  )
}

# ==============================================================================
# PLOT A — OVERLAY GRID
# One panel per ticker: 4 wealth curves (Baseline, A, B, C) + regime shading
# ==============================================================================

plot_overlay_grid <- function(
    comparison,          # output of build_overlay_comparison()
    rt,
    ncol         = 1L,
    fig_height   = NULL  # auto-sized if NULL
) {
  baseline_label <- comparison$baseline_label
  overlay_w      <- comparison$overlay_w

  regime_rect <- rt %>%
    filter(regime %in% c("Fall", "Recovery")) %>%
    mutate(
      fill = if_else(regime == "Fall", "#fca5a5", "#bbf7d0"),
      xmin = as.Date(xmin),
      xmax = as.Date(xmax)
    )

  colours <- c(
    "Baseline" = COL_BASELINE,
    "A"        = COL_A,
    "B"        = COL_B,
    "C"        = COL_C
  )
  lw_map <- c("Baseline" = 1.2, "A" = 0.8, "B" = 0.8, "C" = 0.8)

  panels <- purrr::map(comparison$results, function(res) {
    tk  <- res$ticker
    ret <- res$returns

    # Build long wealth tibble
    wdf <- as.data.frame(ret) %>%
      rownames_to_column("date") %>%
      mutate(date = as.Date(date)) %>%
      pivot_longer(-date, names_to = "series", values_to = "ret") %>%
      mutate(
        type = case_when(
          series == baseline_label      ~ "Baseline",
          startsWith(series, "TypeA")   ~ "A",
          startsWith(series, "TypeB")   ~ "B",
          startsWith(series, "TypeC")   ~ "C",
          TRUE                          ~ series
        )
      ) %>%
      group_by(type) %>%
      arrange(date) %>%
      mutate(wealth = cumprod(1 + ret)) %>%
      ungroup()

    # End-of-line labels
    stats <- res$stats
    last_pts <- wdf %>%
      group_by(type) %>%
      slice_max(date, n = 1) %>%
      left_join(stats %>% select(type, ann_ret, max_dd), by = "type") %>%
      mutate(lbl = sprintf("%s  %+.1f%% / DD%.1f%%",
                           type, ann_ret * 100, max_dd * 100))

    ggplot(wdf, aes(date, wealth, colour = type, linewidth = type)) +
      geom_rect(data        = regime_rect,
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
                inherit.aes = FALSE, alpha = 0.15) +
      scale_fill_identity() +
      geom_line() +
      geom_text(data   = last_pts,
                aes(label = lbl),
                hjust  = 0, nudge_x = 60,
                size   = 4.7, fontface = "bold", lineheight = 0.85) +
      scale_colour_manual(values = colours, guide = "none") +
      scale_linewidth_manual(values = lw_map, guide = "none") +
      scale_y_continuous(labels = dollar_format(prefix = "$"),
                         expand = expansion(mult = c(0.02, 0.05))) +
      scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
      coord_cartesian(clip = "off") +
      labs(
        title    = tk,
        subtitle = sprintf("%.0f%% overlay  |  Navy=Baseline  Purple=A  Amber=B  Green=C",
                           overlay_w * 100),
        x = NULL, y = NULL
      ) +
      theme_minimal(base_size = 17) +
      theme(
        plot.margin      = margin(4, 195, 4, 4),
        panel.grid.minor = element_blank(),
        plot.title       = element_text(face = "bold", size = 19, colour = "#1B3A6B"),
        plot.subtitle    = element_text(colour = "grey50", size = 14),
        axis.text        = element_text(colour = "#1B3A6B"),
        axis.text.y      = element_text(colour = "#1B3A6B", face = "bold")
      )
  })

  n_panels <- length(panels)
  nrow_val <- ceiling(n_panels / ncol)

  combined <- wrap_plots(panels, ncol = ncol) +
    plot_annotation(
      title    = sprintf("Phase 2.2 Overlay Backtest — %.0f%% overlay on %s",
                         overlay_w * 100, baseline_label),
      subtitle = sprintf(
        "Type A: unconditional  |  Type B: SPY regime-conditional (non-Fall)  |  Type C: 200DMA-conditional\n%s = baseline  |  Ann return + MaxDD shown at line end  |  Pink = Fall  |  Green = Recovery",
        baseline_label
      ),
      theme = theme(
        plot.background = element_rect(fill = "#f8fafc", colour = "#1d3461", linewidth = 1.2),
        plot.margin     = margin(10, 10, 6, 10),
        plot.title      = element_text(face = "bold", size = 13, colour = "#1d3461",
                                       margin = margin(b = 4)),
        plot.subtitle   = element_text(colour = "grey50", size = 9)
      )
    )

  combined
}

# ==============================================================================
# PLOT B — OVERLAY SCOREBOARD
# Heatmap table: rows = tickers, columns = AnnRet + MaxDD per type
# Colour-coded relative to baseline: green = improvement, red = deterioration
# ==============================================================================

plot_overlay_scoreboard <- function(
    comparison,
    baseline_label = comparison$baseline_label
) {
  overlay_w <- comparison$overlay_w

  # Collect all stats
  all_stats <- purrr::map_dfr(comparison$results, ~ .x$stats)

  bl_stats <- all_stats %>%
    filter(type == "Baseline") %>%
    select(ticker, bl_ret = ann_ret, bl_dd = max_dd)

  score_df <- all_stats %>%
    filter(type != "Baseline") %>%
    left_join(bl_stats, by = "ticker") %>%
    mutate(
      ret_delta = ann_ret - bl_ret,   # positive = better return vs baseline
      dd_delta  = max_dd  - bl_dd,    # negative = better (shallower) DD vs baseline
      type      = factor(type, levels = c("A", "B", "C"))
    )

  # Long format for heatmap
  heat_df <- score_df %>%
    select(ticker, type, ann_ret, max_dd, ret_delta, dd_delta) %>%
    pivot_longer(c(ann_ret, max_dd, ret_delta, dd_delta),
                 names_to = "metric", values_to = "value") %>%
    mutate(
      col_id = paste(type, metric, sep = "_"),
      label  = case_when(
        metric == "ann_ret"   ~ sprintf("%+.1f%%",  value * 100),
        metric == "max_dd"    ~ sprintf("%.1f%%",   value * 100),
        metric == "ret_delta" ~ sprintf("%+.1f pp", value * 100),
        metric == "dd_delta"  ~ sprintf("%+.1f pp", value * 100)
      ),
      # Fill: for ret_delta, green = positive; for dd_delta, green = negative (shallower DD)
      fill_val = case_when(
        metric == "ret_delta" ~  value,
        metric == "dd_delta"  ~ -value,
        TRUE                  ~ NA_real_
      )
    )

  # Split into absolute and delta panels
  abs_df <- heat_df %>% filter(metric %in% c("ann_ret", "max_dd")) %>%
    mutate(metric_label = if_else(metric == "ann_ret", "Ann Ret", "MaxDD"))

  delta_df <- heat_df %>% filter(metric %in% c("ret_delta", "dd_delta")) %>%
    mutate(
      metric_label = if_else(metric == "ret_delta", "ΔRet vs baseline", "ΔDD vs baseline"),
      col_id       = paste(type, metric_label, sep = " | ")
    )

  # Helper: one heatmap tile plot
  .tile_plot <- function(df, title_str) {
    ggplot(df, aes(x = interaction(type, metric_label, sep = " | "),
                   y = fct_rev(factor(ticker)))) +
      geom_tile(aes(fill = fill_val), colour = "white", linewidth = 0.5) +
      geom_text(aes(label = label),
                size = 3, fontface = "bold", colour = "#1B3A6B") +
      scale_fill_gradient2(
        low      = "#dc2626",
        mid      = "#f3f4f6",
        high     = "#16a34a",
        midpoint = 0,
        na.value = "#e5e7eb",
        guide    = "none"
      ) +
      scale_x_discrete(position = "top") +
      labs(title = title_str, x = NULL, y = NULL) +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.x      = element_text(face = "bold", colour = "#1B3A6B", size = 9,
                                         angle = 30, hjust = 0),
        axis.text.y      = element_text(face = "bold", colour = "#1B3A6B", size = 10),
        panel.grid       = element_blank(),
        plot.title       = element_text(face = "bold", size = 11, colour = "#1d3461")
      )
  }

  p_abs   <- .tile_plot(abs_df,   "Absolute performance")
  p_delta <- .tile_plot(delta_df, sprintf("vs %s baseline (pp delta)", baseline_label))

  combined <- (p_delta | p_abs) +
    plot_annotation(
      title    = sprintf("Phase 2.2 Scoreboard — %.0f%% overlay per ticker", overlay_w * 100),
      subtitle = sprintf(
        "A: unconditional  |  B: non-Fall only  |  C: 200DMA-conditional  |  Green = improvement vs %s",
        baseline_label
      ),
      theme = theme(
        plot.background = element_rect(fill = "#f8fafc", colour = "#1d3461", linewidth = 1.2),
        plot.margin     = margin(10, 10, 6, 10),
        plot.title      = element_text(face = "bold", size = 13, colour = "#1d3461",
                                       margin = margin(b = 4)),
        plot.subtitle   = element_text(colour = "grey50", size = 9)
      )
    )

  combined
}

# ==============================================================================
# OPTIMAL TYPE SELECTION — build_optimal_overlay()
# ==============================================================================
# For each ticker pick the trigger type that maximises the criterion:
#   "enhancer"   → highest IR (return per unit of risk vs baseline)
#   "stabilizer" → lowest MaxDD (deepest protection)
#   "ir"         → highest IR regardless of role
#   "ret"        → highest annualised return
#   "dd"         → lowest (least negative) MaxDD
#
# Returns:
#   $recommendations  tibble — ticker | best_type | reason | stats for all types
#   $optimal_returns  xts    — combined portfolio using each ticker's best type
#   $optimal_stats    tibble — AnnRet / MaxDD / IR of optimal vs baseline

build_optimal_overlay <- function(
    comparison,
    role      = "enhancer",   # "enhancer" | "stabilizer"
    criterion = NULL          # override: "ir" | "ret" | "dd" | NULL → inferred from role
) {
  crit <- criterion %||% if (tolower(role) == "stabilizer") "dd" else "ir"
  baseline_label <- comparison$baseline_label
  overlay_w      <- comparison$overlay_w

  all_stats <- purrr::map_dfr(comparison$results, ~ .x$stats)

  # Pick best type per ticker
  recommendations <- all_stats %>%
    filter(type != "Baseline") %>%
    group_by(ticker) %>%
    mutate(
      best = case_when(
        crit == "ir"  ~ ir  == max(ir,      na.rm = TRUE),
        crit == "ret" ~ ann_ret == max(ann_ret, na.rm = TRUE),
        crit == "dd"  ~ max_dd  == max(max_dd,  na.rm = TRUE),  # max_dd is negative; max = least negative
        TRUE          ~ ir  == max(ir,      na.rm = TRUE)
      )
    ) %>%
    filter(best) %>%
    slice(1) %>%   # break ties: keep first (A > B > C alphabetically)
    ungroup() %>%
    left_join(
      all_stats %>%
        filter(type == "Baseline") %>%
        select(ticker, bl_ret = ann_ret, bl_dd = max_dd, bl_ir = ir),
      by = "ticker"
    ) %>%
    mutate(
      ret_delta = ann_ret - bl_ret,
      dd_delta  = max_dd  - bl_dd,
      reason    = case_when(
        type == "A" ~ "Unconditional — persistent advantage across all regimes",
        type == "B" ~ "Regime-conditional — ALPHA in Recovery/Consolidation, avoids Fall",
        type == "C" ~ "200DMA-conditional — trend filter improves entry/exit timing"
      ),
      criterion_used = crit
    ) %>%
    arrange(desc(
      if (crit == "ir")  ir
      else if (crit == "ret") ann_ret
      else -max_dd
    ))

  # Build combined optimal-type returns (equal-weight across tickers)
  opt_ret_list <- purrr::map(comparison$results, function(res) {
    tk        <- res$ticker
    best_type <- recommendations %>% filter(ticker == tk) %>% pull(type)
    if (length(best_type) == 0) return(NULL)
    col_name  <- sprintf("Type%s_%s", best_type, tk)
    if (!(col_name %in% colnames(res$returns))) return(NULL)
    res$returns[, col_name]
  })
  opt_ret_list <- Filter(Negate(is.null), opt_ret_list)

  if (length(opt_ret_list) > 0) {
    # Align all to common dates; equal-weight combine
    bl_xts <- comparison$baseline_xts
    combined_ret <- Reduce(function(a, b) merge(a, b, join = "inner"), opt_ret_list)
    eq_w         <- 1 / ncol(combined_ret)
    opt_portfolio <- xts(
      rowSums(coredata(combined_ret)) * eq_w,
      order.by = index(combined_ret)
    )
    colnames(opt_portfolio) <- "Optimal"

    # Stats: baseline vs optimal
    bl_aligned <- merge(bl_xts, opt_portfolio, join = "inner")
    optimal_stats <- tibble(
      series  = c(baseline_label, "Optimal"),
      ann_ret = c(.ob_ann_ret(bl_aligned[, 1]), .ob_ann_ret(bl_aligned[, 2])),
      max_dd  = c(.ob_max_dd(bl_aligned[, 1]),  .ob_max_dd(bl_aligned[, 2])),
      ann_vol = c(.ob_vol(bl_aligned[, 1]),      .ob_vol(bl_aligned[, 2])),
      ir      = c(.ob_ir(bl_aligned[, 1]),       .ob_ir(bl_aligned[, 2]))
    )
  } else {
    opt_portfolio <- NULL
    optimal_stats <- NULL
  }

  list(
    recommendations = recommendations,
    optimal_returns = opt_portfolio,
    optimal_stats   = optimal_stats,
    criterion       = crit,
    role            = role
  )
}

# ==============================================================================
# MASTER WRAPPER — run_overlay_backtest()
# ==============================================================================

run_overlay_backtest <- function(
    tickers,
    xts_ret,
    rt,
    role           = "Enhancer",   # "Enhancer" | "Stabilizer" — drives title + criterion
    w_eq           = W_EQ,
    w_fi           = W_FI,
    baseline_label = BASELINE_LABEL,
    overlay_w      = OVERLAY_W,
    ma_win         = MA_WIN,
    trend_signals  = NULL,
    save_png       = FALSE,
    fig_width      = 16,
    fig_height_grid = NULL
) {
  comp <- build_overlay_comparison(
    tickers        = tickers,
    xts_ret        = xts_ret,
    rt             = rt,
    w_eq           = w_eq,
    w_fi           = w_fi,
    baseline_label = baseline_label,
    overlay_w      = overlay_w,
    ma_win         = ma_win,
    trend_signals  = trend_signals
  )

  optimal <- build_optimal_overlay(comp, role = role)
  p_grid  <- plot_overlay_grid(comp, rt)
  p_score <- plot_overlay_scoreboard(comp)

  if (isTRUE(save_png)) {
    slug    <- tolower(role)
    out_dir <- here("key_plots")
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

    n_tickers <- length(comp$results)
    auto_h    <- ceiling(n_tickers / 2) * 3.5 + 2
    fh        <- fig_height_grid %||% auto_h

    ggplot2::ggsave(file.path(out_dir, sprintf("%s_overlay_grid.png",        slug)),
                    plot = p_grid,  width = fig_width, height = fh,  dpi = 150)
    ggplot2::ggsave(file.path(out_dir, sprintf("%s_overlay_scoreboard.png",  slug)),
                    plot = p_score, width = fig_width,
                    height = max(4, n_tickers * 0.55 + 3), dpi = 150)
    message(sprintf("💾 Saved: key_plots/%s_overlay_grid.png + scoreboard.png", slug))
  }

  list(comparison = comp, optimal = optimal, p_grid = p_grid, p_scoreboard = p_score)
}

# ==============================================================================
# RUN WHEN SOURCED DIRECTLY
# ==============================================================================

if (!isTRUE(getOption("knitr.in.progress"))) {

  # Example: run on the enhancer candidates (if available) or a manual set
  demo_tickers <- if (exists("enhancer_candidates")) {
    head(enhancer_candidates$ticker, 8)
  } else {
    c("QQQ", "SMH", "XLK", "GLD", "TLT", "DJP", "PDBC", "USMV")
  }

  ob <- run_overlay_backtest(
    tickers   = demo_tickers,
    xts_ret   = xts_ret,
    rt        = rt,
    overlay_w = OVERLAY_W,
    save_png  = TRUE
  )

  print(ob$p_scoreboard)
  print(ob$p_grid)
}

message("✅ Phase 2.2 overlay backtest pipeline loaded.")
