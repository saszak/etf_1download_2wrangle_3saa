################################################################################
# scripts_saa_taa/taa_portfolio_backtest.R
#
# Purpose : Walk-forward TAA portfolio backtest — two strategies:
#
#   Type A — Pure TAA momentum
#             Top-N tickers (SAA+TAA + TAA Only quadrants), equal-weight,
#             monthly rebalance.  No permanent holdings.
#
#   Type B — SAA base (saa_frac) + TAA satellite (1 - saa_frac)
#             SAA sleeve at fixed weights, monthly rebalanced.
#             TAA sleeve rotates into current top-N momentum winners.
#             Weights additive — if a ticker appears in both sleeves its
#             allocations are summed.
#
# FUNCTIONS
#   build_taa_weights(xts_ret, meta, as_of_date, n_top, score_col, ...)
#     → named weight vector for one rebalance date, or NULL if no candidates
#
#   backtest_taa_pure(xts_ret, meta, n_top, score_col, rebal_freq, burn_in)
#     → list: returns (xts), bop_weight (xts), weight_hist (xts), rebal_log (tibble)
#
#   backtest_taa_overlay(xts_ret, meta, saa_weights, saa_frac, n_top, ...)
#     → same structure as backtest_taa_pure
#
#   compare_taa_strategies(xts_ret, meta, saa_weights, rt, master, ...)
#     → aligned returns list + rebal logs + regime labels
#
#   plot_taa_backtest(results, title)
#     → patchwork: wealth (log) / drawdown / regime-conditional alpha
#
#   run_taa_backtest(xts_ret, meta, rt, ...)
#     → orchestrator: builds A+B, prints table, plots, returns results invisibly
#
# DEPENDS ON
#   scripts_saa_taa/taa_momentum_screen.R   — build_taa_screen()
#   scripts_saa_taa/05_saa_portfolio.R      — calc_saa_portfolio()
#   scripts_spy_dd_regime/spy_dd_regime.R   — build_regime_table(),
#                                             label_daily_regime()
################################################################################

library(tidyverse)
library(xts)
library(PerformanceAnalytics)
library(patchwork)
library(scales)
library(here)

# Source dependency scripts without triggering their auto-runs.
# Setting knitr.in.progress = TRUE suppresses the
#   if (!isTRUE(getOption("knitr.in.progress"))) { ... }
# guard used in every project script's auto-run block.
.src_quiet <- function(path) {
  old <- getOption("knitr.in.progress")
  options(knitr.in.progress = TRUE)
  on.exit(options(knitr.in.progress = old))
  source(path)
}

if (!exists("build_taa_screen"))
  .src_quiet(here("scripts_saa_taa/taa_momentum_screen.R"))

if (!exists("build_regime_table"))
  .src_quiet(here("scripts_spy_dd_regime/spy_dd_regime.R"))

if (!exists("calc_saa_portfolio"))
  .src_quiet(here("scripts_saa_taa/05_saa_portfolio.R"))

# ── Constants ──────────────────────────────────────────────────────────────────
TAA_BT_BURN_IN   <- 504L   # trading days required before first signal (~2Y)
TAA_BT_N_TOP     <- 5L     # default tickers per TAA sleeve
TAA_BT_SAA_FRAC  <- 0.70   # SAA fraction in Type B
TAA_BT_QUADS     <- c("SAA + TAA", "TAA Only")

# SAA weights matching SAA_PORTFOLIO in scripts_saa_taa/saa_tree.R
SAA_WEIGHTS_DEFAULT <- c(
  AGG  = 0.35,
  URTH = 0.25,
  SPY  = 0.25,
  QQQ  = 0.05,
  XLK  = 0.05,
  SMH  = 0.05
)

# ── Colour palette ─────────────────────────────────────────────────────────────
.STRAT_COLS <- c(
  "TAA Pure"    = "#E65100",
  "TAA Overlay" = "#2E7D32",
  "SAA"         = "#1565C0",
  "SPY"         = "#888888"
)

.REGIME_PAL <- c(
  Fall          = "#C0392B",
  Recovery      = "#27AE60",
  Consolidation = "#2980B9"
)

# ── Shared theme ───────────────────────────────────────────────────────────────
.bt_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor  = element_blank(),
    plot.title        = element_text(face = "bold", size = 12),
    plot.subtitle     = element_text(colour = "grey50", size = 8),
    legend.position   = "bottom",
    legend.key.height = unit(0.4, "cm")
  )


################################################################################
# build_taa_weights()
################################################################################

#' Compute TAA weights for one rebalance date — no look-ahead.
#'
#' Runs build_taa_screen() on data up to and including as_of_date, filters to
#' eligible quadrants, picks top-n_top by score_col, and returns equal weights.
#'
#' @param xts_ret     xts   — full daily returns matrix
#' @param meta        data.frame — etf_metadata
#' @param as_of_date  Date  — inclusive upper bound for data slice
#' @param n_top       int   — number of tickers to hold
#' @param score_col   chr   — column to rank on ("rel_6m", "rel_12m", "rel_2y")
#' @param quadrants   chr   — eligible quadrant labels
#' @param master      chr   — benchmark ticker (excluded from selection)
#' @param ...         passed to build_taa_screen()
#' @return named numeric vector summing to 1, or NULL if no valid candidates
build_taa_weights <- function(xts_ret,
                               meta,
                               as_of_date,
                               n_top     = TAA_BT_N_TOP,
                               score_col = "rel_6m",
                               quadrants = TAA_BT_QUADS,
                               master    = "SPY",
                               ...) {

  ret_slice <- xts_ret[paste0("/", as.character(as_of_date))]
  if (nrow(ret_slice) < TAA_BT_BURN_IN) return(NULL)

  scr <- tryCatch(
    build_taa_screen(ret_slice, meta, master = master, ...),
    error = function(e) NULL
  )
  if (is.null(scr) || nrow(scr) == 0L) return(NULL)

  top_tickers <- scr %>%
    filter(quadrant %in% quadrants,
           !is.na(.data[[score_col]])) %>%
    slice_max(order_by = .data[[score_col]], n = n_top, with_ties = FALSE) %>%
    pull(ticker)

  if (length(top_tickers) == 0L) return(NULL)

  w        <- rep(1 / length(top_tickers), length(top_tickers))
  names(w) <- top_tickers
  w
}


################################################################################
# backtest_taa_pure()   — Type A
################################################################################

#' Walk-forward backtest: pure TAA momentum (Type A).
#'
#' At each monthly rebalance date: select top-n_top equal-weight tickers from
#' the TAA eligible quadrants.  No permanent SAA base.
#'
#' @param xts_ret    xts   — daily returns, full universe
#' @param meta       data.frame — etf_metadata
#' @param n_top      int   — tickers per rebalance period
#' @param score_col  chr   — ranking column
#' @param rebal_freq chr   — "months" or "quarters"
#' @param burn_in    int   — trading-day warm-up before first signal
#' @param master     chr   — benchmark ticker (excluded from selection)
#' @param ...        passed to build_taa_weights()
#' @return list: returns, bop_weight, weight_hist, rebal_log
backtest_taa_pure <- function(xts_ret,
                               meta,
                               n_top      = TAA_BT_N_TOP,
                               score_col  = "rel_6m",
                               rebal_freq = "months",
                               burn_in    = TAA_BT_BURN_IN,
                               master     = "SPY",
                               ...) {

  all_dates <- index(xts_ret)
  if (length(all_dates) <= burn_in)
    stop(sprintf("Need > %d trading days, have %d.", burn_in, length(all_dates)))

  start_date  <- all_dates[burn_in + 1L]
  ret_post    <- xts_ret[paste0(start_date, "/")]
  ep_idx      <- endpoints(ret_post, on = rebal_freq)
  rebal_dates <- index(ret_post)[ep_idx[ep_idx > 0]]

  cat(sprintf("  [A] %d rebalance dates  (%s → %s)\n",
              length(rebal_dates),
              format(min(rebal_dates), "%Y-%m-%d"),
              format(max(rebal_dates), "%Y-%m-%d")))

  universe   <- setdiff(colnames(xts_ret), master)
  n_univ     <- length(universe)

  # ── Single-pass: build weights + log simultaneously ──────────────────────────
  passes <- purrr::map(rebal_dates, function(d) {
    w <- build_taa_weights(xts_ret, meta, as_of_date = d,
                           n_top = n_top, score_col = score_col,
                           master = master, ...)
    if (is.null(w)) {
      list(
        wrow = setNames(rep(NA_real_, n_univ), universe),
        log  = tibble(date = d, tickers = NA_character_, n_selected = 0L),
        valid = FALSE
      )
    } else {
      w_full              <- setNames(rep(0, n_univ), universe)
      w_full[names(w)]    <- w
      list(
        wrow  = w_full,
        log   = tibble(date = d,
                       tickers    = paste(names(w), collapse = ","),
                       n_selected = length(w)),
        valid = TRUE
      )
    }
  })

  valid_idx   <- which(sapply(passes, `[[`, "valid"))
  if (length(valid_idx) == 0L)
    stop("No valid rebalance periods — check burn-in and universe coverage.")

  rebal_log   <- bind_rows(lapply(passes, `[[`, "log"))
  wt_matrix   <- do.call(rbind, lapply(passes[valid_idx], `[[`, "wrow"))
  valid_dates <- rebal_dates[valid_idx]
  weight_xts  <- xts(wt_matrix, order.by = valid_dates)

  ret_slice <- xts_ret[paste0(valid_dates[1], "/"), universe]

  port <- Return.portfolio(R = ret_slice, weights = weight_xts, verbose = TRUE)
  colnames(port$returns) <- "TAA_Pure"

  list(
    returns     = port$returns,
    bop_weight  = port$BOP.Weight,
    weight_hist = weight_xts,
    rebal_log   = rebal_log
  )
}


################################################################################
# backtest_taa_overlay()   — Type B
################################################################################

#' Walk-forward backtest: SAA base + TAA satellite (Type B).
#'
#' SAA sleeve: saa_frac of portfolio at SAA weights, rebalanced monthly.
#' TAA sleeve: (1 - saa_frac) of portfolio, rotates into top-n_top momentum
#'             winners.  If no TAA candidates are found, the TAA fraction is
#'             pro-rated back into the SAA weights (fallback to pure SAA).
#' Ticker weights from both sleeves are additive.
#'
#' @param xts_ret     xts   — daily returns
#' @param meta        data.frame — etf_metadata
#' @param saa_weights named numeric — SAA weights summing to 1
#' @param saa_frac    numeric — fraction allocated to SAA sleeve
#' @param n_top       int   — tickers in TAA satellite
#' @param score_col   chr   — ranking column
#' @param rebal_freq  chr   — rebalance frequency
#' @param burn_in     int   — warm-up days
#' @param master      chr   — benchmark ticker (excluded from TAA selection)
#' @param ...         passed to build_taa_weights()
#' @return list: returns, bop_weight, weight_hist, rebal_log
backtest_taa_overlay <- function(xts_ret,
                                  meta,
                                  saa_weights = SAA_WEIGHTS_DEFAULT,
                                  saa_frac    = TAA_BT_SAA_FRAC,
                                  n_top       = TAA_BT_N_TOP,
                                  score_col   = "rel_6m",
                                  rebal_freq  = "months",
                                  burn_in     = TAA_BT_BURN_IN,
                                  master      = "SPY",
                                  ...) {

  stopifnot(abs(sum(saa_weights) - 1) < 0.001)
  stopifnot(saa_frac > 0 && saa_frac < 1)

  missing_saa <- setdiff(names(saa_weights), colnames(xts_ret))
  if (length(missing_saa) > 0) {
    warning("SAA tickers not in xts_ret — dropped and weights renormalised: ",
            paste(missing_saa, collapse = ", "))
    saa_weights <- saa_weights[!names(saa_weights) %in% missing_saa]
    saa_weights <- saa_weights / sum(saa_weights)
  }

  taa_frac  <- 1 - saa_frac
  all_dates <- index(xts_ret)
  if (length(all_dates) <= burn_in)
    stop(sprintf("Need > %d trading days, have %d.", burn_in, length(all_dates)))

  start_date  <- all_dates[burn_in + 1L]
  ret_post    <- xts_ret[paste0(start_date, "/")]
  ep_idx      <- endpoints(ret_post, on = rebal_freq)
  rebal_dates <- index(ret_post)[ep_idx[ep_idx > 0]]

  cat(sprintf("  [B] %d rebalance dates  SAA %.0f%% / TAA %.0f%%\n",
              length(rebal_dates), saa_frac * 100, taa_frac * 100))

  universe <- setdiff(colnames(xts_ret), master)
  n_univ   <- length(universe)

  # Scaled SAA base weights (sum to saa_frac)
  saa_scaled <- saa_weights[names(saa_weights) %in% universe] * saa_frac

  # ── Single-pass ───────────────────────────────────────────────────────────────
  passes <- purrr::map(rebal_dates, function(d) {

    w_taa_raw <- build_taa_weights(xts_ret, meta, as_of_date = d,
                                   n_top = n_top, score_col = score_col,
                                   master = master, ...)

    w_combined <- setNames(rep(0, n_univ), universe)

    # Add SAA sleeve
    for (tk in names(saa_scaled))
      w_combined[tk] <- w_combined[tk] + saa_scaled[tk]

    if (!is.null(w_taa_raw)) {
      # Add TAA sleeve (each raw weight × taa_frac so sleeve sums to taa_frac)
      w_taa_scaled <- w_taa_raw * taa_frac
      for (tk in names(w_taa_scaled))
        if (tk %in% universe) w_combined[tk] <- w_combined[tk] + w_taa_scaled[tk]

      log_row <- tibble(
        date        = d,
        taa_tickers = paste(names(w_taa_raw), collapse = ","),
        n_selected  = length(w_taa_raw),
        total_wt    = round(sum(w_combined), 4)
      )
    } else {
      # Fallback: redistribute TAA fraction pro-rata into SAA
      for (tk in names(saa_weights))
        if (tk %in% universe)
          w_combined[tk] <- w_combined[tk] + saa_weights[tk] * taa_frac

      log_row <- tibble(
        date        = d,
        taa_tickers = NA_character_,
        n_selected  = 0L,
        total_wt    = round(sum(w_combined), 4)
      )
    }

    list(wrow = w_combined, log = log_row)
  })

  rebal_log  <- bind_rows(lapply(passes, `[[`, "log"))
  wt_matrix  <- do.call(rbind, lapply(passes, `[[`, "wrow"))
  weight_xts <- xts(wt_matrix, order.by = rebal_dates)

  ret_slice <- xts_ret[paste0(rebal_dates[1], "/"), universe]

  port <- Return.portfolio(R = ret_slice, weights = weight_xts, verbose = TRUE)
  colnames(port$returns) <- "TAA_Overlay"

  list(
    returns     = port$returns,
    bop_weight  = port$BOP.Weight,
    weight_hist = weight_xts,
    rebal_log   = rebal_log
  )
}


################################################################################
# compare_taa_strategies()
################################################################################

#' Run Type A + Type B + SAA baseline, align to common period.
#'
#' @param xts_ret     xts — daily returns
#' @param meta        data.frame — etf_metadata
#' @param saa_weights named numeric — SAA weights
#' @param rt          data.frame — regime table (rebuilt if NULL)
#' @param master      chr — benchmark
#' @param ...         passed to backtest functions
#' @return list:
#'   returns       — named list of aligned xts series
#'   rebal_log_a   — Type A rebalance log (tibble)
#'   rebal_log_b   — Type B rebalance log (tibble)
#'   weight_hist_a — Type A weight history (xts)
#'   weight_hist_b — Type B weight history (xts)
#'   regime_daily  — daily regime labels (data.frame with date + regime)
#'   rt            — regime table
compare_taa_strategies <- function(xts_ret,
                                    meta,
                                    saa_weights = SAA_WEIGHTS_DEFAULT,
                                    rt          = NULL,
                                    master      = "SPY",
                                    ...) {

  if (is.null(rt) || !is.data.frame(rt))
    rt <- build_regime_table(xts_ret[, master])

  # Filter SAA weights to tickers actually present in xts_ret, renormalise
  missing_saa <- setdiff(names(saa_weights), colnames(xts_ret))
  if (length(missing_saa) > 0) {
    warning("compare_taa_strategies: dropping missing SAA tickers and renormalising: ",
            paste(missing_saa, collapse = ", "))
    saa_weights <- saa_weights[!names(saa_weights) %in% missing_saa]
    saa_weights <- saa_weights / sum(saa_weights)
  }

  cat("Running Type A (Pure TAA)...\n")
  res_a <- backtest_taa_pure(xts_ret, meta, master = master, ...)

  cat("Running Type B (SAA + TAA Overlay)...\n")
  res_b <- backtest_taa_overlay(xts_ret, meta,
                                 saa_weights = saa_weights,
                                 master = master, ...)

  cat("Computing SAA baseline...\n")
  saa_res <- calc_saa_portfolio(
    xts_returns    = xts_ret,
    weights_vector = saa_weights,
    rebalance_freq = "months"
  )

  # Align to the period where all strategies are live
  common_start <- max(
    start(res_a$returns),
    start(res_b$returns),
    start(saa_res$returns)
  )
  s <- paste0(common_start, "/")

  spy_xts <- xts_ret[s, master]
  colnames(spy_xts) <- "SPY"

  ret_list <- list(
    "TAA Pure"    = res_a$returns[s],
    "TAA Overlay" = res_b$returns[s],
    "SAA"         = saa_res$returns[s],
    "SPY"         = spy_xts
  )

  # Pre-compute daily regime labels for the backtest window
  regime_xts  <- label_daily_regime(xts_ret[s, master])
  regime_daily <- data.frame(
    date   = index(regime_xts),
    regime = as.character(regime_xts[, "regime"]),
    stringsAsFactors = FALSE
  )

  list(
    returns       = ret_list,
    rebal_log_a   = res_a$rebal_log,
    rebal_log_b   = res_b$rebal_log,
    weight_hist_a = res_a$weight_hist,
    weight_hist_b = res_b$weight_hist,
    regime_daily  = regime_daily,
    rt            = rt
  )
}


################################################################################
# plot_taa_backtest()
################################################################################

#' Three-panel comparison: wealth / drawdown / regime-conditional alpha.
#'
#' Panel 1: log-scale cumulative wealth for all four series.
#' Panel 2: rolling drawdown from peak.
#' Panel 3: annualised daily alpha vs SPY per regime, faceted by strategy.
#'
#' @param results output of compare_taa_strategies()
#' @param title   optional overall title
#' @return patchwork ggplot
plot_taa_backtest <- function(results,
                               title = "TAA Backtest: Pure Momentum (A) vs SAA Overlay (B)") {

  ret_list     <- results$returns
  regime_daily <- results$regime_daily

  # Merge into one wide xts, then to data.frame
  combined <- do.call(merge, ret_list)
  colnames(combined) <- names(ret_list)
  dates    <- index(combined)

  ret_df <- as.data.frame(combined) %>%
    mutate(date = dates) %>%
    pivot_longer(-date, names_to = "strategy", values_to = "ret") %>%
    mutate(
      strategy = factor(strategy, levels = names(.STRAT_COLS)),
      ret      = replace_na(ret, 0)
    )

  # ── Panel 1: Wealth ──────────────────────────────────────────────────────────
  wealth_df <- ret_df %>%
    group_by(strategy) %>%
    arrange(date) %>%
    mutate(wealth = cumprod(1 + ret)) %>%
    ungroup()

  p_wealth <- ggplot(wealth_df, aes(x = date, y = wealth, colour = strategy)) +
    geom_line(linewidth = 0.8) +
    scale_colour_manual(values = .STRAT_COLS, name = NULL) +
    scale_y_log10(labels = number_format(accuracy = 0.01)) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    labs(
      title    = title,
      subtitle = "Log scale  |  Monthly rebalance  |  No transaction costs",
      x = NULL, y = "Wealth (log)"
    ) +
    .bt_theme

  # ── Panel 2: Drawdown ────────────────────────────────────────────────────────
  dd_df <- ret_df %>%
    group_by(strategy) %>%
    arrange(date) %>%
    mutate(
      cum  = cumprod(1 + ret),
      peak = cummax(cum),
      dd   = cum / peak - 1
    ) %>%
    ungroup()

  p_dd <- ggplot(dd_df, aes(x = date, y = dd, colour = strategy)) +
    geom_line(linewidth = 0.7) +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
    scale_colour_manual(values = .STRAT_COLS, name = NULL) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    labs(subtitle = "Drawdown from peak", x = NULL, y = "Drawdown") +
    .bt_theme +
    theme(legend.position = "none")

  # ── Panel 3: Regime-conditional alpha ────────────────────────────────────────
  strat_taa <- c("TAA Pure", "TAA Overlay")

  alpha_df <- ret_df %>%
    filter(strategy %in% strat_taa) %>%
    left_join(
      ret_df %>% filter(strategy == "SPY") %>%
        select(date, spy_ret = ret),
      by = "date"
    ) %>%
    left_join(regime_daily, by = "date") %>%
    filter(!is.na(regime)) %>%
    mutate(alpha = ret - spy_ret) %>%
    group_by(strategy, regime) %>%
    summarise(
      avg_ann_alpha = mean(alpha, na.rm = TRUE) * 252,
      n_days        = n(),
      .groups       = "drop"
    ) %>%
    mutate(
      regime   = factor(regime, levels = c("Fall", "Recovery", "Consolidation")),
      strategy = factor(strategy, levels = strat_taa)
    )

  p_regime <- ggplot(alpha_df,
                     aes(x = regime, y = avg_ann_alpha, fill = regime)) +
    geom_col(width = 0.6, alpha = 0.85) +
    geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.4) +
    geom_text(
      aes(label  = percent(avg_ann_alpha, accuracy = 0.1),
          vjust  = ifelse(avg_ann_alpha >= 0, -0.4, 1.3)),
      size = 2.8
    ) +
    facet_wrap(~strategy, nrow = 1) +
    scale_fill_manual(values = .REGIME_PAL, guide = "none") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      subtitle = "Annualised avg daily alpha vs SPY by regime",
      x = NULL, y = "Avg ann. alpha"
    ) +
    .bt_theme +
    theme(legend.position = "none")

  p_wealth / p_dd / p_regime +
    plot_layout(heights = c(3, 2, 2)) +
    plot_annotation(theme = theme(plot.margin = margin(5, 5, 5, 5)))
}


################################################################################
# run_taa_backtest()   — orchestrator
################################################################################

#' Build, summarise, and plot TAA backtest (Types A + B).
#'
#' @param xts_ret  xts — daily returns
#' @param meta     data.frame — etf_metadata
#' @param rt       data.frame — regime table (rebuilt if NULL)
#' @param master   chr — benchmark ticker
#' @param ...      passed to compare_taa_strategies()
#' @return results list from compare_taa_strategies(), invisibly
run_taa_backtest <- function(xts_ret,
                              meta,
                              rt     = NULL,
                              master = "SPY",
                              ...) {

  results  <- compare_taa_strategies(xts_ret, meta, rt = rt,
                                      master = master, ...)

  combined <- do.call(merge, results$returns)
  colnames(combined) <- names(results$returns)

  # ── Performance table ─────────────────────────────────────────────────────────
  cat("\n── TAA Backtest — Annualised Performance ────────────────────────────\n")
  print(round(table.AnnualizedReturns(combined, scale = 252), 4))

  cat("\n── Max Drawdown ─────────────────────────────────────────────────────\n")
  for (nm in colnames(combined)) {
    mdd <- -as.numeric(maxDrawdown(combined[, nm]))
    cat(sprintf("  %-14s  %.1f%%\n", nm, mdd * 100))
  }

  cat("\n── Type A — Last 6 rebalance periods ────────────────────────────────\n")
  print(tail(results$rebal_log_a, 6))

  cat("\n── Type B — Last 6 rebalance periods ────────────────────────────────\n")
  print(tail(results$rebal_log_b %>% select(date, taa_tickers, n_selected), 6))

  p <- plot_taa_backtest(results)
  print(p)

  invisible(results)
}


################################################################################
# Auto-run
################################################################################
if (!isTRUE(getOption("knitr.in.progress"))) {

  if (!exists("xts_ret"))
    xts_ret <- readRDS(here::here("02_data_processed/xts_ret_returns.rds"))

  if (!exists("etf_metadata"))
    source(here::here("scripts/00_init_universe.R"))

  # exists("rt") would match stats::rt — check it is actually a data.frame
  if (!is.data.frame(tryCatch(get("rt", inherits = TRUE), error = function(e) NULL)))
    rt <- build_regime_table(xts_ret[, "SPY"])

  cat("\ntaa_portfolio_backtest.R loaded.\n")
  cat("Run: run_taa_backtest(xts_ret, etf_metadata, rt = rt)\n")
}

message("taa_portfolio_backtest: loaded  |  ",
        "run_taa_backtest(xts_ret, etf_metadata, rt = rt)")
