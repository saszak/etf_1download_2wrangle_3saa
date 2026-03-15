################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE: scripts_saa_structured/long_short_overlay.R
# Purpose: Standalone Long/Short Overlay Portfolio
#
# DESIGN:
#   Each position is Long ETF / Short SPY (= xts_rel[ticker]).
#   The portfolio goes long top-ranked ETFs and short bottom-ranked ETFs,
#   ranked by momentum signal (cumulative relative return over signal_lookback days).
#   Dollar neutral: long leg = +100%, short leg = -100%, net = 0.
#
# INPUTS REQUIRED IN ENVIRONMENT:
#   xts_rel — xts of daily relative returns vs SPY (from 01_etf_wrangle.R)
################################################################################

library(PerformanceAnalytics)
library(xts)
library(tidyverse)

# ==============================================================================
# CORE FUNCTION: build_ls_portfolio()
# ==============================================================================
#
# INPUTS:
#   xts_rel         : xts of relative returns (ETF - SPY). SPY column excluded.
#   n_long          : number of ETFs to go long (default 5)
#   n_short         : number of ETFs to go short (default 5)
#   signal_lookback : lookback days for momentum signal (default 63 = 1 quarter)
#   rebalance_freq  : "months", "quarters", or "years" (default "months")
#
# OUTPUTS: list with
#   $returns        : xts of daily L/S portfolio returns
#   $weights_bop    : xts of beginning-of-period weights (from Return.portfolio)
#   $weights_rebal  : xts of weights on rebalance dates only
#   $signal_history : named list of data.frames — one per rebalance date,
#                     showing ticker, signal value, rank, and side (Long/Short/Neutral)
#
# ==============================================================================

build_ls_portfolio <- function(xts_rel,
                                n_long           = 5L,
                                n_short          = 5L,
                                signal_lookback  = 63L,
                                rebalance_freq   = "months",
                                min_hold_days    = 0L,
                                universe_tickers = NULL) {

  # ── Validation ───────────────────────────────────────────────────────────────
  valid_freqs <- c("months", "quarters", "years")
  if (!(rebalance_freq %in% valid_freqs))
    stop("rebalance_freq must be one of: months, quarters, years")

  if (min_hold_days < 0L)
    stop("min_hold_days must be >= 0")

  if (n_long < 1 || n_short < 1)
    stop("n_long and n_short must be >= 1")

  if (!("SPY" %in% colnames(xts_rel)))
    warning("SPY column not found in xts_rel — check data source.")

  # ── Universe: user-supplied filter or full set minus SPY ─────────────────────
  if (!is.null(universe_tickers)) {
    missing_tk <- setdiff(universe_tickers, colnames(xts_rel))
    if (length(missing_tk) > 0)
      warning("universe_tickers not found in xts_rel, dropped: ",
              paste(missing_tk, collapse = ", "))
    universe_tickers <- intersect(universe_tickers, colnames(xts_rel))
  } else {
    universe_tickers <- setdiff(colnames(xts_rel), "SPY")
  }
  n_universe <- length(universe_tickers)
  xts_u      <- xts_rel[, universe_tickers]

  if (n_universe < n_long + n_short)
    stop(sprintf(
      "Universe has %d tickers but n_long + n_short = %d. Reduce n_long/n_short.",
      n_universe, n_long + n_short
    ))

  cat(sprintf(
    "\n── L/S Engine: universe = %d tickers | long top %d | short bottom %d | lookback = %d days | rebal = %s | min_hold = %d days\n",
    n_universe, n_long, n_short, signal_lookback, rebalance_freq, min_hold_days
  ))

  # ── Rebalance endpoints ───────────────────────────────────────────────────────
  ep <- endpoints(xts_u, on = rebalance_freq)
  ep <- ep[ep > 0]   # drop the leading 0 from endpoints()

  cat(sprintf("   Rebalance dates identified: %d periods\n", length(ep)))

  # ── Build weight matrix ───────────────────────────────────────────────────────
  # One row per rebalance date, n_universe columns. All NA initially.
  rebal_dates <- index(xts_u)[ep]
  w_mat       <- matrix(NA_real_,
                        nrow     = length(ep),
                        ncol     = n_universe,
                        dimnames = list(as.character(rebal_dates), universe_tickers))

  signal_history  <- list()
  last_rebal_idx  <- NA_integer_   # index of last accepted rebalance

  for (k in seq_along(ep)) {
    ep_i <- ep[k]

    # ── Minimum holding period guard ─────────────────────────────────────────
    if (min_hold_days > 0L && !is.na(last_rebal_idx)) {
      days_held <- ep_i - last_rebal_idx   # trading days since last rebalance
      if (days_held < min_hold_days) {
        cat(sprintf("   [HOLD]  %s — only %d days since last rebalance (need %d)\n",
                    as.character(index(xts_u)[ep_i]), days_held, min_hold_days))
        next
      }
    }

    # Lookback window ending at this rebalance date
    lb_start <- max(1L, ep_i - signal_lookback + 1L)
    win      <- xts_u[lb_start:ep_i, ]

    # Momentum signal: cumulative return over window
    cum_ret <- vapply(universe_tickers, function(tk) {
      x <- as.numeric(win[, tk])
      x <- x[!is.na(x)]
      if (length(x) == 0) return(NA_real_)
      prod(1 + x) - 1
    }, numeric(1))

    # Drop NAs before ranking
    valid_idx <- !is.na(cum_ret)
    cum_valid <- cum_ret[valid_idx]

    if (length(cum_valid) < n_long + n_short) {
      cat(sprintf("   [SKIP] %s — only %d valid tickers (need %d)\n",
                  as.character(rebal_dates[k]), length(cum_valid), n_long + n_short))
      next
    }

    # Rank descending: top = highest momentum (Long), bottom = lowest (Short)
    ranked      <- sort(cum_valid, decreasing = TRUE)
    long_names  <- names(ranked)[seq_len(n_long)]
    short_names <- names(ranked)[seq(length(ranked) - n_short + 1L, length(ranked))]

    # Equal weight within each leg — dollar neutral
    w_row <- setNames(rep(0, n_universe), universe_tickers)
    w_row[long_names]  <-  1 / n_long
    w_row[short_names] <- -1 / n_short

    w_mat[k, ] <- w_row
    last_rebal_idx <- ep_i   # record this as the last accepted rebalance

    # Save signal snapshot
    signal_history[[as.character(rebal_dates[k])]] <- data.frame(
      ticker  = names(cum_valid),
      signal  = as.numeric(cum_valid),
      rank    = rank(-cum_valid, ties.method = "first"),
      side    = ifelse(names(cum_valid) %in% long_names,  "Long",
                ifelse(names(cum_valid) %in% short_names, "Short", "Neutral")),
      stringsAsFactors = FALSE
    )
  }

  # Drop rows where no weights were assigned (insufficient data periods)
  valid_rows  <- !is.na(w_mat[, 1])
  w_mat       <- w_mat[valid_rows, , drop = FALSE]
  rebal_dates <- rebal_dates[valid_rows]

  if (nrow(w_mat) == 0)
    stop("No valid rebalance dates found. Check signal_lookback vs data history.")

  # ── Build xts weights object (one row per rebalance date) ───────────────────
  w_xts <- xts(w_mat, order.by = rebal_dates)

  # ── Verify dollar neutrality ─────────────────────────────────────────────────
  w_sums <- rowSums(w_xts)
  tol    <- 0.001
  if (any(abs(w_sums) > tol)) {
    bad_dates <- names(w_sums[abs(w_sums) > tol])
    cat(sprintf(
      "   ✗ Dollar-neutral check FAILED on %d date(s): %s\n",
      length(bad_dates), paste(bad_dates[seq_len(min(5, length(bad_dates)))], collapse = ", ")
    ))
  } else {
    cat("   ✓ Dollar-neutral verified: all rebalance weight sums ≈ 0\n")
  }

  # ── Manual return computation (dollar-neutral cannot use Return.portfolio) ────
  #
  # Return.portfolio normalises by sum(BOP_Value), which = 0 for a dollar-neutral
  # portfolio → 0/0 = NaN, silently collapsed to 0 for the whole history.
  #
  # Correct formula: R_port_t = sum(w_i * R_{i,t})
  # Weights are held constant within each rebalance period (no drift adjustment —
  # drift-based renormalisation also breaks under dollar neutrality).
  #
  # Build a daily weight matrix: broadcast each period's weights to its trading days.
  all_dates <- index(xts_u)
  n_days    <- length(all_dates)
  w_daily   <- matrix(NA_real_, nrow = n_days, ncol = n_universe,
                      dimnames = list(NULL, universe_tickers))

  for (k in seq_len(nrow(w_mat))) {
    # Period: from the day AFTER this rebalance date up to (and including) the next
    period_start <- rebal_dates[k]   # inclusive — apply weights on the rebalance day itself
    period_end   <- if (k < nrow(w_mat)) rebal_dates[k + 1L] else tail(all_dates, 1L)

    day_idx <- which(all_dates >= period_start & all_dates <= period_end)
    if (length(day_idx) > 0)
      w_daily[day_idx, ] <- matrix(rep(w_mat[k, ], length(day_idx)),
                                   nrow = length(day_idx), byrow = TRUE)
  }

  w_daily_xts <- xts(w_daily, order.by = all_dates)

  # Drop leading rows before the first rebalance (no position yet)
  first_rebal_idx <- which(all_dates >= rebal_dates[1L])[1L]
  if (first_rebal_idx > 1L) {
    w_daily_xts[seq_len(first_rebal_idx - 1L), ] <- NA
  }

  # R_port_t = sum_i(w_i * R_{i,t}), NA where no position
  port_ret_mat <- w_daily_xts * xts_u
  port_ret_vec <- rowSums(port_ret_mat, na.rm = FALSE)
  port_ret_vec[is.na(port_ret_vec)] <- NA  # keep explicit NAs before first rebal

  port_ret_xts <- xts(port_ret_vec, order.by = all_dates)
  colnames(port_ret_xts) <- "LS_Overlay"

  # Trim to the first valid (non-NA) return
  port_ret_xts <- port_ret_xts[!is.na(port_ret_xts)]

  cat(sprintf("── L/S Portfolio built successfully. Observations: %d\n", nrow(port_ret_xts)))

  list(
    returns        = port_ret_xts,
    weights_bop    = w_daily_xts,   # daily weight view (matches returns index)
    weights_rebal  = w_xts,         # rebalance-date view (for plot_ls_weights)
    signal_history = signal_history
  )
}


# ==============================================================================
# VISUAL: plot_ls_weights()
# ==============================================================================
#
# Heatmap of weight changes over time — shows which ETFs cycle in and out,
# and whether they are Long (+) or Short (-).
#
# INPUTS:
#   ls_obj  : output of build_ls_portfolio()
#   top_n   : maximum number of tickers to show (by activity — most-frequent picks)
#   title   : plot title
#
# ==============================================================================

plot_ls_weights <- function(ls_obj,
                             top_n = 20,
                             title = "L/S Overlay: Weight History (Long = +, Short = -)") {

  w_rebal <- ls_obj$weights_rebal

  # Convert to long tibble
  w_df <- as.data.frame(w_rebal) %>%
    rownames_to_column("date") %>%
    mutate(date = as.Date(date)) %>%
    pivot_longer(-date, names_to = "ticker", values_to = "weight") %>%
    filter(weight != 0)

  # Select tickers with most appearances
  top_tickers <- w_df %>%
    group_by(ticker) %>%
    summarise(n = n(), .groups = "drop") %>%
    slice_max(n, n = top_n) %>%
    pull(ticker)

  w_df <- w_df %>% filter(ticker %in% top_tickers)

  # Order tickers by average long/short classification
  ticker_order <- w_df %>%
    group_by(ticker) %>%
    summarise(avg_w = mean(weight), .groups = "drop") %>%
    arrange(desc(avg_w)) %>%
    pull(ticker)

  w_df <- w_df %>%
    mutate(
      ticker = factor(ticker, levels = ticker_order),
      side   = if_else(weight > 0, "Long", "Short")
    )

  ggplot(w_df, aes(x = date, y = ticker, fill = weight)) +
    geom_tile(color = "white", linewidth = 0.3) +
    scale_fill_gradient2(
      low      = "#c0392b",
      mid      = "white",
      high     = "#27ae60",
      midpoint = 0,
      name     = "Weight",
      labels   = scales::percent_format(accuracy = 1)
    ) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position  = "right"
    ) +
    labs(
      title    = title,
      subtitle = sprintf("Top %d most-active tickers | Green = Long (+), Red = Short (-)",
                         top_n),
      x        = NULL,
      y        = NULL
    )
}


# ==============================================================================
# PERFORMANCE AUDIT
# ==============================================================================
#
# Compares L/S portfolio vs:
#   - SPY relative to itself (flat zero line — the null hypothesis)
#   - Uses charts.PerformanceSummary() and table.AnnualizedReturns()
#
# ==============================================================================

audit_ls_portfolio <- function(ls_obj, xts_rel) {

  ls_ret  <- ls_obj$returns

  # Flat zero benchmark: SPY vs SPY in xts_rel space
  # (trim to the same date range as ls_ret)
  spy_rel <- xts_rel[index(ls_ret), "SPY"]
  colnames(spy_rel) <- "SPY_vs_SPY (Zero)"

  # Merge for comparison
  audit_xts <- merge(ls_ret, spy_rel, join = "left")

  # ── Performance Chart ─────────────────────────────────────────────────────────
  charts.PerformanceSummary(
    audit_xts,
    main       = "L/S Overlay: Cumulative Returns vs Flat Benchmark",
    colorset   = c("#2c3e50", "#bdc3c7"),
    lwd        = c(3, 1),
    legend.loc = "topleft",
    geometric  = FALSE
  )

  # ── Annualized Stats ──────────────────────────────────────────────────────────
  cat("\014")   # clear console so the table is not buried in build diagnostics
  cat("── L/S Overlay: Annualized Performance ─────────────────────────────────\n")
  print(table.AnnualizedReturns(audit_xts, scale = 252, geometric = FALSE))
  cat("─────────────────────────────────────────────────────────────────────────\n")

  invisible(audit_xts)
}


# ==============================================================================
# MAIN: Run the L/S Overlay
# ==============================================================================

if (!exists("xts_rel")) {
  stop(paste0(
    "xts_rel not found in environment.\n",
    "Source scripts/01_etf_wrangle.R first, or load via:\n",
    "  xts_rel <- readRDS(here('02_data_processed/xts_rel.rds'))"
  ))
}

# ── Restricted core universe (15 ETFs from PCA eigenvector analysis) ─────────
# PC1 — Rates/Bonds  : AGG, IEF, TLT, HYG, TIP
# PC2 — International: IEFA, FEZ, VWO
# PC3 — Equity rotate: QQQ, XLK, XLF, XLI, XLE
# Tail/Regime        : GLD, VIXY
core_universe <- c(
  "AGG", "IEF", "TLT", "HYG", "TIP",   # PC1 — rates / bonds
  "IEFA", "FEZ", "VWO",                 # PC2 — international
  "QQQ", "XLK", "XLF", "XLI", "XLE",   # PC3 — equity rotation
  "GLD", "VIXY"                         # tail / regime
)

# ── Full 59-ticker universe (baseline) ───────────────────────────────────────
ls_full <- build_ls_portfolio(
  xts_rel         = xts_rel,
  n_long          = 5L,
  n_short         = 5L,
  signal_lookback = 63L,
  rebalance_freq  = "months",
  min_hold_days   = 0L
)

# ── Restricted 15-ETF core universe ──────────────────────────────────────────
ls_core <- build_ls_portfolio(
  xts_rel          = xts_rel,
  n_long           = 5L,
  n_short          = 5L,
  signal_lookback  = 63L,
  rebalance_freq   = "months",
  min_hold_days    = 0L,
  universe_tickers = core_universe
)

# ── Weight heatmap ────────────────────────────────────────────────────────────
print(plot_ls_weights(ls_core, top_n = 15, title = "Core 15-ETF L/S: Weight History"))

# ── Performance comparison: Full vs Core ─────────────────────────────────────
common_idx <- intersect(index(ls_full$returns), index(ls_core$returns))
comp <- merge(
  ls_full$returns[common_idx],
  ls_core$returns[common_idx]
)
colnames(comp) <- c("LS_Full_59", "LS_Core_15")

cat("\014")
cat("── L/S Overlay: Full Universe (59) vs Core Universe (15) ────────────────\n")
print(table.AnnualizedReturns(comp, scale = 252, geometric = FALSE))
cat("\n")
cat("── Top 3 Drawdowns ──────────────────────────────────────────────────────\n")
print(table.Drawdowns(comp, top = 3))
cat("─────────────────────────────────────────────────────────────────────────\n")

charts.PerformanceSummary(
  comp,
  main       = "L/S Overlay: Full Universe (59) vs Core Universe (15)",
  colorset   = c("#bdc3c7", "#2c3e50"),
  lwd        = c(2, 3),
  legend.loc = "topleft",
  geometric  = FALSE
)

################################################################################
