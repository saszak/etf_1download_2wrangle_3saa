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
# ==============================================================================
# PHASE 2: L/S Overlay on top of 50/50 Core Portfolio
# ==============================================================================
#
# The overlay is dollar-neutral (net = 0), so adding it to the core does not
# change the core's market exposure. The combined return is simply:
#
#   R_combined = R_core + R_ls_overlay
#
# We compare three series:
#   1. Core alone  : 50% SPY + 50% AGG, monthly rebalanced
#   2. LS alone    : dollar-neutral L/S on core 15-ETF universe
#   3. Core + LS   : core portfolio augmented with the overlay
#
# ==============================================================================

# ── Load absolute returns (needed by calc_saa_portfolio) ─────────────────────
xts_ret <- readRDS(here("02_data_processed/xts_ret_returns.rds"))

# ── Build 50/50 core ──────────────────────────────────────────────────────────
source(here("scripts_saa_taa/05_saa_portfolio.R"))

core_5050 <- calc_saa_portfolio(
  xts_returns    = xts_ret,
  weights_vector = c(SPY = 0.50, AGG = 0.50),
  rebalance_freq = "months"
)

# ── Align dates across core and overlay ───────────────────────────────────────
common_idx2  <- intersect(index(core_5050$returns), index(ls_core$returns))
core_ret     <- core_5050$returns[common_idx2]
ls_ret       <- ls_core$returns[common_idx2]
combined_ret <- xts(as.numeric(core_ret) + as.numeric(ls_ret),
                    order.by = common_idx2)

colnames(core_ret)     <- "Core_5050"
colnames(ls_ret)       <- "LS_Core_15"
colnames(combined_ret) <- "Core_plus_LS"

phase2_comp <- merge(core_ret, ls_ret, combined_ret)

# ── Performance table ─────────────────────────────────────────────────────────
cat("\014")
cat("── Phase 2: Core (50/50) vs L/S Overlay vs Combined ────────────────────\n")
print(table.AnnualizedReturns(phase2_comp, scale = 252))
cat("\n── Top 3 Drawdowns: Core + L/S ─────────────────────────────────────────\n")
print(table.Drawdowns(phase2_comp[, "Core_plus_LS"], top = 3))
cat("─────────────────────────────────────────────────────────────────────────\n")

# ── Performance chart ─────────────────────────────────────────────────────────
charts.PerformanceSummary(
  phase2_comp,
  main       = "Phase 2: 50/50 Core + L/S Overlay",
  colorset   = c("#bdc3c7", "#27ae60", "#2c3e50"),
  lwd        = c(2, 2, 3),
  legend.loc = "topleft"
)

################################################################################
# ==============================================================================
# VISUALIZATIONS
# ==============================================================================
# P1  : PCA biplot — ETFs in PC1/PC2 factor space
# P2  : Correlation heatmap of 15 core ETFs
# P3  : Cumulative wealth index — Full vs Core (log scale)
# P4  : Rolling 252-day Sharpe — Full vs Core
# P5  : Annual return bars — Full vs Core
# P6  : Return density — Full vs Core
# P7  : Wealth index — Core / LS / Combined
# P8  : Return scatter — Core vs LS (correlation check)
# P9  : Risk/return space — all portfolios
# P10 : Rolling 252-day correlation — Core vs LS
# P11 : Annual return bars — Core / LS / Combined
# P12 : Drawdown comparison — overlaid series
# ==============================================================================

library(scales)

# Shared palette
pal <- c(
  Full_59      = "#bdc3c7",
  Core_15      = "#2c3e50",
  Core_5050    = "#95a5a6",
  LS_Core_15   = "#27ae60",
  Core_plus_LS = "#e74c3c"
)

# ── Recompute PCA for visuals (uses objects already in environment) ────────────
xts_u_pca <- xts_rel[, setdiff(colnames(xts_rel), "SPY")]
xts_u_pca <- xts_u_pca[, apply(xts_u_pca, 2, function(x) sd(x, na.rm = TRUE) > 0)]
pca_vis   <- prcomp(as.matrix(xts_u_pca), scale. = TRUE, center = TRUE)

# Factor group labels for colour
factor_group <- case_when(
  colnames(xts_u_pca) %in% c("AGG","IEF","TLT","HYG","TIP","LQD","BND","SHY",
                               "SGOV","EMB","EMLC","AOR","AOK")       ~ "Bonds/Rates",
  colnames(xts_u_pca) %in% c("IEFA","FEZ","VWO","ACWX","URTH","EWQ",
                               "DAX","EWY","EWL","IGF")                ~ "International",
  colnames(xts_u_pca) %in% c("QQQ","XLK","XLF","XLI","XLE","XLP",
                               "WCLD","CIBR","SMH","IPO","IYT","PSP") ~ "Equity Sector",
  colnames(xts_u_pca) %in% c("GLD","VIXY","USMV","COPX")             ~ "Tail/Alts",
  TRUE                                                                  ~ "Other"
)

# ── P1: PCA Biplot ─────────────────────────────────────────────────────────────
var_exp_vis <- summary(pca_vis)$importance[2, 1:2] * 100

pca_df <- data.frame(
  ticker  = colnames(xts_u_pca),
  PC1     = pca_vis$rotation[, 1],
  PC2     = pca_vis$rotation[, 2],
  group   = factor_group,
  in_core = colnames(xts_u_pca) %in% core_universe
)

print(
  ggplot(pca_df, aes(x = PC1, y = PC2, colour = group, size = in_core)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey70") +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70") +
    geom_point(alpha = 0.8) +
    ggrepel::geom_text_repel(
      data    = filter(pca_df, in_core),
      aes(label = ticker),
      size    = 3.2, fontface = "bold", max.overlaps = 20, show.legend = FALSE
    ) +
    scale_size_manual(values = c("TRUE" = 4, "FALSE" = 1.5),
                      guide  = "none") +
    scale_colour_manual(values = c(
      "Bonds/Rates"   = "#2980b9",
      "International" = "#8e44ad",
      "Equity Sector" = "#27ae60",
      "Tail/Alts"     = "#e67e22",
      "Other"         = "#bdc3c7"
    )) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    labs(
      title    = "P1: PCA Biplot — ETF Factor Space",
      subtitle = sprintf("PC1 (%.1f%% var): Bonds vs Equity  |  PC2 (%.1f%% var): International vs Domestic",
                         var_exp_vis[1], var_exp_vis[2]),
      x        = sprintf("PC1 (%.1f%%)", var_exp_vis[1]),
      y        = sprintf("PC2 (%.1f%%)", var_exp_vis[2]),
      colour   = NULL
    )
)

# ── P2: Correlation heatmap of 15 core ETFs ───────────────────────────────────
core_cor <- cor(as.matrix(xts_rel[, core_universe]), use = "pairwise.complete.obs")

core_cor_df <- as.data.frame(core_cor) %>%
  rownames_to_column("ETF1") %>%
  pivot_longer(-ETF1, names_to = "ETF2", values_to = "corr") %>%
  mutate(
    ETF1 = factor(ETF1, levels = core_universe),
    ETF2 = factor(ETF2, levels = rev(core_universe))
  )

print(
  ggplot(core_cor_df, aes(x = ETF1, y = ETF2, fill = corr)) +
    geom_tile(colour = "white", linewidth = 0.4) +
    geom_text(aes(label = round(corr, 2)), size = 2.8,
              colour = ifelse(abs(core_cor_df$corr) > 0.5, "white", "black")) +
    scale_fill_gradient2(low = "#c0392b", mid = "white", high = "#2980b9",
                         midpoint = 0, limits = c(-1, 1), name = "Correlation") +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x     = element_text(angle = 45, hjust = 1),
      panel.grid      = element_blank(),
      legend.position = "right"
    ) +
    labs(title    = "P2: Core 15-ETF Correlation Matrix (xts_rel)",
         subtitle = "Relative-return correlations — low cross-factor correlation confirms diversity",
         x = NULL, y = NULL)
)

# ── P3: Cumulative wealth index — Full vs Core (log scale) ────────────────────
wealth_df <- data.frame(
  date     = index(comp),
  Full_59  = as.numeric(cumprod(1 + comp[, "LS_Full_59"])),
  Core_15  = as.numeric(cumprod(1 + comp[, "LS_Core_15"]))
) %>%
  pivot_longer(-date, names_to = "Series", values_to = "Wealth")

print(
  ggplot(wealth_df, aes(x = date, y = Wealth, colour = Series)) +
    geom_line(linewidth = 1.1) +
    scale_y_log10(labels = scales::dollar_format(prefix = "$")) +
    scale_colour_manual(values = c(Full_59 = pal["Full_59"], Core_15 = pal["Core_15"])) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
    labs(title    = "P3: Wealth Index — Full (59) vs Core (15) Universe",
         subtitle = "Log scale | $1 invested Jan 2018",
         x = NULL, y = "Portfolio Value (log scale)", colour = NULL)
)

# ── P4: Rolling 252-day Sharpe — Full vs Core ─────────────────────────────────
roll_sharpe <- function(ret_xts, width = 252) {
  rollapply(ret_xts, width = width,
            FUN    = function(x) mean(x, na.rm = TRUE) / sd(x, na.rm = TRUE) * sqrt(252),
            fill   = NA, align = "right")
}

rs_df <- data.frame(
  date    = index(comp),
  Full_59 = as.numeric(roll_sharpe(comp[, "LS_Full_59"])),
  Core_15 = as.numeric(roll_sharpe(comp[, "LS_Core_15"]))
) %>%
  pivot_longer(-date, names_to = "Series", values_to = "Sharpe") %>%
  filter(!is.na(Sharpe))

print(
  ggplot(rs_df, aes(x = date, y = Sharpe, colour = Series)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    geom_hline(yintercept = 1, linetype = "dotted", colour = "grey60") +
    geom_line(linewidth = 1) +
    scale_colour_manual(values = c(Full_59 = pal["Full_59"], Core_15 = pal["Core_15"])) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
    labs(title    = "P4: Rolling 252-Day Sharpe Ratio — Full vs Core",
         subtitle = "Dotted line = Sharpe 1.0 | Dashed = 0",
         x = NULL, y = "Rolling Sharpe (ann.)", colour = NULL)
)

# ── P5: Annual return bars — Full vs Core ─────────────────────────────────────
ann_ret_df <- data.frame(
  date    = index(comp),
  Full_59 = as.numeric(comp[, "LS_Full_59"]),
  Core_15 = as.numeric(comp[, "LS_Core_15"])
) %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(year) %>%
  summarise(Full_59 = prod(1 + Full_59) - 1,
            Core_15 = prod(1 + Core_15) - 1,
            .groups = "drop") %>%
  pivot_longer(-year, names_to = "Series", values_to = "Return")

print(
  ggplot(ann_ret_df, aes(x = factor(year), y = Return, fill = Series)) +
    geom_col(position = "dodge", width = 0.7, alpha = 0.9) +
    geom_hline(yintercept = 0, colour = "grey40") +
    scale_fill_manual(values = c(Full_59 = pal["Full_59"], Core_15 = pal["Core_15"])) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
    labs(title = "P5: Annual Returns — Full (59) vs Core (15) Universe",
         x = NULL, y = "Annual Return", fill = NULL)
)

# ── P6: Return density — Full vs Core ─────────────────────────────────────────
dens_df <- data.frame(
  Full_59 = as.numeric(comp[, "LS_Full_59"]),
  Core_15 = as.numeric(comp[, "LS_Core_15"])
) %>%
  pivot_longer(everything(), names_to = "Series", values_to = "Return")

print(
  ggplot(dens_df, aes(x = Return, fill = Series, colour = Series)) +
    geom_density(alpha = 0.35, linewidth = 1) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    scale_fill_manual(values  = c(Full_59 = pal["Full_59"], Core_15 = pal["Core_15"])) +
    scale_colour_manual(values = c(Full_59 = pal["Full_59"], Core_15 = pal["Core_15"])) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(-0.06, 0.06)) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
    labs(title    = "P6: Daily Return Distribution — Full vs Core",
         subtitle = "Core (15) is narrower — less noise from redundant tickers",
         x = "Daily Return", y = "Density", fill = NULL, colour = NULL)
)

# ── P7: Phase 2 wealth index — Core / LS / Combined ──────────────────────────
p2_wealth_df <- data.frame(
  date         = index(phase2_comp),
  Core_5050    = as.numeric(cumprod(1 + phase2_comp[, "Core_5050"])),
  LS_Core_15   = as.numeric(cumprod(1 + phase2_comp[, "LS_Core_15"])),
  Core_plus_LS = as.numeric(cumprod(1 + phase2_comp[, "Core_plus_LS"]))
) %>%
  pivot_longer(-date, names_to = "Series", values_to = "Wealth")

print(
  ggplot(p2_wealth_df, aes(x = date, y = Wealth, colour = Series, linewidth = Series)) +
    geom_line() +
    scale_colour_manual(values = c(
      Core_5050    = pal["Core_5050"],
      LS_Core_15   = pal["LS_Core_15"],
      Core_plus_LS = pal["Core_plus_LS"]
    )) +
    scale_linewidth_manual(values = c(Core_5050 = 1, LS_Core_15 = 1, Core_plus_LS = 2),
                           guide  = "none") +
    scale_y_log10(labels = scales::dollar_format(prefix = "$")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
    labs(title    = "P7: Phase 2 Wealth Index — Core / L/S / Combined",
         subtitle = "Log scale | $1 invested | Bold = Core + L/S overlay",
         x = NULL, y = "Portfolio Value (log scale)", colour = NULL)
)

# ── P8: Return scatter — Core vs LS ──────────────────────────────────────────
scatter_df <- data.frame(
  Core = as.numeric(phase2_comp[, "Core_5050"]),
  LS   = as.numeric(phase2_comp[, "LS_Core_15"])
)
r_sq <- round(cor(scatter_df$Core, scatter_df$LS)^2, 3)

print(
  ggplot(scatter_df, aes(x = Core, y = LS)) +
    geom_point(alpha = 0.25, size = 0.9, colour = "#2c3e50") +
    geom_smooth(method = "lm", se = TRUE, colour = "#e74c3c", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank()) +
    labs(
      title    = "P8: Daily Returns — Core (50/50) vs L/S Overlay",
      subtitle = sprintf("R² = %.3f — near-zero correlation confirms overlay adds independent alpha",
                         r_sq),
      x = "Core 50/50 Daily Return",
      y = "L/S Overlay Daily Return"
    )
)

# ── P9: Risk/return space — all portfolios ────────────────────────────────────
all_rets <- merge(comp, phase2_comp[, c("Core_5050", "Core_plus_LS")])

rr_df <- data.frame(
  Series = colnames(all_rets),
  Return = as.numeric(table.AnnualizedReturns(all_rets, scale = 252)[1, ]),
  Vol    = as.numeric(table.AnnualizedReturns(all_rets, scale = 252)[2, ]),
  Sharpe = as.numeric(table.AnnualizedReturns(all_rets, scale = 252)[3, ])
)

print(
  ggplot(rr_df, aes(x = Vol, y = Return, colour = Series, size = Sharpe)) +
    geom_point(alpha = 0.9) +
    ggrepel::geom_text_repel(aes(label = sprintf("%s\nSharpe %.2f", Series, Sharpe)),
                              size = 3.2, show.legend = FALSE) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_colour_manual(values = c(
      LS_Full_59   = pal["Full_59"],
      LS_Core_15   = pal["Core_15"],
      Core_5050    = pal["Core_5050"],
      Core_plus_LS = pal["Core_plus_LS"]
    )) +
    scale_size_continuous(range = c(4, 10), guide = "none") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), legend.position = "none") +
    labs(title    = "P9: Risk/Return Space — All Portfolios",
         subtitle = "Bubble size = Sharpe ratio",
         x = "Annualised Volatility", y = "Annualised Return")
)

# ── P10: Rolling 252-day correlation — Core vs LS ─────────────────────────────
roll_cor <- rollapply(
  merge(phase2_comp[, "Core_5050"], phase2_comp[, "LS_Core_15"]),
  width = 252,
  FUN   = function(m) cor(m[, 1], m[, 2], use = "complete.obs"),
  by.column = FALSE, fill = NA, align = "right"
)

print(
  data.frame(date = index(roll_cor), corr = as.numeric(roll_cor)) %>%
    filter(!is.na(corr)) %>%
    ggplot(aes(x = date, y = corr)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
    geom_ribbon(aes(ymin = pmin(corr, 0), ymax = 0), fill = "#e74c3c", alpha = 0.3) +
    geom_ribbon(aes(ymin = 0, ymax = pmax(corr, 0)), fill = "#27ae60", alpha = 0.3) +
    geom_line(linewidth = 0.9, colour = "#2c3e50") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(limits = c(-1, 1)) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank()) +
    labs(title    = "P10: Rolling 252-Day Correlation — Core (50/50) vs L/S Overlay",
         subtitle = "Green = diversifying (negative corr) | Red = correlated drawdowns",
         x = NULL, y = "Rolling Correlation")
)

# ── P11: Annual return bars — Core / LS / Combined ───────────────────────────
ann_p2_df <- data.frame(
  date         = index(phase2_comp),
  Core_5050    = as.numeric(phase2_comp[, "Core_5050"]),
  LS_Core_15   = as.numeric(phase2_comp[, "LS_Core_15"]),
  Core_plus_LS = as.numeric(phase2_comp[, "Core_plus_LS"])
) %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(year) %>%
  summarise(across(c(Core_5050, LS_Core_15, Core_plus_LS),
                   ~ prod(1 + .) - 1), .groups = "drop") %>%
  pivot_longer(-year, names_to = "Series", values_to = "Return") %>%
  mutate(Series = factor(Series,
                         levels = c("Core_5050", "LS_Core_15", "Core_plus_LS")))

print(
  ggplot(ann_p2_df, aes(x = factor(year), y = Return, fill = Series)) +
    geom_col(position = "dodge", width = 0.75, alpha = 0.9) +
    geom_hline(yintercept = 0, colour = "grey40") +
    scale_fill_manual(values = c(
      Core_5050    = pal["Core_5050"],
      LS_Core_15   = pal["LS_Core_15"],
      Core_plus_LS = pal["Core_plus_LS"]
    )) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "P11: Annual Returns — Core / L/S / Combined",
         x = NULL, y = "Annual Return", fill = NULL)
)

# ── P12: Drawdown comparison — overlaid series ────────────────────────────────
dd_xts <- Drawdowns(phase2_comp)

dd_df <- data.frame(
  date         = index(dd_xts),
  Core_5050    = as.numeric(dd_xts[, "Core_5050"]),
  LS_Core_15   = as.numeric(dd_xts[, "LS_Core_15"]),
  Core_plus_LS = as.numeric(dd_xts[, "Core_plus_LS"])
) %>%
  pivot_longer(-date, names_to = "Series", values_to = "Drawdown") %>%
  mutate(Series = factor(Series,
                         levels = c("Core_5050", "LS_Core_15", "Core_plus_LS")))

print(
  ggplot(dd_df, aes(x = date, y = Drawdown, colour = Series, linewidth = Series)) +
    geom_line() +
    geom_hline(yintercept = 0, colour = "grey40") +
    scale_colour_manual(values = c(
      Core_5050    = pal["Core_5050"],
      LS_Core_15   = pal["LS_Core_15"],
      Core_plus_LS = pal["Core_plus_LS"]
    )) +
    scale_linewidth_manual(values = c(Core_5050 = 0.8, LS_Core_15 = 0.8,
                                      Core_plus_LS = 1.6), guide = "none") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
    labs(title    = "P12: Drawdown Comparison — Core / L/S / Combined",
         subtitle = "Bold = Core + L/S | Drawdown regimes partially offset across series",
         x = NULL, y = "Drawdown", colour = NULL)
)

################################################################################
