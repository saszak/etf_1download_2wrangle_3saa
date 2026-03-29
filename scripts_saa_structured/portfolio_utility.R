################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : scripts_saa_structured/portfolio_utility.R
# Purpose : Portfolio construction utilities — weighting, blending, rebalancing,
#           performance stats, and multi-portfolio comparison.
#
# FUNCTIONS
#   equal_weight()              Named equal-weight vector from ticker vector
#   build_portfolio_returns()   Weighted daily return series from any xts input
#                               (raw universe OR previously built portfolios)
#   rebalance_portfolio()       Periodic rebalancing (daily/monthly/quarterly/annual)
#   portfolio_stats()           Summary stats: Ann return, Vol, Sharpe, MaxDD, Calmar
#   compare_portfolios()        Build & combine multiple portfolios into one xts
#
# NO DEPENDENCIES on regime or plotting scripts — pure portfolio math.
#
# BLENDING TWO PORTFOLIOS
#   port_a   <- build_portfolio_returns(xts_ret, weights_a, "PORT_A")
#   port_b   <- build_portfolio_returns(xts_ret, weights_b, "PORT_B")
#   blended  <- build_portfolio_returns(cbind(port_a, port_b),
#                 weights = c(PORT_A = 0.6, PORT_B = 0.4), port_name = "BLEND")
################################################################################

library(xts)
library(PerformanceAnalytics)
library(tidyverse)

# ==============================================================================
# 1. equal_weight()
# ==============================================================================
# Returns a named numeric vector of equal weights summing to 1.
#
# INPUTS
#   tickers   : character vector of ticker symbols
#
# USAGE
#   equal_weight(c("SPY", "TLT", "GLD"))
#   # → c(SPY = 0.333, TLT = 0.333, GLD = 0.333)
# ==============================================================================

equal_weight <- function(tickers) {
  stopifnot(length(tickers) > 0)
  w <- rep(1 / length(tickers), length(tickers))
  names(w) <- tickers
  w
}


# ==============================================================================
# 2. build_portfolio_returns()
# ==============================================================================
# Constructs a weighted portfolio daily return series.
# Works on both the raw multi-ticker universe AND previously built portfolio xts,
# enabling clean portfolio blending via cbind().
#
# INPUTS
#   xts_returns : any multi-column xts of daily returns (universe or portfolios)
#   weights     : named numeric vector — names must match colnames(xts_returns)
#                 Weights are auto-normalised to sum to 1.
#   port_name   : column name for the output xts  (default "PORTFOLIO")
#
# OUTPUT
#   Single-column xts of daily portfolio returns.
#   Date range = intersection of all constituent date ranges (na.omit applied).
#
# BLENDING EXAMPLE
#   port_a  <- build_portfolio_returns(xts_ret, c(SPY=0.6, TLT=0.4), "PORT_A")
#   port_b  <- build_portfolio_returns(xts_ret, c(GLD=0.5, HYG=0.5), "PORT_B")
#   blended <- build_portfolio_returns(cbind(port_a, port_b),
#                c(PORT_A = 0.7, PORT_B = 0.3), "BLENDED")
# ==============================================================================

build_portfolio_returns <- function(xts_returns,
                                     weights,
                                     port_name = "PORTFOLIO") {

  tickers <- names(weights)
  if (is.null(tickers))
    stop("`weights` must be a named numeric vector.")

  missing_tk <- setdiff(tickers, colnames(xts_returns))
  if (length(missing_tk) > 0)
    stop("Not found in xts_returns: ", paste(missing_tk, collapse = ", "))

  # Normalise weights
  w <- as.numeric(weights) / sum(as.numeric(weights))
  names(w) <- tickers

  # Subset and drop rows with any NA (handles different inception dates)
  sub_ret <- na.omit(xts_returns[, tickers])

  if (nrow(sub_ret) == 0)
    stop("No complete rows after na.omit() — check constituent date ranges.")

  port_ret <- xts(
    as.numeric(as.matrix(sub_ret) %*% w),
    order.by = index(sub_ret)
  )
  colnames(port_ret) <- port_name

  port_ret
}


# ==============================================================================
# 3. rebalance_portfolio()
# ==============================================================================
# Builds a portfolio with periodic rebalancing back to target weights.
# Between rebalance dates, positions drift with market returns.
#
# INPUTS
#   xts_returns : multi-column xts of daily returns
#   weights     : named numeric vector of target weights (auto-normalised)
#   rebal_freq  : "daily" | "monthly" | "quarterly" | "annual"
#                 "daily" is equivalent to build_portfolio_returns() (no drift)
#   port_name   : output column name (default "PORTFOLIO")
#
# OUTPUT
#   Single-column xts of daily portfolio returns with rebalancing applied.
#
# NOTE
#   Uses PerformanceAnalytics::Return.portfolio() under the hood.
#   Transaction costs are NOT modelled — extend via the `...` pass-through
#   if you add a cost model later.
# ==============================================================================

rebalance_portfolio <- function(xts_returns,
                                 weights,
                                 rebal_freq = "monthly",
                                 port_name  = "PORTFOLIO") {

  tickers <- names(weights)
  if (is.null(tickers))
    stop("`weights` must be a named numeric vector.")

  missing_tk <- setdiff(tickers, colnames(xts_returns))
  if (length(missing_tk) > 0)
    stop("Not found in xts_returns: ", paste(missing_tk, collapse = ", "))

  w <- as.numeric(weights) / sum(as.numeric(weights))
  names(w) <- tickers

  sub_ret <- na.omit(xts_returns[, tickers])
  if (nrow(sub_ret) == 0)
    stop("No complete rows after na.omit().")

  rebal_freq <- match.arg(rebal_freq, c("daily", "monthly", "quarterly", "annual"))

  # Build rebalance weight schedule
  rebal_dates <- switch(rebal_freq,
    daily     = index(sub_ret),
    monthly   = index(sub_ret)[endpoints(sub_ret, on = "months")],
    quarterly = index(sub_ret)[endpoints(sub_ret, on = "quarters")],
    annual    = index(sub_ret)[endpoints(sub_ret, on = "years")]
  )

  # xts weight matrix: 0 on non-rebalance days, target weight on rebalance days
  w_mat <- xts(
    matrix(0, nrow = nrow(sub_ret), ncol = length(tickers),
           dimnames = list(NULL, tickers)),
    order.by = index(sub_ret)
  )
  w_mat[rebal_dates, ] <- matrix(
    rep(w, length(rebal_dates)),
    nrow  = length(rebal_dates),
    byrow = TRUE
  )

  port_ret <- Return.portfolio(sub_ret, weights = w_mat, rebalance_on = NULL)
  colnames(port_ret) <- port_name

  port_ret
}


# ==============================================================================
# 4. portfolio_stats()
# ==============================================================================
# Returns a one-row summary tibble of key performance metrics.
#
# INPUTS
#   port_ret   : single- or multi-column xts of daily returns
#   Rf         : daily risk-free rate (default 0)
#   ann_factor : trading days per year (default 252)
#
# OUTPUT
#   tibble with columns: name, ann_return, volatility, sharpe, max_dd, calmar,
#                        total_return, start_date, end_date, n_days
# ==============================================================================

portfolio_stats <- function(port_ret,
                             Rf         = 0,
                             ann_factor = 252) {

  stopifnot(is.xts(port_ret))

  # Vectorised over columns so it works on multi-portfolio xts too
  map_dfr(colnames(port_ret), function(nm) {
    r <- port_ret[, nm]
    r <- r[!is.na(r)]

    ann_r  <- as.numeric(Return.annualized(r, scale = ann_factor))
    vol    <- as.numeric(StdDev.annualized(r, scale = ann_factor))
    sharpe <- as.numeric(SharpeRatio.annualized(r, Rf = Rf, scale = ann_factor))
    mdd    <- as.numeric(maxDrawdown(r))
    calmar <- if (mdd > 0) ann_r / mdd else NA_real_
    total  <- as.numeric(Return.cumulative(r))

    tibble(
      name         = nm,
      ann_return   = ann_r,
      volatility   = vol,
      sharpe       = sharpe,
      max_dd       = mdd,
      calmar       = calmar,
      total_return = total,
      start_date   = as.Date(start(r)),
      end_date     = as.Date(end(r)),
      n_days       = nrow(r)
    )
  })
}


# ==============================================================================
# 5. compare_portfolios()
# ==============================================================================
# Builds multiple portfolios from a named list of weight vectors and returns
# a combined multi-column xts — ready for charting or regime analysis.
# Also prints a side-by-side stats table.
#
# INPUTS
#   xts_returns   : multi-column xts of daily returns (the raw universe)
#   portfolio_list: named list of weight vectors
#                   e.g. list(RISK_BAL = c(SPY=0.4, TLT=0.3, GLD=0.3),
#                             EW_CORE  = equal_weight(c("SPY","TLT","GLD")))
#   rebal_freq    : passed to rebalance_portfolio() for all portfolios
#                   NULL (default) uses build_portfolio_returns() (no drift)
#   Rf            : risk-free rate for Sharpe (default 0)
#   print_stats   : if TRUE, prints portfolio_stats() comparison (default TRUE)
#
# OUTPUT
#   Multi-column xts with one column per portfolio, aligned on common dates.
#   Also invisibly returns the stats tibble.
# ==============================================================================

compare_portfolios <- function(xts_returns,
                                portfolio_list,
                                rebal_freq  = NULL,
                                Rf          = 0,
                                print_stats = TRUE) {

  if (is.null(names(portfolio_list)) || any(names(portfolio_list) == ""))
    stop("`portfolio_list` must be a fully named list.")

  # Build each portfolio
  port_list <- imap(portfolio_list, function(weights, nm) {
    if (!is.null(rebal_freq)) {
      rebalance_portfolio(xts_returns, weights, rebal_freq, port_name = nm)
    } else {
      build_portfolio_returns(xts_returns, weights, port_name = nm)
    }
  })

  # Align on common dates
  combined <- Reduce(function(a, b) {
    common <- intersect(index(a), index(b))
    cbind(a[common], b[common])
  }, port_list)

  # Print comparison stats
  if (print_stats) {
    stats <- portfolio_stats(combined, Rf = Rf)
    cat("\n── Portfolio Comparison ────────────────────────────────────────────────\n")
    stats_fmt <- stats %>%
      mutate(
        ann_return   = scales::percent(ann_return,   accuracy = 0.1),
        volatility   = scales::percent(volatility,   accuracy = 0.1),
        sharpe       = round(sharpe, 2),
        max_dd       = scales::percent(max_dd,       accuracy = 0.1),
        calmar       = round(calmar, 2),
        total_return = scales::percent(total_return, accuracy = 0.1)
      )
    print(stats_fmt, n = Inf)
    cat("────────────────────────────────────────────────────────────────────────\n")
    return(invisible(list(returns = combined, stats = stats)))
  }

  invisible(list(returns = combined, stats = portfolio_stats(combined, Rf = Rf)))
}


################################################################################
# QUICK SMOKE TEST (runs only when script is sourced directly)
################################################################################

if (!exists("xts_ret")) {
  xts_ret <- readRDS(here::here("02_data_processed/xts_ret_returns.rds"))
}

# ── equal_weight helper ───────────────────────────────────────────────────────
ew_core <- equal_weight(c("SPY", "TLT", "GLD", "HYG", "IEF"))
cat("Equal weights:\n"); print(ew_core)

# ── build a buy-and-hold portfolio ────────────────────────────────────────────
risk_bal <- build_portfolio_returns(
  xts_ret,
  weights   = c(SPY = 0.40, TLT = 0.25, GLD = 0.15, HYG = 0.10, IEF = 0.10),
  port_name = "RISK_BAL"
)

# ── blend two portfolios 60/40 ────────────────────────────────────────────────
ew_port  <- build_portfolio_returns(xts_ret, ew_core, "EW_CORE")
blended  <- build_portfolio_returns(
  cbind(risk_bal, ew_port),
  weights   = c(RISK_BAL = 0.6, EW_CORE = 0.4),
  port_name = "BLENDED"
)

# ── rebalanced variant ────────────────────────────────────────────────────────
risk_bal_qtr <- rebalance_portfolio(
  xts_ret,
  weights    = c(SPY = 0.40, TLT = 0.25, GLD = 0.15, HYG = 0.10, IEF = 0.10),
  rebal_freq = "quarterly",
  port_name  = "RISK_BAL_QTR"
)

# ── compare all portfolios ────────────────────────────────────────────────────
comparison <- compare_portfolios(
  xts_ret,
  portfolio_list = list(
    RISK_BAL  = c(SPY = 0.40, TLT = 0.25, GLD = 0.15, HYG = 0.10, IEF = 0.10),
    EW_CORE   = ew_core,
    SPY_BENCH = c(SPY = 1.0)
  ),
  print_stats = TRUE
)

################################################################################
