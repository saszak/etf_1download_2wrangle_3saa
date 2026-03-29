################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : utility/portfolio_utility.R
# Purpose : Portfolio construction utilities — weighting, blending, rebalancing,
#           performance stats, and multi-portfolio comparison.
#
# FUNCTIONS
#   equal_weight()              Named equal-weight vector from ticker vector
#   build_portfolio_returns()   Weighted daily return series from any xts input
#   rebalance_portfolio()       Periodic rebalancing (monthly/quarterly/annual)
#   portfolio_stats()           Ann return, Vol, Sharpe, MaxDD, Calmar
#   compare_portfolios()        Build & compare multiple portfolios side-by-side
#
# WEIGHTS: named or unnamed numeric vector. If unnamed, weights are matched
# positionally to the columns of xts_returns (or the pre-subsetted slice).
################################################################################

library(xts)
library(PerformanceAnalytics)
library(tidyverse)


# ==============================================================================
# INTERNAL HELPER: resolve weights → named vector aligned to xts columns
# ==============================================================================
.resolve_weights <- function(weights, xts_returns) {
  if (!is.null(names(weights))) {
    # Named: validate names exist
    missing_tk <- setdiff(names(weights), colnames(xts_returns))
    if (length(missing_tk) > 0)
      stop("Not found in xts_returns: ", paste(missing_tk, collapse = ", "))
    tickers <- names(weights)
  } else {
    # Unnamed: positional match to columns
    if (length(weights) != ncol(xts_returns))
      stop("Unnamed weights length (", length(weights), ") must equal ncol(xts_returns) (",
           ncol(xts_returns), ").")
    tickers <- colnames(xts_returns)
    names(weights) <- tickers
  }
  w <- as.numeric(weights) / sum(as.numeric(weights))
  names(w) <- tickers
  w
}


# ==============================================================================
# 1. equal_weight()
# ==============================================================================
# Returns a named equal-weight vector summing to 1.
#
# EXAMPLE
#   equal_weight(c("SPY", "TLT", "GLD"))
#   # SPY       TLT       GLD
#   # 0.3333333 0.3333333 0.3333333
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
# Weighted daily return series. Weights may be named or unnamed (positional).
# Pass a pre-subsetted xts to use unnamed weights.
#
# EXAMPLES
#   # Named weights — subset happens inside
#   build_portfolio_returns(xts_ret, c(SPY = 0.6, IEF = 0.4), "6040")
#
#   # Unnamed weights — pre-subset the xts
#   build_portfolio_returns(xts_ret[, c("SPY","IEF")], c(0.6, 0.4), "6040")
#
#   # Blend two portfolios
#   pa <- build_portfolio_returns(xts_ret, c(SPY=0.6, TLT=0.4), "PA")
#   pb <- build_portfolio_returns(xts_ret, c(GLD=0.5, HYG=0.5), "PB")
#   build_portfolio_returns(cbind(pa, pb), c(0.7, 0.3), "BLEND")
# ==============================================================================

build_portfolio_returns <- function(xts_returns, weights, port_name = "PORTFOLIO") {

  w       <- .resolve_weights(weights, xts_returns)
  sub_ret <- na.omit(xts_returns[, names(w)])

  if (nrow(sub_ret) == 0)
    stop("No complete rows after na.omit() — check constituent date ranges.")

  port_ret <- xts(as.numeric(as.matrix(sub_ret) %*% w), order.by = index(sub_ret))
  colnames(port_ret) <- port_name
  port_ret
}


# ==============================================================================
# 3. rebalance_portfolio()
# ==============================================================================
# Like build_portfolio_returns() but with periodic rebalancing (positions drift
# between rebalance dates). Uses PerformanceAnalytics::Return.portfolio().
# Transaction costs are NOT modelled.
#
# EXAMPLES
#   # Named weights
#   rebalance_portfolio(xts_ret, c(SPY=0.6, IEF=0.4), "quarterly", "6040_QTR")
#
#   # Unnamed weights — pre-subset the xts
#   rebalance_portfolio(xts_ret[, c("SPY","IEF")], c(0.6, 0.4), "monthly", "6040_M")
# ==============================================================================

rebalance_portfolio <- function(xts_returns,
                                weights,
                                rebal_freq = "monthly",
                                port_name  = "PORTFOLIO") {

  w          <- .resolve_weights(weights, xts_returns)
  sub_ret    <- na.omit(xts_returns[, names(w)])
  rebal_freq <- match.arg(rebal_freq, c("daily", "monthly", "quarterly", "annual"))

  rebal_dates <- switch(rebal_freq,
    daily     = index(sub_ret),
    monthly   = index(sub_ret)[endpoints(sub_ret, on = "months")],
    quarterly = index(sub_ret)[endpoints(sub_ret, on = "quarters")],
    annual    = index(sub_ret)[endpoints(sub_ret, on = "years")]
  )

  w_mat <- xts(
    matrix(0, nrow = nrow(sub_ret), ncol = length(w),
           dimnames = list(NULL, names(w))),
    order.by = index(sub_ret)
  )
  w_mat[rebal_dates, ] <- matrix(rep(w, length(rebal_dates)),
                                  nrow = length(rebal_dates), byrow = TRUE)

  port_ret <- Return.portfolio(sub_ret, weights = w_mat, rebalance_on = NULL)
  colnames(port_ret) <- port_name
  port_ret
}


# ==============================================================================
# 4. portfolio_stats()
# ==============================================================================
# Summary tibble for one or more portfolio return series.
# Columns: name, ann_return, volatility, sharpe, max_dd, calmar,
#          total_return, start_date, end_date, n_days
#
# EXAMPLE
#   pf <- build_portfolio_returns(xts_ret[, c("SPY","IEF")], c(0.6, 0.4), "6040")
#   portfolio_stats(pf)
#   portfolio_stats(cbind(pf, spy_bench))   # multi-portfolio
# ==============================================================================

portfolio_stats <- function(port_ret, Rf = 0, ann_factor = 252) {

  stopifnot(is.xts(port_ret))

  map_dfr(colnames(port_ret), function(nm) {
    r      <- na.omit(port_ret[, nm])
    ann_r  <- as.numeric(Return.annualized(r, scale = ann_factor))
    vol    <- as.numeric(StdDev.annualized(r, scale = ann_factor))
    sharpe <- as.numeric(SharpeRatio.annualized(r, Rf = Rf, scale = ann_factor))
    mdd    <- as.numeric(maxDrawdown(r))
    tibble(
      name         = nm,
      ann_return   = ann_r,
      volatility   = vol,
      sharpe       = sharpe,
      max_dd       = mdd,
      calmar       = if (mdd > 0) ann_r / mdd else NA_real_,
      total_return = as.numeric(Return.cumulative(r)),
      start_date   = as.Date(start(r)),
      end_date     = as.Date(end(r)),
      n_days       = nrow(r)
    )
  })
}


# ==============================================================================
# 5. compare_portfolios()
# ==============================================================================
# Build multiple portfolios from a named list, align on common dates, and
# print a side-by-side stats table. Returns list($returns, $stats).
#
# EXAMPLE
#   compare_portfolios(
#     xts_ret,
#     list(
#       "60/40"    = c(SPY = 0.6, IEF = 0.4),
#       EW_CORE    = equal_weight(c("SPY","TLT","GLD","HYG","IEF")),
#       SPY_BENCH  = c(SPY = 1.0)
#     )
#   )
#
#   # With quarterly rebalancing
#   compare_portfolios(xts_ret, port_list, rebal_freq = "quarterly")
# ==============================================================================

compare_portfolios <- function(xts_returns,
                               portfolio_list,
                               rebal_freq  = NULL,
                               Rf          = 0,
                               print_stats = TRUE) {

  if (is.null(names(portfolio_list)) || any(names(portfolio_list) == ""))
    stop("`portfolio_list` must be a fully named list.")

  port_list <- imap(portfolio_list, function(weights, nm) {
    if (!is.null(rebal_freq))
      rebalance_portfolio(xts_returns, weights, rebal_freq, port_name = nm)
    else
      build_portfolio_returns(xts_returns, weights, port_name = nm)
  })

  combined <- Reduce(function(a, b) {
    common <- intersect(index(a), index(b))
    cbind(a[common], b[common])
  }, port_list)

  stats <- portfolio_stats(combined, Rf = Rf)

  if (print_stats) {
    cat("\n── Portfolio Comparison ────────────────────────────────────────────────\n")
    print(
      stats %>% mutate(
        ann_return   = scales::percent(ann_return,   accuracy = 0.1),
        volatility   = scales::percent(volatility,   accuracy = 0.1),
        sharpe       = round(sharpe, 2),
        max_dd       = scales::percent(max_dd,       accuracy = 0.1),
        calmar       = round(calmar, 2),
        total_return = scales::percent(total_return, accuracy = 0.1)
      ),
      n = Inf
    )
    cat("────────────────────────────────────────────────────────────────────────\n\n")
  }

  invisible(list(returns = combined, stats = stats))
}

################################################################################
# END OF FILE
################################################################################
