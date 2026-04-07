################################################################################
# FILE    : scripts_daa_saa_taa/rebal_profile.R
# Purpose : For any binary portfolio (two tickers + target weights), compute
#           a full characteristic vector across three rebalancing frequencies
#           (Quarterly / Semi-Annual / Annual) plus Buy & Hold as baseline.
#
# MAIN FUNCTION
#   rebal_profile(ticker1, ticker2, w1, ...)
#   → tibble: 4 strategies × 12 metrics
#
# HELPERS
#   print_rebal_profile(x)   — formatted console table
#   best_rebal_freq(x, by)   — which frequency wins on a given metric
#
# USAGE
#   source(here("scripts_daa_saa_taa/rebal_profile.R"))
#   p <- rebal_profile("SPY", "IEF", w1 = 0.60)
#   print_rebal_profile(p)
#   best_rebal_freq(p, by = "Sharpe")
#
# METRICS
#   Final $1   terminal wealth from $1 invested
#   Ann Ret    compound annual growth rate (CAGR)
#   Ann Vol    annualised daily standard deviation
#   Max DD     peak-to-trough maximum drawdown
#   Sharpe     (Ann Ret - rf) / Ann Vol
#   Sortino    (Ann Ret - rf) / downside vol (semi-deviation below 0)
#   Calmar     Ann Ret / |Max DD|
#   Omega      sum(gains above 0) / sum(losses below 0)  [Keating & Shadwick]
#   VaR 5%     5th-percentile daily return
#   CVaR 5%    mean daily return conditional on r ≤ VaR 5%
#   Rebal N    number of rebalancing events over history
#   Avg Slip   mean absolute daily return on rebalancing days (proxy for slippage risk)
################################################################################

library(tidyverse)
library(xts)
library(here)

# ── Main function ──────────────────────────────────────────────────────────────

rebal_profile <- function(
    ticker1,
    ticker2,
    w1,
    w2      = 1 - w1,
    xts_ret = get("xts_ret", envir = parent.frame()),
    rf      = 0            # annual risk-free rate for Sharpe / Sortino
) {
  # ── Input validation ─────────────────────────────────────────────────────────
  if (!ticker1 %in% colnames(xts_ret))
    stop(sprintf("'%s' not found in xts_ret", ticker1))
  if (!ticker2 %in% colnames(xts_ret))
    stop(sprintf("'%s' not found in xts_ret", ticker2))
  if (abs(w1 + w2 - 1) > 1e-9)
    stop("w1 + w2 must equal 1")

  # ── Align and extract ────────────────────────────────────────────────────────
  common <- na.omit(merge(xts_ret[, ticker1], xts_ret[, ticker2]))
  r1     <- as.numeric(common[, ticker1])
  r2     <- as.numeric(common[, ticker2])
  dates  <- as.Date(index(common))
  n      <- length(dates)

  # Pre-compute period keys for boundary detection
  yr  <- as.integer(format(dates, "%Y"))
  mo  <- as.integer(format(dates, "%m"))
  qtr <- yr * 4L  + (mo - 1L) %/% 3L   # integer: changes at every quarter
  sem <- yr * 2L  + (mo - 1L) %/% 6L   # integer: changes every 6 months

  # ── Rebalancing engine ───────────────────────────────────────────────────────
  # Returns: list(cum, ret, rebal_days)
  .run <- function(freq) {
    w1_v <- numeric(n);  w2_v <- numeric(n)
    w1_v[1] <- w1;       w2_v[1] <- w2
    rebal_days <- integer(0)

    for (i in 2:n) {
      w1_v[i] <- w1_v[i-1] * (1 + r1[i])
      w2_v[i] <- w2_v[i-1] * (1 + r2[i])

      do_rebal <- switch(freq,
        quarterly  = qtr[i] != qtr[i-1],
        semiannual = sem[i] != sem[i-1],
        annual     = yr[i]  != yr[i-1],
        bah        = FALSE
      )

      if (do_rebal) {
        tot      <- w1_v[i] + w2_v[i]
        w1_v[i]  <- tot * w1
        w2_v[i]  <- tot * w2
        rebal_days <- c(rebal_days, i)
      }
    }

    total <- w1_v + w2_v
    list(
      cum        = total / total[1],
      ret        = c(NA, diff(log(total))),
      rebal_days = rebal_days
    )
  }

  # ── Statistics engine ────────────────────────────────────────────────────────
  .stats <- function(res, label) {
    cum <- res$cum
    ret <- res$ret[!is.na(res$ret)]
    rf_d <- rf / 252                         # daily risk-free rate

    # Return
    ann_ret  <- (tail(cum,1) / cum[1])^(252 / length(ret)) - 1

    # Volatility
    ann_vol  <- sd(ret) * sqrt(252)

    # Drawdown
    peak     <- cummax(cum)
    dd_series <- (cum - peak) / peak
    max_dd   <- min(dd_series)

    # Downside vol (semi-deviation, annualised)
    neg_ret  <- ret[ret < rf_d]
    down_vol <- if (length(neg_ret) > 1)
      sqrt(mean((neg_ret - rf_d)^2)) * sqrt(252)
    else NA_real_

    # VaR and CVaR at 5%
    var5   <- as.numeric(quantile(ret, 0.05))
    cvar5  <- mean(ret[ret <= var5])

    # Omega ratio (threshold = rf_d per day)
    gains  <- sum(pmax(ret - rf_d, 0))
    losses <- sum(pmax(rf_d - ret, 0))
    omega  <- if (losses > 0) gains / losses else Inf

    # Average absolute return on rebalancing days (slippage proxy)
    avg_slip <- if (length(res$rebal_days) > 0)
      mean(abs(ret[res$rebal_days - 1]))   # day before rebal = last drift day
    else 0

    tibble(
      Strategy   = label,
      `Final $1` = round(tail(cum,1), 3),
      `Ann Ret`  = round(ann_ret,     4),
      `Ann Vol`  = round(ann_vol,     4),
      `Max DD`   = round(max_dd,      4),
      Sharpe     = round((ann_ret - rf) / ann_vol,  3),
      Sortino    = round((ann_ret - rf) / down_vol, 3),
      Calmar     = round( ann_ret / abs(max_dd),    3),
      Omega      = round(omega, 3),
      `VaR 5%`   = round(var5,  4),
      `CVaR 5%`  = round(cvar5, 4),
      `Rebal N`  = length(res$rebal_days),
      `Avg Slip` = round(avg_slip, 5)
    )
  }

  # ── Run all frequencies ──────────────────────────────────────────────────────
  freqs <- list(
    "Quarterly"   = .run("quarterly"),
    "Semi-Annual" = .run("semiannual"),
    "Annual"      = .run("annual"),
    "Buy & Hold"  = .run("bah")
  )

  result <- purrr::map2_dfr(freqs, names(freqs), ~ .stats(.x, .y))

  # ── Attach metadata ──────────────────────────────────────────────────────────
  attr(result, "pair")    <- paste0(ticker1, "/", ticker2)
  attr(result, "weights") <- paste0(round(w1*100), "/", round(w2*100))
  attr(result, "n_days")  <- n
  attr(result, "from")    <- format(min(dates), "%Y-%m-%d")
  attr(result, "to")      <- format(max(dates), "%Y-%m-%d")
  attr(result, "rf")      <- rf

  result
}

# ── Print helper ───────────────────────────────────────────────────────────────
#   Formats percentages, dollar amounts, adds header with pair metadata.

print_rebal_profile <- function(x, digits = 1) {

  pct_cols <- c("Ann Ret", "Ann Vol", "Max DD", "VaR 5%", "CVaR 5%")
  fmt <- x

  fmt$`Final $1` <- sprintf("$%.2f",   x$`Final $1`)
  fmt$Sharpe     <- sprintf("%.2f",    x$Sharpe)
  fmt$Sortino    <- sprintf("%.2f",    x$Sortino)
  fmt$Calmar     <- sprintf("%.2f",    x$Calmar)
  fmt$Omega      <- sprintf("%.2f",    x$Omega)
  fmt$`Avg Slip` <- sprintf("%.3f%%",  x$`Avg Slip` * 100)
  for (col in pct_cols)
    fmt[[col]] <- sprintf(paste0("%.", digits, "f%%"), x[[col]] * 100)

  # Rank each numeric metric: ★ = best across the 3 rebalanced strategies
  rank_col <- function(col, higher_better = TRUE) {
    vals  <- x[[col]][x$Strategy != "Buy & Hold"]
    best  <- if (higher_better) which.max(vals) else which.min(vals)
    marks <- rep(" ", 4)
    marks[best] <- "\u2605"   # ★
    marks
  }

  stars <- mapply(rank_col,
    col = c("Ann Ret","Ann Vol","Max DD","Sharpe","Sortino","Calmar","Omega","VaR 5%","CVaR 5%"),
    higher_better = c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
    SIMPLIFY = FALSE
  )
  star_row <- do.call(paste0, stars)   # one combined star string per row

  fmt$Best <- star_row

  cat(sprintf(
    "\n%s\n  Pair: %s  |  Weights: %s  |  %s \u2192 %s  (%d days)\n%s\n",
    strrep("\u2550", 82),
    attr(x,"pair"), attr(x,"weights"), attr(x,"from"), attr(x,"to"), attr(x,"n_days"),
    strrep("\u2550", 82)
  ))
  if (attr(x,"rf") > 0)
    cat(sprintf("  Risk-free rate: %.2f%% p.a.\n", attr(x,"rf")*100))
  cat("\n")
  print(as.data.frame(fmt), row.names = FALSE)
  cat(sprintf("\n  \u2605 = best among rebalanced strategies (excludes Buy & Hold)\n"))
  cat(strrep("\u2550", 82), "\n\n")

  invisible(x)
}

# ── Best-frequency helper ──────────────────────────────────────────────────────
#   Returns the name of the best rebalancing frequency on a given metric.
#   Excludes Buy & Hold (since BaH is the baseline, not a rebalancing choice).

best_rebal_freq <- function(x, by = "Sharpe") {
  if (!by %in% colnames(x))
    stop(sprintf("Metric '%s' not found. Available: %s",
                 by, paste(colnames(x)[-1], collapse=", ")))

  higher_better <- !by %in% c("Ann Vol","Max DD","VaR 5%","CVaR 5%","Rebal N","Avg Slip")
  candidates    <- x[x$Strategy != "Buy & Hold", ]
  best_idx      <- if (higher_better) which.max(candidates[[by]]) else which.min(candidates[[by]])
  best          <- candidates$Strategy[best_idx]

  cat(sprintf("\nBest rebalancing frequency by %s: %s  (%s = %s)\n",
              by, best, by,
              ifelse(by %in% c("Ann Ret","Ann Vol","Max DD","VaR 5%","CVaR 5%"),
                     sprintf("%.1f%%", candidates[[by]][best_idx]*100),
                     sprintf("%.3f",   candidates[[by]][best_idx]))))
  invisible(best)
}
