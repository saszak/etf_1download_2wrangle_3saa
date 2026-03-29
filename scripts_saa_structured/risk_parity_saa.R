################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE:    scripts_saa_structured/risk_parity_saa.R
# Purpose: Standalone Risk Parity SAA
#          Equal Risk Contribution (ERC) applied separately within
#          EQ and FI sleeves, then combined into a full portfolio.
#
# REQUIRES: xts_ret in environment (run master_run.R first)
################################################################################

library(tidyverse)
library(PerformanceAnalytics)
library(xts)

# ==============================================================================
# CORE FUNCTION: solve_erc()
# ==============================================================================
# Finds weights such that every asset contributes equally to portfolio variance.
# Uses nlminb optimizer — no extra packages required.
#
# INPUTS:
#   sigma    : covariance matrix (annualized)
#   tol      : convergence tolerance
#
# OUTPUT: named numeric vector of ERC weights (sums to 1)
# ==============================================================================

solve_erc <- function(sigma, tol = 1e-8) {

  n <- nrow(sigma)

  # Objective: minimize sum of squared differences in risk contributions
  obj <- function(w) {
    w      <- abs(w)                        # enforce positivity
    port_var <- as.numeric(t(w) %*% sigma %*% w)
    if (port_var <= 0) return(1e10)
    rc     <- w * (sigma %*% w) / sqrt(port_var)   # risk contributions
    target <- sqrt(port_var) / n                    # equal share
    sum((rc - target)^2)
  }

  # Start from equal weights
  w0 <- rep(1 / n, n)

  result <- nlminb(
    start     = w0,
    objective = obj,
    lower     = rep(1e-6, n),   # long only
    upper     = rep(1.0,  n),
    control   = list(eval.max = 2000, iter.max = 2000, rel.tol = tol)
  )

  w_out <- abs(result$par)
  w_out / sum(w_out)   # normalize to sum to 1
}


# ==============================================================================
# CORE FUNCTION: build_risk_parity_saa()
# ==============================================================================
#
# INPUTS:
#   xts_ret      : full xts of daily returns
#   eq_tickers   : character vector of equity universe ETFs
#   fi_tickers   : character vector of fixed income universe ETFs
#   sleeve_eq    : total portfolio weight for EQ sleeve (default 0.50)
#   sleeve_fi    : total portfolio weight for FI sleeve (default 0.50)
#   lookback     : integer trading days for covariance estimation (default 252)
#
# OUTPUTS: list with
#   $weights_df  : ticker-level weight table
#   $w_vec       : named numeric vector for calc_saa_portfolio()
#   $eq_weights  : ERC weights within EQ sleeve
#   $fi_weights  : ERC weights within FI sleeve
#   $diagnostics : risk contribution table (verify equal contribution)
#
# ==============================================================================

build_risk_parity_saa <- function(xts_ret,
                                   eq_tickers,
                                   fi_tickers,
                                   sleeve_eq = 0.50,
                                   sleeve_fi = 0.50,
                                   lookback  = 252) {

  if (abs(sleeve_eq + sleeve_fi - 1) > 0.001)
    stop("sleeve_eq + sleeve_fi must equal 1.0")

  # ── Data window ──────────────────────────────────────────────────────────────
  xts_window <- tail(xts_ret, lookback)

  # ── Helper: solve ERC for a ticker subset ────────────────────────────────────
  erc_for_sleeve <- function(tickers, sleeve_label) {
    avail <- intersect(tickers, colnames(xts_window))
    miss  <- setdiff(tickers, avail)
    if (length(miss) > 0)
      warning(sleeve_label, " — tickers not in xts_ret: ", paste(miss, collapse = ", "))

    sigma  <- cov(as.matrix(xts_window[, avail])) * 252  # annualized covariance
    w_erc  <- solve_erc(sigma)
    setNames(w_erc, avail)
  }

  eq_w <- erc_for_sleeve(eq_tickers, "EQ")
  fi_w <- erc_for_sleeve(fi_tickers, "FI")

  # ── Scale within-sleeve weights to portfolio level ───────────────────────────
  eq_portfolio_w <- eq_w * sleeve_eq
  fi_portfolio_w <- fi_w * sleeve_fi

  # ── Weight table ─────────────────────────────────────────────────────────────
  weights_df <- bind_rows(
    tibble(sleeve = "EQ", ticker = names(eq_portfolio_w),
           sleeve_weight = as.numeric(eq_w),
           weight        = as.numeric(eq_portfolio_w)),
    tibble(sleeve = "FI", ticker = names(fi_portfolio_w),
           sleeve_weight = as.numeric(fi_w),
           weight        = as.numeric(fi_portfolio_w))
  )

  w_vec <- setNames(weights_df$weight, weights_df$ticker)

  # ── Diagnostics: verify risk contributions are equal ─────────────────────────
  diag_sleeve <- function(tickers_w, sleeve_label) {
    avail  <- names(tickers_w)
    sigma  <- cov(as.matrix(xts_window[, avail])) * 252
    w      <- as.numeric(tickers_w)
    port_var <- as.numeric(t(w) %*% sigma %*% w)
    rc     <- w * as.numeric(sigma %*% w) / sqrt(port_var)
    tibble(
      sleeve = sleeve_label,
      ticker = avail,
      weight = w,
      risk_contribution     = rc,
      risk_contribution_pct = rc / sum(rc)
    )
  }

  diagnostics <- bind_rows(
    diag_sleeve(eq_w, "EQ"),
    diag_sleeve(fi_w, "FI")
  )

  message("── Risk Parity SAA: Weight Summary ──────────────────────────")
  message(sprintf("   Lookback : %d trading days", lookback))
  message(sprintf("   EQ tickers: %s", paste(names(eq_w), collapse = ", ")))
  message(sprintf("   FI tickers: %s", paste(names(fi_w), collapse = ", ")))
  message(sprintf("   Total Weight: %.4f", sum(w_vec)))
  message("── Risk Contributions (should be ~equal within each sleeve) ─")
  print(
    diagnostics %>%
      mutate(
        weight                = scales::percent(weight, accuracy = 0.01),
        risk_contribution_pct = scales::percent(risk_contribution_pct, accuracy = 0.01)
      ) %>%
      select(sleeve, ticker, weight, risk_contribution_pct)
  )
  message("─────────────────────────────────────────────────────────────")

  list(
    weights_df  = weights_df,
    w_vec       = w_vec,
    eq_weights  = eq_w,
    fi_weights  = fi_w,
    diagnostics = diagnostics
  )
}


# ==============================================================================
# EXAMPLE: Define Universes and Run
# ==============================================================================

# ── EQ Universe (Enhancers) ───────────────────────────────────────────────────
eq_universe <- c("SPY", "EQQQ", "XLK", "IEFA", "XLF", "XLI")

# ── FI Universe (Stabilizers) ────────────────────────────────────────────────
fi_universe <- c("AGG", "LQD", "IEF", "TLT", "TIP", "HYG")

# ── Build Risk Parity Portfolio ───────────────────────────────────────────────
rp_saa <- build_risk_parity_saa(
  xts_ret    = xts_ret,
  eq_tickers = eq_universe,
  fi_tickers = fi_universe,
  sleeve_eq  = 0.50,
  sleeve_fi  = 0.50,
  lookback   = 252
)

# ── View weights ──────────────────────────────────────────────────────────────
rp_saa$weights_df


# ==============================================================================
# PERFORMANCE AUDIT vs 50/50 Benchmark
# ==============================================================================

source("scripts_saa_taa/05_saa_portfolio.R")

# --- Risk Parity Portfolio ---
rp_result <- calc_saa_portfolio(
  xts_returns    = xts_ret,
  weights_vector = rp_saa$w_vec,
  rebalance_freq = "quarters"
)

# --- 50/50 Benchmark ---
bmk_5050 <- calc_saa_portfolio(
  xts_returns    = xts_ret,
  weights_vector = c(SPY = 0.50, AGG = 0.50),
  rebalance_freq = "quarters"
)

# --- Combine ---
audit_xts <- merge(rp_result$returns, bmk_5050$returns)
colnames(audit_xts) <- c("Risk_Parity_SAA", "Benchmark_50_50")

# --- Performance Summary ---
charts.PerformanceSummary(
  audit_xts,
  main       = "Risk Parity SAA vs 50/50 Benchmark",
  colorset   = c("#8e44ad", "#e74c3c"),
  lwd        = c(3, 1),
  legend.loc = "topleft"
)

# --- Stats Table ---
table.AnnualizedReturns(audit_xts, scale = 252)

################################################################################
