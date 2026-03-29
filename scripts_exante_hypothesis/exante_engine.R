################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE:    scripts_exante_hypothesis/exante_engine.R
# Purpose: Exante Hypothesis — Ticker Classification Engine
#
# ── HYPOTHESIS ────────────────────────────────────────────────────────────────
#   Given a Benchmark (BMK), every ticker can be classified exante as:
#
#     Dominant   : delta_ret > 0  AND  delta_dd > 0   — better return + less DD
#     Enhancer   : delta_ret > 0  AND  delta_dd ≤ 0   — more return at DD cost
#     Stabilizer : delta_ret ≤ 0  AND  delta_dd > 0   — less DD at return cost
#     Detractor  : delta_ret ≤ 0  AND  delta_dd ≤ 0   — worse on both dimensions
#
#   where:
#     delta_ret = Ann_Return(blend) - Ann_Return(bmk)
#     delta_dd  = MaxDD(blend) - MaxDD(bmk)     ← + means blend has LESS severe DD
#     delta_vol = Ann_Vol(blend) - Ann_Vol(bmk) ← - means blend is less volatile
#
#   Classification is horizon-sensitive: full sample, 1Q (63d), 1Y (252d).
#
# ── ANTI-DIVERSIFICATION INSIGHT ─────────────────────────────────────────────
#   Our L/S analysis showed: correlation ≈ 1 beats correlation ≈ 0 on DD cost.
#   This module tests that systematically in the long-only blended portfolio frame:
#
#   For a 50/50 blend(bmk, ticker):
#     - corr ≈ 1  → blend ≈ bmk    → DD preserved but not worsened
#     - corr ≈ 0  → two independent DD events possible → can WORSEN portfolio DD
#     - corr ≈ -1 → genuine DD hedge → rarest case for equity ETFs
#
#   Key: delta_dd is driven by ticker's STANDALONE DD (dd_tick), not just corr.
#   A zero-corr ticker with high standalone DD is a DD-cost trap.
#
# ── INPUTS REQUIRED IN ENVIRONMENT ───────────────────────────────────────────
#   xts_ret      — xts of winsorized daily log returns (from 01_etf_wrangle.R)
#   etf_metadata — tibble of ticker metadata (from 00_init_universe.R), optional
#
# ── KEY FUNCTIONS ─────────────────────────────────────────────────────────────
#   build_exante_metrics(xts_ret, bmk, tickers, horizons, w_blend)
#     → tibble: one row per ticker × horizon; all metrics + role classification
#
#   classify_role(delta_ret, delta_dd, ret_eps, risk_eps)
#     → character: "Dominant" | "Enhancer" | "Stabilizer" | "Detractor"
################################################################################

library(xts)
library(tidyverse)
library(PerformanceAnalytics)

# PerformanceAnalytics masks dplyr::select — restore the dplyr version
select <- dplyr::select

if (!exists("project_tree")) source(here::here("project_tree.R"))


# ==============================================================================
# SECTION 1: SCALAR HELPERS
# ==============================================================================

# Annualized return (compound, scaled to full year)
# Note: For short windows (1Q), this extrapolates aggressively — cross-period
# median smooths most of the noise.
.ann_ret <- function(x, scale = 252) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  (prod(1 + x) - 1) * (scale / length(x))
}

# Annualized volatility
.ann_vol <- function(x, scale = 252) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  if (length(x) < 2) return(NA_real_)
  sd(x) * sqrt(scale)
}

# Maximum drawdown (always ≤ 0; more negative = worse)
# Returns e.g. -0.30 for a 30% drawdown
.max_dd <- function(x) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  wealth <- cumprod(1 + x)
  min(wealth / cummax(wealth) - 1)
}

# Annualized Sharpe (rf = 0)
.sharpe <- function(x, scale = 252) {
  v <- .ann_vol(x, scale)
  if (is.na(v) || v == 0) return(NA_real_)
  .ann_ret(x, scale) / v
}


# ==============================================================================
# SECTION 2: ROLE CLASSIFICATION
# ==============================================================================
#
# Primary dimensions: delta_ret (return improvement) and delta_dd (DD improvement).
# Vol (delta_vol) is tracked but NOT used for classification — DD is the
# operational risk measure investors actually experience.
#
# Four quadrants:
#   Dominant   : delta_ret >  ret_eps  AND  delta_dd >  risk_eps  → top-right
#   Enhancer   : delta_ret >  ret_eps  AND  delta_dd ≤  risk_eps  → bottom-right
#   Stabilizer : delta_ret ≤  ret_eps  AND  delta_dd >  risk_eps  → top-left
#   Detractor  : delta_ret ≤  ret_eps  AND  delta_dd ≤  risk_eps  → bottom-left
#
# Thresholds (eps) exist to create a "neutral zone" near the origin.
# Default: 0.5 pp/year for return, 0.5 pp for DD.
# ==============================================================================

classify_role <- function(delta_ret, delta_dd,
                           ret_eps  = 0.005,
                           risk_eps = 0.005) {
  dplyr::case_when(
    delta_ret >  ret_eps  & delta_dd >  risk_eps ~ "Dominant",
    delta_ret >  ret_eps  & delta_dd <= risk_eps ~ "Enhancer",
    delta_ret <= ret_eps  & delta_dd >  risk_eps ~ "Stabilizer",
    TRUE                                          ~ "Detractor"
  )
}


# ==============================================================================
# SECTION 3: SINGLE-WINDOW METRIC COMPUTATION
# ==============================================================================

.window_metrics <- function(bmk_ret, tick_ret, w_blend = 0.5) {
  both <- na.omit(cbind(as.numeric(bmk_ret), as.numeric(tick_ret)))
  if (nrow(both) < 10) return(NULL)

  b  <- both[, 1]
  t  <- both[, 2]
  bl <- w_blend * b + (1 - w_blend) * t

  tibble(
    n_obs      = nrow(both),
    # ── Benchmark standalone ──────────────────────────────────────────────────
    ret_bmk    = .ann_ret(b),
    vol_bmk    = .ann_vol(b),
    dd_bmk     = .max_dd(b),
    sharpe_bmk = .sharpe(b),
    # ── Ticker standalone ─────────────────────────────────────────────────────
    ret_tick   = .ann_ret(t),
    vol_tick   = .ann_vol(t),
    dd_tick    = .max_dd(t),
    sharpe_tick = .sharpe(t),
    # ── Blended portfolio ─────────────────────────────────────────────────────
    ret_blend  = .ann_ret(bl),
    vol_blend  = .ann_vol(bl),
    dd_blend   = .max_dd(bl),
    sharpe_blend = .sharpe(bl),
    # ── Key: within-window correlation ───────────────────────────────────────
    corr_window = cor(b, t, use = "complete.obs"),
    # ── Improvement metrics ───────────────────────────────────────────────────
    # Positive = improves portfolio vs bmk alone
    delta_ret  = .ann_ret(bl)  - .ann_ret(b),
    delta_vol  = .ann_vol(bl)  - .ann_vol(b),   # negative = vol reduction
    delta_dd   = .max_dd(bl)   - .max_dd(b),    # positive = less severe DD
    delta_sharpe = .sharpe(bl) - .sharpe(b)
  )
}


# ==============================================================================
# SECTION 4: MAIN ENGINE — build_exante_metrics()
# ==============================================================================
#
# Returns one row per ticker × horizon with:
#   - Median metrics across all non-overlapping windows of that horizon
#   - Consistency stats: fraction of windows in each role
#   - Full-sample correlation (stable reference; less window noise than median)
#   - Role classification based on median delta_ret + median delta_dd
#
# INPUTS:
#   xts_ret   : xts with bmk column + ticker columns (daily log returns)
#   bmk       : benchmark ticker symbol, default "SPY"
#   tickers   : tickers to analyze (NULL = all columns except bmk)
#   horizons  : subset of c("full", "1Q", "1Y")
#   w_blend   : benchmark weight in the blended portfolio (1-w_blend → ticker)
#   ret_eps   : return improvement threshold for role classification (default 0.005)
#   risk_eps  : DD improvement threshold for role classification (default 0.005)
#
# ==============================================================================

build_exante_metrics <- function(xts_ret,
                                  bmk      = "SPY",
                                  tickers  = NULL,
                                  horizons = c("full", "1Q", "1Y"),
                                  w_blend  = 0.5,
                                  ret_eps  = 0.005,
                                  risk_eps = 0.005) {

  # ── Validate ─────────────────────────────────────────────────────────────────
  if (!(bmk %in% colnames(xts_ret)))
    stop(sprintf("Benchmark '%s' not found in xts_ret columns.", bmk))

  valid_horizons <- c("full", "1Q", "1Y")
  bad_h <- setdiff(horizons, valid_horizons)
  if (length(bad_h) > 0)
    stop("Unknown horizon(s): ", paste(bad_h, collapse = ", "),
         ". Use: 'full', '1Q', '1Y'")

  # ── Universe ──────────────────────────────────────────────────────────────────
  if (is.null(tickers)) {
    tickers <- setdiff(colnames(xts_ret), bmk)
  } else {
    missing_tk <- setdiff(tickers, colnames(xts_ret))
    if (length(missing_tk) > 0)
      warning("Tickers not found, dropped: ", paste(missing_tk, collapse = ", "))
    tickers <- intersect(tickers, colnames(xts_ret))
  }

  cat(sprintf(
    "\n── Exante Engine: bmk='%s' | %d tickers | horizons=[%s] | w_blend=%.0f%%\n",
    bmk, length(tickers), paste(horizons, collapse = ", "), w_blend * 100
  ))

  # ── Full-sample correlation (stable reference across all horizons) ───────────
  bmk_full <- as.numeric(xts_ret[, bmk])
  corr_full <- vapply(tickers, function(tk) {
    cor(bmk_full, as.numeric(xts_ret[, tk]), use = "pairwise.complete.obs")
  }, numeric(1))

  # ── Horizon window sizes (trading days) ──────────────────────────────────────
  horizon_sizes <- c(full = nrow(xts_ret), `1Q` = 63L, `1Y` = 252L)
  n_total       <- nrow(xts_ret)

  results <- list()

  for (h in horizons) {
    win_size <- as.integer(horizon_sizes[h])

    # Non-overlapping window start indices
    if (h == "full") {
      starts <- 1L
    } else {
      starts <- seq(1L, n_total - win_size + 1L, by = win_size)
    }

    for (tk in tickers) {
      bmk_col  <- xts_ret[, bmk]
      tick_col <- xts_ret[, tk]

      # Compute metrics for each window
      win_list <- lapply(starts, function(s) {
        e <- min(s + win_size - 1L, n_total)
        .window_metrics(bmk_col[s:e], tick_col[s:e], w_blend)
      })
      win_list <- Filter(Negate(is.null), win_list)

      if (length(win_list) == 0) next

      win_df <- bind_rows(win_list)

      # Median metrics across windows (robust to outlier periods)
      med <- win_df %>%
        summarise(across(everything(), ~ median(.x, na.rm = TRUE))) %>%
        rename(corr_median = corr_window)  # distinguish from corr_full

      # Per-window role distribution (consistency metric)
      win_roles <- win_df %>%
        mutate(role_w = classify_role(delta_ret, delta_dd, ret_eps, risk_eps)) %>%
        count(role_w) %>%
        mutate(pct = n / sum(n)) %>%
        dplyr::select(role_w, pct) %>%
        pivot_wider(names_from = role_w, values_from = pct, names_prefix = "pct_")

      # Ensure all four role columns exist (fill 0 if role never appeared)
      for (col in c("pct_Dominant", "pct_Enhancer", "pct_Stabilizer", "pct_Detractor")) {
        if (!(col %in% colnames(win_roles))) win_roles[[col]] <- 0
      }

      results[[length(results) + 1]] <- tibble(
        bmk         = bmk,
        ticker      = tk,
        horizon     = h,
        n_windows   = nrow(win_df),
        w_blend     = w_blend,
        corr_full   = corr_full[tk]
      ) %>%
        bind_cols(med) %>%
        bind_cols(win_roles) %>%
        mutate(
          role = classify_role(delta_ret, delta_dd, ret_eps, risk_eps),
          # Convenience: fraction of windows where ticker consistently acted as classifier
          consistency = pmax(
            pct_Dominant   %||% 0,
            pct_Enhancer   %||% 0,
            pct_Stabilizer %||% 0,
            pct_Detractor  %||% 0,
            na.rm = TRUE
          )
        )
    }

    cat(sprintf("  [%s] horizon='%s': %d tickers | %d windows each\n",
                bmk, h, length(tickers), length(starts)))
  }

  if (length(results) == 0) stop("No valid results. Check xts_ret data coverage.")

  tbl <- bind_rows(results)

  cat(sprintf("── Exante metrics complete: %d rows (%d tickers × %d horizons)\n",
              nrow(tbl), length(tickers), length(horizons)))

  tbl
}


# ==============================================================================
# SECTION 5: METADATA ENRICHMENT (OPTIONAL)
# ==============================================================================
#
# Joins etf_metadata columns onto the exante metrics table.
# Adds: asset_class, sub_block, pf_function, tree_level, geo
# Enables grouping in visualizations by asset class / portfolio function.
#
# USAGE:
#   exante_tbl <- enrich_with_metadata(exante_tbl, etf_metadata)
# ==============================================================================

enrich_with_metadata <- function(exante_tbl, etf_metadata) {
  meta_slim <- etf_metadata %>%
    dplyr::select(ticker, asset_class, tree_level, geo, sub_block, pf_function) %>%
    distinct(ticker, .keep_all = TRUE)

  exante_tbl %>%
    left_join(meta_slim, by = "ticker")
}


# ==============================================================================
# SECTION 6: SUMMARY HELPERS
# ==============================================================================

# Print a compact role summary for a given horizon
print_role_summary <- function(exante_tbl, horizon_filter = "full") {
  tbl <- exante_tbl %>% filter(horizon == horizon_filter)

  cat(sprintf("\n── Role Summary | bmk=%s | horizon=%s | n=%d tickers ──\n",
              unique(tbl$bmk), horizon_filter, nrow(tbl)))

  role_counts <- tbl %>%
    count(role) %>%
    arrange(desc(n))

  print(role_counts)

  cat("\nTop 5 Dominant (best of both worlds):\n")
  tbl %>%
    filter(role == "Dominant") %>%
    arrange(desc(delta_ret + delta_dd)) %>%
    dplyr::select(ticker, corr_full, delta_ret, delta_dd, delta_vol) %>%
    head(5) %>%
    mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
    print()

  cat("\nTop 5 Stabilizers (DD reduction):\n")
  tbl %>%
    filter(role == "Stabilizer") %>%
    arrange(desc(delta_dd)) %>%
    dplyr::select(ticker, corr_full, delta_ret, delta_dd, delta_vol) %>%
    head(5) %>%
    mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
    print()

  cat("\nTop 5 Enhancers (return improvement):\n")
  tbl %>%
    filter(role == "Enhancer") %>%
    arrange(desc(delta_ret)) %>%
    dplyr::select(ticker, corr_full, delta_ret, delta_dd, delta_vol) %>%
    head(5) %>%
    mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
    print()

  invisible(tbl)
}


# ==============================================================================
# SECTION 7: NULL-COALESCE HELPER (base R doesn't have %||%)
# ==============================================================================

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b
