################################################################################
# scripts_saa_taa/equity_spread_screen.R
# Purpose : Screen all equity tickers vs SPY across two dimensions:
#             A. Structural  — full-history IR → Evergreen / Conditional / Structural UW
#             B. Regime-based — per-regime (Fall/Recovery/Consolidation) mean alpha
#                               and outperformance type (HEDGE/ALPHA/STABLE/LAG/LOSS)
#
# RETURNS
#   equity_spread_screen()  → ranked tibble (one row per equity ticker)
#   print_spread_screen()   → formatted console summary table
#
# USAGE
#   source(here("scripts_saa_taa/equity_spread_screen.R"))
#   screen <- equity_spread_screen(xts_ret, xts_rel, etf_metadata, rt)
#   print_spread_screen(screen)
################################################################################

library(dplyr)
library(tibble)
library(purrr)
library(xts)
library(PerformanceAnalytics)
library(here)

if (!exists("build_regime_table"))
  source(here("scripts_spy_dd_regime/spy_dd_regime.R"))

if (!exists(".outperf_type"))
  source(here("scripts_spy_dd_regime/spy_dd_regime_rel.R"))

# ── Internal helpers ───────────────────────────────────────────────────────────

.ir <- function(spread_xts) {
  r <- as.numeric(spread_xts)
  r <- r[!is.na(r)]
  if (length(r) < 60) return(NA_real_)
  mu  <- mean(r) * 252
  sig <- sd(r)   * sqrt(252)
  if (sig == 0) return(NA_real_)
  mu / sig
}

.maxdd_spread <- function(spread_xts) {
  r <- spread_xts[!is.na(spread_xts)]
  if (length(r) < 2) return(NA_real_)
  cum  <- cumprod(1 + r)
  peak <- cummax(cum)
  dd   <- (cum - peak) / peak
  as.numeric(min(dd, na.rm = TRUE))
}

.period_ret_vec <- function(xts_col, date_from, date_to) {
  w <- xts_col[paste0(format(date_from, "%Y-%m-%d"), "/",
                       format(date_to,   "%Y-%m-%d"))]
  if (nrow(w) == 0) return(NA_real_)
  as.numeric(prod(1 + w, na.rm = TRUE) - 1)
}

.rolling_cor <- function(xts_ret, ticker, benchmark = "SPY", window = 252) {
  if (!all(c(ticker, benchmark) %in% colnames(xts_ret))) return(NA_real_)
  x <- as.numeric(xts_ret[, ticker])
  y <- as.numeric(xts_ret[, benchmark])
  complete <- !is.na(x) & !is.na(y)
  x <- x[complete]; y <- y[complete]
  if (length(x) < window) return(cor(x, y))
  # last-window correlation
  n <- length(x)
  cor(x[(n - window + 1):n], y[(n - window + 1):n])
}

# ── Main function ──────────────────────────────────────────────────────────────

#' equity_spread_screen
#'
#' @param xts_ret   xts of clean daily log returns (all tickers)
#' @param xts_rel   xts of relative returns vs SPY (from 01_etf_wrangle.R)
#' @param etf_metadata  tibble with ticker, asset_class columns
#' @param rt        regime table from build_regime_table()
#' @param benchmark benchmark ticker (default "SPY")
#' @param rho_screen minimum full-history correlation for leeway validity (default 0.75)
#'
#' @return tibble ranked by IR descending, one row per equity ticker

equity_spread_screen <- function(xts_ret,
                                  xts_rel,
                                  etf_metadata,
                                  rt,
                                  benchmark    = "SPY",
                                  rho_screen   = 0.75) {

  # ── 1. Equity tickers available in xts_rel ──────────────────────────────────
  eq_tickers <- etf_metadata %>%
    filter(asset_class == "Equity", ticker != benchmark) %>%
    pull(ticker) %>%
    intersect(colnames(xts_rel)) %>%
    intersect(colnames(xts_ret))

  if (length(eq_tickers) == 0) stop("No equity tickers found in xts_rel.")

  message("Screening ", length(eq_tickers), " equity tickers vs ", benchmark, "...")

  # ── 2. Per-ticker structural metrics ────────────────────────────────────────
  structural <- map_dfr(eq_tickers, function(tk) {

    spread <- xts_rel[, tk]

    tibble(
      ticker      = tk,
      ir          = .ir(spread),
      rho_full    = cor(as.numeric(xts_ret[, tk]),
                        as.numeric(xts_ret[, benchmark]),
                        use = "complete.obs"),
      rho_1y      = .rolling_cor(xts_ret, tk, benchmark, window = 252),
      maxdd_full  = .maxdd_spread(spread)
    )
  })

  # ── 3. Per-regime alpha and type ─────────────────────────────────────────────
  regime_stats <- map_dfr(eq_tickers, function(tk) {

    tk_xts  <- xts_ret[, tk]
    bmk_xts <- xts_ret[, benchmark]

    rt %>%
      rowwise() %>%
      mutate(
        ticker   = tk,
        raw_ret  = .period_ret_vec(tk_xts,  xmin, xmax),
        spy_ret  = .period_ret_vec(bmk_xts, xmin, xmax),
        alpha    = raw_ret - spy_ret,
        type     = .outperf_type(raw_ret, spy_ret, alpha)
      ) %>%
      ungroup() %>%
      select(ticker, regime, alpha, type, raw_ret, spy_ret)
  })

  # Per-regime summary: mean alpha, modal type, MaxDD of spread within regime
  regime_summary <- regime_stats %>%
    group_by(ticker, regime) %>%
    summarise(
      mean_alpha  = mean(alpha,   na.rm = TRUE),
      modal_type  = {
        tbl <- sort(table(type), decreasing = TRUE)
        names(tbl)[1]
      },
      maxdd_regime = min(alpha, na.rm = TRUE),   # worst single episode alpha
      n_episodes   = n(),
      .groups = "drop"
    )

  # Pivot to wide: one row per ticker
  regime_wide <- regime_summary %>%
    tidyr::pivot_wider(
      id_cols     = ticker,
      names_from  = regime,
      values_from = c(mean_alpha, modal_type, maxdd_regime),
      names_glue  = "{regime}_{.value}"
    )

  # ── 4. Structural classification ─────────────────────────────────────────────
  screen <- structural %>%
    left_join(regime_wide, by = "ticker") %>%
    mutate(
      # Correlation gate
      rho_valid = rho_full >= rho_screen,

      # Structural classification based on IR
      classification = case_when(
        !rho_valid            ~ "Corr-Fail",
        ir >= 0.40            ~ "Evergreen Long",
        ir >= 0.10            ~ "Conditional",
        ir >= 0.00            ~ "Weak",
        TRUE                  ~ "Structural UW"
      ),

      # Regime consistency score: count of regimes where modal_type is ALPHA or HEDGE
      regime_score = rowSums(
        across(ends_with("modal_type"),
               ~ .x %in% c("ALPHA", "HEDGE", "STABLE")),
        na.rm = TRUE
      )
    ) %>%
    arrange(desc(ir))

  screen
}


# ── Print helper ───────────────────────────────────────────────────────────────

#' print_spread_screen
#' Prints a clean ranked summary table to the console.

print_spread_screen <- function(screen, top_n = NULL) {

  df <- screen
  if (!is.null(top_n)) df <- df %>% slice_head(n = top_n)

  # Regime type columns (flexible — picks whatever regime columns exist)
  type_cols <- names(df)[grepl("_modal_type$", names(df))]
  regimes   <- sub("_modal_type$", "", type_cols)

  cat("\n══ EQUITY SPREAD SCREEN vs SPY ══════════════════════════════════════════\n")
  cat(sprintf("  %-8s %-20s %6s %6s %8s", "Ticker", "Classification", "IR", "ρ(full)", "MaxDD"))
  for (r in regimes) cat(sprintf("  %-12s", r))
  cat("\n")
  cat(strrep("─", 70 + length(regimes) * 14), "\n")

  for (i in seq_len(nrow(df))) {
    r_row <- df[i, ]
    rho_flag  <- if (!r_row$rho_valid) " ✗" else "  "
    cat(sprintf("  %-8s %-20s %6.2f %6.3f %8.1f%%%s",
                r_row$ticker,
                r_row$classification,
                ifelse(is.na(r_row$ir),       0, r_row$ir),
                ifelse(is.na(r_row$rho_full), 0, r_row$rho_full),
                ifelse(is.na(r_row$maxdd_full), 0, r_row$maxdd_full * 100),
                rho_flag))
    for (rg in regimes) {
      col_type <- paste0(rg, "_modal_type")
      col_alpha <- paste0(rg, "_mean_alpha")
      typ <- if (col_type  %in% names(r_row)) as.character(r_row[[col_type]])  else "—"
      alp <- if (col_alpha %in% names(r_row)) as.numeric(r_row[[col_alpha]])   else NA
      tag <- if (!is.na(alp))
        sprintf("%-6s %+.1f%%", ifelse(is.na(typ), "—", typ), alp * 100)
      else "—"
      cat(sprintf("  %-12s", tag))
    }
    cat("\n")
  }

  cat(strrep("─", 70 + length(regimes) * 14), "\n")
  cat(sprintf("  %d tickers  |  Evergreen: %d  |  Conditional: %d  |  Structural UW: %d  |  Corr-Fail: %d\n",
              nrow(df),
              sum(df$classification == "Evergreen Long",  na.rm = TRUE),
              sum(df$classification == "Conditional",     na.rm = TRUE),
              sum(df$classification == "Structural UW",   na.rm = TRUE),
              sum(df$classification == "Corr-Fail",       na.rm = TRUE)))
  cat("═══════════════════════════════════════════════════════════════════════════\n\n")

  invisible(df)
}


# ── Auto-run guard ────────────────────────────────────────────────────────────

if (!isTRUE(getOption("knitr.in.progress"))) {

  if (!exists("xts_ret"))
    xts_ret <- readRDS(here("02_data_processed/xts_ret_returns.rds"))
  if (!exists("xts_rel"))
    xts_rel <- readRDS(here("02_data_processed/xts_rel.rds"))
  if (!exists("etf_metadata"))
    source(here("scripts/00_init_universe.R"))
  if (!exists("rt") || !is.data.frame(rt))
    rt <- build_regime_table(xts_ret[, "SPY"])

  screen <- equity_spread_screen(xts_ret, xts_rel, etf_metadata, rt)
  print_spread_screen(screen)
}
