################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : scripts_saa_taa/taa_regime_engine.R
# Purpose : TAA regime-driven decision engine
#
# FUNCTIONS (built incrementally — Step 1 only)
#   ticker_long_short(ticker, bmk, xts_ret, rt)
#     → deep L/S analysis for one ticker vs benchmark
#     → returns $summary (1 row) + $regime_tbl (3 rows, one per regime)
#     → includes confidence score per regime
#
# DEPENDS ON
#   scripts_spy_dd_regime/spy_dd_regime.R        — build_regime_table(), REGIME_PAL
#   scripts_spy_dd_regime/spy_dd_regime_rel.R    — .outperf_type()
#   scripts_spy_dd_regime/regime_multi_ticker.R  — .period_ret()
#
# CONFIDENCE SCORE (per regime)
#   Three components, combined into a 0-1 score → tier (High/Medium/Low/Weak)
#
#   1. t-stat (weight 50%) — is avg alpha significantly ≠ 0?
#        t = avg_alpha / (sd_alpha / sqrt(n_episodes))
#        capped at t = 2.0 (anything above is treated as equally strong)
#
#   2. Consistency (weight 30%) — pct of episodes showing the modal type
#        1.0 = every episode showed the same type → high consistency
#        0.5 = coin flip → low consistency
#
#   3. Sample size (weight 20%) — penalise priors with < 6 episodes
#        n_weight = min(n_episodes / 6, 1)
#
#   conf_score = 0.50 * min(|t|/2, 1) + 0.30 * pct_correct + 0.20 * n_weight
#   High   >= 0.75
#   Medium >= 0.50
#   Low    >= 0.30
#   Weak    < 0.30  (data insufficient)
################################################################################

library(tidyverse)
library(xts)
library(PerformanceAnalytics)
library(here)

if (!exists("build_regime_table"))
  source(here("scripts_spy_dd_regime/spy_dd_regime.R"))

if (!exists(".outperf_type"))
  source(here("scripts_spy_dd_regime/spy_dd_regime_rel.R"))

if (!exists(".period_ret"))
  source(here("scripts_spy_dd_regime/regime_multi_ticker.R"))


# ==============================================================================
# ticker_long_short()
# ==============================================================================
#
# @param ticker   Character — ticker to analyse (the "Long" leg)
# @param bmk      Character — benchmark ticker (the "Short" leg, default "SPY")
# @param xts_ret  xts      — daily returns, all tickers
# @param rt       data.frame — regime table from build_regime_table(); rebuilt
#                              from bmk if NULL (pass pre-built rt for speed)
# @param t_fall   Numeric  — Fall threshold (default 0.10)
# @param t_cruise Numeric  — Cruise threshold (default 0.05)
# @param rho_min  Numeric  — minimum ρ for valid L/S pair (default 0.75)
#
# @return list with:
#   $summary    tibble (1 row):  ticker, bmk, ir, rho, rho_1y, max_dd, archetype
#   $regime_tbl tibble (3 rows): regime, modal_type, avg_alpha, sd_alpha, t_stat,
#                                n_episodes, pct_correct, conf_score, confidence
#
# ==============================================================================

ticker_long_short <- function(ticker,
                               bmk      = "SPY",
                               xts_ret,
                               rt       = NULL,
                               t_fall   = 0.10,
                               t_cruise = 0.05,
                               rho_min  = 0.75) {

  # ── Validate inputs ───────────────────────────────────────────────────────────
  if (!ticker %in% colnames(xts_ret))
    stop(sprintf("ticker '%s' not found in xts_ret.", ticker))
  if (!bmk %in% colnames(xts_ret))
    stop(sprintf("bmk '%s' not found in xts_ret.", bmk))
  if (ticker == bmk)
    stop("ticker and bmk must be different.")

  # ── Build regime table if not supplied ────────────────────────────────────────
  if (is.null(rt))
    rt <- build_regime_table(xts_ret[, bmk], t_fall, t_cruise)

  tk_r  <- xts_ret[, ticker]
  bmk_r <- xts_ret[, bmk]

  # Align to common non-NA period
  both   <- which(!is.na(as.numeric(tk_r)) & !is.na(as.numeric(bmk_r)))
  if (length(both) < 252)
    stop(sprintf("Insufficient history for %s vs %s (need ≥ 252 obs, have %d).",
                 ticker, bmk, length(both)))

  r_tk   <- as.numeric(tk_r)[both]
  r_bmk  <- as.numeric(bmk_r)[both]
  spread <- r_tk - r_bmk

  # ── Full-history summary statistics ──────────────────────────────────────────
  rho    <- cor(r_tk, r_bmk)

  # Ticker: annualised return, vol, MaxDD
  ann_ret_ticker <- mean(r_tk)  * 252
  ann_vol_ticker <- sd(r_tk)    * sqrt(252)
  max_dd_ticker  <- -as.numeric(maxDrawdown(tk_r))

  # Spread: annualised return (= excess return), vol, IR, MaxDD
  ann_ret_spread <- mean(spread) * 252
  ann_vol_spread <- sd(spread)   * sqrt(252)
  ir             <- if (ann_vol_spread > 0) ann_ret_spread / ann_vol_spread else NA_real_

  spread_xts     <- tk_r[index(bmk_r)] - bmk_r[index(tk_r)]
  spread_xts     <- spread_xts[!is.na(spread_xts)]
  max_dd_spread  <- -as.numeric(maxDrawdown(spread_xts))

  # 1-year rolling correlation (last 252 trading days)
  n      <- length(r_tk)
  rho_1y <- if (n >= 252) cor(tail(r_tk, 252), tail(r_bmk, 252)) else rho

  # ── Per-episode regime classification ────────────────────────────────────────
  ep_df <- rt %>%
    rowwise() %>%
    mutate(
      raw_ret  = .period_ret(tk_r,  xmin, xmax),
      bmk_ret  = .period_ret(bmk_r, xmin, xmax),
      alpha    = raw_ret - bmk_ret,
      ep_type  = .outperf_type(raw_ret, bmk_ret, alpha)
    ) %>%
    ungroup()

  # ── Per-regime summary + confidence components ────────────────────────────────
  regime_tbl <- ep_df %>%
    group_by(regime) %>%
    summarise(
      n_episodes  = n(),
      avg_alpha   = mean(alpha,  na.rm = TRUE),
      sd_alpha    = sd(alpha,    na.rm = TRUE),
      modal_type  = names(sort(table(ep_type), decreasing = TRUE))[1],
      pct_correct = mean(ep_type == modal_type, na.rm = TRUE),
      .groups     = "drop"
    ) %>%
    mutate(
      # Component 1: t-statistic — significance of avg alpha vs zero
      t_stat = if_else(
        !is.na(sd_alpha) & sd_alpha > 0 & n_episodes > 1,
        avg_alpha / (sd_alpha / sqrt(n_episodes)),
        0
      ),

      # Component 2: sample size weight — full weight at 6+ episodes
      n_weight = pmin(n_episodes / 6, 1),

      # Combined confidence score (0 to 1)
      conf_score = 0.50 * pmin(abs(t_stat) / 2.0, 1.0) +
                   0.30 * pct_correct                   +
                   0.20 * n_weight,

      # Confidence tier
      confidence = case_when(
        conf_score >= 0.75 ~ "High",
        conf_score >= 0.50 ~ "Medium",
        conf_score >= 0.30 ~ "Low",
        TRUE               ~ "Weak"
      ),

      # Ordered factor for display
      regime = factor(regime, levels = c("Fall", "Recovery", "Consolidation"))
    ) %>%
    arrange(regime) %>%
    select(regime, modal_type, avg_alpha, sd_alpha, t_stat,
           n_episodes, pct_correct, conf_score, confidence)

  # ── Archetype from Fall / Recovery / Consolidation modal types ────────────────
  get_modal <- function(reg)
    regime_tbl %>% filter(regime == reg) %>% pull(modal_type)

  fall_type <- get_modal("Fall")
  rec_type  <- get_modal("Recovery")
  con_type  <- get_modal("Consolidation")

  archetype <- dplyr::case_when(
    rho < rho_min                                                          ~ "Corr-Fail",
    fall_type == "LOSS"              &
      rec_type  == "ALPHA"           &
      con_type  == "ALPHA"                                                 ~ "Cycle Amplifier",
    fall_type == "LOSS"              &
      rec_type  == "ALPHA"           &
      con_type  == "LAG"                                                   ~ "Recovery Sprinter",
    fall_type %in% c("STABLE","HEDGE") &
      rec_type  == "LAG"                                                   ~ "Defensive Burden",
    fall_type == "LOSS"              &
      rec_type  == "LAG"                                                   ~ "All-Regime Drag",
    TRUE                                                                   ~ "Other"
  )

  # ── Return structured list ────────────────────────────────────────────────────
  structure(
    list(
      summary = tibble(
        ticker    = ticker,
        bmk       = bmk,
        # Ticker stats
        ann_ret_ticker = ann_ret_ticker,
        ann_vol_ticker = ann_vol_ticker,
        max_dd_ticker  = max_dd_ticker,
        # Spread (L/S overlay) stats
        ann_ret_spread = ann_ret_spread,
        ann_vol_spread = ann_vol_spread,
        ir             = ir,
        max_dd_spread  = max_dd_spread,
        # Correlation
        rho            = rho,
        rho_1y         = rho_1y,
        archetype      = archetype
      ),
      regime_tbl = regime_tbl
    ),
    class = "ticker_ls"
  )
}


# ==============================================================================
# print.ticker_ls()   — clean console display
# ==============================================================================

print.ticker_ls <- function(x, ...) {
  s <- x$summary
  r <- x$regime_tbl

  cat("\n")
  cat(sprintf("══ L/S: Long %-6s / Short %-6s ══════════════════════════════\n",
              s$ticker, s$bmk))
  cat(sprintf("   Archetype : %s\n", s$archetype))
  cat(sprintf("   ρ         : %.3f (full)   %.3f (1y)\n", s$rho, s$rho_1y))
  cat("─────────────────────────────────────────────────────────────────────\n")
  cat(sprintf("  %-10s  %+8s  %8s  %10s\n", "", "Ann Ret", "Ann Vol", "MaxDD"))
  cat(sprintf("  %-10s  %+7.1f%%  %7.1f%%  %9.1f%%\n",
              s$ticker,
              s$ann_ret_ticker * 100,
              s$ann_vol_ticker * 100,
              s$max_dd_ticker  * 100))
  cat(sprintf("  %-10s  %+7.1f%%  %7.1f%%  %9.1f%%   IR: %+.2f\n",
              paste0("L/S Spread"),
              s$ann_ret_spread * 100,
              s$ann_vol_spread * 100,
              s$max_dd_spread  * 100,
              s$ir))
  cat("─────────────────────────────────────────────────────────────────────\n")
  cat(sprintf("  %-14s  %-6s  %+7s  %6s  %5s  %5s  %6s  %s\n",
              "Regime", "Type", "Avg α", "SD α", "t", "n", "Consist", "Confidence"))
  cat(strrep("─", 74), "\n")

  for (i in seq_len(nrow(r))) {
    row <- r[i, ]
    cat(sprintf("  %-14s  %-6s  %+7.1f%%  %6.1f%%  %5.2f  %5d  %5.0f%%  %s\n",
                as.character(row$regime),
                row$modal_type,
                row$avg_alpha   * 100,
                row$sd_alpha    * 100,
                row$t_stat,
                row$n_episodes,
                row$pct_correct * 100,
                row$confidence))
  }

  cat(strrep("─", 74), "\n\n")
  invisible(x)
}


# ==============================================================================
# plot.ticker_ls()   — visual companion: three regime panels
# ==============================================================================
#
# Usage:
#   result <- ticker_long_short("XLK", bmk = "SPY", xts_ret = xts_ret)
#   plot(result, xts_ret = xts_ret)
#
# @param x       ticker_ls object from ticker_long_short()
# @param xts_ret xts of daily returns (needed by the plot functions)
# @param which   character vector — subset of panels to show:
#                "overlay"  → cumulative line + regime α bars
#                "heatmap"  → per-episode type grid
#                "summary"  → avg α per regime bar chart
#                default: all three

plot.ticker_ls <- function(x, xts_ret,
                            which = c("overlay", "heatmap", "summary"), ...) {

  if (!inherits(x, "ticker_ls"))
    stop("x must be a ticker_ls object from ticker_long_short().")

  ticker <- x$summary$ticker
  master <- x$summary$bmk

  if ("overlay" %in% which)
    print(plot_regime_rel_overlay(xts_ret, tickers = ticker, master = master))

  if ("heatmap" %in% which)
    print(plot_regime_rel_heatmap(xts_ret, tickers = ticker, master = master))

  if ("summary" %in% which)
    print(plot_regime_rel_summary(xts_ret, tickers = ticker, master = master))

  invisible(x)
}


# ==============================================================================
# analyze_pair()   — full pair report: stats + factsheet + regime plots
# ==============================================================================
#
# Orchestrates all four outputs in sequence:
#   1. print(ticker_long_short())  → numerical L/S table + confidence
#   2. fs_render()                 → 7-panel factsheet (wealth/spread/DD/dist/ρ/regime ρ/IR)
#   3. plot(..., "overlay","summary") → regime cumulative bars + avg α chart
#   4. plot(..., "heatmap")        → per-episode type grid
#
# Usage:
#   analyze_pair("XLK", bmk = "SPY", xts_ret = xts_ret)
#   analyze_pair("SMH", bmk = "URTH", xts_ret = xts_ret, rt = rt_urth)

analyze_pair <- function(ticker,
                          bmk      = "SPY",
                          xts_ret,
                          rt       = NULL,
                          t_fall   = 0.10,
                          t_cruise = 0.05,
                          rho_min  = 0.75) {

  # Build regime table once — reused by both ticker_long_short and fs_render
  if (is.null(rt) || !is.data.frame(rt))
    rt <- suppressWarnings(
      build_regime_table(xts_ret[, bmk], t_fall, t_cruise)
    )

  # ── 1. Numerical L/S analysis ─────────────────────────────────────────────
  result <- ticker_long_short(ticker, bmk    = bmk,
                               xts_ret = xts_ret,
                               rt      = rt,
                               t_fall  = t_fall,
                               t_cruise = t_cruise,
                               rho_min = rho_min)
  print(result)

  # ── 2. Factsheet — 7 panels ───────────────────────────────────────────────
  if (!exists("fs_render"))
    source(here("utility/factsheet_functions.R"))

  fs_render(ticker, bmk = bmk, ret_xts = xts_ret, rt = rt)

  # ── 3. Regime overlay + avg-alpha summary ─────────────────────────────────
  plot(result, xts_ret = xts_ret, which = c("overlay", "summary"))

  # ── 4. Regime heatmap ─────────────────────────────────────────────────────
  plot(result, xts_ret = xts_ret, which = "heatmap")

  invisible(result)
}


# ==============================================================================
# render_pair_factsheet()   — render Rmd/pair_factsheet.Rmd for one pair
# ==============================================================================
#
# @param ticker   Character — long leg
# @param bmk      Character — short leg / benchmark (default "SPY")
# @param format   "html" (default) or "pdf" or c("html","pdf")
# @param output_dir  Directory for output file(s); default "03_reports"
# @param open     Logical — open the HTML file in browser after render (default TRUE)
#
# @return invisible path(s) to rendered file(s)
#
# Usage:
#   render_pair_factsheet("XLK")
#   render_pair_factsheet("SMH", bmk = "URTH")
#   render_pair_factsheet("GLD", format = c("html","pdf"))

render_pair_factsheet <- function(ticker,
                                   bmk        = "SPY",
                                   format     = "html",
                                   output_dir = here("03_reports"),
                                   open       = TRUE,
                                   t_fall     = 0.10,
                                   t_cruise   = 0.05,
                                   rho_min    = 0.75) {

  if (!requireNamespace("rmarkdown", quietly = TRUE))
    stop("Package 'rmarkdown' is required. Install with: install.packages('rmarkdown')")

  rmd_path <- here("Rmd/pair_factsheet.Rmd")
  if (!file.exists(rmd_path))
    stop("Rmd not found: ", rmd_path)

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  out_paths <- character(0)

  for (fmt in format) {

    out_format <- switch(fmt,
      html = rmarkdown::html_document(
        self_contained = TRUE, toc = FALSE,
        theme = "flatly", highlight = "tango"
      ),
      pdf  = rmarkdown::pdf_document(latex_engine = "xelatex", toc = FALSE),
      stop("format must be 'html' or 'pdf', got: ", fmt)
    )

    ext      <- if (fmt == "html") ".html" else ".pdf"
    filename <- paste0("factsheet_", ticker, "_", bmk, ext)
    out_file <- file.path(output_dir, filename)

    message(sprintf("Rendering %s vs %s → %s ...", ticker, bmk, filename))

    rmarkdown::render(
      input         = rmd_path,
      output_format = out_format,
      output_file   = out_file,
      params        = list(
        ticker   = ticker,
        bmk      = bmk,
        t_fall   = t_fall,
        t_cruise = t_cruise,
        rho_min  = rho_min
      ),
      envir  = new.env(parent = globalenv()),
      quiet  = TRUE
    )

    message("  → ", out_file)
    out_paths <- c(out_paths, out_file)
  }

  if (open && any(grepl("\\.html$", out_paths)))
    utils::browseURL(out_paths[grepl("\\.html$", out_paths)][1])

  invisible(out_paths)
}


# ==============================================================================
# USAGE EXAMPLES — run manually in console
# ==============================================================================

# Single ticker analysis (stats + print)
# result <- ticker_long_short("XLK", bmk = "SPY", xts_ret = xts_ret)
# print(result)
# plot(result, xts_ret = xts_ret)

# Full interactive pair report (console output + plots)
# analyze_pair("XLK",  bmk = "SPY",  xts_ret = xts_ret)
# analyze_pair("SMH",  bmk = "URTH", xts_ret = xts_ret)

# Render HTML factsheet (opens in browser)
# render_pair_factsheet("XLK")
# render_pair_factsheet("SMH", bmk = "URTH")
# render_pair_factsheet("GLD", format = c("html","pdf"))

# Batch render
# for (tk in c("SMH", "XLK", "QQQ")) render_pair_factsheet(tk, open = FALSE)
