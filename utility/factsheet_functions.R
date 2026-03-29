################################################################################
# utility/factsheet_functions.R
# Purpose : Standardised factsheet for any xts — single stock, ETF, or
#           portfolio — built on the existing plot_xts_pair() engine.
#
# FUNCTION
#   fs_render(ticker, bmk, ret_xts, rt)
#     → 7-panel patchwork via plot_xts_pair():
#       P1  Cumulative wealth (ticker vs bmk)
#       P2  L/S spread wealth (long ticker / short bmk)
#       P3  Running drawdown from peak (ticker, bmk, spread)
#       P4  Daily return distribution (boxplot: ticker, bmk, spread)
#       P5  Rolling ρ (250-day) with regime shading
#       P6  Regime-conditional ρ (violin + box, Fall/Recovery/Consolidation)
#       P7  Rolling IR (250-day) with ±0.5 thresholds and regime shading
#
# USAGE
#   source(here("utility/factsheet_functions.R"))
#   fs_render("XLK", bmk = "SPY", ret_xts = xts_d_abs, rt = rt)
#
# DEPENDENCIES
#   utility/plot_xts_pair.R              — 7-panel pair engine
#   scripts_spy_dd_regime/spy_dd_regime.R — build_regime_table()
################################################################################

library(here)

source(here("utility/plot_xts_pair.R"))
if (!exists("build_regime_table")) source(here("scripts_spy_dd_regime/spy_dd_regime.R"))

#' Render a full 7-panel factsheet for one ticker vs a benchmark.
#'
#' @param ticker    character  — asset to analyse (must be in ret_xts columns)
#' @param bmk       character  — benchmark ticker, default "SPY"
#' @param ret_xts   xts        — daily returns matrix (all tickers)
#' @param rt        data.frame — regime table from build_regime_table().
#'                               If NULL, computed automatically from bmk column.
#' @param col1      colour for ticker line (default navy)
#' @param col2      colour for bmk line   (default red)
#' @return          invisible list — same as plot_xts_pair() output
#'                  ($combined, $p_both, $p_spread, $p_dd, $p_box,
#'                   $p_cor, $p_regime_cor, $p_ir, $stats, $full_cor, $full_ir)
fs_render <- function(ticker,
                      bmk     = "SPY",
                      ret_xts,
                      rt      = NULL,
                      col1    = "#1D3557",
                      col2    = "#E63946") {

  stopifnot(ticker %in% colnames(ret_xts),
            bmk     %in% colnames(ret_xts))

  if (is.null(rt) || !is.data.frame(rt)) {
    message("fs_render: building regime table from ", bmk, " ...")
    rt <- build_regime_table(ret_xts[, bmk])
  }

  plot_xts_pair(
    xts1         = ret_xts[, ticker],
    xts2         = ret_xts[, bmk],
    label1       = ticker,
    label2       = bmk,
    col1         = col1,
    col2         = col2,
    regime_tbl   = rt,
    title_prefix = NULL
  )
}

message("factsheet_functions: loaded  |  call fs_render(ticker, bmk, ret_xts, rt)")
