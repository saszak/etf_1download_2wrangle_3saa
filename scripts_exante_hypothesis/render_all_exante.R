################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE:    scripts_exante_hypothesis/render_all_exante.R
# Purpose: Render exante_hypothesis_report.Rmd for each defined benchmark
#
# OUTPUT:  03_reports/exante_{BMK}_w{blend_pct}.html
#
# USAGE:
#   source("scripts_exante_hypothesis/render_all_exante.R")
#   # or run render_all_exante() after sourcing exante_visuals.R
################################################################################

library(rmarkdown)
library(here)

source(here("scripts_exante_hypothesis/exante_visuals.R"))

# ── Benchmark list ─────────────────────────────────────────────────────────────
# Add / remove benchmarks here. Each entry renders one HTML report.
# Keys in the list:
#   bmk      : ticker (must exist in xts_ret)
#   w_blend  : benchmark weight in the blend (default 0.5)
#   ret_eps  : return threshold for role boundary
#   risk_eps : DD threshold for role boundary

EXANTE_BENCHMARKS <- list(

  # ── Primary benchmarks (Exante Hypothesis core) ───────────────────────────
  list(bmk = "SPY",  w_blend = 0.5),   # US Broad Equity
  list(bmk = "AGG",  w_blend = 0.5),   # US Aggregate Bond

  # ── Equity sub-benchmarks ─────────────────────────────────────────────────
  list(bmk = "QQQ",  w_blend = 0.5),   # US Growth / Nasdaq
  list(bmk = "URTH", w_blend = 0.5),   # World Equity

  # ── Alternative / real asset benchmarks ──────────────────────────────────
  list(bmk = "GLD",  w_blend = 0.5),   # Gold
  list(bmk = "IEF",  w_blend = 0.5)    # Intermediate Treasuries
)

# ── Runner ─────────────────────────────────────────────────────────────────────

render_all_exante <- function(benchmarks = EXANTE_BENCHMARKS,
                               output_dir  = here("03_reports")) {

  n <- length(benchmarks)
  cat(sprintf("\n══ Exante Report Batch | %d benchmarks ══\n", n))

  results <- vector("list", n)

  for (i in seq_along(benchmarks)) {
    cfg <- benchmarks[[i]]
    bmk      <- cfg$bmk
    w_blend  <- cfg$w_blend  %||% 0.5
    ret_eps  <- cfg$ret_eps  %||% 0.005
    risk_eps <- cfg$risk_eps %||% 0.005

    cat(sprintf("\n[%d/%d] %s (blend=%.0f%%)\n", i, n, bmk, w_blend * 100))

    results[[i]] <- tryCatch(
      render_exante_report(
        bmk        = bmk,
        w_blend    = w_blend,
        ret_eps    = ret_eps,
        risk_eps   = risk_eps,
        output_dir = output_dir
      ),
      error = function(e) {
        message(sprintf("  ✗ FAILED [%s]: %s", bmk, conditionMessage(e)))
        NULL
      }
    )
  }

  # ── Summary ─────────────────────────────────────────────────────────────────
  succeeded <- Filter(Negate(is.null), results)
  failed    <- n - length(succeeded)

  cat(sprintf(
    "\n══ Batch complete: %d/%d succeeded | %d failed ══\n",
    length(succeeded), n, failed
  ))
  cat("Output files:\n")
  for (f in succeeded) cat(sprintf("  %s\n", f))

  invisible(results)
}

# ── Auto-run when sourced ──────────────────────────────────────────────────────
render_all_exante()
