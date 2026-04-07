################################################################################
# scripts_market_state/ms_sector_pattern.R
#
# PURPOSE
#   Layer 2 of the market state framework.
#   Compute a Cyclical vs Defensive rotation score from 63-day momentum of
#   US GICS sector ETFs and collapse it into a signed pattern label.
#
# CYCLICAL vs DEFENSIVE SPLIT
#   Cyclicals  : XLK, XLY, XLB, XLE, XLI      (growth/risk sensitive)
#   Defensives : XLU, XLP, XLV                 (low-beta, income)
#   Financials : XLF                            (excluded — ambiguous)
#   Real Estate: XLRE                           (excluded — rate sensitive)
#
# SCORE
#   sector_rotation_score = mean(63d momentum cyclicals) − mean(63d momentum defensives)
#   Where "63d momentum" = rolling 63-day cumulative return (≈ 3 months)
#
# PATTERN LABELS (priority order)
#   Strong Risk-On  → score >  0.04  (cyclicals clearly leading)
#   Risk-On         → score >  0.01
#   Neutral         → score ∈ [−0.01, 0.01]
#   Risk-Off        → score < −0.01
#   Strong Risk-Off → score < −0.04  (defensives clearly leading)
#
# PUBLIC FUNCTIONS
#   build_sector_pattern_history(xts_ret, mom_win=63)
#     → tibble: date | cyclical_mom | defensive_mom | rotation_score | sector_pattern
#
#   current_sector_pattern(xts_ret, ...)
#     → single-row tibble for latest available date
#
#   print_sector_pattern(sp_history, n_recent=10)
#     → console summary
#
# DEPENDENCIES
#   No external scripts required — uses standard tidyverse / xts / zoo.
################################################################################

library(tidyverse)
library(xts)
library(zoo)

# ── Sector group definitions ─────────────────────────────────────────────────
SP_CYCLICALS  <- c("XLK", "XLY", "XLB", "XLE", "XLI")
SP_DEFENSIVES <- c("XLU", "XLP", "XLV")

# ── Pattern palette (used by ms_visuals.R) ───────────────────────────────────
SP_PATTERN_PAL <- c(
  `Strong Risk-On`  = "#15803d",   # dark green
  `Risk-On`         = "#4ade80",   # light green
  `Neutral`         = "#94a3b8",   # slate
  `Risk-Off`        = "#fb923c",   # orange
  `Strong Risk-Off` = "#dc2626"    # red
)

SP_PATTERN_LEVELS <- names(SP_PATTERN_PAL)   # ordered risk-on → risk-off

# ── Thresholds ───────────────────────────────────────────────────────────────
SP_STRONG_POS  <-  0.04
SP_WEAK_POS    <-  0.01
SP_WEAK_NEG    <- -0.01
SP_STRONG_NEG  <- -0.04

# ==============================================================================
# .group_momentum()  — mean rolling cumret for a group of tickers (internal)
# ==============================================================================
.group_momentum <- function(xts_ret, tickers, win) {
  avail <- intersect(tickers, colnames(xts_ret))
  if (length(avail) == 0) return(rep(NA_real_, nrow(xts_ret)))

  mom_mat <- sapply(avail, function(tk) {
    r <- as.numeric(coredata(xts_ret[, tk]))
    as.numeric(zoo::rollapply(
      r, win,
      function(x) prod(1 + x) - 1,
      align = "right", fill = NA
    ))
  })

  if (is.null(dim(mom_mat))) mom_mat <- matrix(mom_mat, ncol = 1)
  rowMeans(mom_mat, na.rm = TRUE)
}

# ==============================================================================
# .classify_pattern()  — map rotation score → named pattern (internal)
# ==============================================================================
.classify_pattern <- function(score) {
  if (is.na(score)) return(NA_character_)
  if (score >  SP_STRONG_POS) return("Strong Risk-On")
  if (score >  SP_WEAK_POS)   return("Risk-On")
  if (score >= SP_WEAK_NEG)   return("Neutral")
  if (score >= SP_STRONG_NEG) return("Risk-Off")
  return("Strong Risk-Off")
}

# ==============================================================================
# build_sector_pattern_history()
#   Core function — returns full daily history of rotation score + pattern.
# ==============================================================================
build_sector_pattern_history <- function(
    xts_ret,
    mom_win = 63     # rolling momentum window (≈ 3 months)
) {

  dates <- as.Date(index(xts_ret))

  # ── 1. Group momentum ───────────────────────────────────────────────────────
  cyc_mom <- .group_momentum(xts_ret, SP_CYCLICALS,  mom_win)
  def_mom <- .group_momentum(xts_ret, SP_DEFENSIVES, mom_win)

  # ── 2. Rotation score ───────────────────────────────────────────────────────
  rotation_score <- cyc_mom - def_mom

  # ── 3. Pattern label ────────────────────────────────────────────────────────
  pattern <- vapply(rotation_score, .classify_pattern, character(1))

  # ── 4. Assemble tibble ──────────────────────────────────────────────────────
  tibble(
    date             = dates,
    cyclical_mom     = round(cyc_mom        * 100, 2),   # in %
    defensive_mom    = round(def_mom        * 100, 2),   # in %
    rotation_score   = round(rotation_score * 100, 2),   # in %
    sector_pattern   = factor(pattern, levels = SP_PATTERN_LEVELS)
  )
}

# ==============================================================================
# current_sector_pattern()  — single-row snapshot for latest available date
# ==============================================================================
current_sector_pattern <- function(xts_ret, ...) {
  h <- build_sector_pattern_history(xts_ret, ...)
  h[nrow(h), ]
}

# ==============================================================================
# print_sector_pattern()  — console summary
# ==============================================================================
print_sector_pattern <- function(sp_history, n_recent = 10) {

  valid <- sp_history %>% filter(!is.na(sector_pattern))
  if (nrow(valid) == 0) {
    cat("No valid sector patterns found — check that sector ETFs are in xts_ret.\n")
    return(invisible(NULL))
  }

  cur  <- tail(valid, 1)
  hist <- tail(valid, n_recent)

  cat(sprintf(
    "\n\u2554\u2550 Sector Pattern \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\n",
    NULL
  ))
  cat(sprintf(
    "  As of          : %s\n  Pattern        : %s\n  Rotation score : %+.2f%%\n",
    format(cur$date), as.character(cur$sector_pattern), cur$rotation_score
  ))
  cat(sprintf(
    "  Cyclical mom   : %+.2f%%  (%s)\n  Defensive mom  : %+.2f%%  (%s)\n",
    cur$cyclical_mom,  paste(intersect(SP_CYCLICALS,  colnames(xts_ret)), collapse="+"),
    cur$defensive_mom, paste(intersect(SP_DEFENSIVES, colnames(xts_ret)), collapse="+")
  ))

  cat("\n  Recent history:\n")
  print(hist %>%
    select(date, sector_pattern, rotation_score, cyclical_mom, defensive_mom),
    n = n_recent)

  dist <- sp_history %>%
    filter(!is.na(sector_pattern)) %>%
    count(sector_pattern) %>%
    mutate(pct = paste0(round(n / sum(n) * 100, 1), "%"))

  cat("\n  Historical pattern distribution:\n")
  print(dist)

  invisible(cur)
}

# ==============================================================================
# Quick-run (guarded)
# ==============================================================================
if (!isTRUE(getOption("knitr.in.progress"))) {
  library(here)

  if (!exists("xts_ret"))
    xts_ret <- readRDS(here("02_data_processed/xts_ret_returns.rds"))

  sp_hist <- build_sector_pattern_history(xts_ret)
  print_sector_pattern(sp_hist)
}
