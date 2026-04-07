################################################################################
# scripts_market_state/ms_risk_allocation.R
#
# PURPOSE
#   Layer 3 of the market state framework.
#   Combine the 5-signal market state vector (Layer 1) with the sector rotation
#   pattern (Layer 2) into a final equity allocation recommendation.
#
# COMBINATION LOGIC
#   Base equity weight comes from Layer 1 market_state → MS_EQ_WEIGHT.
#   Layer 2 sector_pattern adjusts it:
#
#   Adjustment table:
#     Strong Risk-On  → +0.05
#     Risk-On         → +0.02
#     Neutral         →  0.00
#     Risk-Off        → −0.02
#     Strong Risk-Off → −0.05
#
#   Final weight is clipped to [0.25, 0.75].
#
# COMBINED STATE NAME
#   market_state × sector_pattern → combined_label (e.g. "Expansion / Risk-On")
#
# PUBLIC FUNCTIONS
#   build_risk_allocation_history(xts_ret, ...)
#     → tibble: date | market_state | sector_pattern | base_eq_weight |
#               adj_eq_weight | final_eq_weight | fi_weight
#
#   current_risk_allocation(xts_ret, ...)
#     → single-row tibble for latest available date
#
#   print_risk_allocation(ra_history, n_recent=10)
#     → console summary
#
# DEPENDENCIES
#   scripts_market_state/ms_vector_state.R
#   scripts_market_state/ms_sector_pattern.R
################################################################################

library(tidyverse)
library(xts)
library(zoo)

# ── Layer 2 adjustment map ────────────────────────────────────────────────────
SP_ADJ <- c(
  `Strong Risk-On`  =  0.05,
  `Risk-On`         =  0.02,
  `Neutral`         =  0.00,
  `Risk-Off`        = -0.02,
  `Strong Risk-Off` = -0.05
)

RA_EQ_MIN <- 0.25
RA_EQ_MAX <- 0.75

# ==============================================================================
# build_risk_allocation_history()
# ==============================================================================
build_risk_allocation_history <- function(
    xts_ret,
    roll_win    = 60,
    ma_win      = 200,
    t_fall      = 0.10,
    mom_win     = 63,
    min_persist = 1L
) {

  # ── Layer 1 ──────────────────────────────────────────────────────────────────
  ms <- build_market_state_history(xts_ret,
                                   roll_win    = roll_win,
                                   ma_win      = ma_win,
                                   t_fall      = t_fall,
                                   min_persist = min_persist)

  # ── Layer 2 ──────────────────────────────────────────────────────────────────
  sp <- build_sector_pattern_history(xts_ret, mom_win = mom_win)

  # ── Join on date ─────────────────────────────────────────────────────────────
  combined <- ms %>%
    select(date, market_state, eq_weight) %>%
    rename(base_eq_weight = eq_weight) %>%
    left_join(
      sp %>% select(date, sector_pattern, rotation_score),
      by = "date"
    )

  # ── Adjustment ───────────────────────────────────────────────────────────────
  combined <- combined %>%
    mutate(
      adj = SP_ADJ[as.character(sector_pattern)],
      adj = replace_na(adj, 0),
      adj_eq_weight   = base_eq_weight + adj,
      final_eq_weight = pmin(RA_EQ_MAX, pmax(RA_EQ_MIN, adj_eq_weight)),
      fi_weight       = 1 - final_eq_weight,
      combined_label  = if_else(
        !is.na(market_state) & !is.na(sector_pattern),
        paste0(as.character(market_state), " / ", as.character(sector_pattern)),
        NA_character_
      )
    )

  combined %>%
    select(date, market_state, sector_pattern, rotation_score,
           base_eq_weight, adj_eq_weight, final_eq_weight, fi_weight,
           combined_label)
}

# ==============================================================================
# current_risk_allocation()
# ==============================================================================
current_risk_allocation <- function(xts_ret, ...) {
  h <- build_risk_allocation_history(xts_ret, ...)
  h[nrow(h), ]
}

# ==============================================================================
# print_risk_allocation()
# ==============================================================================
print_risk_allocation <- function(ra_history, n_recent = 10) {

  valid <- ra_history %>% filter(!is.na(final_eq_weight))
  if (nrow(valid) == 0) {
    cat("No valid allocation found — check Layer 1 and Layer 2 outputs.\n")
    return(invisible(NULL))
  }

  cur  <- tail(valid, 1)
  hist <- tail(valid, n_recent)

  cat(sprintf(
    "\n\u2554\u2550 Risk Allocation \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\n",
    NULL
  ))
  cat(sprintf(
    "  As of            : %s\n  Combined state   : %s\n",
    format(cur$date), cur$combined_label
  ))
  cat(sprintf(
    "  Market state     : %s  (base eq: %.0f%%)\n",
    as.character(cur$market_state), cur$base_eq_weight * 100
  ))
  cat(sprintf(
    "  Sector pattern   : %s  (adj: %+.0f%%)\n",
    as.character(cur$sector_pattern),
    (cur$adj_eq_weight - cur$base_eq_weight) * 100
  ))
  cat(sprintf(
    "  \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\n"
  ))
  cat(sprintf(
    "  Final EQ weight  : %.0f%%\n  Final FI weight  : %.0f%%\n",
    cur$final_eq_weight * 100,
    cur$fi_weight * 100
  ))

  cat("\n  Recent history:\n")
  print(hist %>%
    mutate(
      base_eq = paste0(round(base_eq_weight * 100), "%"),
      final_eq = paste0(round(final_eq_weight * 100), "%"),
      fi       = paste0(round(fi_weight       * 100), "%")
    ) %>%
    select(date, market_state, sector_pattern, base_eq, final_eq, fi),
    n = n_recent)

  invisible(cur)
}

# ==============================================================================
# Quick-run (guarded)
# ==============================================================================
if (!isTRUE(getOption("knitr.in.progress"))) {
  library(here)

  if (!exists("xts_ret"))
    xts_ret <- readRDS(here("02_data_processed/xts_ret_returns.rds"))

  if (!exists("build_market_state_history"))
    source(here("scripts_market_state/ms_vector_state.R"))

  if (!exists("build_sector_pattern_history"))
    source(here("scripts_market_state/ms_sector_pattern.R"))

  ra_hist <- build_risk_allocation_history(xts_ret)
  print_risk_allocation(ra_hist)
}
