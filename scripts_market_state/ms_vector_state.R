################################################################################
# scripts_market_state/ms_vector_state.R
#
# PURPOSE
#   Compute a 5-dimensional market state vector from the URTH→SPY→Sector
#   hierarchy and collapse it into one of 5 named states with an associated
#   recommended equity allocation weight.
#
# HIERARCHY
#   Tier 0  URTH   — MSCI World, global regime anchor
#   Tier 1  SPY    — US regime (≈65% of URTH)
#           VWO    — EM risk appetite (diverges from DM in risk-off)
#   Tier 2  g_sector_tickers — US sector breadth (10 GICS sectors)
#
# FIVE SIGNALS
#   1. urth_regime    : Fall / Recovery / Consolidation   (build_regime_table)
#   2. spy_regime     : Fall / Recovery / Consolidation   (build_regime_table)
#   3. us_premium     : +1 / -1  (sign of 60d SPY − URTH cumret)
#   4. em_appetite    : +1 / -1  (sign of 60d VWO − SPY cumret)
#   5. sector_breadth : 0–1      (% US sectors above 200DMA)
#
# FIVE NAMED STATES (priority order — most severe checked first)
#   Contraction   → eq_weight 0.35
#   Deterioration → eq_weight 0.42
#   Late-Cycle    → eq_weight 0.50
#   US-Led        → eq_weight 0.58
#   Expansion     → eq_weight 0.65
#
# PUBLIC FUNCTIONS
#   build_market_state_history(xts_ret, roll_win=60, ma_win=200, t_fall=0.10)
#     → tibble: date | 5 signals | market_state | eq_weight
#
#   current_market_state(xts_ret, ...)
#     → single-row tibble for today (last available date)
#
#   print_market_state(ms_history)
#     → console summary of current state + recent history
#
# DEPENDENCIES
#   scripts/00_init_universe.R     — g_sector_tickers
#   scripts_spy_dd_regime/spy_dd_regime.R — label_daily_regime()
################################################################################

library(tidyverse)
library(xts)
library(zoo)

# ── State palette (used by ms_visuals.R) ──────────────────────────────────────
MS_STATE_PAL <- c(
  Expansion     = "#16a34a",   # green
  `US-Led`      = "#2563eb",   # blue
  `Late-Cycle`  = "#d97706",   # amber
  Deterioration = "#ea580c",   # orange-red
  Contraction   = "#dc2626"    # red
)

MS_EQ_WEIGHT <- c(
  Expansion     = 0.65,
  `US-Led`      = 0.58,
  `Late-Cycle`  = 0.50,
  Deterioration = 0.42,
  Contraction   = 0.35
)

MS_STATE_LEVELS <- names(MS_STATE_PAL)   # ordered risk-on → risk-off

# ==============================================================================
# .roll_cumret()  — rolling n-day cumulative return (internal)
# ==============================================================================
.roll_cumret <- function(ret_vec, n) {
  as.numeric(zoo::rollapply(
    ret_vec, n,
    function(x) prod(1 + x) - 1,
    align = "right", fill = NA
  ))
}

# ==============================================================================
# .sector_breadth()  — % sector tickers above their ma_win-day moving average
# ==============================================================================
.sector_breadth <- function(xts_ret, sector_tickers, ma_win = 200) {

  avail <- intersect(sector_tickers, colnames(xts_ret))
  if (length(avail) == 0) return(rep(NA_real_, nrow(xts_ret)))

  dates <- as.Date(index(xts_ret))

  # Wealth index (base 1) for each sector
  above_mat <- sapply(avail, function(tk) {
    r   <- as.numeric(coredata(xts_ret[, tk]))
    w   <- cumprod(1 + replace(r, is.na(r), 0))
    ma  <- as.numeric(zoo::rollmean(w, ma_win, align = "right", fill = NA))
    as.integer(!is.na(ma) & w >= ma)
  })

  if (is.null(dim(above_mat))) above_mat <- matrix(above_mat, ncol = 1)

  rowMeans(above_mat, na.rm = TRUE)
}

# ==============================================================================
# .classify_state()  — map 5 signals → named state (priority order)
# ==============================================================================
.classify_state <- function(urth_r, spy_r, us_prem, em_app, breadth) {

  # Guard: any NA → return NA
  if (any(is.na(c(urth_r, spy_r, us_prem, em_app, breadth))))
    return(NA_character_)

  both_fall   <- urth_r == "Fall"   & spy_r == "Fall"
  either_fall <- urth_r == "Fall"   | spy_r == "Fall"

  if (both_fall   & em_app  < 0 & breadth < 0.40) return("Contraction")
  if (either_fall & breadth < 0.55)                return("Deterioration")
  if (breadth     < 0.55)                          return("Late-Cycle")
  if (breadth     < 0.70 & em_app < 0)             return("Late-Cycle")
  if (us_prem     > 0    & em_app <= 0)            return("US-Led")
  return("Expansion")
}

# ==============================================================================
# .apply_persist_filter()
#   Require a state to persist for min_persist consecutive days before it is
#   "confirmed". Until confirmed the previous confirmed state is held.
#   Returns a character vector of confirmed states (same length as raw_state).
# ==============================================================================
.apply_persist_filter <- function(raw_state, min_persist = 1L) {

  n <- length(raw_state)
  if (min_persist <= 1L) return(raw_state)          # no-op for default

  confirmed  <- raw_state                            # output vector
  cur_state  <- raw_state[1]                         # current confirmed state
  candidate  <- raw_state[1]                         # candidate new state
  streak     <- 1L                                   # days candidate has held

  for (i in seq_len(n)) {
    s <- raw_state[i]
    if (is.na(s)) {
      confirmed[i] <- cur_state
      next
    }

    if (identical(s, candidate)) {
      streak <- streak + 1L
    } else {
      candidate <- s
      streak    <- 1L
    }

    if (streak >= min_persist) {
      cur_state <- candidate
    }

    confirmed[i] <- cur_state
  }

  confirmed
}

# ==============================================================================
# build_market_state_history()
#   Core function — returns full daily history of the 5-signal vector + state.
# ==============================================================================
build_market_state_history <- function(
    xts_ret,
    roll_win    = 60,   # rolling window for premium / appetite spreads (days)
    ma_win      = 200,  # moving-average window for sector breadth
    t_fall      = 0.10, # regime threshold (matches spy_dd_regime.R default)
    min_persist = 1L    # days new state must persist before confirmation (1 = raw)
) {

  required <- c("URTH", "SPY", "VWO")
  missing  <- setdiff(required, colnames(xts_ret))
  if (length(missing) > 0)
    stop("ms_vector_state: missing tickers in xts_ret — ", paste(missing, collapse = ", "))

  dates <- as.Date(index(xts_ret))

  # ── 1. Daily regime labels ──────────────────────────────────────────────────
  # label_daily_regime() returns xts with character column — convert via tibble
  # to avoid xts coercing characters to NA on merge.
  .extract_regime <- function(ticker) {
    ldr <- label_daily_regime(xts_ret[, ticker], t_fall)
    reg_tbl <- tibble(
      date   = as.Date(index(ldr)),
      regime = as.character(coredata(ldr)[, "regime"])
    )
    tibble(date = dates) %>%
      left_join(reg_tbl, by = "date") %>%
      tidyr::fill(regime, .direction = "down") %>%
      pull(regime)
  }

  urth_reg <- .extract_regime("URTH")
  spy_reg  <- .extract_regime("SPY")

  # ── 2. US premium: 60d SPY − URTH cumret ───────────────────────────────────
  spy_roll  <- .roll_cumret(as.numeric(coredata(xts_ret[, "SPY"])),  roll_win)
  urth_roll <- .roll_cumret(as.numeric(coredata(xts_ret[, "URTH"])), roll_win)
  vwo_roll  <- .roll_cumret(as.numeric(coredata(xts_ret[, "VWO"])),  roll_win)

  us_premium_val  <- spy_roll  - urth_roll   # + = US leading
  em_appetite_val <- vwo_roll  - spy_roll    # + = EM leading (risk-on)

  us_prem_sign <- sign(us_premium_val)
  em_app_sign  <- sign(em_appetite_val)

  # ── 3. Sector breadth ───────────────────────────────────────────────────────
  sec_tickers <- if (exists("g_sector_tickers")) g_sector_tickers else
    c("XLK","XLV","XLF","XLI","XLU","XLP","XLY","XLB","XLE","XLRE")

  breadth <- .sector_breadth(xts_ret, sec_tickers, ma_win)

  # ── 4. Classify raw state ────────────────────────────────────────────────────
  state_raw <- mapply(
    .classify_state,
    urth_r  = urth_reg,
    spy_r   = spy_reg,
    us_prem = us_prem_sign,
    em_app  = em_app_sign,
    breadth = breadth
  )

  # ── 5. Apply persistence filter ─────────────────────────────────────────────
  state_confirmed <- .apply_persist_filter(state_raw, as.integer(min_persist))

  # ── 6. Assemble tibble ──────────────────────────────────────────────────────
  tibble(
    date              = dates,
    urth_regime       = urth_reg,
    spy_regime        = spy_reg,
    us_premium_60d    = round(us_premium_val  * 100, 2),   # in %
    em_appetite_60d   = round(em_appetite_val * 100, 2),   # in %
    us_premium        = us_prem_sign,
    em_appetite       = em_app_sign,
    sector_breadth    = round(breadth, 3),
    market_state_raw  = factor(state_raw,       levels = MS_STATE_LEVELS),
    market_state      = factor(state_confirmed, levels = MS_STATE_LEVELS),
    eq_weight         = MS_EQ_WEIGHT[state_confirmed]
  )
}

# ==============================================================================
# current_market_state()  — single-row snapshot for latest available date
# ==============================================================================
current_market_state <- function(xts_ret, ...) {
  h <- build_market_state_history(xts_ret, ...)
  h[nrow(h), ]
}

# ==============================================================================
# print_market_state()  — console summary
# ==============================================================================
print_market_state <- function(ms_history, n_recent = 10) {

  valid <- ms_history %>% filter(!is.na(market_state))
  if (nrow(valid) == 0) {
    cat("No valid market states found — check that URTH/SPY/VWO data and",
        "g_sector_tickers are loaded, and that xts_ret has sufficient history.\n")
    return(invisible(NULL))
  }
  cur  <- tail(valid, 1)
  hist <- tail(valid, n_recent)

  cat(sprintf(
    "\n\u2554\u2550 Market State  \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\n",
    NULL
  ))
  cat(sprintf(
    "  As of        : %s\n  State        : %s\n  Eq Weight    : %.0f%%\n",
    format(cur$date), as.character(cur$market_state), cur$eq_weight * 100
  ))
  cat(sprintf(
    "  URTH regime  : %s\n  SPY  regime  : %s\n",
    cur$urth_regime, cur$spy_regime
  ))
  cat(sprintf(
    "  US premium   : %+.1f%% (%s)\n  EM appetite  : %+.1f%% (%s)\n",
    cur$us_premium_60d,
    if (cur$us_premium > 0) "US leading" else "US lagging",
    cur$em_appetite_60d,
    if (cur$em_appetite > 0) "EM leading \u2191 risk-on" else "EM lagging \u2193 risk-off"
  ))
  cat(sprintf(
    "  Sector breadth: %.0f%% above 200DMA\n", cur$sector_breadth * 100
  ))

  cat("\n  Recent history:\n")
  print(hist %>%
    select(date, market_state, eq_weight, sector_breadth,
           us_premium_60d, em_appetite_60d) %>%
    mutate(eq_weight = paste0(round(eq_weight * 100), "%"),
           sector_breadth = paste0(round(sector_breadth * 100), "%")),
    n = n_recent)

  # State distribution over full history
  dist <- ms_history %>%
    filter(!is.na(market_state)) %>%
    count(market_state) %>%
    mutate(pct = paste0(round(n / sum(n) * 100, 1), "%"))

  cat("\n  Historical state distribution:\n")
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

  if (!exists("label_daily_regime"))
    source(here("scripts_spy_dd_regime/spy_dd_regime.R"))

  if (!exists("g_sector_tickers"))
    source(here("scripts/00_init_universe.R"))

  ms_hist <- build_market_state_history(xts_ret, min_persist = 5L)
  print_market_state(ms_hist)
}
