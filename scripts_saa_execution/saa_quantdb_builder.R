################################################################################
# scripts_saa_execution/saa_quantdb_builder.R
# Purpose : Build the 8-layer ETF Quant Database for the SAA execution universe.
#           Ported from etf_saa_taa/etf_quantdb_builder.R.
#
# Input   : saa_bridge_config.R → xts_ret, xts_rel, xts_w_abs, xts_m_abs,
#                                  saa_metadata, tickers
# Output  : etf_quantdb  (tibble, in global env)
#           02_data_processed/saa_quantdb.rds
#
# Layers
#   L1 abs  : ann_ret, ann_vol, max_dd, kurtosis, skew, count_dd_10, max_recovery_d
#   L2 rel  : alpha_6y (cumulative), ir_ratio, beta_spy, corr_to_bmk, vol_scalar
#   L4 mom  : mom_d (daily z), mom_w (weekly EMA4), mom_m (monthly EMA3),
#             is_confirmed, mom_stretch
################################################################################

library(PerformanceAnalytics)
library(dplyr)
library(moments)
library(xts)
library(TTR)
library(here)

if (!exists("xts_d_abs")) source(here::here("scripts_saa_execution/saa_bridge_config.R"))

# Restrict to the SAA execution universe
xts_ret_saa <- xts_d_abs[, tickers]
xts_rel_saa <- xts_d_rel[, tickers]
xts_w_saa   <- xts_w_abs[, tickers]
xts_m_saa   <- xts_m_abs[, tickers]

# SPY anchor vectors (used in beta / corr calculations)
spy_ret     <- as.matrix(xts_ret_saa[, "SPY"])
spy_vol_ann <- sd(spy_ret, na.rm = TRUE) * sqrt(252)

# ── Helper: max recovery days from top drawdown ───────────────────────────────
.max_recovery <- function(x) {
  x_m <- as.matrix(na.omit(x))
  if (nrow(x_m) < 10) return(0L)
  dd <- PerformanceAnalytics::table.Drawdowns(x_m, top = 1)
  if (nrow(dd) == 0) return(0L)
  as.integer(dd$Recovery)
}

# ── L1: Absolute DNA ──────────────────────────────────────────────────────────
message("saa_quantdb [L1]: Absolute DNA ...")
l1_abs <- data.frame(
  ticker = tickers,
  ann_ret = apply(xts_ret_saa, 2, function(x)
    as.numeric(PerformanceAnalytics::Return.annualized(as.matrix(na.omit(x))))),
  ann_vol = apply(xts_ret_saa, 2, function(x)
    as.numeric(PerformanceAnalytics::StdDev.annualized(as.matrix(na.omit(x))))),
  max_dd = apply(xts_ret_saa, 2, function(x)
    as.numeric(PerformanceAnalytics::maxDrawdown(as.matrix(na.omit(x))))),
  kurtosis_val = apply(xts_ret_saa, 2, function(x)
    moments::kurtosis(as.numeric(na.omit(x))) - 3),   # excess kurtosis
  skew_val = apply(xts_ret_saa, 2, function(x)
    moments::skewness(as.numeric(na.omit(x)))),
  count_dd_10 = apply(xts_ret_saa, 2, function(x) {
    x_m <- as.matrix(na.omit(x))
    dd  <- PerformanceAnalytics::table.Drawdowns(x_m)
    if (nrow(dd) == 0) return(0L)
    sum(dd$Depth < -0.10, na.rm = TRUE)
  }),
  max_recovery_d = apply(xts_ret_saa, 2, .max_recovery)
)

# ── L2: Relative Edge ─────────────────────────────────────────────────────────
message("saa_quantdb [L2]: Relative Edge ...")
l2_rel <- data.frame(
  ticker = tickers,
  alpha_6y = apply(xts_rel_saa, 2, function(x)
    as.numeric(PerformanceAnalytics::Return.cumulative(as.matrix(na.omit(x))))),
  ir_ratio = apply(xts_rel_saa, 2, function(x) {
    x_m <- as.matrix(na.omit(x))
    as.numeric(PerformanceAnalytics::Return.annualized(x_m) /
               PerformanceAnalytics::StdDev.annualized(x_m))
  }),
  beta_spy = apply(xts_ret_saa, 2, function(x)
    cov(x, spy_ret, use = "complete.obs") / var(spy_ret, na.rm = TRUE)),
  corr_to_bmk = apply(xts_ret_saa, 2, function(x)
    as.numeric(cor(x, spy_ret, use = "complete.obs"))),
  vol_scalar = round(
    spy_vol_ann / (apply(xts_ret_saa, 2, sd, na.rm = TRUE) * sqrt(252)), 4
  )
)

# ── L4: Momentum Waterfall ────────────────────────────────────────────────────
message("saa_quantdb [L4]: Momentum Waterfall ...")
l4_mom <- data.frame(
  ticker = tickers,
  # Daily: latest return vs 21-day mean, z-scored by 21-day sd
  mom_d = apply(xts_ret_saa, 2, function(x) {
    x_c <- na.omit(as.numeric(x))
    if (length(x_c) < 22) return(0)
    (last(x_c) - mean(tail(x_c, 21))) / sd(tail(x_c, 21))
  }),
  # Weekly: EMA(4) of weekly returns
  mom_w = apply(xts_w_saa, 2, function(x) {
    x_c <- na.omit(as.numeric(x))
    if (length(x_c) < 5) return(0)
    last(TTR::EMA(x_c, n = 4))
  }),
  # Monthly: EMA(3) of monthly returns
  mom_m = apply(xts_m_saa, 2, function(x) {
    x_c <- na.omit(as.numeric(x))
    if (length(x_c) < 4) return(0)
    last(TTR::EMA(x_c, n = 3))
  })
) %>%
  mutate(
    is_confirmed = ifelse(sign(mom_d) == sign(mom_w), "YES", "NO"),
    mom_stretch  = round(mom_d - mom_m, 4)
  )

# ── Master Join ───────────────────────────────────────────────────────────────
message("saa_quantdb: Master Join ...")
etf_quantdb <- l1_abs %>%
  left_join(l2_rel,      by = "ticker") %>%
  left_join(l4_mom,      by = "ticker") %>%
  left_join(saa_metadata, by = "ticker") %>%
  mutate(across(where(is.numeric), ~ round(., 4))) %>%
  select(ticker, category, cluster, ann_ret, alpha_6y, is_confirmed,
         vol_scalar, everything()) %>%
  arrange(desc(alpha_6y))

# ── Save ──────────────────────────────────────────────────────────────────────
saveRDS(etf_quantdb, here::here("02_data_processed/saa_quantdb.rds"))
message("saa_quantdb: SAVED → 02_data_processed/saa_quantdb.rds  (",
        nrow(etf_quantdb), " tickers)")

# ── Auto-run guard ────────────────────────────────────────────────────────────
if (!isTRUE(getOption("knitr.in.progress"))) {
  cat("\n── Top 10 by Cumulative Alpha ──────────────────────────\n")
  print(etf_quantdb %>%
    select(ticker, cluster, ann_ret, alpha_6y, corr_to_bmk,
           is_confirmed, mom_d) %>%
    head(10))
}
