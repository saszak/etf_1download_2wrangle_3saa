# ==============================================================================
# PROJECT: etf_saa_taa
# MODULE: 01_build_quantdb.R
# PURPOSE: Construct 8-Layer Alpha Database (High-Stability Engine)
# ==============================================================================

source("00_bridge_config.R") 

library(PerformanceAnalytics)
library(dplyr)
library(moments)
library(xts)
library(TTR)

# --- SETUP & VALIDATION ---
xts_ret <- as.xts(as.matrix(xts_ret))
xts_rel <- as.xts(as.matrix(xts_rel))
tickers <- colnames(xts_ret)

if (!("SPY" %in% tickers)) stop("❌ SPY Anchor missing.")

# Force Benchmark to be a strict matrix to prevent environment errors
spy_ret     <- as.matrix(xts_ret[, "SPY"])
spy_vol_ann <- sd(spy_ret, na.rm = TRUE) * sqrt(252)

# Helper: Max Recovery Days
get_max_recovery <- function(x) {
  x_m <- as.matrix(na.omit(x))
  if(nrow(x_m) < 10) return(0)
  dd <- PerformanceAnalytics::table.Drawdowns(x_m, top = 1)
  if(nrow(dd) == 0) return(0)
  return(as.numeric(dd$Recovery)) 
}

# --- LAYER 01: abs (Absolute DNA) ---
message("🧬 Calculating Layer 01: Absolute DNA...")
l1_abs <- data.frame(
  ticker = tickers,
  ann_ret = apply(xts_ret, 2, function(x) {
    as.numeric(PerformanceAnalytics::Return.annualized(as.matrix(na.omit(x))))
  }),
  ann_vol = apply(xts_ret, 2, function(x) {
    as.numeric(PerformanceAnalytics::StdDev.annualized(as.matrix(na.omit(x))))
  }),
  max_dd = apply(xts_ret, 2, function(x) {
    as.numeric(PerformanceAnalytics::maxDrawdown(as.matrix(na.omit(x))))
  }),
  kurtosis_val   = apply(xts_ret, 2, function(x) moments::kurtosis(as.numeric(na.omit(x))) - 3),
  skew_val       = apply(xts_ret, 2, function(x) moments::skewness(as.numeric(na.omit(x)))),
  count_dd_10    = apply(xts_ret, 2, function(x) {
    x_m <- as.matrix(na.omit(x))
    dd <- PerformanceAnalytics::table.Drawdowns(x_m)
    if(nrow(dd) == 0) return(0)
    sum(dd$Depth < -0.10, na.rm = TRUE)
  }),
  max_recovery_d = apply(xts_ret, 2, get_max_recovery)
)

# --- LAYER 02: rel (Relative Edge) ---
message("⚖️ Calculating Layer 02: Relative Edge...")
l2_rel <- data.frame(
  ticker   = tickers,
  alpha_6y = apply(xts_rel, 2, function(x) {
    as.numeric(PerformanceAnalytics::Return.cumulative(as.matrix(na.omit(x))))
  }),
  ir_ratio = apply(xts_rel, 2, function(x) {
    x_m <- as.matrix(na.omit(x))
    as.numeric(PerformanceAnalytics::Return.annualized(x_m) / PerformanceAnalytics::StdDev.annualized(x_m))
  }),
  # MANUAL BETA: Bypasses the 'as.environment' error by using base R math
  beta_spy    = apply(xts_ret, 2, function(x) {
    return(cov(x, spy_ret, use="complete.obs") / var(spy_ret, na.rm=TRUE))
  }),
  corr_to_bmk = apply(xts_ret, 2, function(x) as.numeric(cor(x, spy_ret, use = "complete.obs"))),
  vol_scalar  = round(spy_vol_ann / (apply(xts_ret, 2, sd, na.rm=TRUE) * sqrt(252)), 4)
)

# --- LAYER 04: mom (Momentum Waterfall) ---
message("🌊 Calculating Layer 04: Momentum Waterfall...")
l4_mom <- data.frame(
  ticker = tickers,
  mom_d  = apply(xts_ret, 2, function(x) {
    x_c <- na.omit(as.numeric(x))
    if(length(x_c) < 22) return(0)
    (last(x_c) - mean(tail(x_c, 21))) / sd(tail(x_c, 21))
  }),
  mom_w  = apply(xts_w_abs, 2, function(x) {
    x_c <- na.omit(as.numeric(x))
    if(length(x_c) < 5) return(0)
    last(TTR::EMA(x_c, n = 4))
  }),
  mom_m  = apply(xts_m_abs, 2, function(x) {
    x_c <- na.omit(as.numeric(x))
    if(length(x_c) < 4) return(0)
    last(TTR::EMA(x_c, n = 3))
  })
) %>%
  mutate(
    is_confirmed = ifelse(sign(mom_d) == sign(mom_w), "YES", "NO"),
    mom_stretch  = round(mom_d - mom_m, 4)
  )

# --- THE MASTER JOIN ---
message("🔗 Finalizing Master Join...")
etf_quantdb <- l1_abs %>%
  left_join(l2_rel, by = "ticker") %>%
  left_join(l4_mom, by = "ticker") %>%
  left_join(etf_metadata, by = "ticker") %>%
  mutate(across(where(is.numeric), ~ round(., 4))) %>%
  select(ticker, category, cluster, ann_ret, alpha_6y, is_confirmed, vol_scalar, everything()) %>%
  arrange(desc(alpha_6y))

# Save
if(!dir.exists("data")) dir.create("data")
saveRDS(etf_quantdb, "data/etf_quantdb.rds")
message("✨ SUCCESS: etf_quantdb is ready.")