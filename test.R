# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./test.R
# Purpose: Biotech Regime Analysis — XBI, IBB, 50/50 XLV+IHI vs SPY
# Run with: source("test.R")
# ==============================================================================

library(tidyverse)
library(xts)
library(PerformanceAnalytics)
library(scales)
library(patchwork)
library(here)

# ── Data ───────────────────────────────────────────────────────────────────────
xts_ret <- readRDS(here("02_data_processed/xts_ret_returns.rds"))

# ── Sources ────────────────────────────────────────────────────────────────────
if (!exists("build_regime_table"))
  source(here("scripts_spy_dd_regime/spy_dd_regime.R"))
if (!exists("plot_regime_rel_overlay"))
  source(here("scripts_spy_dd_regime/spy_dd_regime_rel.R"))

# ── Synthetic portfolio: 50% XLV + 50% IHI ────────────────────────────────────
pf_ret <- 0.5 * xts_ret[, "XLV"] + 0.5 * xts_ret[, "IHI"]
colnames(pf_ret) <- "XLV_IHI"

# ── Local xts for this analysis (does not mutate global xts_ret) ───────────────
biotech_xts     <- cbind(xts_ret[, c("SPY", "XBI", "IBB")], pf_ret)
biotech_tickers <- c("XBI", "IBB", "XLV_IHI")

# ── Plot 1: Mixed overlay (SPY absolute, others α vs SPY) ─────────────────────
plot_regime_rel_overlay(
  xts_ret = biotech_xts,
  tickers = biotech_tickers,
  master  = "SPY",
  t_fall  = 0.10
)

