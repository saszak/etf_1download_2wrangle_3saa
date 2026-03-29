################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE:    key_plots/saa_sunburst_plotly.R
# Purpose: Interactive plotly sunburst of the SAA portfolio — kept as a
#          standalone example. NOT sourced in Rmd (HTML-only, no PDF support).
#          For the Rmd-safe static treemap use plot_saa_treemap() in saa_tree.R
################################################################################

library(tidyverse)
library(scales)
library(plotly)
library(here)

source(here("scripts_saa_taa/saa_tree.R"))

if (!exists("xts_ret")) {
  xts_ret <- readRDS(here("02_data_processed/xts_ret_returns.rds"))
}

# ── Interactive sunburst (3 rings: Asset Class → Role → Ticker) ───────────────
plot_saa_sunburst(xts_ret = xts_ret)
