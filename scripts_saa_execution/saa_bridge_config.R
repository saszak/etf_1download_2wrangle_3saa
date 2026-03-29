################################################################################
# scripts_saa_execution/saa_bridge_config.R
# Purpose : Bridge between parent project data products and the SAA/TAA
#           execution pipeline.  Replaces the T7-SSD dependency from the old
#           etf_saa_taa project with here::here() paths to
#           02_data_processed/.
#
# Provides
#   · xts_ret / xts_rel          daily log returns & alpha (already in parent)
#   · xts_d_abs / _w / _m / _q   daily / weekly / monthly / quarterly abs ret
#   · xts_d_rel / _w / _m / _q   same but relative (alpha vs SPY)
#   · saa_metadata               34-ticker execution universe with cluster col
#   · Bucket vectors             defensive / growth / cycle / intl / anchors
#   · Global constants           saa_ratio, taa_ratio, trade_ratio, eq_bmk, fi_bmk
#
# Dependencies (loaded by parent master_run.R or sourced manually):
#   00_init_universe.R  → etf_metadata
#   01_etf_wrangle.R    → 02_data_processed/xts_ret_returns.rds
#                          02_data_processed/xts_rel.rds
################################################################################

library(tidyverse)
library(xts)
library(here)

# ── 1. LOAD CORE DATA PRODUCTS ────────────────────────────────────────────────

if (!exists("xts_ret")) {
  xts_ret <- readRDS(here::here("02_data_processed/xts_ret_returns.rds"))
  message("saa_bridge: loaded xts_ret (", ncol(xts_ret), " tickers)")
}

if (!exists("xts_rel")) {
  xts_rel <- readRDS(here::here("02_data_processed/xts_rel.rds"))
  message("saa_bridge: loaded xts_rel")
}

if (!exists("etf_metadata")) {
  source(here::here("scripts/00_init_universe.R"))
}

# ── 2. BUILD MULTI-FREQUENCY RETURN CUBES ─────────────────────────────────────
# Old project loaded pre-built weekly/monthly/quarterly rds from the refinery.
# Here we aggregate on-the-fly from the daily xts_ret / xts_rel.

.agg_ret <- function(xts_daily, period = c("weekly", "monthly", "quarterly")) {
  period <- match.arg(period)
  FUN <- switch(period,
    weekly    = xts::apply.weekly,
    monthly   = xts::apply.monthly,
    quarterly = xts::apply.quarterly
  )
  # Sum log returns within each period → equivalent to compounded return
  FUN(xts_daily, function(x) colSums(x, na.rm = TRUE))
}

message("saa_bridge: building multi-frequency cubes ...")
xts_d_abs <- xts_ret
xts_w_abs <- .agg_ret(xts_ret, "weekly")
xts_m_abs <- .agg_ret(xts_ret, "monthly")
xts_q_abs <- .agg_ret(xts_ret, "quarterly")

xts_d_rel <- xts_rel
xts_w_rel <- .agg_ret(xts_rel, "weekly")
xts_m_rel <- .agg_ret(xts_rel, "monthly")
xts_q_rel <- .agg_ret(xts_rel, "quarterly")

# ── 3. SAA EXECUTION UNIVERSE (34-TICKER CLUSTER MAP) ─────────────────────────
# Subset of the 96-ticker Sovereign Universe enriched with a `cluster` column
# used by the execution pipeline for crowding / drift analysis.
# VLUE replaced by VTV (parent project canonical; VLUE dropped at intake).

saa_metadata <- tribble(
  ~ticker,  ~category,  ~cluster,
  # ── Equity: Broad Market ──────────────────────────────────────────────────
  "SPY",    "Equity",   "Broad Market",
  # ── Equity: Factor ───────────────────────────────────────────────────────
  "QUAL",   "Equity",   "Factor Play",
  "VTV",    "Equity",   "Factor Play",        # VTV replaces VLUE
  "MTUM",   "Equity",   "Factor Play",
  "USMV",   "Equity",   "Factor Play",
  # ── Equity: Sector Growth ────────────────────────────────────────────────
  "XLK",    "Equity",   "Growth Laggards",
  # ── Equity: Sector Cycle ────────────────────────────────────────────────
  "XLF",    "Equity",   "Cycle & Breadth",
  "XLE",    "Equity",   "Cycle & Breadth",
  "XLI",    "Equity",   "Cycle & Breadth",
  "XLP",    "Equity",   "Cycle & Breadth",
  "XLY",    "Equity",   "Cycle & Breadth",
  # ── Equity: Defensive Sector ────────────────────────────────────────────
  "XLV",    "Equity",   "Defensive Bulwark",
  # ── Equity: Utilities / Infra ───────────────────────────────────────────
  "XLU",    "Equity",   "Pipes & Power",
  "IGF",    "Equity",   "Pipes & Power",
  # ── Equity: International ───────────────────────────────────────────────
  "AAXJ",   "Equity",   "Multipolar Alpha",   # replaced EWY — broad AC Asia ex Japan
  "EWJ",    "Equity",   "Multipolar Alpha",
  # ── Fixed Income: Cash ──────────────────────────────────────────────────
  "SGOV",   "Fixed",    "Cash/Ultra-Short",
  # ── Fixed Income: Gov Curve ─────────────────────────────────────────────
  "SHY",    "Fixed",    "Defensive Bulwark",
  "IEF",    "Fixed",    "Defensive Bulwark",
  "TLT",    "Fixed",    "Defensive Bulwark",
  "TIP",    "Fixed",    "Defensive Bulwark",
  # ── Fixed Income: IG & HY ───────────────────────────────────────────────
  "LQD",    "Fixed",    "Cycle & Breadth",
  "HYG",    "Fixed",    "Cycle & Breadth",
  # ── Fixed Income: EM ────────────────────────────────────────────────────
  "EMB",    "Fixed",    "Multipolar Alpha",
  "EMLC",   "Fixed",    "Multipolar Alpha",
  # ── Fixed Income: Aggregate / Benchmark ─────────────────────────────────
  "AGG",    "Fixed",    "Broad Bonds",
  "BND",    "Fixed",    "Broad Bonds",
  # ── Multi-Asset: Balanced ───────────────────────────────────────────────
  "AOK",    "Multi",    "Defensive Bulwark",
  "AOM",    "Multi",    "Cycle & Breadth",
  "AOR",    "Multi",    "Cycle & Breadth",
  # ── Alternatives: Precious Metals ───────────────────────────────────────
  "GLD",    "Alt",      "Defensive Bulwark",
  "SLV",    "Alt",      "Defensive Bulwark",
  # ── Alternatives: REIT ──────────────────────────────────────────────────
  "IYR",    "Alt",      "Cycle & Breadth"
)

# Keep only tickers present in the loaded xts_ret (handles missing downloads)
saa_metadata <- saa_metadata %>%
  filter(ticker %in% colnames(xts_ret))

# Full metadata: enrich with parent's etf_metadata where available
saa_metadata <- saa_metadata %>%
  left_join(
    etf_metadata %>% select(ticker, name, asset_class, sub_block, pf_function),
    by = "ticker"
  )

tickers <- saa_metadata$ticker

# ── 4. GLOBAL BUCKET VECTORS ──────────────────────────────────────────────────
defensive_tickers <- saa_metadata %>% filter(cluster == "Defensive Bulwark") %>% pull(ticker)
growth_tickers    <- saa_metadata %>% filter(cluster == "Growth Laggards")   %>% pull(ticker)
cycle_tickers     <- saa_metadata %>% filter(cluster == "Cycle & Breadth")   %>% pull(ticker)
intl_tickers      <- saa_metadata %>% filter(cluster == "Multipolar Alpha")  %>% pull(ticker)
anchors           <- c("SPY", "AGG", "GLD")

# ── 5. GLOBAL CONSTANTS ───────────────────────────────────────────────────────
saa_ratio   <- 0.60
taa_ratio   <- 0.30
trade_ratio <- 0.10
eq_bmk      <- "SPY"
fi_bmk      <- "AGG"

# Validate SPY anchor in relative data
if (any(xts_d_rel[, eq_bmk] != 0, na.rm = TRUE)) {
  warning("saa_bridge: SPY column in xts_rel is not all-zero. Check 01_etf_wrangle.R.")
}

message("saa_bridge: READY | ", nrow(saa_metadata), " tickers | ",
        "Defensives=", length(defensive_tickers),
        "  Cycle=", length(cycle_tickers),
        "  Intl=", length(intl_tickers))
