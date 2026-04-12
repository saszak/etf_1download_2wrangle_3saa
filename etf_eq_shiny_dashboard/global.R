# ==============================================================================
# shiny_dashboard/global.R
# PURPOSE : Bridge to root project — loads all shared data + builds perf_data.
#           Sourced automatically by Shiny before app.R.
# ==============================================================================

library(shiny)
library(bslib)
library(reactable)
library(plotly)
library(tidyverse)
library(htmltools)
library(here)
library(scales)
library(lubridate)
library(patchwork)
library(ggrepel)
library(cowplot)
library(PerformanceAnalytics)
library(TTR)

# PerformanceAnalytics pulls in MASS which masks dplyr::select — restore dplyr
select <- dplyr::select

# ── Utilities + modules FIRST — so theme constants are always defined ──────────
# Use relative paths — Shiny sets working dir to shiny_dashboard/ at launch
# Modules and helpers: always re-source so updated signatures are picked up
source("utils/helpers.R")
source("saa_config.R")
source("modules/mod_perf_table.R")
source("modules/mod_treemap.R")
source("modules/mod_plots.R")
source("modules/mod_absrel.R")
source("modules/mod_calyear.R")
source("modules/mod_comp.R")
source("modules/mod_technical.R")
source("modules/mod_regime.R")
source("modules/mod_archetypes.R")
source("modules/mod_riskret.R")
source("modules/mod_surprise.R")
source("modules/mod_patterns.R")
source(here::here("scripts_state_machine/sm_engine.R"))
source(here::here("scripts_state_machine/sm_visuals.R"))
source(here::here("key_plots/chart_ma200_signal_panel.R"))
source(here::here("key_plots/chart_multi_wealth_endlabel.R"))
source(here::here("key_plots/chart_stock_matrix.R"))
source(here::here("key_plots/chart_regime_rel_overlay.R"))
source(here::here("scripts_spy_dd_regime/spy_dd_regime_rel.R"))
source(here::here("utility/plot_calendar_perf.R"))
source(here::here("utility/plot_calendar_analog.R"))
# Source only function defs from spy_dd_regime.R — skip auto-run block (line 635+)
.spy_lines <- readLines(here::here("scripts_spy_dd_regime/spy_dd_regime.R"))
eval(parse(text = .spy_lines[seq_len(634L)]))
rm(.spy_lines)

# ── Root project bridge ────────────────────────────────────────────────────────
if (!exists("project_tree")) source(here::here("project_tree.R"))
if (!exists("REBAL_FREQ"))   source(here::here("00_global_params.R"))
if (!exists("etf_metadata") || !"short_name" %in% names(etf_metadata))
  source(here::here(project_tree$scripts$init))
# Always re-source — ensures build_derived_returns() and other function
# definitions stay current even in warm R sessions (no !exists guard here).
source(here::here("scripts/00_derived_universe.R"))

if (!exists("raw_data"))       raw_data       <- read_rds(here::here(project_tree$products$raw_p_d))
if (!exists("tech_summary"))   tech_summary   <- read_rds(here::here(project_tree$products$tech_summary))
if (!exists("outlier_report")) outlier_report <- read_rds(here::here(project_tree$products$ref_report))

# xts returns + relative returns (used by AbsRel tab)
if (!exists("xts_ret_shiny")) xts_ret_shiny <- read_rds(here::here(project_tree$products$refined_ret))
if (!exists("xts_rel_shiny")) xts_rel_shiny <- read_rds(here::here("02_data_processed/xts_rel.rds"))
if (!exists("rt_shiny"))      rt_shiny      <- build_regime_table(xts_ret_shiny[, "SPY"])

# ── Universe sync check ────────────────────────────────────────────────────────
# Warn at startup if any universe tickers are missing from the loaded RDS.
# Root cause: RDS was built before ticker was added. Fix: re-run 01_etf_wrangle.R
.missing_tks <- setdiff(etf_metadata$ticker, colnames(xts_ret_shiny))
if (length(.missing_tks) > 0) {
  message(
    "\n⚠️  xts_ret_shiny is OUT OF SYNC with etf_metadata (",
    length(.missing_tks), " tickers missing):\n",
    paste(" -", .missing_tks, collapse = "\n"), "\n",
    "→ Re-run scripts/01_etf_wrangle.R to rebuild the RDS, then restart the app.\n"
  )
}
rm(.missing_tks)

# ── Merge pre-built derived portfolio columns into xts_ret_shiny ───────────────
# xts_derived_ret.rds is built by 01_etf_wrangle.R — no runtime computation here.
if (!exists("xts_derived_ret"))
  xts_derived_ret <- read_rds(here::here(project_tree$products$derived_ret))
new_cols <- setdiff(colnames(xts_derived_ret), colnames(xts_ret_shiny))
if (length(new_cols) > 0)
  xts_ret_shiny <- merge(xts_ret_shiny, xts_derived_ret[, new_cols, drop = FALSE], join = "left")
message("✅ xts_ret_shiny derived cols: ",
        paste(intersect(DERIVED_UNIVERSE$id, colnames(xts_ret_shiny)), collapse = " | "),
        "  [total cols: ", ncol(xts_ret_shiny), "]")

# ── Date constants ─────────────────────────────────────────────────────────────
today     <- Sys.Date()
ytd_start <- as.Date(paste0(year(today), "-01-01"))
mtd_start <- floor_date(today, "month")
w52_start <- today - 365

# ── Build performance table ────────────────────────────────────────────────────
if (exists("perf_data") && "short_name" %in% names(perf_data)) return(invisible(NULL))

perf_data <- raw_data %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(adjusted = as.numeric(adjusted)) %>%
  summarise(
    latest_price = last(adjusted),
    latest_date  = last(date),
    ret_1d  = (last(adjusted) / nth(adjusted, -2L))  - 1,
    ret_5d  = (last(adjusted) / nth(adjusted, -6L))  - 1,
    ret_1m  = (last(adjusted) / nth(adjusted, -22L)) - 1,
    ret_ytd = (last(adjusted) / first(adjusted[date >= ytd_start])) - 1,
    ret_mtd = (last(adjusted) / first(adjusted[date >= mtd_start])) - 1,
    ret_3m  = (last(adjusted) / nth(adjusted, -63L)) - 1,
    low_52w   = min(adjusted[date >= w52_start],  na.rm = TRUE),
    high_52w  = max(adjusted[date >= w52_start],  na.rm = TRUE),
    range_pct = pmin(pmax(
      (last(adjusted) - min(adjusted[date >= w52_start], na.rm = TRUE)) /
      (max(adjusted[date >= w52_start], na.rm = TRUE) -
       min(adjusted[date >= w52_start], na.rm = TRUE)), 0), 1),
    spark     = .svg_spark(adjusted),
    .groups   = "drop"
  ) %>%
  left_join(
    etf_metadata %>% dplyr::select(ticker, name, short_name, asset_class, pf_function,
                            tree_level, sub_block),
    by = c("symbol" = "ticker")
  ) %>%
  left_join(
    tech_summary %>% dplyr::select(ticker, trend_regime, momentum_status),
    by = c("symbol" = "ticker")
  ) %>%
  left_join(
    outlier_report %>% dplyr::select(ticker, label),
    by = c("symbol" = "ticker")
  ) %>%
  filter(!is.na(ret_ytd), symbol %in% etf_metadata$ticker) %>%
  mutate(
    asset_class = coalesce(asset_class, "Other"),
    sub_block   = coalesce(sub_block,   "Other")
  )

# ── Cluster map ────────────────────────────────────────────────────────────────
CLUSTER_MAP <- tibble::tribble(
  ~ticker,  ~cluster,
  "URTH",   "Core Assets",   "ACWI",  "Core Assets",
  "SPY",    "Core Assets",   "QQQ",   "Core Assets",
  "IEFA",   "Core Assets",   "ACWX",  "Core Assets",
  "XLK",    "SPY Sectors",   "XLC",   "SPY Sectors",
  "XLV",    "SPY Sectors",   "XLF",   "SPY Sectors",
  "XLI",    "SPY Sectors",   "XLP",   "SPY Sectors",
  "XLY",    "SPY Sectors",   "XLE",   "SPY Sectors",
  "XLB",    "SPY Sectors",   "XLRE",  "SPY Sectors",
  "XLU",    "SPY Sectors"
)

perf_data <- perf_data %>%
  left_join(CLUSTER_MAP,  by = c("symbol" = "ticker")) %>%
  mutate(cluster = coalesce(cluster, "Rest of Universe")) %>%
  left_join(SAA_CONFIG,    by = c("symbol" = "ticker")) %>%
  left_join(DISPLAY_RANK,  by = c("symbol" = "ticker")) %>%
  mutate(display_rank = coalesce(display_rank, 9999L))
  # saa_depth / saa_bucket = NA for tickers outside the SAA tree
  # display_rank = 9999 for tickers not in DISPLAY_RANK

# Benchmark reference row (used by KPI strip)
spy_row <- perf_data %>% filter(symbol == "SPY")

# ── Equity coverage list (EquityCoreList.xlsx) ────────────────────────────────
if (!exists("eq_coverage")) {
  eq_coverage <- readxl::read_excel(
    here::here("input/EquityCoreList.xlsx"), skip = 3
  ) %>%
    filter(!is.na(Bloomberg)) %>%
    mutate(
      ticker     = sub(" [A-Z]{2}$", "", Bloomberg),
      region     = coalesce(Region, "Unknown"),
      sector     = coalesce(`Factset Sector`, "Other"),
      exp_ret    = as.numeric(`Expected Return`),
      tgt_price  = as.numeric(`Target Price`),
      price      = as.numeric(Price),
      upside_pct = round((tgt_price / price - 1) * 100, 1),
      div_yield  = as.numeric(`Dividend Yield`),
      pe         = suppressWarnings(as.numeric(`P/E`)),
      eps25      = as.numeric(`Earnings per Share (EPS) 2025`),
      eps26      = as.numeric(`Earnings per Share (EPS) 2026(E)`),
      top_pick   = `Top Picks` == "Yes",
      rating     = Rating,
      risk       = Risk,
      analyst    = Analyst,
      mkt_cap    = `Market Value in bn`
    ) %>%
    dplyr::select(ticker, Company, region, sector, rating, risk, top_pick,
           exp_ret, tgt_price, price, upside_pct, div_yield,
           pe, eps25, eps26, analyst, mkt_cap)
}

# ── Regime Archetypes (equity tickers vs SPY) ─────────────────────────────────
if (!exists("archetype_data")) {
  source(here::here("scripts_saa_taa/equity_spread_screen.R"))
  .screen <- equity_spread_screen(xts_ret_shiny, xts_rel_shiny, etf_metadata, rt_shiny)
  archetype_data <- .screen %>%
    mutate(
      archetype = dplyr::case_when(
        rho_full < 0.75                                                      ~ "Corr-Fail",
        Fall_modal_type == "LOSS" & Recovery_modal_type == "ALPHA" &
          Consolidation_modal_type == "ALPHA"                                ~ "Cycle Amplifier",
        Fall_modal_type == "LOSS" & Recovery_modal_type == "ALPHA" &
          Consolidation_modal_type == "LAG"                                  ~ "Recovery Sprinter",
        Fall_modal_type %in% c("STABLE", "HEDGE") &
          Recovery_modal_type == "LAG"                                       ~ "Defensive Burden",
        Fall_modal_type == "LOSS" & Recovery_modal_type == "LAG"             ~ "All-Regime Drag",
        TRUE                                                                 ~ "Other"
      )
    )
  rm(.screen)
}

# ── 200DMA trend signals (used by Plots tab) ───────────────────────────────────
# Computed once at launch from raw_data; stats::filter() = base R rolling mean
if (!exists("trend_signals_shiny")) {
  trend_signals_shiny <- raw_data %>%
    arrange(symbol, date) %>%
    mutate(adjusted = as.numeric(adjusted)) %>%
    group_by(symbol) %>%
    mutate(
      ma200  = as.numeric(stats::filter(adjusted, rep(1/200, 200), sides = 1)),
      signal = as.integer(!is.na(ma200) & adjusted >= ma200)
    ) %>%
    ungroup()
}
