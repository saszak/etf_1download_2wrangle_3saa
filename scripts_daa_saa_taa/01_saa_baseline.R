################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : scripts_daa_saa_taa/01_saa_baseline.R
# Purpose : Define the 60/40 SAA baseline portfolio and passive benchmark.
#           Provides the neutral starting point for all DAA/TAA overlays.
#
# OUTPUTS
#   SAA_60_40          tribble  — 8-ticker 60/40 portfolio with weights & roles
#   TAA_RANGES         tribble  — allowed min/neutral/max per bucket
#   saa_returns        xts      — daily returns of the SAA portfolio
#   benchmark_returns  xts      — daily returns of passive SPY60/AGG40 benchmark
#   p_saa_wealth               — wealth comparison plot (SAA vs benchmark)
#
# FUNCTIONS
#   calc_portfolio_returns(weights_tbl, xts_ret)
#   build_passive_benchmark(xts_ret, eq_w, fi_w)
#   plot_saa_wealth(saa_ret, bmk_ret, rt)
################################################################################

library(tidyverse)
library(xts)
library(PerformanceAnalytics)
library(scales)
library(here)

if (!exists("project_tree"))  source(here("project_tree.R"))
if (!exists("etf_metadata"))  source(here(project_tree$scripts$init))
if (!exists("xts_ret"))       source(here(project_tree$scripts$wrangle))
if (!exists("build_regime_table"))
  source(here("scripts_spy_dd_regime/spy_dd_regime.R"))
if (!exists("rt") || !is.data.frame(rt))
  rt <- build_regime_table(xts_ret[, "SPY"])

# ==============================================================================
# 1. SAA PORTFOLIO DEFINITION (60 / 40)
#    Built from the confirmed JB DAA sub-asset universe (PDF page 17).
#    Each ticker maps 1-to-1 to a JB sub-asset category.
# ==============================================================================
#
# Bucket  Weight  JB Sub-Asset          Ticker
# ─────── ──────  ────────────────────  ──────────────────────────────────────
# EQ       20%    S&P 500               SPY
# EQ       10%    Nasdaq 100            QQQ
# EQ       10%    MSCI Europe / DM      IEFA
# EQ        8%    Russell 2000          IWM
# EQ        5%    MSCI Canada           EWC
# EQ        4%    S&P Biotech           IBB
# EQ        3%    Asia ex-Japan REITs   VNQI
# ─────── ──────  ────────────────────  ──────────────────────────────────────
# FI       12%    US Treasuries mid     IEF
# FI        8%    Treasury 20yr+        TLT
# FI        6%    USD High Yield        HYG
# FI        5%    TIPS                  TIP
# FI        5%    EM HC Debt            EMB
# FI        4%    Liquidity / Cash      SGOV
# ─────── ──────
# Total   100%    EQ 60% / FI 40%

SAA_60_40 <- tribble(
  ~ticker, ~weight, ~asset_class,  ~role,             ~saa_bucket, ~jb_sub_asset,
  # ── Equity (60%) ────────────────────────────────────────────────────────────
  "SPY",   0.20,    "Equity",      "Anchor",          "EQ",   "S&P 500",
  "QQQ",   0.10,    "Equity",      "Core-Growth",     "EQ",   "Nasdaq 100",
  "IEFA",  0.10,    "Equity",      "Core-Growth",     "EQ",   "MSCI Europe/DM",
  "IWM",   0.08,    "Equity",      "Core-Growth",     "EQ",   "Russell 2000",
  "EWC",   0.05,    "Equity",      "Tactical",        "EQ",   "MSCI Canada",
  "IBB",   0.04,    "Equity",      "Satellite",       "EQ",   "S&P Biotech",
  "VNQI",  0.03,    "RealAsset",   "Tactical",        "EQ",   "Asia ex-JP REITs",
  # ── Fixed Income (40%) ──────────────────────────────────────────────────────
  "IEF",   0.12,    "FixedIncome", "Core-Stabilizer", "FI",   "US Treasuries mid",
  "TLT",   0.08,    "FixedIncome", "Core-Stabilizer", "FI",   "Treasury 20yr+",
  "HYG",   0.06,    "FixedIncome", "Tactical",        "FI",   "USD HY Bonds",
  "TIP",   0.05,    "FixedIncome", "Real-Shield",     "FI",   "TIPS",
  "EMB",   0.05,    "FixedIncome", "Tactical",        "FI",   "EM HC Debt",
  "SGOV",  0.04,    "FixedIncome", "Core-Stabilizer", "Cash", "Liquidity"
)

stopifnot(abs(sum(SAA_60_40$weight) - 1) < 1e-9)

# ==============================================================================
# 2. TAA RANGE TABLE
# Defines the allowed weight band per bucket for TAA tilts.
# The overlay (03_taa_rules.R) will shift weights within these bands.
# ==============================================================================
TAA_RANGES <- tribble(
  ~bucket, ~min,  ~neutral, ~max,  ~note,
  "EQ",    0.40,   0.60,    0.80,  "Equity sleeve — shift ±20pp max",
  "FI",    0.15,   0.35,    0.55,  "Fixed income sleeve",
  "Real",  0.00,   0.05,    0.12,  "Gold / inflation buffer",
  "Cash",  0.00,   0.00,    0.20,  "SGOV activated only in deep Fall"
)

# ==============================================================================
# 3. FUNCTIONS
# ==============================================================================

#' calc_portfolio_returns
#'
#' Compute daily portfolio returns from a weights tibble and an xts return matrix.
#' Weights are applied as a static (buy-and-hold) vector — rebalancing handled
#' upstream by the backtest engine.
#'
#' @param weights_tbl  tibble with columns: ticker, weight
#' @param xts_ret      xts matrix of daily log returns (tickers as column names)
#' @return             xts vector of daily portfolio returns
calc_portfolio_returns <- function(weights_tbl, xts_ret) {
  tickers <- weights_tbl$ticker
  weights <- weights_tbl$weight

  # Keep only tickers present in xts_ret
  available <- intersect(tickers, colnames(xts_ret))
  missing   <- setdiff(tickers, colnames(xts_ret))
  if (length(missing) > 0)
    warning("calc_portfolio_returns: tickers not in xts_ret — skipped: ",
            paste(missing, collapse = ", "))

  w <- weights[match(available, tickers)]
  w <- w / sum(w)   # re-normalise if any tickers dropped

  ret_subset <- xts_ret[, available]
  port_ret   <- xts(as.numeric(ret_subset %*% w),
                    order.by = index(ret_subset))
  colnames(port_ret) <- "SAA_60_40"
  port_ret
}

#' build_passive_benchmark
#'
#' Two-asset passive 60/40: SPY (equity) + IEF (fixed income).
#' IEF is the primary FI anchor in the JB DAA universe (12% weight).
#'
#' @param xts_ret  xts return matrix
#' @param eq_w     equity weight (default 0.60)
#' @param fi_w     fixed income weight (default 0.40)
#' @return         xts vector of daily benchmark returns
build_passive_benchmark <- function(xts_ret, eq_w = 0.60, fi_w = 0.40) {
  stopifnot(abs(eq_w + fi_w - 1) < 1e-9)
  stopifnot(all(c("SPY", "IEF") %in% colnames(xts_ret)))

  bmk_vals <- as.numeric(xts_ret[, "SPY"]) * eq_w +
              as.numeric(xts_ret[, "IEF"]) * fi_w
  bmk <- xts(bmk_vals, order.by = index(xts_ret))
  colnames(bmk) <- "Passive_60_40"
  bmk
}

#' plot_saa_wealth
#'
#' Cumulative wealth chart comparing SAA 60/40 vs passive benchmark,
#' with regime shading from rt.
#'
#' @param saa_ret  xts — SAA daily returns
#' @param bmk_ret  xts — benchmark daily returns
#' @param rt       data.frame — regime table from build_regime_table()
#' @return         ggplot object
plot_saa_wealth <- function(saa_ret, bmk_ret, rt) {
  combined <- merge(saa_ret, bmk_ret, join = "inner")

  wealth_df <- as.data.frame(combined) %>%
    rownames_to_column("date") %>%
    mutate(date = as.Date(date)) %>%
    pivot_longer(-date, names_to = "portfolio", values_to = "ret") %>%
    group_by(portfolio) %>%
    arrange(date) %>%
    mutate(wealth = cumprod(1 + ret)) %>%
    ungroup()

  regime_rect <- rt %>%
    filter(regime %in% c("Fall", "Recovery")) %>%
    mutate(fill = if_else(regime == "Fall", "#f87171", "#86efac"),
           alpha = 0.12)

  ggplot(wealth_df, aes(date, wealth, colour = portfolio)) +
    geom_rect(data = regime_rect,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = regime_rect$fill,
              inherit.aes = FALSE, alpha = 0.12) +
    geom_line(linewidth = 0.9) +
    scale_colour_manual(
      name   = NULL,
      values = c("SAA_60_40"    = "#3b82f6",
                 "Passive_60_40"= "#9ca3af"),
      labels = c("SAA_60_40"    = "SAA 60/40 — JB DAA universe (13 tickers)",
                 "Passive_60_40"= "Passive 60/40 — SPY / IEF")
    ) +
    scale_y_continuous(
      labels = scales::dollar_format(prefix = "$"),
      limits = c(0.75, NA),
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      title    = "SAA 60/40 vs Passive Benchmark — Cumulative Wealth ($1 Invested)",
      subtitle = "Background shading: red = Fall regime   |   green = Recovery regime",
      x = NULL, y = "Wealth ($)", colour = NULL
    ) +
    # Direct end-of-line labels — no legend needed
    geom_text(
      data = wealth_df %>% group_by(portfolio) %>% slice_max(date, n = 1),
      aes(label = case_when(
        portfolio == "SAA_60_40"     ~ "SAA 60/40\n(JB DAA universe)",
        portfolio == "Passive_60_40" ~ "Passive\nSPY / IEF",
        TRUE ~ portfolio
      )),
      hjust = 0, nudge_x = 60, size = 3.2, fontface = "bold",
      lineheight = 0.9
    ) +
    guides(colour = "none") +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 12) +
    theme(
      plot.margin      = margin(5, 120, 5, 5),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 13)
    )
}

# ==============================================================================
# 4. COMPUTE BASELINE
# ==============================================================================
message("📐 SAA 60/40 — JB DAA sub-asset universe (13 tickers):")
SAA_60_40 %>%
  mutate(wt = sprintf("%4.0f%%", weight * 100)) %>%
  select(saa_bucket, ticker, wt, jb_sub_asset) %>%
  as.data.frame() %>%
  print(row.names = FALSE)

message("📐 Building SAA 60/40 baseline returns...")

saa_returns       <- calc_portfolio_returns(SAA_60_40, xts_ret)
benchmark_returns <- build_passive_benchmark(xts_ret)

# Summary statistics
.saa_stats <- function(ret_xts, label) {
  ann_ret  <- prod(1 + as.numeric(ret_xts))^(252 / length(ret_xts)) - 1
  ann_vol  <- sd(as.numeric(ret_xts)) * sqrt(252)
  max_dd   <- as.numeric(maxDrawdown(ret_xts))
  ir       <- ann_ret / ann_vol
  cat(sprintf("  %-22s  Return: %+.1f%%  Vol: %.1f%%  MaxDD: %.1f%%  IR: %.2f\n",
              label, ann_ret * 100, ann_vol * 100, max_dd * 100, ir))
}

message("📊 Baseline performance:")
.saa_stats(saa_returns,       "SAA 60/40 (8 tickers)")
.saa_stats(benchmark_returns, "Passive SPY60/AGG40  ")

# ==============================================================================
# 5. PLOTS (guarded for knitr)
# ==============================================================================
if (!isTRUE(getOption("knitr.in.progress"))) {
  p_saa_wealth <- plot_saa_wealth(saa_returns, benchmark_returns, rt)
  print(p_saa_wealth)
}

# ==============================================================================
# 6. COMPONENT RELATIVE ANALYSIS
#    Each EQ ticker individually vs SPY (regime-relative overlay + heatmap).
#    Each FI ticker individually vs IEF.
#    Requires spy_dd_regime_rel.R functions.
# ==============================================================================
if (!exists("run_regime_rel_analysis"))
  source(here("scripts_spy_dd_regime/spy_dd_regime_rel.R"))
if (!exists("plot_xts_pair"))
  source(here("utility/plot_xts_pair.R"))

if (!isTRUE(getOption("knitr.in.progress"))) {

  eq_tickers <- SAA_60_40 %>%
    filter(saa_bucket == "EQ", ticker != "SPY") %>%
    pull(ticker)

  fi_tickers <- SAA_60_40 %>%
    filter(saa_bucket %in% c("FI", "Cash"), ticker != "IEF") %>%
    pull(ticker)

  # ── Regime relative analysis (overlay + heatmap + summary) ──────────────────
  message("\n── SAA EQ Components vs SPY — Regime Relative ──")
  for (tk in eq_tickers) {
    message(sprintf("  plotting %s vs SPY...", tk))
    run_regime_rel_analysis(xts_ret, tickers = tk, master = "SPY")
  }

  message("\n── SAA FI Components vs IEF — Regime Relative ──")
  for (tk in fi_tickers) {
    message(sprintf("  plotting %s vs IEF...", tk))
    run_regime_rel_analysis(xts_ret, tickers = tk, master = "IEF")
  }

  # ── XTS pair analysis (7-panel: wealth / spread / DD / dist / ρ / regime-ρ / IR) ──
  message("\n── SAA EQ Components vs SPY — Pair Analysis ──")
  for (tk in eq_tickers) {
    if (!tk %in% colnames(xts_ret)) {
      message(sprintf("  skipping %s — not in xts_ret", tk)); next
    }
    message(sprintf("  pair analysis %s vs SPY...", tk))
    plot_xts_pair(
      xts_ret[, tk],  xts_ret[, "SPY"],
      label1      = tk,    label2      = "SPY",
      title_prefix = sprintf("SAA EQ: %s", tk),
      regime_tbl  = rt
    )
  }

  message("\n── SAA FI Components vs IEF — Pair Analysis ──")
  for (tk in fi_tickers) {
    if (!tk %in% colnames(xts_ret)) {
      message(sprintf("  skipping %s — not in xts_ret", tk)); next
    }
    message(sprintf("  pair analysis %s vs IEF...", tk))
    plot_xts_pair(
      xts_ret[, tk],  xts_ret[, "IEF"],
      label1      = tk,    label2      = "IEF",
      title_prefix = sprintf("SAA FI: %s", tk),
      regime_tbl  = rt
    )
  }
}

message("✅ Stage DAA-01 complete: SAA baseline and benchmark constructed.")
