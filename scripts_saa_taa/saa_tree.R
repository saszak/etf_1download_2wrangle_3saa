################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE:    scripts_saa_taa/saa_tree.R
# Purpose: Visualise the SAA portfolio — static ggplot only (PDF + HTML safe)
#
# PORTFOLIO
#   35% AGG  — Fixed Income core
#   25% URTH — Global equity anchor
#   25% SPY  — US broad equity anchor
#    5% QQQ  — US growth
#    5% XLK  — US technology sector
#    5% SMH  — Semiconductors (satellite)
#
# OUTPUT
#   calc_ytd()            — compute YTD return from xts_ret for each ticker
#   enrich_ytd()          — join YTD onto portfolio table
#   plot_saa_ggbar()      — static ggplot stacked bar with YTD
#   plot_saa_treemap()    — static ggplot treemap via treemapify with YTD
#
# NOTE: Interactive plotly sunburst lives in key_plots/saa_sunburst_plotly.R
################################################################################

library(tidyverse)
library(scales)
library(xts)
library(here)
library(treemapify)

# ── Portfolio definition ───────────────────────────────────────────────────────

SAA_PORTFOLIO <- tribble(
  ~ticker, ~name,                        ~weight, ~asset_class,   ~layer,        ~role,
  "AGG",   "US Aggregate Bond",          0.35,    "Fixed Income",  "FI_IG",       "Core-Stabilizer",
  "URTH",  "iShares MSCI World",         0.25,    "Equity",        "L1_World",    "Anchor",
  "SPY",   "SPDR S&P 500",               0.25,    "Equity",        "L2_US_Broad", "Anchor",
  "QQQ",   "Invesco Nasdaq 100",         0.05,    "Equity",        "L2_US_Broad", "Core-Growth",
  "XLK",   "Technology Select Sector",   0.05,    "Equity",        "L3_Sector",   "Core-Growth",
  "SMH",   "VanEck Semiconductor",       0.05,    "Equity",        "Satellite",   "Satellite"
)

# Palette — asset class level
ASSET_CLASS_PAL <- c(
  "Fixed Income" = "#2D6A4F",
  "Equity"       = "#1D3557"
)

# Palette — role level
ROLE_PAL <- c(
  "Core-Stabilizer" = "#52B788",
  "Anchor"          = "#457B9D",
  "Core-Growth"     = "#E9C46A",
  "Satellite"       = "#E76F51"
)

# ── YTD helper ─────────────────────────────────────────────────────────────────

calc_ytd <- function(xts_ret, tickers) {
  ytd_start <- as.Date(paste0(format(max(index(xts_ret)), "%Y"), "-01-01"))
  ytd_ret   <- xts_ret[paste0(ytd_start, "/"), tickers]

  sapply(tickers, function(tk) {
    r <- ytd_ret[, tk]
    r <- r[!is.na(r)]
    if (length(r) == 0) return(NA_real_)
    as.numeric(prod(1 + r) - 1)
  })
}

# ── Enrich portfolio with YTD ──────────────────────────────────────────────────

enrich_ytd <- function(portfolio, xts_ret) {
  ytd <- calc_ytd(xts_ret, portfolio$ticker)
  portfolio %>%
    mutate(
      ytd       = ytd[ticker],
      ytd_label = if_else(
        is.na(ytd), "n/a",
        paste0(if_else(ytd >= 0, "+", ""), percent(ytd, accuracy = 0.1))
      ),
      ytd_sign  = case_when(
        is.na(ytd)  ~ "n/a",
        ytd >= 0    ~ "pos",
        TRUE        ~ "neg"
      )
    )
}

# ── 1. plot_saa_ggbar() ────────────────────────────────────────────────────────
# Static ggplot horizontal stacked bar — suitable for Rmd / PDF embedding
# Label inside each segment: TICKER  wt%  YTD%

plot_saa_ggbar <- function(portfolio = SAA_PORTFOLIO, xts_ret = NULL) {

  if (!is.null(xts_ret)) portfolio <- enrich_ytd(portfolio, xts_ret)

  df <- portfolio %>%
    mutate(
      ticker = factor(ticker, levels = rev(ticker)),
      label  = if ("ytd_label" %in% names(.)) {
        paste0(ticker, "\n", percent(weight, accuracy = 1), "  YTD ", ytd_label)
      } else {
        paste0(ticker, "\n", percent(weight, accuracy = 1))
      }
    )

  ytd_note <- if (!is.null(xts_ret))
    paste0("  |  YTD as of ", format(max(index(xts_ret)), "%d %b %Y"))
  else ""

  ggplot(df, aes(x = weight, y = "SAA", fill = role)) +
    geom_col(width = 0.55, color = "white", linewidth = 0.8) +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      size = 3.4, fontface = "bold", color = "white", lineheight = 1.15
    ) +
    scale_fill_manual(values = ROLE_PAL, name = "Role") +
    scale_x_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0)) +
    theme_minimal(base_size = 12) +
    theme(
      axis.title.y       = element_blank(),
      axis.text.y        = element_blank(),
      axis.ticks.y       = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      legend.position    = "bottom",
      plot.title         = element_text(face = "bold", size = 14),
      plot.subtitle      = element_text(color = "grey50", size = 10)
    ) +
    labs(
      title    = "SAA Portfolio — Allocation by Role",
      subtitle = paste0("Equity 65%  |  Fixed Income 35%", ytd_note),
      x        = NULL
    )
}

# ── 2. plot_saa_treemap() ──────────────────────────────────────────────────────
# Static ggplot treemap via treemapify — PDF + HTML safe
# Area = weight | Fill = role colour | YTD sign overlay when available

plot_saa_treemap <- function(portfolio = SAA_PORTFOLIO, xts_ret = NULL) {

  if (!requireNamespace("treemapify", quietly = TRUE))
    stop("treemapify is required. Install with: install.packages('treemapify')")

  if (!is.null(xts_ret)) portfolio <- enrich_ytd(portfolio, xts_ret)

  df <- portfolio %>%
    mutate(
      # Leaf fill: YTD green/red when available, else role colour
      leaf_fill = if ("ytd" %in% names(.)) {
        case_when(
          is.na(ytd) ~ ROLE_PAL[role],
          ytd >= 0   ~ "#40916C",
          TRUE       ~ "#C1121F"
        )
      } else ROLE_PAL[role],
      cell_label = if ("ytd_label" %in% names(.)) {
        paste0(ticker, "\n", percent(weight, accuracy = 1), "\n", ytd_label)
      } else {
        paste0(ticker, "\n", percent(weight, accuracy = 1))
      }
    )

  ytd_note <- if (!is.null(xts_ret))
    paste0("  |  YTD as of ", format(max(index(xts_ret)), "%d %b %Y"))
  else ""

  ggplot(df, aes(area = weight, fill = I(leaf_fill),
                  label = cell_label, subgroup = asset_class)) +
    treemapify::geom_treemap(colour = "white", size = 2) +
    treemapify::geom_treemap_subgroup_border(colour = "white", size = 4) +
    treemapify::geom_treemap_subgroup_text(
      place     = "topleft",
      colour    = "white",
      fontface  = "bold",
      size      = 13,
      alpha     = 0.75,
      grow      = FALSE
    ) +
    treemapify::geom_treemap_text(
      colour   = "white",
      fontface = "bold",
      size     = 11,
      place    = "centre",
      grow     = FALSE
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      plot.title      = element_text(face = "bold", size = 14),
      plot.subtitle   = element_text(colour = "grey50", size = 10)
    ) +
    labs(
      title    = "SAA Portfolio — Treemap",
      subtitle = paste0("Area = weight  |  Colour = YTD (green pos / red neg)", ytd_note)
    )
}

# ── Run ────────────────────────────────────────────────────────────────────────
# Guard: skip auto-run when sourced inside a knitr/Rmd render

if (!isTRUE(getOption("knitr.in.progress"))) {
  if (!exists("xts_ret")) {
    xts_ret <- readRDS(here("02_data_processed/xts_ret_returns.rds"))
  }
  print(plot_saa_ggbar(xts_ret = xts_ret))
  print(plot_saa_treemap(xts_ret = xts_ret))
}
