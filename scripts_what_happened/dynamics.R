################################################################################
# SUBPROJECT : What Has Happened
# FUNCTION A : The Dynamics
# FILE       : scripts_what_happened/dynamics.R
#
# WHAT IT DOES
#   Returns period returns (WTD / MTD / QTD / YTD) for all tickers,
#   enriched with a factor_style grouping that captures the Market Zeitgeist:
#   which styles / themes are winning or losing across meaningful time windows.
#
# FUNCTIONS
#   build_dynamics(tickers, xts_ret, etf_metadata)
#     → tibble: ticker | name | factor_style | WTD | MTD | QTD | YTD | state | norm_pos
#
#   plot_dynamics(dyn, groups = NULL, periods = c("WTD","MTD","QTD","YTD"))
#     → heatmap: tickers × periods, fill = return %, grouped by factor_style
#
# INVOKE
#   source(here("scripts_what_happened/dynamics.R"))
#   dyn <- build_dynamics(colnames(xts_ret), xts_ret, etf_metadata)
#   plot_dynamics(dyn)
#   plot_dynamics(dyn, groups = c("US Momentum","Cyclical Growth","Quality"))
################################################################################

library(tidyverse)
library(lubridate)
library(scales)
library(here)

# ── Factor style map ──────────────────────────────────────────────────────────
# Overlays the existing asset_class / tree_level taxonomy with an
# investable factor / theme grouping relevant to tactical rotation.
# Tickers not listed fall back to their asset_class from etf_metadata.

FACTOR_STYLE <- tribble(
  ~ticker,  ~factor_style,
  # ── US Momentum / Growth ────────────────────────────────────────────────────
  "QQQ",    "US Momentum",
  "SMH",    "US Momentum",
  "XLK",    "US Momentum",
  "XLC",    "US Momentum",
  "MTUM",   "US Momentum",
  "AIQ",    "US Momentum",
  "CIBR",   "US Momentum",
  "WCLD",   "US Momentum",
  "IBIT",   "US Momentum",
  # ── Cyclical Growth ─────────────────────────────────────────────────────────
  "XLY",    "Cyclical Growth",
  "XLI",    "Cyclical Growth",
  "IJH",    "Cyclical Growth",
  "IWM",    "Cyclical Growth",
  "ITB",    "Cyclical Growth",
  "IYT",    "Cyclical Growth",
  "PSP",    "Cyclical Growth",
  "IPO",    "Cyclical Growth",
  # ── Cyclical Value ──────────────────────────────────────────────────────────
  "XLF",    "Cyclical Value",
  "XLE",    "Cyclical Value",
  "XLB",    "Cyclical Value",
  "KRE",    "Cyclical Value",
  "VTV",    "Cyclical Value",
  "COWZ",   "Cyclical Value",
  # ── Quality ─────────────────────────────────────────────────────────────────
  "QUAL",   "Quality",
  "USMV",   "Quality",
  "XLV",    "Quality",
  "VHT",    "Quality",
  "IHI",    "Quality",
  "DGRW",   "Quality",
  "JEPI",   "Quality",
  # ── Defensive ───────────────────────────────────────────────────────────────
  "XLU",    "Defensive",
  "XLP",    "Defensive",
  "ACWV",   "Defensive",
  # ── Global Equity ───────────────────────────────────────────────────────────
  "URTH",   "Global Equity",
  "ACWI",   "Global Equity",
  "SPY",    "Global Equity",
  "VTI",    "Global Equity",
  "ACWX",   "Global Equity",
  # ── DM International ────────────────────────────────────────────────────────
  "IEFA",   "DM Intl",
  "EWJ",    "DM Intl",
  "DXJ",    "DM Intl",
  "FEZ",    "DM Intl",
  "EZU",    "DM Intl",
  "IEV",    "DM Intl",
  "EWC",    "DM Intl",
  "EWA",    "DM Intl",
  "DAX",    "DM Intl",
  "EWQ",    "DM Intl",
  "EWL",    "DM Intl",
  "EWU",    "DM Intl",
  "EWI",    "DM Intl",
  "EWP",    "DM Intl",
  # ── Emerging Markets ────────────────────────────────────────────────────────
  "VWO",    "Emerging Markets",
  "AAXJ",   "Emerging Markets",
  "INDA",   "Emerging Markets",
  "FXI",    "Emerging Markets",
  "EWT",    "Emerging Markets",
  "EWY",    "Emerging Markets",
  "EWZ",    "Emerging Markets",
  # ── Fixed Income Gov ────────────────────────────────────────────────────────
  "SGOV",   "FI Gov",
  "SHY",    "FI Gov",
  "IEI",    "FI Gov",
  "IEF",    "FI Gov",
  "TLT",    "FI Gov",
  "BNDX",   "FI Gov",
  "PFIX",   "FI Gov",
  # ── Fixed Income Credit ─────────────────────────────────────────────────────
  "AGG",    "FI Credit",
  "LQD",    "FI Credit",
  "VCIT",   "FI Credit",
  "MUB",    "FI Credit",
  "HYG",    "FI Credit",
  "EMB",    "FI Credit",
  "EMLC",   "FI Credit",
  # ── Inflation / Real Assets ─────────────────────────────────────────────────
  "GLD",    "Inflation Shield",
  "SLV",    "Inflation Shield",
  "TIP",    "Inflation Shield",
  "LTPZ",   "Inflation Shield",
  "PDBC",   "Inflation Shield",
  "DJP",    "Inflation Shield",
  "COPX",   "Inflation Shield",
  "URA",    "Inflation Shield",
  "LIT",    "Inflation Shield",
  "MOO",    "Inflation Shield",
  "DBA",    "Inflation Shield",
  # ── Real Estate / Income ────────────────────────────────────────────────────
  "XLRE",   "Real Estate",
  "IYR",    "Real Estate",
  "VNQI",   "Real Estate",
  "IGF",    "Real Estate",
  # ── Alternatives / Overlay ──────────────────────────────────────────────────
  "DBMF",   "Alternatives",
  "CTA",    "Alternatives",
  "BTAL",   "Alternatives",
  "TAIL",   "Alternatives",
  "SH",     "Alternatives",
  "RPAR",   "Alternatives",
  # ── FX ──────────────────────────────────────────────────────────────────────
  "UUP",    "FX",
  "FXE",    "FX",
  "FXY",    "FX",
  "FXB",    "FX",
  "FXF",    "FX",
  # ── Biotech / Healthcare Satellites ─────────────────────────────────────────
  "IBB",    "Biotech",
  "XBI",    "Biotech",
  "ITA",    "Defense",
  "IWF",    "Cyclical Growth"
)

# Canonical display order for factor_style groups
STYLE_ORDER <- c(
  "US Momentum", "Cyclical Growth", "Cyclical Value", "Quality", "Defensive",
  "Global Equity", "DM Intl", "Emerging Markets",
  "FI Gov", "FI Credit", "Inflation Shield", "Real Estate",
  "Alternatives", "FX", "Biotech", "Defense"
)

# ── Period return helper ───────────────────────────────────────────────────────
.period_ret <- function(r_vec, dates, from_date) {
  idx <- which(dates >= from_date)
  if (length(idx) < 1) return(NA_real_)
  (prod(1 + r_vec[idx], na.rm = TRUE) - 1) * 100
}

# ── build_dynamics ─────────────────────────────────────────────────────────────
build_dynamics <- function(
    tickers,
    xts_ret,
    etf_metadata = NULL
) {
  tickers <- tickers[tickers %in% colnames(xts_ret)]
  if (length(tickers) == 0) stop("No valid tickers found in xts_ret")

  today   <- as.Date(tail(index(xts_ret), 1))
  dates   <- as.Date(index(xts_ret))

  # Period start dates
  ytd_start <- floor_date(today, "year")
  qtd_start <- floor_date(today, "quarter")
  mtd_start <- floor_date(today, "month")
  wtd_start <- floor_date(today, "week", week_start = 1)   # Monday

  # Gauge state + norm_pos (if build_gauge_vectors is available)
  has_gauge <- exists("build_gauge_vectors", mode = "function")
  if (has_gauge) {
    gv <- tryCatch(
      build_gauge_vectors(tickers, xts_ret),
      error = function(e) NULL
    )
  } else {
    gv <- NULL
  }

  raw <- purrr::map_dfr(tickers, function(tk) {
    r <- as.numeric(coredata(xts_ret[, tk]))
    r[is.na(r)] <- 0

    tibble(
      ticker = tk,
      WTD    = .period_ret(r, dates, wtd_start),
      MTD    = .period_ret(r, dates, mtd_start),
      QTD    = .period_ret(r, dates, qtd_start),
      YTD    = .period_ret(r, dates, ytd_start)
    )
  })

  # Join factor_style (deduplicate — keep first assignment per ticker)
  raw <- raw %>%
    left_join(FACTOR_STYLE %>% distinct(ticker, .keep_all = TRUE), by = "ticker")

  # Join etf_metadata for name + asset_class fallback
  if (!is.null(etf_metadata)) {
    raw <- raw %>%
      left_join(
        etf_metadata %>% select(ticker, name, asset_class),
        by = "ticker"
      ) %>%
      mutate(
        factor_style = coalesce(factor_style, asset_class),
        name         = coalesce(name, ticker)
      )
  } else {
    raw <- raw %>% mutate(
      name         = ticker,
      asset_class  = NA_character_,
      factor_style = coalesce(factor_style, "Other")
    )
  }

  # Join gauge metrics
  if (!is.null(gv)) {
    raw <- raw %>%
      left_join(gv %>% select(ticker, state, norm_pos, z_200dma), by = "ticker")
  } else {
    raw <- raw %>% mutate(state = NA_character_, norm_pos = NA_real_, z_200dma = NA_real_)
  }

  # Factor order
  style_levels <- c(
    STYLE_ORDER,
    setdiff(unique(raw$factor_style), STYLE_ORDER)
  )

  raw %>%
    mutate(
      factor_style = factor(factor_style, levels = style_levels),
      as_of        = today
    ) %>%
    arrange(factor_style, desc(YTD)) %>%
    select(ticker, name, factor_style, WTD, MTD, QTD, YTD,
           state, norm_pos, z_200dma, as_of)
}

# ── plot_dynamics ──────────────────────────────────────────────────────────────
plot_dynamics <- function(
    dyn,
    groups  = NULL,                               # NULL = all groups
    periods = c("WTD", "MTD", "QTD", "YTD")
) {
  if (!is.null(groups))
    dyn <- dyn %>% filter(factor_style %in% groups)

  # Ticker order: by factor_style then YTD
  tk_order <- dyn %>%
    arrange(factor_style, desc(YTD)) %>%
    pull(ticker)

  long <- dyn %>%
    select(ticker, factor_style, all_of(periods)) %>%
    tidyr::pivot_longer(
      cols      = all_of(periods),
      names_to  = "period",
      values_to = "ret"
    ) %>%
    mutate(
      ticker = factor(ticker, levels = rev(tk_order)),
      period = factor(period, levels = periods)
    )

  # Symmetric colour scale
  lim <- max(abs(long$ret), na.rm = TRUE)

  ggplot(long, aes(x = period, y = ticker, fill = ret)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    geom_text(
      aes(label   = sprintf("%+.1f%%", ret),
          colour  = I(ifelse(abs(ret) > lim * 0.55, "white", "#374151"))),
      size = 2.8, fontface = "bold", na.rm = TRUE
    ) +
    scale_fill_gradient2(
      low      = "#b91c1c",
      mid      = "#f9fafb",
      high     = "#15803d",
      midpoint = 0,
      limits   = c(-lim, lim),
      labels   = percent_format(scale = 1, accuracy = 1),
      name     = "Return"
    ) +
    facet_grid(
      rows     = vars(factor_style),
      scales   = "free_y",
      space    = "free_y",
      switch   = "y"
    ) +
    labs(
      title    = "The Dynamics — Period Returns by Factor Style",
      subtitle = sprintf(
        "As of %s  |  WTD / MTD / QTD / YTD  |  Sorted by YTD within group",
        format(dyn$as_of[1])
      ),
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid        = element_blank(),
      plot.title        = element_text(face = "bold", size = 12),
      plot.subtitle     = element_text(size = 8.5, colour = "#555"),
      axis.text.x       = element_text(face = "bold", size = 9),
      axis.text.y       = element_text(size = 8),
      strip.text.y.left = element_text(angle = 0, face = "bold", size = 8,
                                       hjust = 1, colour = "#2c3e50"),
      strip.placement   = "outside",
      legend.position   = "bottom",
      legend.key.width  = unit(1.5, "cm")
    )
}

# ── Quick-run (guarded) ───────────────────────────────────────────────────────
if (!isTRUE(getOption("knitr.in.progress"))) {
  library(here)

  if (!exists("xts_ret"))
    xts_ret <- readRDS(here("02_data_processed/xts_ret_returns.rds"))
  if (!exists("etf_metadata"))
    source(here("scripts/00_init_universe.R"))
  if (!exists("build_gauge_vectors", mode = "function"))
    source(here("key_plots/chart_ma_range.R"))

  dyn <- build_dynamics(colnames(xts_ret), xts_ret, etf_metadata)
  print(dyn)

  p <- plot_dynamics(dyn)
  print(p)

  # Focused view — equity styles only
  p2 <- plot_dynamics(
    dyn,
    groups = c("US Momentum", "Cyclical Growth", "Cyclical Value",
               "Quality", "Defensive", "Global Equity")
  )
  print(p2)
}
