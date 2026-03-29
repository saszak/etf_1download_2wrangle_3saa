# ==============================================================================
# REGIME ALPHA BARS — Year-end absolute vs relative return, coloured by regime
# ==============================================================================
#
# Demonstrates how plot_rel_calendar_perf acts as a regime detector:
#   - In Fall years  : absolute ≈ relative  (ticker hedges; both near 0 or positive)
#   - In Recovery    : absolute >> relative  (SPY rockets; ticker lags on relative basis)
#   - In Consolidation: varies by ticker
#
# The GAP between the two bars = SPY's own annual return (approximately).
# A large positive gap (abs >> rel) = beta year (SPY dominated)
# A small gap or negative gap      = hedge/alpha year (ticker held up vs falling SPY)
#
# Usage:
#   source("utility/plot_regime_alpha_bars.R")
#   xts_ret <- readRDS("02_data_processed/xts_ret_returns.rds")
#   xts_rel <- readRDS("02_data_processed/xts_rel.rds")
#   rt      <- build_regime_table(xts_ret[, "SPY"], t_fall = 0.10)
#
#   plot_regime_alpha_bars("GLD", xts_ret, xts_rel, rt)
#   plot_regime_alpha_bars("TLT", xts_ret, xts_rel, rt)
#   plot_regime_alpha_bars("XLK", xts_ret, xts_rel, rt)
# ==============================================================================

library(tidyverse)
library(scales)

# ── Regime fill palette ────────────────────────────────────────────────────────
REGIME_FILL <- c(
  "Fall"          = "#FADBD8",   # light red
  "Recovery"      = "#D5F5E3",   # light green
  "Consolidation" = "#EBF5FB"    # light blue
)
REGIME_TEXT <- c(
  "Fall"          = "#C0392B",
  "Recovery"      = "#1E8449",
  "Consolidation" = "#2471A3"
)

# ── Helper: dominant regime for each calendar year ────────────────────────────
.dominant_regime <- function(rt, years) {
  map_dfr(years, function(yr) {
    yr_start <- as.Date(paste0(yr, "-01-01"))
    yr_end   <- as.Date(paste0(yr, "-12-31"))

    over <- rt %>%
      filter(xmin <= yr_end, xmax >= yr_start) %>%
      mutate(
        ov_start = pmax(xmin, yr_start),
        ov_end   = pmin(xmax, yr_end),
        ov_days  = as.numeric(ov_end - ov_start)
      ) %>%
      filter(ov_days > 0)

    if (nrow(over) == 0) return(tibble(year = yr, regime = "Consolidation"))
    dominant <- over %>% slice_max(ov_days, n = 1, with_ties = FALSE)
    tibble(year = yr, regime = dominant$regime)
  })
}

# ── Helper: year-end cumulative return from xts column ────────────────────────
.annual_cum <- function(xts_col, years) {
  map_dbl(years, function(yr) {
    idx <- format(as.Date(index(xts_col)), "%Y") == as.character(yr)
    v   <- as.numeric(xts_col[idx, 1])
    v   <- v[!is.na(v)]
    if (length(v) == 0) return(NA_real_)
    prod(1 + v) - 1
  })
}

# ── Main function ──────────────────────────────────────────────────────────────
plot_regime_alpha_bars <- function(ticker, xts_ret, xts_rel, rt,
                                   min_year = NULL, max_year = NULL) {

  all_years <- sort(unique(as.integer(format(as.Date(index(xts_ret)), "%Y"))))
  if (!is.null(min_year)) all_years <- all_years[all_years >= min_year]
  if (!is.null(max_year)) all_years <- all_years[all_years <= max_year]

  # ── Year-end returns ─────────────────────────────────────────────────────────
  annual <- tibble(year = all_years) %>%
    mutate(
      abs_ret = .annual_cum(xts_ret[, ticker], year),
      rel_ret = .annual_cum(xts_rel[, ticker], year),
      spy_ret = .annual_cum(xts_ret[, "SPY"],  year)
    ) %>%
    filter(!is.na(abs_ret), !is.na(rel_ret))

  # ── Dominant regime per year ─────────────────────────────────────────────────
  reg <- .dominant_regime(rt, annual$year)
  annual <- left_join(annual, reg, by = "year")

  # ── Regime background rectangles ─────────────────────────────────────────────
  regime_rects <- annual %>%
    mutate(
      xmin = year - 0.45,
      xmax = year + 0.45,
      ymin = -Inf,
      ymax = Inf
    )

  # ── Long format for bars ─────────────────────────────────────────────────────
  bar_df <- annual %>%
    select(year, abs_ret, rel_ret) %>%
    pivot_longer(c(abs_ret, rel_ret),
                 names_to  = "series",
                 values_to = "ret") %>%
    mutate(series = recode(series,
                           abs_ret = paste0(ticker, " Absolute"),
                           rel_ret = paste0(ticker, " vs SPY")))

  bar_pal <- c(
    setNames("#4A90D9", paste0(ticker, " Absolute")),
    setNames("#E67E22", paste0(ticker, " vs SPY"))
  )

  # ── SPY return dot ────────────────────────────────────────────────────────────
  spy_df <- annual %>% select(year, spy_ret)

  # ── Gap annotation: abs - rel ≈ SPY return ───────────────────────────────────
  gap_df <- annual %>%
    mutate(
      gap     = abs_ret - rel_ret,
      gap_lbl = sprintf("%+.0f%%", gap * 100),
      ypos    = pmax(abs_ret, rel_ret) + 0.015
    )

  # ── Plot ─────────────────────────────────────────────────────────────────────
  ggplot() +

    # regime shading
    geom_rect(
      data = regime_rects,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = regime),
      alpha = 0.35, inherit.aes = FALSE
    ) +
    scale_fill_manual(values = REGIME_FILL, name = "Dominant Regime") +

    # zero line
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.5) +

    # bars
    geom_col(
      data     = bar_df,
      aes(x = year, y = ret, colour = series),
      fill     = NA,
      linewidth = 1.0,
      width    = 0.6,
      position = position_dodge(width = 0.65)
    ) +
    geom_col(
      data     = bar_df,
      aes(x = year, y = ret, fill = series),
      alpha    = 0.55,
      width    = 0.6,
      position = position_dodge(width = 0.65),
      show.legend = FALSE
    ) +
    scale_colour_manual(values = bar_pal, name = NULL) +
    scale_fill_manual(  values = bar_pal, name = NULL) +

    # SPY dot
    geom_point(
      data  = spy_df,
      aes(x = year, y = spy_ret),
      shape = 18, size = 3, colour = "#2C3E50"
    ) +
    geom_text(
      data  = spy_df,
      aes(x = year, y = spy_ret,
          label = sprintf("%+.0f%%", spy_ret * 100)),
      vjust = -0.7, size = 2.5, colour = "#2C3E50", fontface = "bold"
    ) +

    # gap label (abs - rel ≈ SPY)
    geom_text(
      data  = gap_df,
      aes(x = year, y = ypos, label = gap_lbl),
      size  = 2.2, colour = "grey40", vjust = 0
    ) +

    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      name   = "Annual Return"
    ) +
    scale_x_continuous(
      breaks = all_years,
      name   = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.4),
      legend.position    = "bottom",
      legend.text        = element_text(size = 10),
      legend.title       = element_text(size = 10, face = "bold"),
      axis.text.x        = element_text(size = 9,  face = "bold", angle = 45, hjust = 1),
      axis.text.y        = element_text(size = 10),
      axis.title.y       = element_text(size = 11, face = "bold"),
      plot.title         = element_text(face = "bold", size = 14),
      plot.subtitle      = element_text(size = 10, colour = "grey40"),
      plot.caption       = element_text(size = 8,  colour = "grey55")
    ) +
    guides(
      fill   = guide_legend(order = 1, override.aes = list(alpha = 0.5)),
      colour = guide_legend(order = 2)
    ) +
    labs(
      title    = paste0(ticker, "  —  Annual Return: Absolute vs vs SPY, by Regime"),
      subtitle = paste0(
        "Bars = year-end cum. return  |  Diamond = SPY return  |  ",
        "Grey label = gap (abs − rel ≈ SPY)  |  ",
        "Red bg = Fall · Green bg = Recovery · Blue bg = Consolidation"
      ),
      caption  = paste0(
        "Regime detector: large gap → beta year (SPY dominates)  |  ",
        "Small/negative gap → hedge/alpha year  |  ",
        "Source: daily log returns"
      )
    )
}
