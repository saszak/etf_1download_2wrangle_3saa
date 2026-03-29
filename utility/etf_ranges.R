# ==============================================================================
# ETF RANGES — Yahoo-style quote & range viewer
# ==============================================================================
#
# Fetches 1-year daily price history via tidyquant and produces:
#   (1) A formatted quote table (Last, Change%, Day Range, 52Wk Range, Vol, AvgVol)
#   (2) A range chart: 52-wk bar (grey) + day range bar (blue) + current price dot
#
# Usage:
#   source("utility/etf_ranges.R")                     # uses default SAA symbols
#   etf_ranges(c("SPY","QQQ","GLD","TLT","XLK"))       # custom symbols
# ==============================================================================

library(tidyquant)
library(tidyverse)
library(scales)
library(ggrepel)

# ── Default symbols ────────────────────────────────────────────────────────────
DEFAULT_SYMBOLS <- c("AGG", "URTH", "SPY", "QQQ", "XLK", "SMH",
                     "GLD", "IEF", "TLT", "HYG", "XLF", "XLI",
                     "XLE", "XLP", "XLU", "XLV")

# ── Main function ──────────────────────────────────────────────────────────────
etf_ranges <- function(symbols = DEFAULT_SYMBOLS,
                       label_size  = 2.8,
                       point_size  = 3.0,
                       bar_height  = 0.35) {

  cat(sprintf("Fetching 1-year price history for %d symbols...\n", length(symbols)))

  raw <- tq_get(symbols,
                get  = "stock.prices",
                from = Sys.Date() - 365,
                to   = Sys.Date()) %>%
    filter(!is.na(close))

  if (nrow(raw) == 0) stop("No data returned — check symbols and internet connection.")

  # ── Deduplicate: keep one row per (symbol, date) — last non-NA close ────────
  raw <- raw %>%
    group_by(symbol, date) %>%
    slice_max(close, n = 1, with_ties = FALSE) %>%
    ungroup()

  # ── Build quote table ──────────────────────────────────────────────────────
  quote_tbl <- raw %>%
    group_by(symbol) %>%
    arrange(date) %>%
    summarise(
      last_date  = max(date),
      last       = last(close),
      prev_close = nth(close, -2),
      day_open   = last(open),
      day_high   = last(high),
      day_low    = last(low),
      wk52_high  = max(high,   na.rm = TRUE),
      wk52_low   = min(low,    na.rm = TRUE),
      avg_vol_3m = mean(tail(volume, 63), na.rm = TRUE),
      volume     = last(volume),
      .groups = "drop"
    ) %>%
    mutate(
      change     = last - prev_close,
      change_pct = change / prev_close,
      # position within 52-wk range [0, 1]
      pos_52wk   = (last - wk52_low) / (wk52_high - wk52_low),
      # position within day range [0, 1]
      pos_day    = if_else(
        day_high > day_low,
        (last - day_low) / (day_high - day_low),
        0.5
      )
    ) %>%
    arrange(symbol)

  # ── Print table ────────────────────────────────────────────────────────────
  cat("\n")
  quote_tbl %>%
    transmute(
      Symbol      = symbol,
      Last        = sprintf("%.2f",  last),
      `Chg%`      = sprintf("%+.2f%%", change_pct * 100),
      `Chg`       = sprintf("%+.2f",  change),
      `Day Range` = sprintf("%.2f – %.2f", day_low, day_high),
      `52Wk Range`= sprintf("%.2f – %.2f", wk52_low, wk52_high),
      `Pos 52Wk`  = sprintf("%.0f%%", pos_52wk * 100),
      Volume      = case_when(
        volume >= 1e9  ~ sprintf("%.2fB", volume / 1e9),
        volume >= 1e6  ~ sprintf("%.2fM", volume / 1e6),
        volume >= 1e3  ~ sprintf("%.1fK", volume / 1e3),
        TRUE           ~ as.character(round(volume))
      ),
      `AvgVol 3M` = case_when(
        avg_vol_3m >= 1e9 ~ sprintf("%.2fB", avg_vol_3m / 1e9),
        avg_vol_3m >= 1e6 ~ sprintf("%.2fM", avg_vol_3m / 1e6),
        avg_vol_3m >= 1e3 ~ sprintf("%.1fK", avg_vol_3m / 1e3),
        TRUE              ~ as.character(round(avg_vol_3m))
      ),
      `As of`     = format(last_date, "%d %b %Y")
    ) %>%
    print(n = 100)

  # ── Normalise to [0, 1] within 52-week range ──────────────────────────────
  plot_df <- quote_tbl %>%
    mutate(
      # normalised positions (0 = 52wk low, 1 = 52wk high)
      n_day_low  = (day_low  - wk52_low) / (wk52_high - wk52_low),
      n_day_high = (day_high - wk52_low) / (wk52_high - wk52_low),
      n_last     = pos_52wk,   # already computed as (last - wk52_low)/(wk52_high - wk52_low)
      symbol     = fct_reorder(symbol, n_last),
      chg_col    = if_else(change >= 0, "#27AE60", "#C0392B"),
      # actual price labels
      lbl_low    = sprintf("%.2f", wk52_low),
      lbl_high   = sprintf("%.2f", wk52_high),
      lbl_price  = sprintf("%.1f", last),
      lbl_chg    = sprintf("(%+.1f%%)", change_pct * 100)
    )

  p <- ggplot(plot_df, aes(y = symbol)) +

    # ── 52-week range bar (background, always 0→1) ──────────────────────────
    geom_segment(
      aes(x = 0, xend = 1, yend = symbol),
      colour = "#DDDDDD", linewidth = bar_height * 10, lineend = "round"
    ) +

    # ── Day range bar (foreground, normalised) ───────────────────────────────
    geom_segment(
      aes(x = n_day_low, xend = n_day_high, yend = symbol),
      colour = "#4A90D9", linewidth = bar_height * 5, lineend = "round"
    ) +

    # ── Current price dot ────────────────────────────────────────────────────
    geom_point(
      aes(x = n_last, fill = chg_col),
      shape = 21, size = point_size, colour = "white", stroke = 1.2
    ) +
    scale_fill_identity() +

    # ── 52-wk low: actual price at x=0 ───────────────────────────────────────
    geom_text(
      aes(x = 0, label = lbl_low),
      hjust = 1.3, size = label_size, colour = "grey45", fontface = "plain"
    ) +

    # ── 52-wk high: actual price at x=1 ──────────────────────────────────────
    geom_text(
      aes(x = 1, label = lbl_high),
      hjust = -0.3, size = label_size, colour = "grey45", fontface = "plain"
    ) +

    # ── Current price: right of dot, on the bar ──────────────────────────────
    geom_text(
      aes(x = n_last, label = lbl_price, colour = chg_col),
      hjust = -0.45, vjust = 0.5, size = label_size, fontface = "bold"
    ) +
    # ── Change%: above the dot as before ─────────────────────────────────────
    geom_text(
      aes(x = n_last, label = lbl_chg, colour = chg_col),
      vjust = -0.6, hjust = 0.5, size = label_size, fontface = "bold"
    ) +
    scale_colour_identity() +

    # ── Vertical gridlines every 10% ─────────────────────────────────────────
    geom_vline(xintercept = seq(0.10, 0.90, by = 0.10),
               colour = "grey75", linewidth = 0.4, linetype = "dashed") +

    # ── Axes & theme ──────────────────────────────────────────────────────────
    scale_x_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(-0.15, 1.15),
      breaks = seq(0, 1, by = 0.10),
      expand = expansion(0)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title         = element_blank(),
      axis.text.y        = element_text(face = "bold", size = 11),
      axis.text.x        = element_text(size = 10, colour = "grey55"),
      plot.title         = element_text(face = "bold", size = 14),
      plot.subtitle      = element_text(size = 10, colour = "grey45"),
      plot.caption       = element_text(size = 8, colour = "grey55")
    ) +
    labs(
      title    = "ETF Range Viewer — Normalised 52-Week Scale",
      subtitle = paste0(
        "0% = 52-wk low  |  100% = 52-wk high  |  ",
        "Grey bar = 52-wk range  |  Blue bar = today's range  |  ",
        "Dot = last price  |  Actual prices shown at ends & dot"
      ),
      caption  = paste0("Data: Yahoo Finance via tidyquant  |  As of ",
                        format(max(quote_tbl$last_date), "%d %b %Y"))
    )

  p_xy <- make_xy_plot(quote_tbl, label_size = label_size, point_size = point_size)

  print(p)
  print(p_xy)
  invisible(list(table = quote_tbl, plot = p, plot_xy = p_xy))
}

# ── Standalone XY position map (reusable) ─────────────────────────────────────
make_xy_plot <- function(df, group_title = "52-Week Position Map",
                         label_size = 2.8, point_size = 4) {
  xy_df <- df %>%
    filter(!is.na(last), !is.na(wk52_low), !is.na(wk52_high)) %>%
    mutate(
      pct_above_low  = (last - wk52_low)  / wk52_low  * 100,
      pct_below_high = (wk52_high - last) / wk52_high * 100,
      chg_col        = if_else(change >= 0, "#27AE60", "#C0392B")
    )

  ggplot(xy_df, aes(x = pct_above_low, y = pct_below_high)) +
    geom_vline(xintercept = mean(xy_df$pct_above_low,  na.rm = TRUE),
               linetype = "dotted", colour = "grey75", linewidth = 0.5) +
    geom_hline(yintercept = mean(xy_df$pct_below_high, na.rm = TRUE),
               linetype = "dotted", colour = "grey75", linewidth = 0.5) +
    geom_point(aes(fill = chg_col), shape = 21, size = point_size,
               colour = "white", stroke = 1.2, alpha = 0.9) +
    scale_fill_identity() +
    geom_text_repel(aes(label = symbol), size = label_size, fontface = "bold",
                    max.overlaps = 40, segment.color = "grey70") +
    scale_x_continuous(labels = function(x) paste0("+", round(x), "%"),
                       name   = "% Above 52-Wk Low") +
    scale_y_reverse(labels = function(x) paste0("-", round(x), "%"),
                    limits = c(30, 0),
                    name   = "% Below 52-Wk High") +
    annotate("text", x = Inf,  y = -Inf, hjust = 1.1,  vjust = -0.5,
             label = "Near 52-Wk HIGH", size = 3.2, colour = "grey50", fontface = "italic") +
    annotate("text", x = -Inf, y =  Inf, hjust = -0.1, vjust =  1.5,
             label = "Near 52-Wk LOW",  size = 3.2, colour = "grey50", fontface = "italic") +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey88", linewidth = 0.4),
      axis.title       = element_text(size = 12, face = "bold"),
      axis.text        = element_text(size = 10, face = "bold"),
      plot.title       = element_text(face = "bold", size = 14),
      plot.subtitle    = element_text(size = 10, colour = "grey45"),
      plot.caption     = element_text(size = 8,  colour = "grey55")
    ) +
    labs(
      title    = group_title,
      subtitle = "Top = near 52-wk high  |  Bottom = near 52-wk low  |  Green = up today, Red = down",
      caption  = paste0("As of ", format(max(df$last_date, na.rm = TRUE), "%d %b %Y"))
    )
}

# ── Auto-run ──────────────────────────────────────────────────────────────────
if (!isTRUE(getOption("knitr.in.progress"))) {
  etf_ranges()
}
