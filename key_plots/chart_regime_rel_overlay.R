################################################################################
# CHART TEMPLATE : regime_rel_overlay
# FILE           : key_plots/chart_regime_rel_overlay.R
#
# WHAT IT SHOWS
#   Extended version of plot_regime_overlay() with up to three panels:
#
#   Panel 1 (top, large)   — regime overlay:
#     BMK cumulative return + Fall/Recovery/Consolidation shading + ticker overlay.
#   Strip  (very slim)     — Rel(ticker − SPY) colour bar:
#     Green when ticker is cumulatively ahead of SPY; red when behind.
#     Always anchored to SPY regardless of BMK choice.
#   Panel 2 (bottom, slim) — ticker relative to BMK area chart:
#     (cumprod(1+tk) − cumprod(1+bmk)), pp difference — matches visual gap on shared axis.
#
# FUNCTION
#   plot_regime_overlay_ext(ticker, bmk = "SPY", xts_ret,
#                            t_fall = 0.10, t_cruise = 0.05)
#
# QUICK RUN (example at bottom, guarded)
################################################################################

library(tidyverse)
library(patchwork)
library(scales)
library(xts)

# ── Colour constants (match regime palette from spy_dd_regime.R) ──────────────
.REGIME_PAL <- c(
  Fall          = "#e74c3c",
  Recovery      = "#27ae60",
  Consolidation = "#2980b9"
)
.FALL_FILL    <- "#fca5a5"
.RECOV_FILL   <- "#bbf7d0"

# ==============================================================================
# .build_rel_panel()  — internal helper: one relative-return slim panel
# ==============================================================================
.build_rel_panel <- function(dates, rel_cum, regime_rect,
                              ref_label, ticker, t_fall,
                              show_caption = FALSE) {

  current_alpha <- last(rel_cum)
  alpha_label   <- sprintf("\u03b1: %+.1fpp", current_alpha * 100)
  alpha_col     <- if (current_alpha >= 0) "#16a34a" else "#dc2626"

  p <- ggplot(tibble(date = dates, rel = rel_cum), aes(date, rel)) +

    geom_rect(
      data        = regime_rect,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
      inherit.aes = FALSE, alpha = 0.18
    ) +
    scale_fill_identity() +

    geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.5) +

    geom_area(aes(fill = rel >= 0), alpha = 0.18, show.legend = FALSE) +
    scale_fill_manual(values = c("TRUE" = "#16a34a", "FALSE" = "#dc2626"), guide = "none") +

    geom_line(colour = "#374151", linewidth = 0.75) +

    annotate(
      "text",
      x = max(dates), y = Inf,
      label   = alpha_label,
      hjust   = 1.05, vjust = 1.5,
      size    = 3.2, fontface = "bold", colour = alpha_col
    ) +

    # y-axis label identifies the reference
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      name   = sprintf("vs %s", ref_label)
    ) +
    scale_x_date(expand = expansion(mult = c(0.13, 0.04)))

  if (show_caption) {
    p <- p + labs(
      caption = sprintf(
        "%s vs %s  |  Relative cumulative return  |  Regime defined by %s \u2265%.0f%% drawdown",
        ticker, ref_label, ref_label, t_fall * 100
      )
    )
  } else {
    p <- p + labs(caption = NULL)
  }

  p +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(colour = "grey93", linewidth = 0.5),
      axis.title.x       = element_blank(),
      axis.title.y       = element_text(size = 8, colour = "grey50", angle = 90),
      axis.text.x        = element_text(size = 9, colour = "grey40"),
      plot.caption       = element_text(size = 8, colour = "grey50"),
      plot.margin        = margin(t = 2, r = 15, b = 8, l = 40)
    )
}

# ==============================================================================
# plot_regime_overlay_ext()
# Extended regime overlay: P1 (BMK+ticker) / colour strip (Rel ticker−SPY) /
# P2 (ticker vs BMK) [/ P3 (ticker vs SPY) when bmk ≠ "SPY"]
# ==============================================================================
plot_regime_overlay_ext <- function(
    ticker,
    bmk      = "SPY",
    xts_ret,                  # full multi-column xts of log returns
    t_fall   = 0.10,
    t_cruise = 0.05
) {

  stopifnot(ticker %in% colnames(xts_ret), bmk %in% colnames(xts_ret))

  tk_ret  <- xts_ret[, ticker]
  bmk_ret <- xts_ret[, bmk]

  # ── Align ticker + BMK to common date range ──────────────────────────────────
  both    <- merge(tk_ret, bmk_ret, join = "inner")
  tk_ret  <- both[, 1]
  bmk_ret <- both[, 2]
  dates   <- as.Date(index(both))

  # ── Regime table (based on BMK drawdown) ────────────────────────────────────
  rt <- build_regime_table(bmk_ret, t_fall, t_cruise)

  # ── Regime shading rectangles ───────────────────────────────────────────────
  regime_rect <- rt %>%
    filter(regime %in% c("Fall", "Recovery")) %>%
    transmute(
      xmin = as.Date(xmin),
      xmax = as.Date(xmax),
      fill = if_else(regime == "Fall", .FALL_FILL, .RECOV_FILL)
    )

  # ── Panel 1: existing regime overlay ────────────────────────────────────────
  p1 <- plot_regime_overlay(
    xts_ret_col  = bmk_ret,
    t_fall       = t_fall,
    t_cruise     = t_cruise,
    asset_name   = bmk,
    overlay_ret  = tk_ret,
    overlay_name = ticker
  ) +
    theme(plot.margin = margin(t = 8, r = 15, b = 2, l = 60))

  # ── Replicate plot_regime_overlay() coordinate system ───────────────────────
  cum_bmk_pct <- as.numeric(cumprod(1 + coredata(bmk_ret)) - 1)
  min_val     <- min(cum_bmk_pct)
  max_val     <- max(cum_bmk_pct)
  total_range <- max_val - min_val
  bar_h       <- total_range * 0.08
  gap         <- total_range * 0.015
  y_row1      <- min_val - total_range * 0.18   # SPY macro bar
  y_row2      <- y_row1 - bar_h - gap           # ticker (QQQ) bar
  y_row3      <- y_row2 - bar_h - gap           # 3rd bar: Rel(ticker − BMK)

  # ── Per-regime relative returns ──────────────────────────────────────────────
  rel_bars <- rt %>%
    rowwise() %>%
    mutate(
      tk_period  = {
        w <- tk_ret[paste0(format(xmin), "/", format(xmax))]
        if (length(w) == 0L) 0 else prod(1 + as.numeric(w)) - 1
      },
      bk_period  = {
        w <- bmk_ret[paste0(format(xmin), "/", format(xmax))]
        if (length(w) == 0L) 0 else prod(1 + as.numeric(w)) - 1
      },
      rel_ret = tk_period - bk_period
    ) %>%
    ungroup() %>%
    mutate(
      fill      = if_else(rel_ret >= 0, "#16a34a", "#dc2626"),
      rel_label = paste0(ifelse(rel_ret >= 0, "+", ""), round(rel_ret * 100, 1), "pp"),
      is_thin   = as.numeric(xmax - xmin) < 30
    )

  # ── End-of-line cumret labels on the two lines in p1 ────────────────────────
  cum_tk_pct  <- as.numeric(cumprod(1 + coredata(tk_ret))  - 1)
  last_date   <- last(dates)
  last_bmk    <- last(cum_bmk_pct)
  last_tk     <- last(cum_tk_pct)

  p1 <- p1 +
    annotate("text",
             x = last_date, y = last_bmk,
             label = scales::percent(last_bmk, accuracy = 1),
             hjust = -0.15, vjust = 0.5, size = 3,
             colour = "#1D3557", fontface = "bold") +
    annotate("text",
             x = last_date, y = last_tk,
             label = scales::percent(last_tk, accuracy = 1),
             hjust = -0.15, vjust = 0.5, size = 3,
             colour = "grey35", fontface = "bold")

  # ── Add third bar to p1 ──────────────────────────────────────────────────────
  p1 <- p1 +
    geom_rect(
      data = rel_bars,
      aes(xmin = xmin, xmax = xmax,
          ymin = y_row3, ymax = y_row3 + bar_h,
          fill = I(fill)),
      colour = "white", linewidth = 0.3, inherit.aes = FALSE, alpha = 0.9
    ) +
    geom_text(
      data = rel_bars %>% filter(!is_thin),
      aes(x     = xmin + (xmax - xmin) / 2,
          y     = y_row3 + bar_h / 2,
          label = rel_label),
      colour = "white", size = 2.8, fontface = "bold", inherit.aes = FALSE
    ) +
    annotate(
      "text",
      x     = min(dates), y = y_row3 + bar_h / 2,
      label = "Rel",
      hjust = 1.3, size = 4, fontface = "bold", colour = "grey35"
    )

  # ── Panel 2: ticker relative to BMK ─────────────────────────────────────────
  cum_tk  <- as.numeric(cumprod(1 + coredata(tk_ret)))
  cum_bmk <- as.numeric(cumprod(1 + coredata(bmk_ret)))

  p2 <- .build_rel_panel(
    dates        = dates,
    rel_cum      = cum_tk - cum_bmk,   # pp difference — matches visual gap on shared axis
    regime_rect  = regime_rect,
    ref_label    = bmk,
    ticker       = ticker,
    t_fall       = t_fall,
    show_caption = TRUE
  )

  # ── Combine: regime overlay (with strip) / relative area ─────────────────────
  p1 / p2 + plot_layout(heights = c(3, 1))
}


# ==============================================================================
# plot_regime_triple()
# PURPOSE : Single-panel chart — BMK + ticker + relative cumulative return,
#           all on one shared y-axis (cumulative return %).
#           Matches the exact colour scheme, coordinate system, year grid,
#           transition arrows, bar strips, stats label and theme of
#           plot_regime_overlay().
# ARGS
#   ticker     : character  e.g. "XLK"
#   bmk        : character  e.g. "SPY"
#   xts_ret    : multi-column xts of daily log returns
#   t_fall     : drawdown threshold (default 0.10)
#   t_cruise   : cruise threshold   (default 0.05)
#   label_size : text size for bar labels (default 2.8)
# ==============================================================================
plot_regime_triple <- function(ticker,
                                bmk        = "SPY",
                                xts_ret,
                                t_fall     = 0.10,
                                t_cruise   = 0.05,
                                label_size = 2.8,
                                roll_win   = 252) {

  stopifnot(ticker %in% colnames(xts_ret), bmk %in% colnames(xts_ret))

  both    <- merge(xts_ret[, ticker], xts_ret[, bmk], join = "inner")
  tk_ret  <- both[, 1]
  bmk_ret <- both[, 2]

  # ── Cumulative returns — all start at 0, same units ───────────────────────
  cum_bmk <- as.numeric(cumprod(1 + coredata(bmk_ret)) - 1)
  cum_tk  <- as.numeric(cumprod(1 + coredata(tk_ret))  - 1)
  # Simple difference so the grey line tracks the VISUAL gap between the two
  # cumret lines on the shared axis (130% − 98% = 32pp, not ratio 16.2%).
  cum_rel <- cum_tk - cum_bmk

  cum_df <- tibble(
    date   = as.Date(index(both)),
    cumret = cum_bmk,        # used for coordinate sizing (same as plot_regime_overlay)
    tk     = cum_tk,
    rel    = cum_rel
  )

  # ── Regime table ──────────────────────────────────────────────────────────
  rt <- build_regime_table(bmk_ret, t_fall, t_cruise)

  # ── Regime shading rectangles (for p2 panel) ──────────────────────────────
  regime_rect <- rt %>%
    filter(regime %in% c("Fall", "Recovery")) %>%
    transmute(
      xmin = as.Date(xmin),
      xmax = as.Date(xmax),
      fill = if_else(regime == "Fall", .FALL_FILL, .RECOV_FILL)
    )

  # ── Coordinate system — identical to plot_regime_overlay() ───────────────
  max_val     <- max(c(cum_bmk, cum_tk), na.rm = TRUE)
  min_val     <- min(c(cum_bmk, cum_tk, cum_rel), na.rm = TRUE)
  total_range <- max_val - min_val

  bar_h  <- total_range * 0.08
  gap    <- total_range * 0.015
  y_row1 <- min_val - total_range * 0.18   # BMK bar
  y_row2 <- y_row1 - bar_h - gap           # ticker bar
  y_row3 <- y_row2 - bar_h - gap           # Rel bar
  y_floor <- y_row3 - bar_h - total_range * 0.20

  # ── Year grid ─────────────────────────────────────────────────────────────
  year_markers <- cum_df %>%
    mutate(year = format(date, "%Y")) %>%
    group_by(year) %>% slice(1) %>% ungroup()

  # ── Summary stats (BMK) ───────────────────────────────────────────────────
  total_r  <- percent(as.numeric(Return.cumulative(bmk_ret)),   accuracy = 0.1)
  ann_r    <- percent(as.numeric(Return.annualized(bmk_ret)),    accuracy = 0.1)
  mdd      <- percent(as.numeric(maxDrawdown(bmk_ret)),          accuracy = 0.1)
  vol      <- percent(as.numeric(StdDev.annualized(bmk_ret)),    accuracy = 0.1)
  sharpe   <- round(as.numeric(SharpeRatio.annualized(bmk_ret, Rf = 0)), 2)
  # ticker stats
  ann_r_tk <- percent(as.numeric(Return.annualized(tk_ret)),     accuracy = 0.1)
  mdd_tk   <- percent(as.numeric(maxDrawdown(tk_ret)),           accuracy = 0.1)
  sharpe_tk <- round(as.numeric(SharpeRatio.annualized(tk_ret, Rf = 0)), 2)
  stats_label <- sprintf(
    "%s — Ann: %s  MaxDD: %s  Sharpe: %s     %s — Ann: %s  MaxDD: %s  Sharpe: %s",
    bmk,    ann_r,    mdd,    sharpe,
    ticker, ann_r_tk, mdd_tk, sharpe_tk
  )

  # ── Current regime badge ──────────────────────────────────────────────────
  current_regime <- as.character(tail(rt$regime, 1))
  current_since  <- format(tail(rt$xmin, 1), "%d %b %Y")

  # ── Transition arrows ─────────────────────────────────────────────────────
  trans_arrows <- rt %>%
    arrange(xmin) %>%
    mutate(next_color = lead(as.character(color))) %>%
    filter(!is.na(next_color))

  # ── Per-regime bar data ───────────────────────────────────────────────────
  bar_data <- rt %>%
    rowwise() %>%
    mutate(
      win      = paste0(format(xmin), "/", format(xmax)),
      r_bk     = { w <- bmk_ret[win]; if (length(w)==0) 0 else as.numeric(Return.cumulative(w)) },
      r_tk     = { w <- tk_ret[win];  if (length(w)==0) 0 else as.numeric(Return.cumulative(w)) },
      r_rl     = r_tk - r_bk,
      fill_rl  = if_else(r_rl >= 0, "#16a34a", "#dc2626"),
      label_bk = percent(r_bk, accuracy = 1),
      label_tk = percent(r_tk, accuracy = 1),
      label_rl = paste0(ifelse(r_rl >= 0, "+", ""), round(r_rl * 100, 1), "pp")
    ) %>%
    ungroup()

  # ── End-of-line labels ────────────────────────────────────────────────────
  last_date <- last(cum_df$date)
  eol <- tibble(
    date   = rep(last_date, 3),
    y      = c(last(cum_bmk), last(cum_tk), last(cum_rel)),
    label  = c(
      percent(last(cum_bmk), accuracy = 1),
      percent(last(cum_tk),  accuracy = 1),
      paste0(ifelse(last(cum_rel) >= 0, "+", ""), round(last(cum_rel) * 100, 1), "pp")
    ),
    colour = c("#C0392B", "#1D3557", "grey50")
  )

  # ── Build plot ────────────────────────────────────────────────────────────
  p <- ggplot() +

    # Consolidation background (same α=0.06 as plot_regime_overlay)
    geom_rect(
      data = rt %>% filter(regime == "Consolidation"),
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = regime),
      alpha = 0.06, inherit.aes = FALSE
    ) +
    scale_fill_manual(
      name   = "Regime",
      values = REGIME_PAL,
      guide  = guide_legend(override.aes = list(size = 4))
    ) +

    # Year grid
    geom_vline(data = year_markers, aes(xintercept = date),
               color = "grey92", linewidth = 0.8) +

    # Phase transition markers
    geom_vline(
      data = rt %>% filter(regime == "Fall"),
      aes(xintercept = xmin),
      color = REGIME_PAL["Fall"], linetype = "dashed", alpha = 0.35, linewidth = 0.5
    ) +
    geom_vline(
      data = rt %>% filter(regime == "Recovery"),
      aes(xintercept = xmax),
      color = REGIME_PAL["Recovery"], linetype = "dotted", alpha = 0.35, linewidth = 0.5
    ) +

    # Zero waterline
    geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +

    # ── Three cumulative return lines ───────────────────────────────────
    geom_line(data = cum_df, aes(x = date, y = cumret),
              color = "#C0392B", linewidth = 0.7) +
    geom_line(data = cum_df, aes(x = date, y = tk),
              color = "#1D3557", linewidth = 0.7) +
    geom_line(data = cum_df, aes(x = date, y = rel),
              color = "grey50", linewidth = 0.7) +

    # End-of-line labels
    geom_text(
      data        = eol,
      aes(x = date, y = y, label = label, color = I(colour)),
      hjust = -0.15, vjust = 0.5, size = 3.0, fontface = "bold",
      inherit.aes = FALSE
    ) +
    scale_color_identity() +

    # ── BMK bar row ──────────────────────────────────────────────────────
    geom_rect(
      data = bar_data,
      aes(xmin = xmin, xmax = xmax,
          ymin = y_row1, ymax = y_row1 + bar_h, fill = regime),
      color = "white", linewidth = 0.3, show.legend = FALSE
    ) +
    geom_text(
      data = bar_data %>% filter(!is_thin),
      aes(x = xmin + (xmax - xmin)/2, y = y_row1 + bar_h/2, label = label_bk),
      color = "white", size = label_size, fontface = "bold"
    ) +
    geom_text(
      data = bar_data %>% filter(is_thin),
      aes(x = xmin + (xmax - xmin)/2,
          y = y_row1 + bar_h + total_range * 0.03,
          label = label_bk, color = color),
      angle = 45, hjust = 0, size = label_size * 0.85, fontface = "bold",
      show.legend = FALSE
    ) +
    annotate("text", x = min(cum_df$date), y = y_row1 + bar_h/2,
             label = bmk, hjust = 1.3, size = 4, fontface = "bold",
             color = "#C0392B") +

    # Transition arrows
    geom_text(
      data = trans_arrows,
      aes(x = xmax, y = y_row1 + bar_h/2, label = "\u25b6", color = next_color),
      size = 3.2, hjust = 0.5, fontface = "bold", show.legend = FALSE
    ) +

    # ── Ticker bar row ───────────────────────────────────────────────────
    geom_rect(
      data = bar_data,
      aes(xmin = xmin, xmax = xmax,
          ymin = y_row2, ymax = y_row2 + bar_h, fill = regime),
      color = "white", linewidth = 0.3, alpha = 0.85, show.legend = FALSE
    ) +
    geom_text(
      data = bar_data %>% filter(!is_thin),
      aes(x = xmin + (xmax - xmin)/2, y = y_row2 + bar_h/2, label = label_tk),
      color = "white", size = label_size, fontface = "bold"
    ) +
    annotate("text", x = min(cum_df$date), y = y_row2 + bar_h/2,
             label = ticker, hjust = 1.3, size = 4, fontface = "bold",
             color = "#1D3557") +

    # ── Rel bar row (green/red) ──────────────────────────────────────────
    geom_rect(
      data = bar_data,
      aes(xmin = xmin, xmax = xmax,
          ymin = y_row3, ymax = y_row3 + bar_h,
          fill = I(fill_rl)),
      color = "white", linewidth = 0.3, alpha = 0.9, inherit.aes = FALSE
    ) +
    geom_text(
      data = bar_data %>% filter(!is_thin),
      aes(x = xmin + (xmax - xmin)/2, y = y_row3 + bar_h/2, label = label_rl),
      color = "white", size = label_size, fontface = "bold"
    ) +
    annotate("text", x = min(cum_df$date), y = y_row3 + bar_h/2,
             label = "Rel", hjust = 1.3, size = 4, fontface = "bold",
             color = "grey50") +

    # ── Stats + year labels ──────────────────────────────────────────────
    annotate("text",
             x     = min(cum_df$date) + (max(cum_df$date) - min(cum_df$date)) / 2,
             y     = y_floor + total_range * 0.04,
             label = stats_label,
             hjust = 0.5, size = 3, fontface = "bold", color = "grey40") +
    geom_text(
      data = year_markers,
      aes(x = date, y = y_floor + total_range * 0.10, label = year),
      size = 3, fontface = "bold", color = "grey45"
    ) +

    scale_x_date(expand = expansion(mult = c(0.13, 0.08))) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      breaks = seq(-2, 20, by = 0.20),
      limits = c(y_floor, max_val * 1.08)
    ) +

    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(color = "grey93", linewidth = 0.6),
      axis.title         = element_blank(),
      axis.text.x        = element_blank(),
      legend.position    = "top",
      legend.title       = element_text(face = "bold"),
      plot.title         = element_text(face = "bold", size = 14),
      plot.subtitle      = element_text(color = "grey50", size = 10),
      plot.margin        = margin(t = 10, r = 60, b = 2, l = 60)
    ) +

    labs(
      title    = paste0(ticker, " vs ", bmk, " \u2014 Cumulative Return + Relative"),
      subtitle = paste0(
        bmk, " (red)  \u2502  ",
        ticker, " (dark blue)  \u2502  ",
        "Spread ", ticker, " \u2212 ", bmk, " in pp (grey)  \u2502  ",
        "Regime defined by ", bmk, " \u2265", scales::percent(t_fall, accuracy = 1), " drawdown"
      )
    )

  # ── Panel 2: rolling 12m relative return ──────────────────────────────────
  r_spread   <- as.numeric(coredata(tk_ret)) - as.numeric(coredata(bmk_ret))
  roll_rel   <- zoo::rollapply(r_spread, roll_win,
                               function(x) prod(1 + x) - 1,
                               align = "right", fill = NA)

  roll_df    <- tibble(date = as.Date(index(both)), rrel = as.numeric(roll_rel))
  roll_clean <- roll_df %>% filter(!is.na(rrel))

  current_rrel <- last(roll_clean$rrel)
  rrel_col     <- if (current_rrel >= 0) "#16a34a" else "#dc2626"
  rrel_label   <- sprintf("12m \u03b1: %+.1f%%", current_rrel * 100)

  p2 <- ggplot(roll_clean, aes(x = date, y = rrel)) +

    geom_rect(
      data        = regime_rect,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
      inherit.aes = FALSE, alpha = 0.18
    ) +
    scale_fill_identity() +

    geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.5) +

    geom_ribbon(
      data = roll_clean %>% filter(rrel >= 0),
      aes(ymin = 0, ymax = rrel), fill = "#16a34a", alpha = 0.20
    ) +
    geom_ribbon(
      data = roll_clean %>% filter(rrel < 0),
      aes(ymin = rrel, ymax = 0), fill = "#dc2626", alpha = 0.20
    ) +
    geom_line(colour = "grey30", linewidth = 0.75) +

    annotate(
      "text",
      x = max(roll_clean$date), y = Inf,
      label = rrel_label,
      hjust = 1.05, vjust = 1.5, size = 3.2, fontface = "bold", colour = rrel_col
    ) +

    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      name   = sprintf("%dd α", roll_win)
    ) +
    scale_x_date(expand = expansion(mult = c(0.13, 0.08))) +
    labs(caption = sprintf(
      "%s vs %s  |  Rolling %d-day relative return  |  Green = outperform  |  Red = underperform",
      ticker, bmk, roll_win
    )) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(colour = "grey93", linewidth = 0.5),
      axis.title.x       = element_blank(),
      axis.title.y       = element_text(size = 8, colour = "grey50", angle = 90),
      axis.text.x        = element_text(size = 9, colour = "grey40"),
      plot.caption       = element_text(size = 8, colour = "grey50"),
      plot.margin        = margin(t = 0, r = 60, b = 8, l = 60)
    )

  p / p2 + plot_layout(heights = c(4, 1))
}


# ── Quick-run example (guarded) ───────────────────────────────────────────────
if (!isTRUE(getOption("knitr.in.progress"))) {
  library(here)
  source(here("project_tree.R"))
  source(here("scripts_spy_dd_regime/spy_dd_regime.R"))

  xts_ret <- readRDS(here(project_tree$products$refined_ret))

  print(plot_regime_overlay_ext("QQQ", bmk = "SPY",  xts_ret = xts_ret))
  print(plot_regime_overlay_ext("QQQ", bmk = "URTH", xts_ret = xts_ret))
  print(plot_regime_triple("XLK", bmk = "SPY", xts_ret = xts_ret))
}
