# ==============================================================================
# CALENDAR ANALOG PROJECTION — Trend vs Reversal
# ==============================================================================
#
# Two tools:
#
#   compute_transitions(xts_col, rt)
#     Computes regime-conditioned transition matrix (Up→Up, Up→Down, etc.)
#     and lag-1 autocorrelation of annual returns.
#
#   plot_calendar_analog(xts_col, rt, ...)
#     Extends plot_calendar_perf with:
#       - Analog years highlighted (years with similar YTD at today's day-of-year)
#       - Blue cone: 25–75th pct range of where analog years went from here
#       - Dashed median analog path
#       - Annotation: P(trend), P(reversal), analog years, current regime
#
# Usage:
#   source("utility/plot_calendar_analog.R")
#   xts_ret <- readRDS("02_data_processed/xts_ret_returns.rds")
#   rt      <- build_regime_table(xts_ret[, "SPY"], t_fall = 0.10)
#
#   compute_transitions(xts_ret[, "GLD"], rt)
#   plot_calendar_analog(xts_ret[, "SPY"], rt)
#   plot_calendar_analog(xts_ret[, "GLD"], rt, window_pct = 0.08)
# ==============================================================================

library(tidyverse)
library(scales)

# ── Helper: dominant regime for a date range ──────────────────────────────────
.regime_for_year <- function(rt, yr) {
  if (is.null(rt)) return("Unknown")
  yr_start <- as.Date(paste0(yr, "-01-01"))
  yr_end   <- as.Date(paste0(yr, "-12-31"))
  over <- rt %>%
    filter(xmin <= yr_end, xmax >= yr_start) %>%
    mutate(
      ov_days = as.numeric(pmin(xmax, yr_end) - pmax(xmin, yr_start))
    ) %>%
    filter(ov_days > 0)
  if (nrow(over) == 0) return("Consolidation")
  over %>% slice_max(ov_days, n = 1, with_ties = FALSE) %>% pull(regime) %>% as.character()
}

# ── Helper: build annual returns tibble ───────────────────────────────────────
.annual_returns <- function(xts_col) {
  tibble(
    date = as.Date(index(xts_col)),
    ret  = as.numeric(xts_col[, 1])
  ) %>%
    filter(!is.na(ret)) %>%
    mutate(year = as.integer(format(date, "%Y"))) %>%
    group_by(year) %>%
    arrange(date) %>%
    summarise(
      annual_ret = prod(1 + ret) - 1,
      .groups = "drop"
    )
}

# ==============================================================================
# compute_transitions — regime-conditioned transition matrix
# ==============================================================================
compute_transitions <- function(xts_col, rt = NULL) {

  ticker <- colnames(xts_col)[1]

  ann <- .annual_returns(xts_col) %>%
    mutate(
      regime    = map_chr(year, ~ .regime_for_year(rt, .x)),
      direction = if_else(annual_ret >= 0, "Up", "Down"),
      next_dir  = lead(direction),
      next_ret  = lead(annual_ret)
    ) %>%
    filter(!is.na(next_dir))

  # ── Lag-1 autocorrelation ─────────────────────────────────────────────────
  rho <- cor(ann$annual_ret, ann$next_ret, use = "complete.obs")
  cat(sprintf("\n%s — Annual return lag-1 autocorrelation: ρ = %.3f\n", ticker, rho))
  if (rho > 0.1)  cat("  → Mild MOMENTUM tendency (trend more likely than reversal)\n")
  if (rho < -0.1) cat("  → Mild MEAN-REVERSION tendency (reversal more likely than trend)\n")
  if (abs(rho) <= 0.1) cat("  → No clear tendency (coin-flip at annual horizon)\n")

  # ── Overall transition matrix ─────────────────────────────────────────────
  cat("\n── Overall Transition Matrix ─────────────────────────────────────────\n")
  trans_overall <- ann %>%
    count(direction, next_dir) %>%
    group_by(direction) %>%
    mutate(prob = n / sum(n)) %>%
    ungroup() %>%
    mutate(label = sprintf("%.0f%% (n=%d)", prob * 100, n))

  cat("  Given this year is UP:\n")
  cat(sprintf("    → Next year Up:   %s\n",
      trans_overall %>% filter(direction=="Up",   next_dir=="Up")   %>% pull(label) %>% paste(collapse="")))
  cat(sprintf("    → Next year Down: %s\n",
      trans_overall %>% filter(direction=="Up",   next_dir=="Down") %>% pull(label) %>% paste(collapse="")))
  cat("  Given this year is DOWN:\n")
  cat(sprintf("    → Next year Up:   %s\n",
      trans_overall %>% filter(direction=="Down", next_dir=="Up")   %>% pull(label) %>% paste(collapse="")))
  cat(sprintf("    → Next year Down: %s\n",
      trans_overall %>% filter(direction=="Down", next_dir=="Down") %>% pull(label) %>% paste(collapse="")))

  # ── Regime-conditioned ────────────────────────────────────────────────────
  if (!is.null(rt)) {
    cat("\n── Regime-Conditioned Transitions ────────────────────────────────────\n")
    trans_regime <- ann %>%
      count(regime, direction, next_dir) %>%
      group_by(regime, direction) %>%
      mutate(prob = n / sum(n)) %>%
      ungroup()

    for (rg in c("Fall", "Recovery", "Consolidation")) {
      sub <- trans_regime %>% filter(regime == rg)
      if (nrow(sub) == 0) next
      cat(sprintf("  [%s year]\n", rg))
      for (d in c("Up", "Down")) {
        cat(sprintf("    This year %s → next Up: %s  |  next Down: %s\n",
          d,
          sub %>% filter(direction==d, next_dir=="Up")   %>%
            mutate(lbl=sprintf("%.0f%%",prob*100)) %>% pull(lbl) %>% paste(collapse="n/a"),
          sub %>% filter(direction==d, next_dir=="Down") %>%
            mutate(lbl=sprintf("%.0f%%",prob*100)) %>% pull(lbl) %>% paste(collapse="n/a")
        ))
      }
    }
  }

  invisible(list(autocorr = rho, transitions = trans_overall, data = ann))
}

# ==============================================================================
# plot_calendar_analog — analog year overlay + trend/reversal probability
# ==============================================================================
plot_calendar_analog <- function(xts_col,
                                 rt           = NULL,
                                 current_year = as.integer(format(Sys.Date(), "%Y")),
                                 window_pct   = 0.06,   # ±6pp YTD to define analogs
                                 n_min        = 3) {    # minimum analogs before widening

  ticker <- colnames(xts_col)[1]

  # ── Build day-level data ──────────────────────────────────────────────────
  df <- tibble(
    date = as.Date(index(xts_col)),
    ret  = as.numeric(xts_col[, 1])
  ) %>%
    filter(!is.na(ret)) %>%
    mutate(year = as.integer(format(date, "%Y"))) %>%
    group_by(year) %>%
    arrange(date) %>%
    mutate(tday = row_number(), cum_ret = cumprod(1 + ret) - 1) %>%
    ungroup()

  # ── Current year snapshot ─────────────────────────────────────────────────
  curr_df   <- df %>% filter(year == current_year)
  if (nrow(curr_df) == 0) stop("No data for current_year")
  curr_tday <- max(curr_df$tday)
  curr_ytd  <- curr_df %>% slice_max(tday, n = 1) %>% pull(cum_ret)

  # ── YTD of each historical year at curr_tday ──────────────────────────────
  ytd_hist <- df %>%
    filter(year < current_year) %>%
    group_by(year) %>%
    filter(tday <= curr_tday) %>%
    slice_max(tday, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(year, ytd_hist = cum_ret,
              dist = abs(ytd_hist - curr_ytd))

  # ── Select analogs: within window, widen if too few ──────────────────────
  win <- window_pct
  analog_years <- ytd_hist %>% filter(dist <= win) %>% pull(year)
  while (length(analog_years) < n_min && win < 0.50) {
    win <- win + 0.02
    analog_years <- ytd_hist %>% filter(dist <= win) %>% pull(year)
  }
  actual_window <- win

  # ── Analog year outcomes ──────────────────────────────────────────────────
  analog_outcomes <- df %>%
    filter(year %in% analog_years) %>%
    group_by(year) %>%
    slice_max(tday, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(trend = sign(cum_ret) == sign(curr_ytd))

  p_trend    <- mean(analog_outcomes$trend, na.rm = TRUE)
  p_reversal <- 1 - p_trend
  n_analogs  <- length(analog_years)

  # ── Forward cone: distribution of analog paths from curr_tday onward ──────
  cone_df <- df %>%
    filter(year %in% analog_years, tday >= curr_tday) %>%
    group_by(tday) %>%
    summarise(
      q10 = quantile(cum_ret, 0.10, na.rm = TRUE),
      q25 = quantile(cum_ret, 0.25, na.rm = TRUE),
      med = median(cum_ret,         na.rm = TRUE),
      q75 = quantile(cum_ret, 0.75, na.rm = TRUE),
      q90 = quantile(cum_ret, 0.90, na.rm = TRUE),
      .groups = "drop"
    )

  # ── Day-0 anchors ─────────────────────────────────────────────────────────
  anchors <- df %>%
    group_by(year) %>%
    slice_min(tday, n = 1) %>%
    mutate(tday = 0L, cum_ret = 0, ret = 0) %>%
    ungroup()

  plot_df <- bind_rows(anchors, df) %>%
    arrange(year, tday) %>%
    mutate(year_type = case_when(
      year == current_year  ~ "current",
      year %in% analog_years ~ "analog",
      TRUE                   ~ "other"
    ))

  # ── Current regime ────────────────────────────────────────────────────────
  curr_regime <- .regime_for_year(rt, current_year)

  # ── Annotation label ──────────────────────────────────────────────────────
  direction_word <- if (curr_ytd >= 0) "UP" else "DOWN"
  ann <- sprintf(
    "Current YTD: %+.1f%%  (%s)\nAnalog years (n=%d, ±%.0fpp): %s\nP(Trend cont.):  %.0f%%\nP(Reversal):      %.0f%%\nCurrent regime: %s",
    curr_ytd * 100, direction_word,
    n_analogs, actual_window * 100,
    paste(sort(analog_years), collapse = ", "),
    p_trend    * 100,
    p_reversal * 100,
    curr_regime
  )

  # ── Year-end labels for analog years ─────────────────────────────────────
  analog_labels <- df %>%
    filter(year %in% analog_years) %>%
    group_by(year) %>%
    slice_max(tday, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(lbl = paste0(sprintf("%+.0f%%", cum_ret * 100), "  ", year))

  # ── Plot ─────────────────────────────────────────────────────────────────
  ggplot() +

    # All other years — faint grey
    geom_line(
      data      = plot_df %>% filter(year_type == "other"),
      aes(x = tday, y = cum_ret, group = factor(year)),
      colour    = "grey82", linewidth = 0.35, alpha = 0.7
    ) +

    # Analog years — blue
    geom_line(
      data      = plot_df %>% filter(year_type == "analog"),
      aes(x = tday, y = cum_ret, group = factor(year)),
      colour    = "#4A90D9", linewidth = 0.65, alpha = 0.7
    ) +

    # Cone outer band (10–90)
    geom_ribbon(
      data  = cone_df,
      aes(x = tday, ymin = q10, ymax = q90),
      fill  = "#4A90D9", alpha = 0.08, inherit.aes = FALSE
    ) +

    # Cone inner band (25–75)
    geom_ribbon(
      data  = cone_df,
      aes(x = tday, ymin = q25, ymax = q75),
      fill  = "#4A90D9", alpha = 0.18, inherit.aes = FALSE
    ) +

    # Median analog path from curr_tday
    geom_line(
      data      = cone_df,
      aes(x = tday, y = med),
      colour    = "#2471A3", linewidth = 0.9, linetype = "dashed", alpha = 0.85
    ) +

    # Current year — red, thick
    geom_line(
      data      = plot_df %>% filter(year_type == "current"),
      aes(x = tday, y = cum_ret, group = factor(year)),
      colour    = "#E74C3C", linewidth = 1.5
    ) +

    # Analog year-end labels
    geom_text(
      data        = analog_labels,
      aes(x = tday, y = cum_ret, label = lbl),
      hjust       = -0.1, size = 2.2, colour = "#2471A3", fontface = "bold"
    ) +

    # Vertical "today" line
    geom_vline(
      xintercept = curr_tday,
      colour     = "grey45", linewidth = 0.5, linetype = "dotted"
    ) +
    annotate("text", x = curr_tday, y = Inf,
             label = "← today", vjust = -0.3, hjust = 1.1,
             size = 3, colour = "grey45") +

    # Zero line
    geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.4) +

    # Annotation box
    annotate("label",
             x = Inf, y = Inf,
             label     = ann,
             hjust     = 1.05, vjust = 1.05,
             size      = 3.0,  fontface = "plain",
             fill      = "white", colour = "grey30",
             label.size = 0.3, label.padding = unit(0.4, "lines")) +

    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      name   = "Cumulative Return"
    ) +
    scale_x_continuous(
      name   = "Trading Day of Year",
      expand = expansion(mult = c(0.01, 0.12))
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey92", linewidth = 0.4),
      axis.title       = element_text(size = 11, face = "bold"),
      axis.text        = element_text(size = 10),
      plot.title       = element_text(face = "bold", size = 14),
      plot.subtitle    = element_text(size = 10, colour = "grey40"),
      plot.caption     = element_text(size = 8,  colour = "grey55")
    ) +
    labs(
      title    = paste0(ticker, "  —  Calendar Year Analog Projection"),
      subtitle = paste0(
        "Grey = all years  |  Blue = analog years (similar YTD at today)  |  ",
        "Shaded cone = 10–90th pct of analog paths  |  Red = current year"
      ),
      caption  = paste0(
        sprintf("Analog window: ±%.0fpp YTD  |  ", actual_window * 100),
        sprintf("P(Trend): %.0f%%  P(Reversal): %.0f%%  |  ", p_trend*100, p_reversal*100),
        "Method: historical analog matching"
      )
    )
}
