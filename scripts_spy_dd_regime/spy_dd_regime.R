################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : scripts_spy_dd_regime/spy_dd_regime.R
# Purpose : SPY Drawdown Regime Engine — Fall / Recovery / Cruise / Consolidation
#
# REGIME DEFINITIONS
#   Fall          : SPY drawdown from recent peak ≥ t_fall  (e.g. 10%)
#   Recovery      : After a Fall — drawdown between 0 and t_fall, heading back
#   Cruise        : New-high territory with running DD < t_cruise  (e.g. 5%)
#   Consolidation : Between Cruise and Fall — DD between t_cruise and t_fall
#
# FUNCTIONS
#   build_regime_table()    Core engine → period-level regime data frame
#   label_daily_regime()    Day-level regime xts (for downstream analytics)
#   plot_regime_overlay()   Main chart: cumulative line + 2 bar rows + stats
#   plot_regime_stats()     Bar chart: avg return & avg duration per regime
#   plot_regime_calendar()  Monthly return heatmap coloured by regime
#   run_regime_analysis()       Convenience wrapper — runs all three plots
#   plot_self_regime_multi()    Per-ticker own regime overlay (not SPY's cycle)
#
# INPUTS (no global env dependency)
#   xts_ret_col   : single-column xts of daily returns (e.g. xts_ret[,"SPY"])
#   t_fall        : drawdown threshold for Fall regime  (default 0.10)
#   t_cruise      : max DD allowed in Cruise regime     (default 0.05)
#
################################################################################

library(tidyverse)
library(xts)
library(PerformanceAnalytics)
library(scales)
library(lubridate)

# Palette — shared across all visuals
REGIME_PAL <- c(
  Fall          = "#D90429",   # red
  Recovery      = "#F77F00",   # orange
  Consolidation = "#2D6A4F"    # dark green
)

# ==============================================================================
# 1. build_regime_table()
# ==============================================================================
# Returns a data frame with one row per contiguous regime period:
#   regime, xmin, xmax, days, period_return, ann_return, label, is_thin
# ==============================================================================

build_regime_table <- function(xts_ret_col,
                                t_fall   = 0.10,
                                t_cruise = 0.05) {

  stopifnot(is.xts(xts_ret_col), ncol(xts_ret_col) == 1)
  colnames(xts_ret_col) <- "r"
  all_dates <- index(xts_ret_col)

  # ── Step 1: Get macro drawdown events (Fall + Recovery periods) ───────────
  dd_raw <- tryCatch(
    table.Drawdowns(xts_ret_col, top = 100),
    error = function(e) NULL
  )
  if (is.null(dd_raw) || nrow(dd_raw) == 0)
    stop("No drawdown events found. Check input data.")

  macro_dd <- dd_raw %>%
    filter(Depth <= -t_fall) %>%
    arrange(From) %>%
    mutate(
      From   = as.Date(From),
      Trough = as.Date(Trough),
      To     = as.Date(ifelse(is.na(To), max(all_dates), To))
    )

  if (nrow(macro_dd) == 0)
    stop(sprintf("No drawdowns ≥ %.0f%% found.", t_fall * 100))

  # ── Step 2: Build Fall + Recovery rows ────────────────────────────────────
  fr_rows <- map_dfr(seq_len(nrow(macro_dd)), function(i) {
    d <- macro_dd[i, ]
    bind_rows(
      tibble(regime = "Fall",     xmin = d$From,   xmax = d$Trough),
      tibble(regime = "Recovery", xmin = d$Trough, xmax = d$To)
    )
  })

  # ── Step 3: Identify gaps between macro events (candidate Cruise/Consol) ──
  # Gap i = end of recovery i → start of Fall i+1
  recovery_ends <- fr_rows %>% filter(regime == "Recovery") %>% pull(xmax)
  fall_starts   <- fr_rows %>% filter(regime == "Fall")     %>% pull(xmin)

  gap_starts <- c(min(all_dates), recovery_ends)          # first gap = data start
  gap_ends   <- c(fall_starts, max(all_dates))            # last gap = data end

  # Only keep gaps with positive width
  gaps <- tibble(xmin = gap_starts, xmax = gap_ends) %>%
    filter(xmax > xmin)

  # ── Step 4: All gap periods are Consolidation ────────────────────────────
  gap_rows <- map_dfr(seq_len(nrow(gaps)), function(i) {
    g      <- gaps[i, ]
    window <- xts_ret_col[paste0(g$xmin, "/", g$xmax)]
    if (length(window) == 0) return(NULL)
    tibble(regime = "Consolidation", xmin = g$xmin, xmax = g$xmax)
  })

  # ── Step 5: Combine, sort, compute period stats ───────────────────────────
  all_rows <- bind_rows(fr_rows, gap_rows) %>%
    arrange(xmin) %>%
    mutate(
      # Clamp xmax to last available date (handles NA To in open drawdowns)
      xmax = pmin(as.Date(xmax), max(all_dates))
    ) %>%
    rowwise() %>%
    mutate(
      days          = as.numeric(xmax - xmin),
      period_return = {
        w <- xts_ret_col[paste0(xmin, "/", xmax)]
        if (length(w) == 0) 0 else as.numeric(Return.cumulative(w))
      },
      ann_return    = {
        if (days < 2) NA_real_
        else as.numeric(Return.annualized(
          xts_ret_col[paste0(xmin, "/", xmax)], scale = 252
        ))
      },
      label   = percent(period_return, accuracy = 1),
      is_thin = days < 45,
      color   = REGIME_PAL[regime]
    ) %>%
    ungroup() %>%
    mutate(regime = factor(regime, levels = names(REGIME_PAL)))

  all_rows
}


# ==============================================================================
# 2. label_daily_regime()
# ==============================================================================
# Returns xts with columns: regime (character), regime_color (hex)
# Useful for downstream analytics (e.g. regime-conditioned Sharpe).
# ==============================================================================

label_daily_regime <- function(xts_ret_col,
                                t_fall   = 0.10,
                                t_cruise = 0.05) {

  rt  <- build_regime_table(xts_ret_col, t_fall, t_cruise)
  idx <- index(xts_ret_col)

  regime_vec <- rep(NA_character_, length(idx))
  for (i in seq_len(nrow(rt))) {
    mask <- idx >= rt$xmin[i] & idx <= rt$xmax[i]
    regime_vec[mask] <- as.character(rt$regime[i])
  }

  xts(data.frame(regime = regime_vec, stringsAsFactors = FALSE),
      order.by = idx)
}


# ==============================================================================
# 3. plot_regime_overlay()
# ==============================================================================
# Main chart — three-layer layout:
#   Top     : Cumulative return line with Cruise/Consol background shading
#   Middle  : Macro regime bar row (Fall / Recovery / Cruise / Consolidation)
#   Bottom  : Micro regime bar row (t_cruise threshold)
#   Footer  : Summary stats box + current regime badge
# ==============================================================================

plot_regime_overlay <- function(xts_ret_col,
                                 t_fall        = 0.10,
                                 t_cruise      = 0.05,
                                 asset_name    = "SPY",
                                 label_size    = 2.8,
                                 overlay_ret   = NULL,     # optional 2nd ticker xts
                                 overlay_name  = NULL,
                                 t_fall_jitter = NULL) {   # optional finer threshold bar

  rt     <- build_regime_table(xts_ret_col, t_fall, t_cruise)
  cum_df <- tibble(
    date   = index(xts_ret_col),
    cumret = as.numeric(cumprod(1 + xts_ret_col) - 1)
  )

  # ── Jitter regime table (finer threshold) ─────────────────────────────────
  rt_jitter <- if (!is.null(t_fall_jitter)) {
    stopifnot(t_fall_jitter < t_fall)
    build_regime_table(xts_ret_col, t_fall_jitter, t_fall_jitter / 2)
  } else NULL

  # ── Coordinate system ──────────────────────────────────────────────────────
  max_val     <- max(cum_df$cumret, na.rm = TRUE)
  min_val     <- min(cum_df$cumret, na.rm = TRUE)
  total_range <- max_val - min_val

  bar_h    <- total_range * 0.08
  gap      <- total_range * 0.015
  n_rows   <- 1L +
              (!is.null(overlay_ret))  +
              (!is.null(t_fall_jitter))

  y_row1        <- min_val - total_range * 0.18               # macro bar (t_fall)
  y_row2        <- y_row1 - bar_h - gap                       # overlay ticker bar
  y_row_jitter  <- y_row1 - bar_h - gap                       # jitter bar (no overlay)
  if (!is.null(overlay_ret)) y_row_jitter <- y_row2 - bar_h - gap
  y_floor  <- y_row1 - (n_rows * (bar_h + gap)) - total_range * 0.20

  # ── Year grid ──────────────────────────────────────────────────────────────
  year_markers <- cum_df %>%
    mutate(year = format(date, "%Y")) %>%
    group_by(year) %>% slice(1) %>% ungroup()

  # ── Summary stats ─────────────────────────────────────────────────────────
  total_r <- percent(as.numeric(Return.cumulative(xts_ret_col)), accuracy = 0.1)
  ann_r   <- percent(as.numeric(Return.annualized(xts_ret_col)), accuracy = 0.1)
  mdd     <- percent(as.numeric(maxDrawdown(xts_ret_col)), accuracy = 0.1)
  vol     <- percent(as.numeric(StdDev.annualized(xts_ret_col)), accuracy = 0.1)
  sharpe  <- round(as.numeric(SharpeRatio.annualized(xts_ret_col, Rf = 0)), 2)
  stats_label <- sprintf(
    "Total: %s  |  Ann: %s  |  MaxDD: %s  |  Vol: %s  |  Sharpe: %s",
    total_r, ann_r, mdd, vol, sharpe
  )

  # ── Current regime ─────────────────────────────────────────────────────────
  current_regime <- as.character(tail(rt$regime, 1))
  current_color  <- REGIME_PAL[current_regime]
  current_since  <- format(tail(rt$xmin, 1), "%d %b %Y")
  current_label  <- sprintf("NOW: %s (since %s)", current_regime, current_since)

  # ── Transition arrows: each boundary coloured by the incoming regime ───────
  trans_arrows <- rt %>%
    arrange(xmin) %>%
    mutate(next_color = lead(as.character(color))) %>%
    filter(!is.na(next_color))

  # ── Overlay bar data ───────────────────────────────────────────────────────
  if (!is.null(overlay_ret)) {
    stopifnot(is.xts(overlay_ret), ncol(overlay_ret) == 1)
    colnames(overlay_ret) <- "r"
    o_bars <- rt %>%
      rowwise() %>%
      mutate(
        o_ret   = {
          w <- overlay_ret[paste0(xmin, "/", xmax)]
          if (length(w) == 0) 0 else as.numeric(Return.cumulative(w))
        },
        o_label = percent(o_ret, accuracy = 1)
      ) %>% ungroup()
  }

  # ── Build plot ─────────────────────────────────────────────────────────────
  p <- ggplot() +

    # Background shading for Consolidation periods
    geom_rect(
      data = rt %>% filter(regime == "Consolidation"),
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = regime),
      alpha = 0.06, inherit.aes = FALSE
    ) +
    scale_fill_manual(
      values = c(Consolidation = "#2D6A4F"),
      guide  = "none"
    ) +

    # Year grid
    geom_vline(data = year_markers, aes(xintercept = date),
               color = "grey92", linewidth = 0.8) +

    # Phase transition markers (dashed = Fall start, solid = Recovery end)
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

    # Cumulative return line
    geom_line(data = cum_df, aes(x = date, y = cumret),
              color = "#1D3557", linewidth = 0.7) +

    # ── Macro regime bar row ────────────────────────────────────────────────
    geom_rect(
      data = rt,
      aes(xmin = xmin, xmax = xmax,
          ymin = y_row1, ymax = y_row1 + bar_h,
          fill = regime),
      color = "white", linewidth = 0.3, show.legend = TRUE
    ) +
    scale_fill_manual(
      name   = "Regime",
      values = REGIME_PAL,
      guide  = guide_legend(override.aes = list(size = 4))
    ) +

    # Labels inside macro bars
    geom_text(
      data = rt %>% filter(!is_thin),
      aes(x = xmin + (xmax - xmin) / 2,
          y = y_row1 + bar_h / 2,
          label = label),
      color = "white", size = label_size, fontface = "bold"
    ) +
    # Angled labels for thin bars
    geom_text(
      data = rt %>% filter(is_thin),
      aes(x = xmin + (xmax - xmin) / 2,
          y = y_row1 + bar_h + total_range * 0.03,
          label = label, color = color),
      angle = 45, hjust = 0, size = label_size * 0.85, fontface = "bold",
      show.legend = FALSE
    ) +

    # Transition arrows: ▶ at each regime boundary, coloured by next state
    geom_text(
      data = trans_arrows,
      aes(x = xmax, y = y_row1 + bar_h / 2,
          label = "▶", color = next_color),
      size = 3.2, hjust = 0.5, fontface = "bold",
      show.legend = FALSE
    ) +
    scale_color_identity() +

    # Ticker label left of macro bar
    annotate("text",
             x     = min(cum_df$date),
             y     = y_row1 + bar_h / 2,
             label = asset_name,
             hjust = 1.3, size = 4, fontface = "bold", color = "#1D3557")

  # ── Optional overlay bar row ───────────────────────────────────────────────
  if (!is.null(overlay_ret)) {
    p <- p +
      geom_rect(
        data = o_bars,
        aes(xmin = xmin, xmax = xmax,
            ymin = y_row2, ymax = y_row2 + bar_h,
            fill = regime),
        color = "white", linewidth = 0.3, alpha = 0.85,
        show.legend = FALSE
      ) +
      geom_text(
        data = o_bars %>% filter(!is_thin),
        aes(x    = xmin + (xmax - xmin) / 2,
            y    = y_row2 + bar_h / 2,
            label = o_label),
        color = "white", size = label_size, fontface = "bold"
      ) +
      annotate("text",
               x     = min(cum_df$date),
               y     = y_row2 + bar_h / 2,
               label = overlay_name %||% "Overlay",
               hjust = 1.3, size = 4, fontface = "bold", color = "grey35")
  }

  # ── Optional jitter bar row ────────────────────────────────────────────────
  if (!is.null(rt_jitter)) {
    p <- p +
      geom_rect(
        data = rt_jitter,
        aes(xmin = xmin, xmax = xmax,
            ymin = y_row_jitter, ymax = y_row_jitter + bar_h,
            fill = regime),
        color = "white", linewidth = 0.2, alpha = 0.70,
        show.legend = FALSE
      ) +
      geom_text(
        data = rt_jitter %>% filter(!is_thin),
        aes(x     = xmin + (xmax - xmin) / 2,
            y     = y_row_jitter + bar_h / 2,
            label = label),
        color = "white", size = label_size * 0.85, fontface = "bold"
      ) +
      annotate("text",
               x     = min(cum_df$date),
               y     = y_row_jitter + bar_h / 2,
               label = "Jitter",
               hjust = 1.3, size = 3.5, fontface = "italic", color = "grey45")
  }

  # ── Regime legend & current regime badge ──────────────────────────────────
  p <- p +

    # Summary stats (bottom of chart, below year labels)
    annotate("text",
             x     = min(cum_df$date) + (max(cum_df$date) - min(cum_df$date)) / 2,
             y     = y_floor + total_range * 0.04,
             label = stats_label,
             hjust = 0.5, size = 3, fontface = "bold", color = "grey40") +

    # Year labels
    geom_text(
      data = year_markers,
      aes(x = date, y = y_floor + total_range * 0.10, label = year),
      size = 3, fontface = "bold", color = "grey45"
    ) +

    scale_x_date(expand = expansion(mult = c(0.13, 0.04))) +
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
      plot.margin        = margin(t = 10, r = 15, b = 50, l = 60)
    ) +

    labs(
      title    = paste(asset_name, "— Drawdown Regime Chart"),
      subtitle = if (!is.null(t_fall_jitter)) {
        sprintf(
          "Macro threshold: ≥%.0f%%  |  Jitter threshold: ≥%.0f%%  |  Regimes: Fall / Recovery / Consolidation",
          t_fall * 100, t_fall_jitter * 100
        )
      } else {
        sprintf(
          "Fall threshold: ≥%.0f%%  |  Regimes: Fall / Recovery / Consolidation",
          t_fall * 100
        )
      }
    )

  p
}


# ==============================================================================
# 4. plot_regime_stats()
# ==============================================================================
# Two-panel bar chart:
#   Left  : Average period return per regime type
#   Right : Average period duration (calendar days) per regime type
# ==============================================================================

plot_regime_stats <- function(xts_ret_col,
                               t_fall   = 0.10,
                               t_cruise = 0.05,
                               asset_name = "SPY") {

  rt <- build_regime_table(xts_ret_col, t_fall, t_cruise)

  stats <- rt %>%
    group_by(regime) %>%
    summarise(
      n_periods    = n(),
      avg_return   = mean(period_return, na.rm = TRUE),
      med_return   = median(period_return, na.rm = TRUE),
      avg_days     = mean(days, na.rm = TRUE),
      total_days   = sum(days, na.rm = TRUE),
      pct_time     = sum(days, na.rm = TRUE) / sum(rt$days, na.rm = TRUE),
      .groups      = "drop"
    ) %>%
    mutate(
      color        = REGIME_PAL[as.character(regime)],
      ret_label    = percent(avg_return, accuracy = 0.1),
      days_label   = sprintf("%.0f days", avg_days),
      pct_label    = percent(pct_time, accuracy = 1)
    )

  p_ret <- ggplot(stats, aes(x = regime, y = avg_return, fill = color)) +
    geom_col(width = 0.65, show.legend = FALSE) +
    geom_text(aes(label = ret_label,
                  vjust = ifelse(avg_return >= 0, -0.4, 1.3)),
              size = 3.5, fontface = "bold", color = "grey20") +
    geom_hline(yintercept = 0, linewidth = 0.7) +
    scale_fill_identity() +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.title.x = element_blank()) +
    labs(title = "Avg Period Return", y = NULL)

  p_dur <- ggplot(stats, aes(x = regime, y = avg_days, fill = color)) +
    geom_col(width = 0.65, show.legend = FALSE) +
    geom_text(aes(label = days_label), vjust = -0.4,
              size = 3.5, fontface = "bold", color = "grey20") +
    scale_fill_identity() +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.title.x = element_blank()) +
    labs(title = "Avg Period Duration (days)", y = NULL)

  p_pct <- ggplot(stats, aes(x = regime, y = pct_time, fill = color)) +
    geom_col(width = 0.65, show.legend = FALSE) +
    geom_text(aes(label = pct_label), vjust = -0.4,
              size = 3.5, fontface = "bold", color = "grey20") +
    scale_fill_identity() +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.title.x = element_blank()) +
    labs(title = "% of Time in Regime", y = NULL)

  patchwork::wrap_plots(p_ret, p_dur, p_pct, nrow = 1) +
    patchwork::plot_annotation(
      title    = paste(asset_name, "— Regime Statistics"),
      subtitle = sprintf(
        "Fall ≥%.0f%%  |  Cruise <%.0f%%  |  n_periods shown inside bars",
        t_fall * 100, t_cruise * 100
      ),
      theme = theme(
        plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "grey50", size = 10)
      )
    )
}


# ==============================================================================
# 5. plot_regime_calendar()
# ==============================================================================
# Monthly return heatmap with regime colour border — quickly shows which
# calendar months belong to which regime.
# ==============================================================================

plot_regime_calendar <- function(xts_ret_col,
                                  t_fall     = 0.10,
                                  t_cruise   = 0.05,
                                  asset_name = "SPY") {

  daily_reg <- label_daily_regime(xts_ret_col, t_fall, t_cruise)

  monthly_df <- data.frame(
    date   = index(xts_ret_col),
    ret    = as.numeric(xts_ret_col),
    regime = as.character(daily_reg$regime)
  ) %>%
    mutate(
      year  = year(date),
      month = month(date, label = TRUE, abbr = TRUE)
    ) %>%
    group_by(year, month) %>%
    summarise(
      monthly_ret    = prod(1 + ret) - 1,
      dominant_regime = names(sort(table(regime), decreasing = TRUE))[1],
      .groups = "drop"
    ) %>%
    mutate(
      fill_val    = monthly_ret,
      border_col  = REGIME_PAL[dominant_regime],
      ret_label   = percent(monthly_ret, accuracy = 0.1),
      txt_color   = if_else(abs(monthly_ret) > 0.04, "white", "grey20")
    )

  ggplot(monthly_df, aes(x = month, y = factor(year, levels = rev(sort(unique(year)))))) +
    geom_tile(aes(fill = fill_val), color = "white", linewidth = 1.2) +
    geom_tile(aes(color = border_col), fill = NA, linewidth = 1.0,
              show.legend = FALSE) +
    geom_text(aes(label = ret_label, color = txt_color),
              size = 2.6, fontface = "bold", show.legend = FALSE) +
    scale_fill_gradient2(
      low      = "#D90429", mid = "white", high = "#2D6A4F",
      midpoint = 0,
      name     = "Monthly Return",
      labels   = percent_format(accuracy = 1)
    ) +
    scale_color_identity() +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid  = element_blank(),
      axis.title  = element_blank(),
      legend.position = "right"
    ) +
    labs(
      title    = paste(asset_name, "— Monthly Return Calendar"),
      subtitle = "Fill = return magnitude  |  Border colour = dominant regime that month"
    )
}


# ==============================================================================
# 6. run_regime_analysis()   Convenience wrapper
# ==============================================================================

run_regime_analysis <- function(xts_ret_col,
                                 t_fall      = 0.10,
                                 t_cruise    = 0.05,
                                 asset_name  = "SPY",
                                 overlay_ret = NULL,
                                 overlay_name = NULL) {

  cat(sprintf(
    "\n── Regime Analysis: %s | Fall ≥%.0f%% | Cruise <%.0f%% ──────────────────\n",
    asset_name, t_fall * 100, t_cruise * 100
  ))

  rt <- build_regime_table(xts_ret_col, t_fall, t_cruise)

  # Print regime table summary
  summary_tbl <- rt %>%
    group_by(regime) %>%
    summarise(
      Periods    = n(),
      Avg_Return = percent(mean(period_return), accuracy = 0.1),
      Avg_Days   = round(mean(days)),
      Pct_Time   = percent(sum(days) / sum(rt$days), accuracy = 1),
      .groups    = "drop"
    )
  print(summary_tbl)
  cat(sprintf("Current regime: %s (since %s)\n",
              as.character(tail(rt$regime, 1)),
              format(tail(rt$xmin, 1), "%d %b %Y")))
  cat("─────────────────────────────────────────────────────────────────────────\n")

  print(plot_regime_overlay(xts_ret_col, t_fall, t_cruise, asset_name,
                             overlay_ret = overlay_ret,
                             overlay_name = overlay_name))
  print(plot_regime_stats(xts_ret_col, t_fall, t_cruise, asset_name))
  print(plot_regime_calendar(xts_ret_col, t_fall, t_cruise, asset_name))

  invisible(rt)
}

# ==============================================================================
# plot_self_regime_multi()
# PURPOSE : For each ticker in `tickers`, build its own regime table and render
#           plot_regime_overlay() using the ticker's own drawdown cycle.
#           Each ticker gets its own Fall/Recovery/Consolidation — not SPY's.
# ARGS
#   xts_ret  : xts of daily returns (multi-column)
#   tickers  : character vector of tickers to plot
#   t_fall   : drawdown threshold (default 0.10)
#   t_cruise : cruise threshold   (default 0.05)
# ==============================================================================
plot_self_regime_multi <- function(xts_ret,
                                   tickers,
                                   t_fall   = 0.10,
                                   t_cruise = 0.05) {

  tickers <- tickers[tickers %in% colnames(xts_ret)]

  for (tk in tickers) {
    cat(sprintf("\n── Self-Regime: %s ──\n", tk))
    p <- suppressWarnings(
      plot_regime_overlay(
        xts_ret_col = xts_ret[, tk],
        t_fall      = t_fall,
        t_cruise    = t_cruise,
        asset_name  = tk
      )
    )
    print(p)
  }

  invisible(tickers)
}

# ==============================================================================
# plot_regime_sync()
# PURPOSE : Regime synchronisation heatmap — one row per ticker, x = time,
#           fill = each ticker's own Fall/Recovery/Consolidation cycle.
#           SPY added as reference row at top (separator line).
#           Tickers sorted by % time in Fall (most defensive at bottom).
# ARGS
#   xts_ret     : xts of daily returns (multi-column)
#   tickers     : character vector (SPY added automatically as reference)
#   t_fall      : drawdown threshold (default 0.10)
#   t_cruise    : cruise threshold   (default 0.05)
#   title       : optional title override
# ==============================================================================
plot_regime_sync <- function(xts_ret,
                              tickers,
                              t_fall   = 0.10,
                              t_cruise = 0.05,
                              title    = NULL) {

  tickers <- tickers[tickers %in% colnames(xts_ret)]
  all_tks <- unique(c("SPY", tickers))
  all_tks <- all_tks[all_tks %in% colnames(xts_ret)]

  # Build regime rectangles per ticker
  rects <- map_dfr(all_tks, function(tk) {
    suppressWarnings(build_regime_table(xts_ret[, tk], t_fall, t_cruise)) %>%
      mutate(ticker = tk)
  })

  # Ticker order: SPY first (top), then tickers sorted by % time in Fall asc
  fall_pct <- rects %>%
    filter(ticker != "SPY") %>%
    group_by(ticker) %>%
    summarise(
      fall_pct = sum(days[regime == "Fall"], na.rm = TRUE) /
                 sum(days, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(fall_pct))

  ticker_order <- c(rev(fall_pct$ticker), "SPY")   # SPY at top
  rects <- rects %>%
    mutate(
      ticker = factor(ticker, levels = ticker_order),
      color  = unname(REGIME_PAL[as.character(regime)])
    )

  plot_title <- title %||% paste0(
    "Regime Synchronisation \u2014 Each Ticker\u2019s Own Cycle  |  T_fall = ",
    scales::percent(t_fall, accuracy = 1)
  )

  ggplot(rects) +
    geom_rect(aes(xmin = xmin, xmax = xmax,
                  ymin = as.numeric(ticker) - 0.45,
                  ymax = as.numeric(ticker) + 0.45,
                  fill = color)) +
    # SPY separator line
    geom_hline(yintercept = which(ticker_order == "SPY") - 0.5,
               color = "white", linewidth = 0.8) +
    scale_fill_identity(
      guide  = "legend",
      name   = NULL,
      labels = names(REGIME_PAL),
      breaks = unname(REGIME_PAL)
    ) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                 expand = c(0.01, 0)) +
    scale_y_continuous(
      breaks = seq_along(ticker_order),
      labels = ticker_order,
      expand = c(0.02, 0)
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid   = element_blank(),
      axis.title   = element_blank(),
      axis.text.y  = element_text(face = "bold", size = 9),
      axis.text.x  = element_text(size = 9),
      legend.position   = "bottom",
      legend.key.width  = unit(1.2, "cm"),
      legend.key.height = unit(0.35, "cm"),
      plot.title   = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "grey45", size = 9)
    ) +
    labs(
      title    = plot_title,
      subtitle = paste0("SPY = reference (top)  |  Others = own drawdown cycle  |  ",
                        "sorted by % time in Fall (most defensive at top)")
    )
}

# ==============================================================================
# MAIN: Run analysis on SPY
# ==============================================================================

if (!exists("xts_ret")) {
  xts_ret <- readRDS(here::here("02_data_processed/xts_ret_returns.rds"))
}

spy_ret <- xts_ret[, "SPY"]

# ── Default: SPY standalone ───────────────────────────────────────────────────
spy_regimes <- run_regime_analysis(
  xts_ret_col  = spy_ret,
  t_fall       = 0.10,
  t_cruise     = 0.05,
  asset_name   = "SPY"
)

# ── With QQQ overlay (relative strength per regime) ──────────────────────────
run_regime_analysis(
  xts_ret_col  = spy_ret,
  t_fall       = 0.10,
  t_cruise     = 0.05,
  asset_name   = "SPY",
  overlay_ret  = xts_ret[, "QQQ"],
  overlay_name = "QQQ"
)

################################################################################
