################################################################################
# utility/plot_sovereign_tapes.R
# Purpose : Regime-aware performance tape functions — horizontal stress bars,
#           timeline heatmaps, and the alpha tape (SPY absolute + others
#           relative) with quarterly x-axis labels.
#           Ported from etf_saa_taa/plot_sovereign_stress_bars.R.
#
# Functions (all self-contained, no global dependencies)
#   plot_sovereign_stress_bars()       horizontal DD vs Recovery bar chart
#   plot_sovereign_heatmap_continuous() phase-colored timeline heatmap
#   plot_sovereign_full_spectrum_V2()  continuous performance tape (all periods)
#   plot_sovereign_multi_tape_v2()     stacked tapes — absolute returns
#   plot_sovereign_alpha_tape_v12()    ** primary ** SPY abs + others relative,
#                                      quarterly labels, regime markers
#
# Usage
#   source(here::here("utility/plot_sovereign_tapes.R"))
#   plot_sovereign_alpha_tape_v12(xts_ret, xts_rel,
#     target_tickers = c("GLD", "IEF", "XLK"), dd_threshold = 0.10)
################################################################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(PerformanceAnalytics)
library(xts)

# ── 1. STRESS BARS ────────────────────────────────────────────────────────────
#' Horizontal bar chart comparing Drawdown vs Recovery per ticker per event.
#'
#' @param results_df  Tibble with columns: ticker, event, Drawdown, Recovery.
plot_sovereign_stress_bars <- function(results_df) {

  plot_data <- results_df %>%
    pivot_longer(cols = c(Drawdown, Recovery),
                 names_to  = "Metric",
                 values_to = "Performance") %>%
    mutate(Metric = factor(Metric, levels = c("Drawdown", "Recovery")))

  ggplot(plot_data,
         aes(y = reorder(ticker, Performance),
             x = Performance, fill = Metric)) +
    geom_col(position = position_dodge(width = 0.8),
             width = 0.7, colour = "white", linewidth = 0.2) +
    geom_text(aes(label = paste0(round(Performance * 100, 1), "%")),
              position = position_dodge(width = 0.8),
              hjust = ifelse(plot_data$Performance >= 0, -0.2, 1.2),
              size = 3.5, fontface = "bold") +
    facet_wrap(~event, scales = "free_x") +
    scale_fill_manual(values = c(Drawdown  = "#D90429",
                                 Recovery  = "#2D6A4F")) +
    geom_vline(xintercept = 0, colour = "black",
               linetype = "solid", alpha = 0.3) +
    theme_minimal(base_size = 12) +
    theme(legend.position       = "top",
          panel.grid.major.y    = element_blank(),
          axis.text.y           = element_text(face = "bold", size = 11),
          plot.title            = element_text(face = "bold", size = 16),
          strip.background      = element_rect(fill = "grey95", colour = NA),
          strip.text            = element_text(face = "bold")) +
    labs(title    = "Sovereign Stress Analysis: SPY vs. Others",
         subtitle = "Red = Peak-to-Trough  |  Green = Trough-to-Peak",
         x = "Total Return during Period",
         y = NULL, fill = "Phase:")
}

# ── 2. CONTINUOUS TIMELINE HEATMAP ───────────────────────────────────────────
#' Timeline heatmap with phase-colored blocks per ticker.
#'
#' @param results_df  Tibble with: ticker, event, start_date, end_date,
#'                    Drawdown, Recovery.
plot_sovereign_heatmap_continuous <- function(results_df) {

  plot_data <- results_df %>%
    mutate(
      start_dn = as.Date(start_date),
      end_dn   = start_dn + (as.Date(end_date) - start_dn) * 0.4,
      start_up = end_dn,
      end_up   = as.Date(end_date)
    ) %>%
    pivot_longer(cols = c(Drawdown, Recovery),
                 names_to = "Phase", values_to = "Perf") %>%
    mutate(
      xmin = if_else(Phase == "Drawdown", start_dn, start_up),
      xmax = if_else(Phase == "Drawdown", end_dn,   end_up)
    )

  ggplot(plot_data) +
    geom_rect(aes(xmin = xmin, xmax = xmax,
                  ymin = as.numeric(factor(ticker)) - 0.4,
                  ymax = as.numeric(factor(ticker)) + 0.4,
                  fill = Perf),
              colour = "white", linewidth = 0.3) +
    geom_text(aes(x     = xmin + (xmax - xmin) / 2,
                  y     = as.numeric(factor(ticker)),
                  label = paste0(round(Perf * 100, 0), "%")),
              colour = "black", size = 3, fontface = "bold") +
    scale_fill_gradient2(low = "#D90429", mid = "white", high = "#2D6A4F",
                         midpoint = 0, name = "Return %") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(
      breaks = seq_along(unique(plot_data$ticker)),
      labels = levels(factor(plot_data$ticker))
    ) +
    theme_minimal() +
    labs(title    = "Sovereign Performance Timeline (Phase-Corrected)",
         subtitle = "Red = Drawdown Phase  |  Green = Recovery Phase",
         x = "Timeline", y = NULL) +
    theme(panel.grid.major.y = element_blank(),
          axis.text.y        = element_text(face = "bold", size = 11))
}

# ── 3. FULL SPECTRUM TAPE V2 ──────────────────────────────────────────────────
#' Continuous tape filling quiet/gap periods between stress events.
#'
#' @param results_df  Tibble with: ticker, event, start_date, end_date,
#'                    Drawdown, Recovery.
#' @param return_xts  XTS of absolute daily returns (all tickers).
plot_sovereign_full_spectrum_V2 <- function(results_df, return_xts) {

  all_tickers <- unique(results_df$ticker)

  stress_phases <- results_df %>%
    mutate(
      start_dn = as.Date(start_date),
      trough   = start_dn + (as.Date(end_date) - start_dn) * 0.4,
      end_up   = as.Date(end_date)
    ) %>%
    rowwise() %>%
    reframe(ticker,
            xmin  = c(start_dn, trough),
            xmax  = c(trough,   end_up),
            Perf  = c(Drawdown, Recovery),
            Phase = c("Down", "Up"))

  event_windows <- results_df %>%
    group_by(start_date, end_date) %>%
    summarise(.groups = "drop") %>%
    arrange(start_date)

  gap_dates <- data.frame(
    g_start = c(min(zoo::index(return_xts)), as.Date(event_windows$end_date)),
    g_end   = c(as.Date(event_windows$start_date), max(zoo::index(return_xts)))
  ) %>% filter(g_start < g_end)

  gap_returns <- purrr::map_df(seq_len(nrow(gap_dates)), function(i) {
    s           <- gap_dates$g_start[i]
    e           <- gap_dates$g_end[i]
    period_xts  <- return_xts[paste0(s, "/", e)]
    if (nrow(period_xts) < 2) return(NULL)
    PerformanceAnalytics::Return.cumulative(period_xts) %>%
      as.data.frame() %>%
      pivot_longer(cols = everything(),
                   names_to = "ticker", values_to = "Perf") %>%
      mutate(xmin = s, xmax = e, Phase = "Quiet")
  })

  full_data <- bind_rows(stress_phases, gap_returns) %>%
    filter(ticker %in% all_tickers) %>%
    mutate(ticker = factor(ticker, levels = rev(all_tickers)))

  ggplot(full_data,
         aes(xmin = xmin, xmax = xmax,
             ymin = as.numeric(ticker) - 0.4,
             ymax = as.numeric(ticker) + 0.4)) +
    geom_rect(aes(fill = Perf), colour = "white", linewidth = 0.1) +
    geom_text(aes(x     = xmin + (xmax - xmin) / 2,
                  y     = as.numeric(ticker),
                  label = if_else(abs(Perf) > 0.03,
                                  paste0(round(Perf * 100, 0), "%"), "")),
              size = 3, fontface = "bold") +
    scale_fill_gradient2(low = "#D90429", mid = "white", high = "#2D6A4F",
                         midpoint = 0, name = "Return %") +
    scale_x_date(expand = c(0, 0), date_breaks = "1 year",
                 date_labels = "%Y") +
    scale_y_continuous(breaks = seq_along(all_tickers),
                       labels = levels(full_data$ticker)) +
    theme_minimal() +
    labs(title    = "Sovereign Full-Spectrum Tape",
         subtitle = "Continuous: Red (Crashes)  |  Green (Gains/Bull Markets)",
         x = "Timeline", y = NULL) +
    theme(panel.grid       = element_blank(),
          axis.text.y      = element_text(face = "bold", size = 12))
}

# ── 4. MULTI TAPE V2 (absolute returns) ───────────────────────────────────────
#' Stacked tapes coloured by SPY DD regime — absolute returns per ticker.
#'
#' @param return_xts      XTS of absolute daily returns.
#' @param target_tickers  Tickers to plot (SPY always added as master row).
#' @param dd_threshold    SPY drawdown threshold to define stress events.
plot_sovereign_multi_tape_v2 <- function(return_xts,
                                          target_tickers = c("XLK", "GLD", "IEF"),
                                          dd_threshold   = 0.10) {

  spy_rets   <- return_xts[, "SPY"]
  spy_events <- PerformanceAnalytics::table.Drawdowns(spy_rets, top = 10) %>%
    filter(Depth <= -dd_threshold) %>%
    arrange(From)

  stress_segments <- purrr::map_df(seq_len(nrow(spy_events)), function(i) {
    ev <- spy_events[i, ]
    data.frame(
      xmin   = c(as.Date(ev$From),   as.Date(ev$Trough)),
      xmax   = c(as.Date(ev$Trough), as.Date(ev$To)),
      Regime = c("Drawdown", "Recovery")
    )
  })

  all_event_dates <- sort(unique(c(stress_segments$xmin,
                                   stress_segments$xmax)))
  gap_dates <- data.frame(
    g_start = c(min(zoo::index(spy_rets)), all_event_dates),
    g_end   = c(all_event_dates, max(zoo::index(spy_rets)))
  ) %>%
    filter(g_start < g_end,
           !g_start %in% stress_segments$xmin) %>%
    mutate(Regime = "Grind") %>%
    rename(xmin = g_start, xmax = g_end)

  master_timeline <- bind_rows(stress_segments, gap_dates) %>% arrange(xmin)
  all_assets      <- c(target_tickers, "SPY")

  comparison_data <- purrr::map_df(seq_len(nrow(master_timeline)), function(i) {
    seg        <- master_timeline[i, ]
    period_rets <- return_xts[paste0(seg$xmin, "/", seg$xmax), all_assets]
    if (nrow(period_rets) < 2) return(NULL)
    as.data.frame(t(PerformanceAnalytics::Return.cumulative(period_rets))) %>%
      rownames_to_column("ticker") %>%
      mutate(xmin = seg$xmin, xmax = seg$xmax, Regime = seg$Regime)
  }) %>% rename(Perf = 2)

  plot_data <- comparison_data %>%
    mutate(
      ticker   = factor(ticker, levels = c("SPY", rev(target_tickers))),
      Fill_Col = case_when(
        Regime == "Drawdown" ~ "#E63946",
        Regime == "Recovery" ~ "#52B788",
        Perf   >= 0          ~ "#D8F3DC",
        TRUE                 ~ "#FAD2E1"
      )
    )

  ggplot(plot_data,
         aes(xmin = xmin, xmax = xmax,
             ymin = as.numeric(ticker) - 0.4,
             ymax = as.numeric(ticker) + 0.4)) +
    geom_rect(aes(fill = Fill_Col), colour = "white", linewidth = 0.2) +
    geom_text(aes(x     = xmin + (xmax - xmin) / 2,
                  y     = as.numeric(ticker),
                  label = paste0(round(Perf * 100, 0), "%")),
              colour = "black", fontface = "bold", size = 3) +
    scale_fill_identity() +
    scale_x_date(expand = c(0, 0), date_breaks = "1 year",
                 date_labels = "%Y") +
    scale_y_continuous(
      breaks = seq_along(all_assets),
      labels = c(expression(atop(bold("SPY"), scriptstyle("(Master)"))),
                 rev(target_tickers))
    ) +
    theme_minimal() +
    labs(title    = "Sovereign Multi-Ticker Tape",
         subtitle = paste0("SPY ", dd_threshold * 100, "% DD threshold"),
         x = NULL, y = NULL) +
    theme(panel.grid   = element_blank(),
          axis.text.y  = element_text(face = "bold", size = 11))
}

# ── 5. ALPHA TAPE V12 (primary function) ──────────────────────────────────────
#' Regime-aware alpha tape: SPY row shows absolute returns; all other rows show
#' relative returns (alpha vs SPY).  Quarterly x-axis with regime entry/exit
#' vertical markers.
#'
#' @param xts_abs         XTS of absolute daily log returns.
#' @param xts_rel         XTS of relative daily returns vs SPY.
#' @param target_tickers  Tickers to compare (SPY added automatically as master).
#' @param dd_threshold    SPY drawdown threshold defining stress events.
plot_sovereign_alpha_tape_v12 <- function(xts_abs,
                                           xts_rel,
                                           target_tickers = c("GLD", "IEF", "XLK"),
                                           dd_threshold   = 0.10) {

  spy_rets   <- xts_abs[, "SPY"]
  spy_events <- PerformanceAnalytics::table.Drawdowns(spy_rets, top = 10) %>%
    filter(Depth <= -dd_threshold) %>%
    arrange(From)

  stress_starts  <- as.Date(spy_events$From)
  recovery_ends  <- as.Date(spy_events$To)

  stress_segments <- purrr::map_df(seq_len(nrow(spy_events)), function(i) {
    ev <- spy_events[i, ]
    data.frame(
      xmin   = c(as.Date(ev$From),   as.Date(ev$Trough)),
      xmax   = c(as.Date(ev$Trough), as.Date(ev$To)),
      Regime = c("Drawdown", "Recovery")
    )
  })

  all_event_dates <- sort(unique(c(stress_segments$xmin,
                                   stress_segments$xmax)))
  gap_dates <- data.frame(
    g_start = c(min(zoo::index(spy_rets)), all_event_dates),
    g_end   = c(all_event_dates, max(zoo::index(spy_rets)))
  ) %>%
    filter(g_start < g_end,
           !g_start %in% stress_segments$xmin) %>%
    mutate(Regime = "Grind") %>%
    rename(xmin = g_start, xmax = g_end)

  master_timeline <- bind_rows(stress_segments, gap_dates) %>% arrange(xmin)
  all_assets      <- c(target_tickers, "SPY")

  # SPY row: absolute return; others: relative return (alpha)
  comparison_data <- purrr::map_df(seq_len(nrow(master_timeline)), function(i) {
    seg    <- master_timeline[i, ]
    window <- paste0(seg$xmin, "/", seg$xmax)

    spy_p    <- as.numeric(
      PerformanceAnalytics::Return.cumulative(xts_abs[window, "SPY"]))
    others_p <- PerformanceAnalytics::Return.cumulative(
      xts_rel[window, target_tickers])

    df_others <- as.data.frame(t(others_p)) %>%
      rownames_to_column("ticker") %>%
      rename(Perf = 2)
    df_spy    <- data.frame(ticker = "SPY", Perf = spy_p)

    bind_rows(df_spy, df_others) %>%
      mutate(xmin = seg$xmin, xmax = seg$xmax, Regime = seg$Regime)
  })

  plot_data <- comparison_data %>%
    mutate(
      ticker   = factor(ticker, levels = c("SPY", rev(target_tickers))),
      Fill_Col = case_when(
        ticker == "SPY" & Regime == "Drawdown" ~ "#E63946",
        ticker == "SPY" & Regime == "Recovery" ~ "#52B788",
        ticker == "SPY" & Perf   >= 0          ~ "#D8F3DC",
        ticker == "SPY" & Perf   <  0          ~ "#FAD2E1",
        Perf >= 0                              ~ "#2D6A4F",
        TRUE                                   ~ "#BC4749"
      ),
      Text_Col = if_else(ticker == "SPY", "#1A3A6D", "white")
    )

  ggplot(plot_data) +
    geom_rect(aes(xmin = xmin, xmax = xmax,
                  ymin = as.numeric(ticker) - 0.4,
                  ymax = as.numeric(ticker) + 0.4,
                  fill = Fill_Col),
              colour = "white", linewidth = 0.2) +
    # Regime entry markers
    geom_vline(xintercept = stress_starts,
               linetype = "dashed", colour = "#1A3A6D",
               linewidth = 0.5, alpha = 0.6) +
    annotate("text", x = stress_starts, y = 0.4,
             label  = format(stress_starts, "%Y-%m-%d"),
             angle  = 90, vjust = -0.5, hjust = 0,
             colour = "#1A3A6D", size = 2.5, fontface = "italic") +
    # Regime exit markers
    geom_vline(xintercept = recovery_ends,
               linetype = "dotted", colour = "#1A3A6D", linewidth = 0.6) +
    annotate("text", x = recovery_ends, y = 0.4,
             label  = format(recovery_ends, "%Y-%m-%d"),
             angle  = 90, vjust = 1.3, hjust = 0,
             colour = "#1A3A6D", size = 2.5) +
    # Performance labels
    geom_text(aes(x     = xmin + (xmax - xmin) / 2,
                  y     = as.numeric(ticker),
                  label = paste0(
                    ifelse(ticker != "SPY" & Perf > 0, "+", ""),
                    round(Perf * 100, 1), "%"),
                  colour = Text_Col),
              fontface = "bold", size = 3) +
    scale_fill_identity() +
    scale_colour_identity() +
    # Quarterly x-axis
    scale_x_date(
      expand      = c(0, 0),
      date_breaks = "3 months",
      labels      = function(x) {
        paste0(format(x, "%Y"), "-Q",
               (as.integer(format(x, "%m")) - 1L) %/% 3L + 1L)
      }
    ) +
    scale_y_continuous(
      breaks = seq_along(all_assets),
      labels = c(expression(atop(bold("SPY"), scriptstyle("(Abs)"))),
                 rev(target_tickers))
    ) +
    theme_minimal() +
    labs(title    = "Relative to Abs SPY Returns",
         subtitle = paste0("SPY = Absolute  |  Others = Alpha vs SPY",
                           "  |  Threshold: ", dd_threshold * 100, "%"),
         x = NULL, y = NULL) +
    theme(panel.grid   = element_blank(),
          axis.text.x  = element_text(angle = 45, hjust = 1,
                                      size = 9, colour = "grey30"),
          axis.text.y  = element_text(face = "bold", size = 11),
          plot.title   = element_text(face = "bold", size = 16,
                                      colour = "#1A3A6D"))
}

# ── Auto-run guard ─────────────────────────────────────────────────────────────
if (!isTRUE(getOption("knitr.in.progress"))) {
  if (exists("xts_ret") && exists("xts_rel")) {
    plot_sovereign_alpha_tape_v12(
      xts_abs        = xts_ret,
      xts_rel        = xts_rel,
      target_tickers = c("GLD", "IEF", "XLK"),
      dd_threshold   = 0.10
    )
  }
}
