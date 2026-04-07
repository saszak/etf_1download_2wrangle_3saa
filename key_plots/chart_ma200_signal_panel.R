################################################################################
# CHART TEMPLATE : ma200_signal_panel
# NAME           : 200DMA Signal Panel (Multi-Ticker Grid)
# FILE           : key_plots/chart_ma200_signal_panel.R
#
# WHAT IT SHOWS
#   A grid of per-ticker price charts, each with its 200-day moving average.
#   Red shading marks every period when price is BELOW the 200DMA (trend OFF).
#   One panel per ticker, assembled into EQ sleeve and FI sleeve pages.
#
# DESIGN PRINCIPLES
#   • Price rebased to 100 at first date (comparable across tickers)
#   • Amber dashed line = 200DMA  |  Dark blue line = price
#   • Red fill shading = trend-off episodes (weight parked in SGOV)
#   • Minimal grid (minor off); bold ticker title per panel
#   • Assembled with patchwork::wrap_plots(ncol=2)
#
# INVOKE
#   source(here("key_plots/chart_ma200_signal_panel.R"))
#   p <- plot_ma200_signal_panel(trend_signals_tbl, saa_tbl)
#   print(p$eq)   # equity sleeve
#   print(p$fi)   # fixed income sleeve
#
# INPUTS
#   trend_signals_tbl  tibble — cols: date, symbol, adjusted, ma200, signal
#   saa_tbl            tibble — cols: ticker, weight, saa_bucket
#
# USED IN
#   scripts_daa_saa_taa/03_taa_rules.R  →  plot_all_tickers_trend()
#   executive_summary.Rmd  Section 13 (DAA Framework)
################################################################################

library(tidyverse)
library(patchwork)
library(scales)

# ── Single ticker panel ────────────────────────────────────────────────────────
.ma200_single_panel <- function(trend_signals_tbl, ticker_sym,
                                saa_wt = NULL, hurst = NULL, label = NULL) {

  df <- trend_signals_tbl %>%
    filter(symbol == ticker_sym) %>%
    arrange(date)

  if (nrow(df) == 0) return(NULL)

  # Red shading rectangles for below-MA periods
  off_periods <- df %>%
    mutate(grp = cumsum(signal != lag(signal, default = signal[1]))) %>%
    group_by(grp, signal) %>%
    summarise(xmin = min(date), xmax = max(date), .groups = "drop") %>%
    filter(signal == 0)

  # Rebase to 100
  df <- df %>%
    mutate(idx = adjusted / adjusted[1] * 100,
           ma  = ma200   / adjusted[1] * 100)

  pct_on   <- mean(df$signal, na.rm = TRUE)
  wt_label <- if (!is.null(saa_wt)) sprintf("%.0f%%", saa_wt * 100) else ""

  # Hurst label: value + zone emoji
  h_label  <- if (!is.null(hurst) && !is.na(hurst))
    sprintf("  H=%.2f %s", hurst,
            dplyr::case_when(hurst > 0.55 ~ "↑", hurst < 0.45 ~ "↓", TRUE ~ "~"))
  else ""

  title_str <- paste0(
    ticker_sym,
    if (!is.null(label) && nchar(label) > 0) paste0(" \u2014 ", label) else "",
    if (nchar(wt_label) > 0) sprintf("  (%s SAA)", wt_label) else "",
    h_label
  )

  ggplot(df, aes(date)) +
    geom_rect(data        = off_periods,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE,
              fill = "#ef4444", alpha = 0.12) +
    geom_line(aes(y = ma),  colour = "#f59e0b", linewidth = 0.65, linetype = "dashed") +
    geom_line(aes(y = idx), colour = "#1d3461", linewidth = 0.85) +
    annotate("text",
             x     = max(df$date),
             y     = Inf,
             label = sprintf("%.0f%% above MA", pct_on * 100),
             hjust = 1.05, vjust = 1.5,
             size  = 2.8, colour = "#374151") +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    scale_y_continuous(labels = number_format(accuracy = 1)) +
    labs(title    = title_str,
         subtitle = NULL,
         x = NULL, y = NULL) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(),
          plot.title       = element_text(face = "bold", size = 10))
}

# ── Full grid assembler ────────────────────────────────────────────────────────
plot_ma200_signal_panel <- function(trend_signals_tbl, saa_tbl, tqs = NULL) {
  # tqs: optional trend_quality_scores tibble from 06_trend_quality.R
  #      if supplied, Hurst exponent is shown in each panel title

  eq_tickers <- saa_tbl %>% filter(saa_bucket == "EQ") %>%
    arrange(desc(weight)) %>% pull(ticker)
  fi_tickers <- saa_tbl %>% filter(saa_bucket != "EQ") %>%
    arrange(desc(weight)) %>% pull(ticker)

  .make_grid <- function(tickers, sleeve_title) {
    plots <- map(tickers, function(tk) {
      wt <- saa_tbl$weight[saa_tbl$ticker == tk]
      h  <- if (!is.null(tqs))
              tqs$hurst[tqs$ticker == tk][1]
            else NULL
      .ma200_single_panel(trend_signals_tbl, tk, saa_wt = wt, hurst = h)
    }) %>% compact()

    if (length(plots) == 0) return(NULL)

    wrap_plots(plots, ncol = 2) +
      plot_annotation(
        title    = sprintf("%s — 200DMA Trend Signals", sleeve_title),
        subtitle = "Blue = price (rebased 100)  |  Amber dashed = 200DMA  |  Red = trend OFF (parked in SGOV)  |  % = days above MA  |  H = Hurst (↑>0.55 trending  ~=random  ↓<0.45 mean-rev)",
        theme    = theme(
          plot.title    = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(colour = "grey50", size = 9)
        )
      )
  }

  list(
    eq = .make_grid(eq_tickers, "Equity Sleeve"),
    fi = .make_grid(fi_tickers, "Fixed Income Sleeve")
  )
}

# ── Quick-run example (guarded) ───────────────────────────────────────────────
if (FALSE) {
  library(here)
  source(here("project_tree.R"))
  source(here("scripts_daa_saa_taa/01_saa_baseline.R"))
  ts <- readRDS(here("02_data_processed/trend_signals.rds"))
  p  <- plot_ma200_signal_panel(ts, SAA_60_40)
  print(p$eq)
  print(p$fi)
}
