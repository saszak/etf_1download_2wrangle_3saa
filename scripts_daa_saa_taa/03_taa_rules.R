################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : scripts_daa_saa_taa/03_taa_rules.R
# Purpose : 200-Day MA Trend Filter TAA
#
# RULE (Faber 2007 / tested approach)
#   For each SAA ticker on each trading day:
#     signal = 1  if price > 200-day MA  →  hold at full SAA weight
#     signal = 0  if price ≤ 200-day MA  →  park weight in SGOV (cash)
#   SGOV always holds at minimum its SAA weight plus all parked allocations.
#
# WHY THIS WORKS (vs regime rotation)
#   ✓ No look-ahead bias — signal is observable same-day
#   ✓ No regime lag — fires as soon as a ticker breaks its MA
#   ✓ Ticker-level, not portfolio-level — only the broken ticker parks, not all
#   ✓ Documented empirically (Faber 2007, Antonacci 2012)
#
# OUTPUTS
#   trend_signals        tibble — daily signal (0/1) per SAA ticker
#   taa_weights_history  tibble — date × ticker × taa_weight
#   p_signal_heatmap     ggplot — green/red grid: when each ticker was above MA
#   p_cash_park_level    ggplot — % parked in SGOV over time + regime shading
#   p_ticker_trend[]     list   — per-ticker price + MA + shaded-off periods
#
# DEPENDS ON
#   01_saa_baseline.R   (SAA_60_40)
#   01b_technical_signals.R  (ma_table — adjusted prices + ma200)
################################################################################

library(tidyverse)
library(xts)
library(patchwork)
library(scales)
library(here)

if (!exists("project_tree"))   source(here("project_tree.R"))
if (!exists("etf_metadata"))   source(here(project_tree$scripts$init))
if (!exists("xts_ret"))        source(here(project_tree$scripts$wrangle))
if (!exists("build_regime_table"))
  source(here("scripts_spy_dd_regime/spy_dd_regime.R"))
if (!exists("rt") || !is.data.frame(rt))
  rt <- build_regime_table(xts_ret[, "SPY"])
if (!exists("SAA_60_40"))
  source(here("scripts_daa_saa_taa/01_saa_baseline.R"))
if (!exists("ma_table"))
  source(here(project_tree$scripts$signals))

# ==============================================================================
# 1. BUILD DAILY 200DMA SIGNALS
# ==============================================================================
# signal_t = 1  →  price_t > ma200_t  →  hold ticker at SAA weight
# signal_t = 0  →  price_t ≤ ma200_t  →  park weight in SGOV

saa_tickers <- SAA_60_40$ticker

trend_signals <- ma_table %>%
  filter(symbol %in% saa_tickers,
         !is.na(ma200),
         !is.na(adjusted)) %>%
  select(date, symbol, adjusted, ma200) %>%
  mutate(
    signal      = as.integer(adjusted > ma200),
    pct_vs_ma   = (adjusted / ma200 - 1),          # how far above/below
    date        = as.Date(date)
  ) %>%
  arrange(symbol, date)

message(sprintf("📡 Trend signals built: %d tickers × %d trading days",
                n_distinct(trend_signals$symbol),
                n_distinct(trend_signals$date)))

# ==============================================================================
# 2. BUILD DAILY TAA WEIGHTS
# ==============================================================================
# For every date:
#   taa_weight[ticker] = saa_weight[ticker]  if signal = 1
#                      = 0                   if signal = 0
#   taa_weight[SGOV]   = saa_weight[SGOV] + sum of all parked weights

build_taa_history <- function(trend_signals, saa_tbl = SAA_60_40) {

  saa_w <- saa_tbl %>% select(ticker, saa_weight = weight)

  trend_signals %>%
    left_join(saa_w, by = c("symbol" = "ticker")) %>%
    group_by(date) %>%
    mutate(
      # Weight if above MA; 0 if below → gets parked
      live_weight   = saa_weight * signal,
      parked_weight = saa_weight * (1L - signal)
    ) %>%
    mutate(
      total_parked = sum(parked_weight, na.rm = TRUE),
      # SGOV absorbs all parked weight (on top of its own SAA weight)
      taa_weight = case_when(
        symbol == "SGOV" ~ saa_weight + total_parked,
        TRUE             ~ live_weight
      )
    ) %>%
    ungroup() %>%
    mutate(
      tilt           = taa_weight - saa_weight,
      role_activated = case_when(
        symbol == "SGOV" & total_parked > 0.001 ~ "Cash-Park",
        signal == 0L                             ~ "Parked→SGOV",
        TRUE                                     ~ "Trend-On"
      )
    ) %>%
    select(date, ticker = symbol, signal, pct_vs_ma,
           saa_weight, taa_weight, tilt, role_activated)
}

message("⚙️  Building TAA weight history (200DMA trend filter)...")
taa_weights_history <- build_taa_history(trend_signals)

# ==============================================================================
# 3. CONSOLE SUMMARY
# ==============================================================================

signal_summary <- trend_signals %>%
  group_by(symbol) %>%
  summarise(
    pct_above_ma  = mean(signal, na.rm = TRUE),
    n_days        = n(),
    avg_pct_vs_ma = mean(pct_vs_ma, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(SAA_60_40 %>% select(ticker, weight, saa_bucket), by = c("symbol" = "ticker")) %>%
  arrange(saa_bucket, desc(pct_above_ma))

message("\n📋 Trend Signal Summary (% of days above 200DMA):")
signal_summary %>%
  mutate(pct_above_ma  = sprintf("%.0f%%", pct_above_ma * 100),
         avg_pct_vs_ma = sprintf("%+.1f%%", avg_pct_vs_ma * 100),
         weight        = sprintf("%.0f%%", weight * 100)) %>%
  select(saa_bucket, symbol, weight, pct_above_ma, avg_pct_vs_ma, n_days) %>%
  print(n = 20)

# ==============================================================================
# 4. PLOT A — SIGNAL HEATMAP
#    Green = ticker above 200DMA (trend on)
#    Red   = ticker below 200DMA (parked in SGOV)
#    Width of red = duration of trend-off episode
# ==============================================================================

plot_signal_heatmap <- function(trend_signals, saa_tbl = SAA_60_40, rt) {

  # Order tickers: EQ first (by weight desc), then FI
  ticker_order <- saa_tbl %>%
    arrange(saa_bucket, desc(weight)) %>%
    pull(ticker)

  df <- trend_signals %>%
    filter(symbol %in% ticker_order) %>%
    mutate(symbol = factor(symbol, levels = rev(ticker_order)))

  regime_rect <- rt %>%
    filter(regime == "Fall") %>%
    mutate(xmin = as.Date(xmin), xmax = as.Date(xmax))

  ggplot(df, aes(date, symbol, fill = factor(signal))) +
    # Fall regime background
    geom_rect(data = regime_rect,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = "#fca5a5", alpha = 0.20) +
    geom_tile(colour = NA) +
    scale_fill_manual(
      values = c("0" = "#ef4444", "1" = "#22c55e"),
      labels = c("0" = "Below 200DMA → Parked in SGOV",
                 "1" = "Above 200DMA → Hold at SAA weight"),
      name   = NULL
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                 expand = expansion(mult = 0.005)) +
    labs(
      title    = "200-Day MA Trend Signal — SAA Universe",
      subtitle = "Green = trend ON (full SAA weight)  |  Red = trend OFF (weight parked in SGOV)  |  Pink = SPY Fall regime",
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid       = element_blank(),
      legend.position  = "top",
      plot.title       = element_text(face = "bold", size = 13),
      axis.text.y      = element_text(face = "bold", size = 10)
    )
}

# ==============================================================================
# 5. PLOT B — CASH PARK LEVEL
#    Shows how much of the portfolio is sitting in SGOV on any day.
#    Regime-shaded so you can see: does the filter fire in Fall?
# ==============================================================================

plot_cash_park_level <- function(taa_weights_history, rt) {

  cash_df <- taa_weights_history %>%
    group_by(date) %>%
    summarise(
      sgov_taa = taa_weights_history$taa_weight[taa_weights_history$date == date[1] &
                   taa_weights_history$ticker == "SGOV"],
      sgov_saa = taa_weights_history$saa_weight[taa_weights_history$date == date[1] &
                   taa_weights_history$ticker == "SGOV"],
      .groups = "drop"
    )

  # Simpler version: compute directly
  cash_df <- taa_weights_history %>%
    filter(ticker == "SGOV") %>%
    select(date, sgov_taa = taa_weight, sgov_saa = saa_weight) %>%
    mutate(parked = sgov_taa - sgov_saa)

  regime_rect <- rt %>%
    filter(regime %in% c("Fall", "Recovery")) %>%
    mutate(fill = if_else(regime == "Fall", "#fca5a5", "#bbf7d0"),
           xmin = as.Date(xmin), xmax = as.Date(xmax))

  ggplot(cash_df, aes(date)) +
    geom_rect(data = regime_rect,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
              inherit.aes = FALSE, alpha = 0.18) +
    scale_fill_identity() +
    geom_ribbon(aes(ymin = sgov_saa, ymax = sgov_taa),
                fill = "#6b7280", alpha = 0.35) +
    geom_line(aes(y = sgov_taa), colour = "#1e40af", linewidth = 0.8) +
    geom_hline(aes(yintercept = sgov_saa[1]),
               linetype = "dashed", colour = "#6b7280", linewidth = 0.5) +
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.05))) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    annotate("text", x = min(cash_df$date), y = cash_df$sgov_saa[1] + 0.003,
             label = "SAA baseline SGOV (4%)", hjust = 0, size = 3,
             colour = "#6b7280") +
    labs(
      title    = "SGOV Allocation — How Much is Parked at Any Point?",
      subtitle = "Blue = total SGOV weight  |  Grey fill = parked from trend-off tickers  |  Dashed = SAA baseline 4%\nPink = Fall regime  |  Green = Recovery regime",
      x = NULL, y = "SGOV Weight (%)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 13)
    )
}

# ==============================================================================
# 6. PLOT C — PER-TICKER: PRICE + 200DMA + SHADED OFF PERIODS
#    One panel per SAA ticker — price line, MA line, red shading when below MA.
#    The most intuitive view: you see exactly WHEN and HOW FAR below MA.
# ==============================================================================

plot_ticker_trend <- function(trend_signals, ticker_sym, saa_wt = NULL) {

  df <- trend_signals %>%
    filter(symbol == ticker_sym) %>%
    arrange(date)

  if (nrow(df) == 0) {
    message("plot_ticker_trend: no data for ", ticker_sym); return(NULL)
  }

  # Build "below MA" shading rectangles
  off_periods <- df %>%
    mutate(grp = cumsum(signal != lag(signal, default = signal[1]))) %>%
    group_by(grp, signal) %>%
    summarise(xmin = min(date), xmax = max(date), .groups = "drop") %>%
    filter(signal == 0)

  # Normalise price to 100 at start
  df <- df %>% mutate(idx = adjusted / adjusted[1] * 100,
                      ma  = ma200   / adjusted[1] * 100)

  wt_label <- if (!is.null(saa_wt)) sprintf(" (SAA wt: %.0f%%)", saa_wt * 100) else ""

  ggplot(df, aes(date)) +
    geom_rect(data = off_periods,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = "#ef4444", alpha = 0.12) +
    geom_line(aes(y = ma),  colour = "#f59e0b", linewidth = 0.7,
              linetype = "dashed") +
    geom_line(aes(y = idx), colour = "#1d3461", linewidth = 0.9) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(labels = number_format(accuracy = 1)) +
    labs(
      title    = paste0(ticker_sym, wt_label),
      subtitle = "Blue = price (rebased 100)  |  Amber dashed = 200DMA  |  Red shading = trend OFF (weight parked)",
      x = NULL, y = "Index (100 = start)"
    ) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(),
          plot.title       = element_text(face = "bold", size = 11))
}

plot_all_tickers_trend <- function(trend_signals, saa_tbl = SAA_60_40) {
  plots <- map(saa_tbl$ticker, function(tk) {
    wt <- saa_tbl$weight[saa_tbl$ticker == tk]
    plot_ticker_trend(trend_signals, tk, saa_wt = wt)
  }) %>%
    compact()

  # EQ sleeve panel
  eq_tks  <- saa_tbl %>% filter(saa_bucket == "EQ") %>% pull(ticker)
  fi_tks  <- saa_tbl %>% filter(saa_bucket != "EQ") %>% pull(ticker)

  eq_plots <- plots[saa_tbl$ticker %in% eq_tks]
  fi_plots <- plots[saa_tbl$ticker %in% fi_tks]

  p_eq <- wrap_plots(eq_plots, ncol = 2) +
    plot_annotation(title    = "Equity Sleeve — 200DMA Trend Signals",
                    subtitle = "Red shading = weight parked in SGOV",
                    theme    = theme(plot.title = element_text(face = "bold", size = 13)))

  p_fi <- wrap_plots(fi_plots, ncol = 2) +
    plot_annotation(title    = "Fixed Income Sleeve — 200DMA Trend Signals",
                    subtitle = "Red shading = weight parked in SGOV",
                    theme    = theme(plot.title = element_text(face = "bold", size = 13)))

  list(eq = p_eq, fi = p_fi)
}

# ==============================================================================
# 7. PLOT D — WEIGHT COMPOSITION OVER TIME
#    Stacked area: what does the portfolio actually look like day-to-day?
# ==============================================================================

plot_taa_composition <- function(taa_weights_history, saa_tbl = SAA_60_40, rt) {

  # Colour: EQ = blue shades, FI = green shades, SGOV = grey
  bucket_map <- saa_tbl %>% select(ticker, saa_bucket, weight) %>%
    arrange(saa_bucket, desc(weight))

  df <- taa_weights_history %>%
    left_join(bucket_map %>% select(ticker, saa_bucket), by = "ticker") %>%
    mutate(ticker = factor(ticker, levels = bucket_map$ticker))

  regime_rect <- rt %>%
    filter(regime == "Fall") %>%
    mutate(xmin = as.Date(xmin), xmax = as.Date(xmax))

  ggplot(df, aes(date, taa_weight, fill = ticker)) +
    geom_rect(data = regime_rect,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = "#fca5a5", alpha = 0.22) +
    geom_area(position = "stack", colour = "white", linewidth = 0.1, alpha = 0.88) +
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.01))) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      title    = "Portfolio Composition — 200DMA Trend Filter",
      subtitle = "SGOV expands when tickers fall below 200DMA  |  Pink = SPY Fall regime",
      x = NULL, y = "Weight", fill = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          plot.title       = element_text(face = "bold", size = 13),
          legend.position  = "right",
          legend.key.size  = unit(0.35, "cm"))
}

# ==============================================================================
# 8. PERSIST
# ==============================================================================
write_rds(taa_weights_history, here("02_data_processed/taa_weights_history.rds"))
write_rds(trend_signals,       here("02_data_processed/trend_signals.rds"))
message("💾 Saved: taa_weights_history, trend_signals")

# ==============================================================================
# 9. PRINT ALL PLOTS (guarded)
# ==============================================================================
if (!isTRUE(getOption("knitr.in.progress"))) {

  p_signal_heatmap  <- plot_signal_heatmap(trend_signals, SAA_60_40, rt)
  p_cash_park_level <- plot_cash_park_level(taa_weights_history, rt)
  p_taa_composition <- plot_taa_composition(taa_weights_history, SAA_60_40, rt)
  p_ticker_trend    <- plot_all_tickers_trend(trend_signals, SAA_60_40)

  print(p_signal_heatmap)
  print(p_cash_park_level)
  print(p_taa_composition)
  print(p_ticker_trend$eq)
  print(p_ticker_trend$fi)
}

message("✅ Stage DAA-03 complete: 200DMA trend filter TAA.")
