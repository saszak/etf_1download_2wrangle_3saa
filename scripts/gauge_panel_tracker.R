################################################################################
# GAUGE PANEL TRACKER
# FILE : scripts/gauge_panel_tracker.R
#
# Logs daily build_gauge_vectors() snapshots to a running panel dataset.
# Each run appends one row per ticker; deduplicates by (ticker, as_of).
#
# FUNCTIONS
#   log_gauge_snapshot(tickers, xts_ret, panel_path)
#     → appends today's fingerprint to panel RDS; returns full panel invisibly
#
#   load_gauge_panel(panel_path)
#     → reads the full panel; returns tibble sorted by ticker + as_of
#
#   plot_gauge_norm_pos(panel, tickers)
#     → time series of norm_pos (0=52W lo, 1=52W hi) per ticker
#
#   plot_gauge_state_history(panel, tickers)
#     → tile chart: date × ticker, colour = market state
#
# INVOKE
#   source(here("key_plots/chart_ma_range.R"))   # for build_gauge_vectors()
#   source(here("scripts/gauge_panel_tracker.R"))
#
#   log_gauge_snapshot(tk, xts_ret)              # append today
#   panel <- load_gauge_panel()                  # read full history
#   plot_gauge_norm_pos(panel, tk)
#   plot_gauge_state_history(panel, tk)
#
# STORAGE
#   02_data_processed/gauge_panel.rds
################################################################################

library(tidyverse)
library(here)

GAUGE_PANEL_PATH <- here("02_data_processed/gauge_panel.rds")

# ── log_gauge_snapshot ────────────────────────────────────────────────────────
log_gauge_snapshot <- function(
    tickers,
    xts_ret,
    panel_path = GAUGE_PANEL_PATH
) {
  if (!exists("build_gauge_vectors", mode = "function"))
    source(here("key_plots/chart_ma_range.R"))

  snap <- build_gauge_vectors(tickers, xts_ret)

  # Load existing panel or start fresh
  if (file.exists(panel_path)) {
    existing <- readRDS(panel_path)
    # Deduplicate: drop any existing rows for same (ticker, as_of)
    existing <- existing %>%
      anti_join(snap %>% select(ticker, as_of), by = c("ticker", "as_of"))
    panel <- bind_rows(existing, snap) %>% arrange(ticker, as_of)
  } else {
    panel <- snap %>% arrange(ticker, as_of)
  }

  saveRDS(panel, panel_path)
  message(sprintf("Logged %d tickers for %s  |  Panel: %d rows  |  %s",
                  nrow(snap), format(snap$as_of[1]),
                  nrow(panel), panel_path))
  invisible(panel)
}

# ── load_gauge_panel ──────────────────────────────────────────────────────────
load_gauge_panel <- function(panel_path = GAUGE_PANEL_PATH) {
  if (!file.exists(panel_path))
    stop("No panel found at: ", panel_path, "\nRun log_gauge_snapshot() first.")
  readRDS(panel_path) %>% arrange(ticker, as_of)
}

# ── plot_gauge_norm_pos ───────────────────────────────────────────────────────
# Time series of normalised 52W position (0=low, 1=high) per ticker.
# Dashed line at 0.5 = mid-range. State colour on the final point.
plot_gauge_norm_pos <- function(
    panel,
    tickers = NULL
) {
  STATE_COL <- c(
    Bear       = "#7f1d1d",
    Trending   = "#16a34a",
    Stretched  = "#38bdf8",
    Pullback   = "#fbbf24",
    Stressed   = "#f97316",
    Correction = "#dc2626"
  )

  if (!is.null(tickers)) panel <- panel %>% filter(ticker %in% tickers)

  last_pts <- panel %>% group_by(ticker) %>% slice_tail(n = 1) %>% ungroup()

  ggplot(panel, aes(x = as_of, y = norm_pos, group = ticker)) +
    geom_hline(yintercept = 0.5, linetype = "dashed",
               colour = "#9ca3af", linewidth = 0.4) +
    geom_hline(yintercept = c(0, 1), colour = "#e5e7eb", linewidth = 0.3) +
    geom_line(colour = "#1d3461", linewidth = 0.7, alpha = 0.8) +
    geom_point(
      data = last_pts,
      aes(colour = I(STATE_COL[state])),
      size = 3, shape = 18
    ) +
    geom_text(
      data = last_pts,
      aes(label = sprintf("%.2f", norm_pos)),
      hjust = -0.3, size = 2.8, fontface = "bold",
      colour = "#374151"
    ) +
    facet_wrap(~ ticker, ncol = 3) +
    scale_y_continuous(
      limits = c(-0.05, 1.10),
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = c("52W Lo", "25%", "Mid", "75%", "52W Hi")
    ) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
    labs(
      title    = "52W Normalised Position History",
      subtitle = "0 = at 52W Low  |  1 = at 52W High  |  Diamond = current state",
      x = NULL, y = "Norm. position in 52W range"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(face = "bold"),
      plot.subtitle     = element_text(size = 9, colour = "#555"),
      axis.text.x       = element_text(angle = 45, hjust = 1, size = 8),
      strip.text        = element_text(face = "bold", size = 10)
    )
}

# ── plot_gauge_state_history ──────────────────────────────────────────────────
# Tile chart: rows = tickers, columns = dates, fill = market state.
# Shows regime transitions at a glance across all tracked tickers.
plot_gauge_state_history <- function(
    panel,
    tickers = NULL
) {
  STATE_COL <- c(
    Bear       = "#7f1d1d",
    Trending   = "#16a34a",
    Stretched  = "#38bdf8",
    Pullback   = "#fbbf24",
    Stressed   = "#f97316",
    Correction = "#dc2626"
  )

  if (!is.null(tickers)) panel <- panel %>% filter(ticker %in% tickers)

  # Order tickers by most recent norm_pos (most overbought at top)
  tk_order <- panel %>%
    group_by(ticker) %>%
    slice_tail(n = 1) %>%
    arrange(desc(norm_pos)) %>%
    pull(ticker)

  panel <- panel %>%
    mutate(
      ticker = factor(ticker, levels = tk_order),
      state  = factor(state, levels = names(STATE_COL))
    )

  ggplot(panel, aes(x = as_of, y = ticker, fill = state)) +
    geom_tile(colour = "white", linewidth = 0.2) +
    scale_fill_manual(
      values = STATE_COL,
      name   = "State",
      drop   = FALSE
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
    labs(
      title    = "Market State History",
      subtitle = "Each cell = one logged snapshot  |  Tickers ordered by current norm_pos (top = most overbought)",
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid   = element_blank(),
      plot.title   = element_text(face = "bold"),
      plot.subtitle = element_text(size = 9, colour = "#555"),
      axis.text.x  = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y  = element_text(face = "bold", size = 10),
      legend.position = "bottom",
      legend.key.size = unit(0.5, "cm")
    )
}

# ── Quick-run (guarded) ───────────────────────────────────────────────────────
if (!isTRUE(getOption("knitr.in.progress"))) {
  if (!exists("xts_ret"))
    xts_ret <- readRDS(here("02_data_processed/xts_ret_returns.rds"))

  tk <- c("SPY", "QQQ", "URTH", "AGG", "XLK", "SMH", "GLD", "IEF")

  log_gauge_snapshot(tk, xts_ret)

  panel <- load_gauge_panel()
  print(panel)

  if (length(unique(panel$as_of)) >= 2) {
    print(plot_gauge_norm_pos(panel, tk))
    print(plot_gauge_state_history(panel, tk))
  } else {
    message("Only one snapshot so far — run again tomorrow for time-series plots.")
  }
}
