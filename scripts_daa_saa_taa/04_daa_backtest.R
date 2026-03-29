################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : scripts_daa_saa_taa/04_daa_backtest.R
# Purpose : Walk-forward simulation comparing three portfolios:
#             (A) Passive 60/40 — SPY + AGG
#             (B) SAA 60/40     — JB DAA 13-ticker static
#             (C) DAA           — SAA 60/40 + regime-driven TAA tilts
#
# DEPENDS ON
#   01_saa_baseline.R   (saa_returns, benchmark_returns, SAA_60_40)
#   03_taa_rules.R      (taa_weights_history)
#
# OUTPUTS
#   daa_returns         xts    — daily DAA portfolio returns
#   backtest_summary    tibble — Ann Return / Vol / MaxDD / IR / Calmar for A/B/C
#   p_backtest_wealth          — cumulative wealth chart (3 lines)
#   p_backtest_dd              — drawdown chart (3 lines)
#   p_regime_perf              — bar chart: excess return vs passive per regime
#
# FUNCTIONS
#   calc_daa_returns(taa_weights_history, xts_ret)
#   backtest_stats(ret_xts, label)
#   plot_backtest_wealth(ret_list, rt)
#   plot_backtest_dd(ret_list, rt)
#   plot_regime_excess(ret_list, rt)
################################################################################

library(tidyverse)
library(xts)
library(PerformanceAnalytics)
library(scales)
library(here)

if (!exists("project_tree"))         source(here("project_tree.R"))
if (!exists("etf_metadata"))         source(here(project_tree$scripts$init))
if (!exists("xts_ret"))              source(here(project_tree$scripts$wrangle))
if (!exists("build_regime_table"))   source(here("scripts_spy_dd_regime/spy_dd_regime.R"))
if (!exists("rt") || !is.data.frame(rt))
  rt <- build_regime_table(xts_ret[, "SPY"])
if (!exists("SAA_60_40"))            source(here("scripts_daa_saa_taa/01_saa_baseline.R"))
if (!exists("daa_class_tbl")) {
  daa_path <- here("02_data_processed/daa_class_tbl.rds")
  if (file.exists(daa_path)) daa_class_tbl <- readRDS(daa_path)
  else source(here("scripts_daa_saa_taa/02_daa_classifier.R"))
}
if (!exists("taa_weights_history")) {
  taa_path <- here("02_data_processed/taa_weights_history.rds")
  if (file.exists(taa_path)) taa_weights_history <- readRDS(taa_path)
  else source(here("scripts_daa_saa_taa/03_taa_rules.R"))
}

# ==============================================================================
# 1. DAA RETURN CALCULATOR
# ==============================================================================

#' calc_daa_returns
#'
#' For each trading day, look up that day's regime-driven weight vector from
#' taa_weights_history and compute the 1-day portfolio return.
#' Weights are applied at start of day (prior close weights, as if rebalanced
#' at previous day's close).
#'
#' @param taa_hist  tibble from build_taa_history()  — date / ticker / taa_weight
#' @param xts_ret   xts daily return matrix
#' @return          xts daily returns, named "DAA"
calc_daa_returns <- function(taa_hist, xts_ret) {
  all_dates <- sort(unique(taa_hist$date))
  # Restrict to dates present in xts_ret
  valid_dates <- all_dates[all_dates %in% index(xts_ret)]

  daily_ret <- vapply(valid_dates, function(d) {
    w_row <- taa_hist %>%
      filter(date == d) %>%
      select(ticker, taa_weight)

    available <- intersect(w_row$ticker, colnames(xts_ret))
    if (length(available) == 0) return(NA_real_)

    w <- w_row$taa_weight[match(available, w_row$ticker)]
    w <- w / sum(w, na.rm = TRUE)    # re-normalise after subsetting

    ret_day <- as.numeric(xts_ret[as.character(d), available])
    if (any(is.na(ret_day))) {
      # Fill missing with 0 (security not trading that day)
      ret_day[is.na(ret_day)] <- 0
    }
    sum(w * ret_day)
  }, numeric(1))

  daa_xts <- xts(daily_ret, order.by = valid_dates)
  colnames(daa_xts) <- "DAA"
  daa_xts
}

# ==============================================================================
# 2. PERFORMANCE STATISTICS
# ==============================================================================

#' backtest_stats
#'
#' Annualised performance stats for a single daily-return xts.
#'
#' @param ret_xts  xts column
#' @param label    character label
#' @return         named tibble row
backtest_stats <- function(ret_xts, label) {
  r   <- as.numeric(ret_xts[!is.na(ret_xts)])
  n   <- length(r)
  ann_ret <- prod(1 + r)^(252 / n) - 1
  ann_vol <- sd(r) * sqrt(252)
  mdd     <- as.numeric(maxDrawdown(ret_xts))
  ir      <- ann_ret / ann_vol
  calmar  <- ann_ret / mdd

  tibble(
    portfolio  = label,
    ann_return = ann_ret,
    ann_vol    = ann_vol,
    max_dd     = mdd,
    ir         = ir,
    calmar     = calmar,
    n_days     = n
  )
}

# ==============================================================================
# 3. PLOT FUNCTIONS
# ==============================================================================

#' plot_backtest_wealth
#'
#' Three-line cumulative wealth chart: DAA / SAA / Passive.
#' Regime shading from rt.
plot_backtest_wealth <- function(ret_list, rt) {
  combined <- Reduce(function(a, b) merge(a, b, join = "inner"), ret_list)

  wealth_df <- as.data.frame(combined) %>%
    rownames_to_column("date") %>%
    mutate(date = as.Date(date)) %>%
    pivot_longer(-date, names_to = "portfolio", values_to = "ret") %>%
    group_by(portfolio) %>%
    arrange(date) %>%
    mutate(wealth = cumprod(1 + ret)) %>%
    ungroup()

  regime_rect <- rt %>%
    filter(regime %in% c("Fall", "Recovery")) %>%
    mutate(fill  = if_else(regime == "Fall", "#f87171", "#86efac"),
           alpha = 0.12)

  port_colours <- c(
    "DAA"           = "#7c3aed",   # purple
    "SAA_60_40"     = "#3b82f6",   # blue
    "Passive_60_40" = "#9ca3af"    # grey
  )
  port_labels <- c(
    "DAA"           = "DAA — regime-driven tilts",
    "SAA_60_40"     = "SAA 60/40 — JB universe (static)",
    "Passive_60_40" = "Passive 60/40 — SPY / IEF"
  )

  # End-of-series labels — include annualised return
  last_points <- wealth_df %>%
    group_by(portfolio) %>%
    slice_max(date, n = 1) %>%
    left_join(
      wealth_df %>%
        group_by(portfolio) %>%
        summarise(
          n_days  = n(),
          ann_ret = last(wealth)^(252 / n()) - 1,
          .groups = "drop"
        ),
      by = "portfolio"
    ) %>%
    mutate(end_label = sprintf("%s\n%+.1f%% p.a.",
                               recode(portfolio, !!!port_labels),
                               ann_ret * 100)) %>%
    ungroup()

  ggplot(wealth_df, aes(date, wealth, colour = portfolio)) +
    geom_rect(data = regime_rect,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = regime_rect$fill, inherit.aes = FALSE, alpha = 0.12) +
    geom_line(linewidth = 0.9) +
    geom_text(
      data     = last_points,
      aes(label = end_label),
      hjust = 0, nudge_x = 60, size = 3, fontface = "bold", lineheight = 0.85
    ) +
    scale_colour_manual(values = port_colours, guide = "none") +
    scale_y_continuous(
      labels = scales::dollar_format(prefix = "$"),
      limits = c(0.7, NA),
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    coord_cartesian(clip = "off") +
    labs(
      title    = "DAA vs SAA vs Passive — Cumulative Wealth ($1 Invested)",
      subtitle = "Red = Fall  |  Green = Recovery  |  Purple = regime-tilted DAA",
      x = NULL, y = "Wealth ($)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.margin      = margin(5, 160, 5, 5),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 13)
    )
}

#' plot_backtest_dd
#'
#' Drawdown chart for all three portfolios.
plot_backtest_dd <- function(ret_list, rt) {
  combined <- Reduce(function(a, b) merge(a, b, join = "inner"), ret_list)

  dd_df <- as.data.frame(combined) %>%
    rownames_to_column("date") %>%
    mutate(date = as.Date(date)) %>%
    pivot_longer(-date, names_to = "portfolio", values_to = "ret") %>%
    group_by(portfolio) %>%
    arrange(date) %>%
    mutate(
      wealth   = cumprod(1 + ret),
      peak     = cummax(wealth),
      drawdown = wealth / peak - 1
    ) %>%
    ungroup()

  regime_rect <- rt %>%
    filter(regime == "Fall") %>%
    mutate(fill = "#f87171")

  port_colours <- c("DAA" = "#7c3aed", "SAA_60_40" = "#3b82f6", "Passive_60_40" = "#9ca3af")

  ggplot(dd_df, aes(date, drawdown, colour = portfolio)) +
    geom_rect(data = regime_rect,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = 0),
              fill = "#f87171", inherit.aes = FALSE, alpha = 0.08) +
    geom_line(linewidth = 0.7) +
    geom_hline(yintercept = 0, colour = "#374151", linewidth = 0.3) +
    scale_colour_manual(
      values = port_colours,
      labels = c("DAA" = "DAA", "SAA_60_40" = "SAA 60/40", "Passive_60_40" = "Passive")
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      title    = "Drawdown Comparison — DAA / SAA / Passive",
      subtitle = "Red shading = Fall regime episodes",
      x = NULL, y = "Drawdown (%)", colour = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 13),
      legend.position  = "top"
    )
}

#' plot_regime_excess
#'
#' Bar chart: excess return of DAA vs Passive 60/40 by regime.
plot_regime_excess <- function(ret_list, rt) {
  combined <- Reduce(function(a, b) merge(a, b, join = "inner"), ret_list)

  daily_df <- as.data.frame(combined) %>%
    rownames_to_column("date") %>%
    mutate(date = as.Date(date))

  # Attach regime label to each day
  regime_daily <- rt %>%
    rowwise() %>%
    mutate(date = list(seq(as.Date(xmin), as.Date(xmax), by = "day"))) %>%
    unnest(date) %>%
    select(date, regime)

  df <- daily_df %>%
    left_join(regime_daily, by = "date") %>%
    filter(!is.na(regime)) %>%
    pivot_longer(cols = -c(date, regime), names_to = "portfolio", values_to = "ret")

  # Cumulative return per portfolio × regime
  regime_cum <- df %>%
    group_by(regime, portfolio) %>%
    summarise(cum_ret = prod(1 + ret, na.rm = TRUE) - 1, .groups = "drop")

  # Excess vs Passive
  passive_ret <- regime_cum %>%
    filter(portfolio == "Passive_60_40") %>%
    rename(passive = cum_ret) %>%
    select(regime, passive)

  excess <- regime_cum %>%
    filter(portfolio != "Passive_60_40") %>%
    left_join(passive_ret, by = "regime") %>%
    mutate(excess_ret = cum_ret - passive,
           regime = factor(regime, levels = c("Fall", "Consolidation", "Recovery")))

  ggplot(excess, aes(regime, excess_ret * 100, fill = portfolio)) +
    geom_col(position = "dodge", width = 0.6, alpha = 0.85) +
    geom_hline(yintercept = 0, colour = "#374151", linewidth = 0.4) +
    geom_text(aes(label = sprintf("%+.1f%%", excess_ret * 100),
                  vjust = if_else(excess_ret >= 0, -0.4, 1.2)),
              position = position_dodge(0.6), size = 3.2, fontface = "bold") +
    scale_fill_manual(
      values = c("DAA" = "#7c3aed", "SAA_60_40" = "#3b82f6"),
      labels = c("DAA" = "DAA vs Passive", "SAA_60_40" = "SAA vs Passive")
    ) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title    = "Excess Return vs Passive 60/40 — by Regime",
      subtitle = "Cumulative excess return accumulated within each regime type",
      x = NULL, y = "Excess Return (pp)", fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold", size = 13),
      legend.position = "top"
    )
}

# ==============================================================================
# 4. RUN BACKTEST
# ==============================================================================
message("📐 Computing DAA daily returns...")
daa_returns <- calc_daa_returns(taa_weights_history, xts_ret)

# Align all three series to common date range
common_start <- max(
  index(daa_returns)[1],
  index(saa_returns)[1],
  index(benchmark_returns)[1]
)
daa_xts <- daa_returns[paste0(common_start, "/")]
saa_xts <- saa_returns[paste0(common_start, "/")]
bmk_xts <- benchmark_returns[paste0(common_start, "/")]

# Combined list for plotting
ret_list <- list(
  DAA           = daa_xts,
  SAA_60_40     = saa_xts,
  Passive_60_40 = bmk_xts
)

# Performance stats table
message("📊 Backtest performance summary:")
backtest_summary <- bind_rows(
  backtest_stats(daa_xts, "DAA"),
  backtest_stats(saa_xts, "SAA 60/40"),
  backtest_stats(bmk_xts, "Passive 60/40")
)

backtest_summary %>%
  mutate(
    ann_return = sprintf("%+.1f%%", ann_return * 100),
    ann_vol    = sprintf("%.1f%%",  ann_vol    * 100),
    max_dd     = sprintf("%.1f%%",  max_dd     * 100),
    ir         = sprintf("%.2f",    ir),
    calmar     = sprintf("%.2f",    calmar)
  ) %>%
  select(-n_days) %>%
  print()

write_rds(daa_returns,      here("02_data_processed/daa_returns.rds"))
write_rds(backtest_summary, here("02_data_processed/daa_backtest_summary.rds"))
message("💾 DAA returns and summary saved to 02_data_processed/")

# ==============================================================================
# 5. PLOTS (guarded)
# ==============================================================================
if (!isTRUE(getOption("knitr.in.progress"))) {
  p_backtest_wealth <- plot_backtest_wealth(ret_list, rt)
  p_backtest_dd     <- plot_backtest_dd(ret_list, rt)
  p_regime_perf     <- plot_regime_excess(ret_list, rt)

  print(p_backtest_wealth)
  print(p_backtest_dd)
  print(p_regime_perf)
}

message("✅ Stage DAA-04 complete: walk-forward backtest finished.")
