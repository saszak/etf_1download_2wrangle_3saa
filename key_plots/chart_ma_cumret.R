################################################################################
# CHART TEMPLATE : ma_cumret
# FILE           : key_plots/chart_ma_cumret.R
#
# FUNCTION
#   plot_ma_cumret(ticker, xts_ret, from_date, ma1, ma2, t_fall)
#   → patchwork: price + MA panel / drawdown panel
#   → y-axis shows cumulative return (wealth index, base 1.0)
#
# SEE ALSO
#   key_plots/chart_ma_price.R  — same chart using actual adjusted price ($)
#
# INVOKE
#   source(here("key_plots/chart_ma_cumret.R"))
#   plot_ma_cumret("URTH", xts_ret)
#   plot_ma_cumret("SPY",  xts_ret, from_date = "2022-01-01", ma1 = 50, ma2 = 200)
################################################################################

library(tidyverse)
library(xts)
library(zoo)
library(patchwork)
library(scales)

plot_ma_cumret <- function(
    ticker,
    xts_ret,
    from_date = "2020-01-01",
    ma1       = 50,
    ma2       = 200,
    t_fall    = 0.10
) {

  if (!ticker %in% colnames(xts_ret))
    stop("ticker '", ticker, "' not found in xts_ret")

  r <- as.numeric(coredata(xts_ret[, ticker]))
  r[is.na(r)] <- 0

  df <- tibble(
    date  = as.Date(index(xts_ret)),
    price = cumprod(1 + r)
  ) %>%
    mutate(
      ma_s    = as.numeric(zoo::rollmean(price, ma1,  align = "right", fill = NA)),
      ma_l    = as.numeric(zoo::rollmean(price, ma2,  align = "right", fill = NA)),
      peak    = cummax(price),
      dd      = (price - peak) / peak,
      below_s = price < ma_s,
      below_l = price < ma_l
    ) %>%
    filter(date >= as.Date(from_date))

  last   <- tail(df, 1)
  vs_s   <- (last$price / last$ma_s  - 1) * 100
  vs_l   <- (last$price / last$ma_l  - 1) * 100
  dd_pct <- last$dd * 100

  # ── Panel 1: Price + MAs ───────────────────────────────────────────────────
  p1 <- ggplot(df, aes(x = date)) +

    geom_ribbon(
      aes(ymin = ifelse(below_l, price, NA_real_),
          ymax = ifelse(below_l, ma_l,  NA_real_)),
      fill = "#ef4444", alpha = 0.15, na.rm = TRUE
    ) +
    geom_ribbon(
      aes(ymin = ifelse(below_s & !below_l, price, NA_real_),
          ymax = ifelse(below_s & !below_l, ma_s,  NA_real_)),
      fill = "#f97316", alpha = 0.15, na.rm = TRUE
    ) +

    geom_hline(yintercept = last$peak * (1 - t_fall),
               linetype = "dashed", colour = "#dc2626", linewidth = 0.55) +
    annotate("text",
             x     = min(df$date) + as.integer(diff(range(df$date)) * 0.02),
             y     = last$peak * (1 - t_fall) * 0.994,
             label = paste0("\u2212", round(t_fall * 100), "% Fall trigger"),
             colour = "#dc2626", size = 2.8, hjust = 0) +

    geom_line(aes(y = ma_l), colour = "#1d3461", linewidth = 0.85,
              linetype = "dashed", na.rm = TRUE) +
    geom_line(aes(y = ma_s), colour = "#f59e0b", linewidth = 0.75,
              linetype = "dashed", na.rm = TRUE) +
    geom_line(aes(y = price), colour = "#1d3461", linewidth = 1.0) +

    annotate("text", x = last$date + as.integer(diff(range(df$date)) * 0.015),
             y = last$price, hjust = 0, size = 2.7, fontface = "bold",
             colour = "#1d3461",
             label = sprintf("%s\n%+.1f%% vs %dDMA\n%+.1f%% vs %dDMA",
                             ticker, vs_s, ma1, vs_l, ma2)) +
    annotate("text", x = last$date + as.integer(diff(range(df$date)) * 0.015),
             y = last$ma_s, hjust = 0, size = 2.4, colour = "#f59e0b",
             label = sprintf("%dDMA", ma1)) +
    annotate("text", x = last$date + as.integer(diff(range(df$date)) * 0.015),
             y = last$ma_l, hjust = 0, size = 2.4, colour = "#1d3461", alpha = 0.65,
             label = sprintf("%dDMA", ma2)) +

    scale_x_date(expand = expansion(mult = c(0, 0.12))) +
    scale_y_continuous(labels = label_number(accuracy = 0.01)) +
    labs(
      title    = paste0(ticker, "  \u2014  Cumulative Return vs ", ma1, "DMA & ", ma2, "DMA"),
      subtitle = sprintf(
        "As of %s  |  vs %dDMA: %+.1f%%  |  vs %dDMA: %+.1f%%  |  DD from peak: %.1f%%",
        format(last$date), ma1, vs_s, ma2, vs_l, dd_pct
      ),
      x = NULL, y = "Wealth index (base 1.0)"
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          plot.title    = element_text(face = "bold"),
          plot.subtitle = element_text(size = 9, colour = "#555"))

  # ── Panel 2: Drawdown ──────────────────────────────────────────────────────
  p2 <- ggplot(df, aes(x = date)) +
    geom_ribbon(aes(ymin = dd, ymax = 0), fill = "#dc2626", alpha = 0.22) +
    geom_line(aes(y = dd), colour = "#dc2626", linewidth = 0.7) +
    geom_hline(yintercept = -t_fall,
               linetype = "dashed", colour = "#7c3aed", linewidth = 0.55) +
    annotate("text",
             x     = min(df$date) + as.integer(diff(range(df$date)) * 0.02),
             y     = -t_fall - diff(range(df$dd, na.rm = TRUE)) * 0.04,
             label = paste0("\u2212", round(t_fall * 100), "% trigger"),
             colour = "#7c3aed", size = 2.6, hjust = 0) +
    annotate("point", x = last$date, y = last$dd,
             colour = "#dc2626", size = 2) +
    annotate("text",
             x = last$date + as.integer(diff(range(df$date)) * 0.015),
             y = last$dd, hjust = 0, size = 2.8, fontface = "bold",
             colour = "#dc2626",
             label = sprintf("%.1f%%", dd_pct)) +
    scale_x_date(expand = expansion(mult = c(0, 0.12))) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(x = NULL, y = "Drawdown") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())

  # ── Combine ────────────────────────────────────────────────────────────────
  p1 / p2 + plot_layout(heights = c(3, 1))
}

# ── Quick-run (guarded) ───────────────────────────────────────────────────────
if (!isTRUE(getOption("knitr.in.progress"))) {
  library(here)
  if (!exists("xts_ret"))
    xts_ret <- readRDS(here("02_data_processed/xts_ret_returns.rds"))

  print(plot_ma_cumret("URTH", xts_ret))
}
