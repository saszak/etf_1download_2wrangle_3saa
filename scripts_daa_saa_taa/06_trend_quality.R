################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : scripts_daa_saa_taa/06_trend_quality.R
# Purpose : Score every ticker on five dimensions of "trendability" — how well
#           does a 200DMA filter actually work for this specific ticker?
#
# FIVE METRICS
#   1. hit_rate_diff    Hit rate above MA minus hit rate below MA (+pp = filter predicts)
#   2. capture_ratio    Cum return in signal=1 days / total cum return (>1 = filter adds)
#   3. whipsaw_rate     % of signal episodes lasting < 20 days (lower = cleaner signal)
#   4. avg_episode_days Mean duration of signal-on AND signal-off episodes (longer = better)
#   5. hurst            Hurst exponent via R/S analysis (>0.5 = trending; <0.5 = mean-rev)
#
# COMPOSITE SCORE
#   trendability_score  0–100, equal-weight of normalised metrics
#   Interpretation:
#     > 70  Strong trend follower — 200DMA filter adds clear value
#     50–70 Moderate — filter helps but with noise
#     < 50  Weak / mean-reverting — filter may hurt more than help
#
# OUTPUTS
#   trend_quality_scores  tibble  — all metrics + composite score per ticker
#   p_trend_scorecard     ggplot  — heatmap: ticker × metric, colour = score
#   p_trend_ranking       ggplot  — horizontal bar: tickers ranked by composite
#   p_trend_hurst         ggplot  — Hurst exponent strip with H=0.5 reference
#
# DEPENDS ON
#   03_taa_rules.R  (trend_signals — date/symbol/adjusted/ma200/signal)
#   01_saa_baseline.R (SAA_60_40)
################################################################################

library(tidyverse)
library(patchwork)
library(scales)
library(here)

if (!exists("project_tree"))   source(here("project_tree.R"))
if (!exists("xts_ret"))        source(here(project_tree$scripts$wrangle))
if (!exists("SAA_60_40"))      source(here("scripts_daa_saa_taa/01_saa_baseline.R"))

if (!exists("trend_signals")) {
  p <- here("02_data_processed/trend_signals.rds")
  if (file.exists(p)) trend_signals <- readRDS(p)
  else source(here("scripts_daa_saa_taa/03_taa_rules.R"))
}

# ==============================================================================
# 1. METRIC FUNCTIONS
# ==============================================================================

# ── Hit Rate Differential ─────────────────────────────────────────────────────
# Compares next-day positive return rates when above vs below MA.
# Positive = filter correctly separates good and bad periods.
.hit_rate_diff <- function(df) {
  df <- df %>%
    arrange(date) %>%
    mutate(next_ret = (lead(adjusted) / adjusted) - 1)

  on  <- df %>% filter(signal == 1, !is.na(next_ret)) %>% pull(next_ret)
  off <- df %>% filter(signal == 0, !is.na(next_ret)) %>% pull(next_ret)

  if (length(on) < 20 || length(off) < 20) return(NA_real_)
  mean(on > 0) - mean(off > 0)
}

# ── Return Capture Ratio ──────────────────────────────────────────────────────
# What fraction of total buy-and-hold return is earned during trend-on periods?
# > 1.0 means the filter kept the gains and avoided the losses.
.capture_ratio <- function(df) {
  total_ret <- prod(1 + (diff(df$adjusted) / head(df$adjusted, -1)), na.rm = TRUE) - 1
  if (abs(total_ret) < 1e-6) return(NA_real_)

  on_df  <- df %>% filter(signal == 1) %>% arrange(date)
  on_ret <- if (nrow(on_df) > 1)
    prod(1 + (diff(on_df$adjusted) / head(on_df$adjusted, -1)), na.rm = TRUE) - 1
  else 0

  on_ret / total_ret
}

# ── Whipsaw Rate ──────────────────────────────────────────────────────────────
# Fraction of signal episodes shorter than 20 trading days.
# Low = signal is stable; high = signal oscillates around MA constantly.
.whipsaw_rate <- function(df) {
  episodes <- df %>%
    arrange(date) %>%
    mutate(grp = cumsum(signal != lag(signal, default = signal[1]))) %>%
    group_by(grp, signal) %>%
    summarise(duration = n(), .groups = "drop")

  if (nrow(episodes) < 3) return(NA_real_)
  mean(episodes$duration < 20)
}

# ── Average Episode Duration ─────────────────────────────────────────────────
# Mean days per signal episode (both on and off).
# Long episodes = persistent, trend-following behaviour.
.avg_episode_days <- function(df) {
  episodes <- df %>%
    arrange(date) %>%
    mutate(grp = cumsum(signal != lag(signal, default = signal[1]))) %>%
    group_by(grp) %>%
    summarise(duration = n(), .groups = "drop")

  if (nrow(episodes) < 3) return(NA_real_)
  mean(episodes$duration)
}

# ── Hurst Exponent (R/S method) ───────────────────────────────────────────────
# H > 0.5 → persistent / trending    (MA filter will work)
# H = 0.5 → random walk              (MA filter neutral)
# H < 0.5 → mean-reverting           (MA filter will hurt)
.hurst_rs <- function(prices, n_scales = 20, min_n = 8) {
  r <- diff(log(prices[!is.na(prices)]))
  n_total <- length(r)
  if (n_total < 100) return(NA_real_)

  scales <- unique(round(exp(seq(log(min_n), log(n_total / 4), length.out = n_scales))))
  scales <- scales[scales >= min_n]

  rs_vals <- vapply(scales, function(s) {
    n_blocks <- floor(n_total / s)
    if (n_blocks < 2) return(NA_real_)
    block_rs <- vapply(seq_len(n_blocks), function(b) {
      chunk <- r[((b - 1) * s + 1):(b * s)]
      chunk <- chunk - mean(chunk)
      cum_c <- cumsum(chunk)
      S <- sd(chunk)
      if (S < 1e-10) return(NA_real_)
      (max(cum_c) - min(cum_c)) / S
    }, numeric(1))
    mean(block_rs, na.rm = TRUE)
  }, numeric(1))

  valid <- !is.na(rs_vals) & rs_vals > 0
  if (sum(valid) < 4) return(NA_real_)

  tryCatch(
    as.numeric(coef(lm(log(rs_vals[valid]) ~ log(scales[valid])))[2]),
    error = function(e) NA_real_
  )
}

# ==============================================================================
# 2. SCORE ENGINE
# ==============================================================================

score_trend_quality <- function(trend_signals, saa_tbl = SAA_60_40) {

  tickers <- unique(trend_signals$symbol)
  message(sprintf("📐 Scoring trend quality for %d tickers...", length(tickers)))

  raw <- purrr::map_dfr(tickers, function(tk) {
    df <- trend_signals %>%
      filter(symbol == tk) %>%
      arrange(date)

    if (nrow(df) < 100) return(NULL)

    tibble(
      ticker         = tk,
      hit_rate_diff  = .hit_rate_diff(df),
      capture_ratio  = .capture_ratio(df),
      whipsaw_rate   = .whipsaw_rate(df),
      avg_ep_days    = .avg_episode_days(df),
      hurst          = .hurst_rs(df$adjusted),
      pct_above_ma   = mean(df$signal, na.rm = TRUE),
      n_days         = nrow(df)
    )
  })

  # ── Normalise each metric to 0–100 ──────────────────────────────────────────
  # Higher is always "better trend quality" after transformation
  raw %>%
    mutate(
      # hit_rate_diff: higher = better  (range roughly −0.15 to +0.20)
      score_hit    = scales::rescale(hit_rate_diff,  to = c(0, 100)),
      # capture_ratio: higher = better  (clip at 0 and 3)
      score_cap    = scales::rescale(pmin(pmax(capture_ratio, 0), 3), to = c(0, 100)),
      # whipsaw_rate: LOWER = better  (invert)
      score_whip   = scales::rescale(-whipsaw_rate,  to = c(0, 100)),
      # avg_ep_days: higher = better   (clip at 200)
      score_ep     = scales::rescale(pmin(avg_ep_days, 200), to = c(0, 100)),
      # hurst: > 0.5 = better, centre on 0.5
      score_hurst  = scales::rescale(pmin(pmax(hurst, 0.3), 0.7), to = c(0, 100)),

      # Composite: equal weight of all five
      trendability = rowMeans(cbind(score_hit, score_cap, score_whip,
                                    score_ep, score_hurst), na.rm = TRUE),

      # Verdict
      verdict = case_when(
        trendability >= 70 ~ "Strong trend follower",
        trendability >= 50 ~ "Moderate",
        TRUE               ~ "Weak / mean-reverting"
      )
    ) %>%
    left_join(saa_tbl %>% select(ticker, saa_bucket, weight), by = "ticker") %>%
    arrange(desc(trendability))
}

# ==============================================================================
# 3. PLOTS
# ==============================================================================

# ── Plot A: Scorecard Heatmap ─────────────────────────────────────────────────
# Tickers × metrics — each cell coloured by normalised score (0–100).
# Instantly shows which tickers are strong across all five dimensions.

plot_trend_scorecard <- function(tqs) {

  ticker_order <- tqs %>% arrange(trendability) %>% pull(ticker)

  long_df <- tqs %>%
    select(ticker, trendability,
           `Hit Rate\nDiff`     = score_hit,
           `Return\nCapture`    = score_cap,
           `Whipsaw\n(inverted)`= score_whip,
           `Avg Episode\nDays`  = score_ep,
           `Hurst\nExponent`    = score_hurst) %>%
    pivot_longer(-c(ticker, trendability),
                 names_to = "metric", values_to = "score") %>%
    mutate(
      ticker = factor(ticker, levels = ticker_order),
      metric = factor(metric, levels = c("Hit Rate\nDiff", "Return\nCapture",
                                          "Whipsaw\n(inverted)", "Avg Episode\nDays",
                                          "Hurst\nExponent"))
    )

  ggplot(long_df, aes(metric, ticker, fill = score)) +
    geom_tile(colour = "white", linewidth = 0.6) +
    geom_text(aes(label = round(score, 0),
                  colour = score < 40),
              size = 3, fontface = "bold") +
    scale_fill_gradient2(
      low      = "#7f0000", mid = "#f3f4f6", high = "#14532d",
      midpoint = 50, limits = c(0, 100),
      name     = "Score\n(0–100)"
    ) +
    scale_colour_manual(values = c("FALSE" = "#1a1a1a", "TRUE" = "#ffffff"),
                        guide = "none") +
    labs(
      title    = "Trend Quality Scorecard — How Well Does 200DMA Work per Ticker?",
      subtitle = "Green = strong trending behaviour  |  Red = mean-reverting / noisy\nAll metrics normalised 0–100; composite = equal weight",
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid   = element_blank(),
      plot.title   = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(colour = "grey50", size = 9),
      axis.text.x  = element_text(face = "bold", size = 9, lineheight = 0.85),
      axis.text.y  = element_text(face = "bold", size = 10)
    )
}

# ── Plot B: Composite Ranking Bar ─────────────────────────────────────────────
# Horizontal bars ranked by trendability score, colour = verdict.

plot_trend_ranking <- function(tqs) {

  verdict_colours <- c(
    "Strong trend follower" = "#14532d",
    "Moderate"              = "#92400e",
    "Weak / mean-reverting" = "#7f0000"
  )

  df <- tqs %>%
    mutate(ticker = fct_reorder(ticker, trendability)) %>%
    filter(!is.na(trendability))

  ggplot(df, aes(trendability, ticker, fill = verdict)) +
    geom_col(width = 0.7, alpha = 0.88) +
    geom_vline(xintercept = c(50, 70), linetype = "dashed",
               colour = "#6b7280", linewidth = 0.4) +
    geom_text(aes(label = sprintf("%.0f", trendability)),
              hjust = -0.2, size = 3, fontface = "bold") +
    annotate("text", x = 50, y = 0.4, label = "50",
             hjust = 0.5, vjust = 0, size = 3, colour = "#6b7280") +
    annotate("text", x = 70, y = 0.4, label = "70",
             hjust = 0.5, vjust = 0, size = 3, colour = "#6b7280") +
    scale_fill_manual(values = verdict_colours, name = NULL) +
    scale_x_continuous(limits = c(0, 110), expand = expansion(mult = c(0, 0))) +
    labs(
      title    = "Trendability Ranking — Composite Score (0–100)",
      subtitle = "> 70: 200DMA adds clear value  |  50–70: moderate  |  < 50: filter may hurt",
      x = "Trendability Score", y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(face = "bold", size = 13),
      plot.subtitle      = element_text(colour = "grey50", size = 9),
      legend.position    = "top",
      axis.text.y        = element_text(face = "bold", size = 10)
    )
}

# ── Plot C: Hurst Exponent Strip ──────────────────────────────────────────────
# Shows H for each ticker with H=0.5 reference and zones labelled.

plot_trend_hurst <- function(tqs) {

  df <- tqs %>%
    filter(!is.na(hurst)) %>%
    mutate(ticker  = fct_reorder(ticker, hurst),
           zone    = case_when(hurst > 0.55 ~ "Trending",
                               hurst < 0.45 ~ "Mean-reverting",
                               TRUE          ~ "Random walk"))

  zone_colours <- c("Trending" = "#14532d", "Random walk" = "#92400e",
                    "Mean-reverting" = "#7f0000")

  ggplot(df, aes(hurst, ticker, colour = zone)) +
    # Zone shading
    annotate("rect", xmin = 0.55, xmax = 1,    ymin = -Inf, ymax = Inf,
             fill = "#d1fae5", alpha = 0.35) +
    annotate("rect", xmin = 0,    xmax = 0.45, ymin = -Inf, ymax = Inf,
             fill = "#fee2e2", alpha = 0.35) +
    geom_vline(xintercept = 0.5, colour = "#6b7280",
               linewidth = 0.8, linetype = "dashed") +
    geom_segment(aes(x = 0.5, xend = hurst, yend = ticker),
                 linewidth = 0.6, alpha = 0.5) +
    geom_point(size = 4, alpha = 0.9) +
    geom_text(aes(label = sprintf("%.2f", hurst)),
              hjust = -0.4, size = 3, fontface = "bold") +
    annotate("text", x = 0.72, y = length(unique(df$ticker)) + 0.3,
             label = "Trending →", hjust = 0, size = 3.5,
             colour = "#14532d", fontface = "bold") +
    annotate("text", x = 0.28, y = length(unique(df$ticker)) + 0.3,
             label = "← Mean-reverting", hjust = 1, size = 3.5,
             colour = "#7f0000", fontface = "bold") +
    scale_colour_manual(values = zone_colours, guide = "none") +
    scale_x_continuous(limits = c(0.25, 0.85),
                       breaks = c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8)) +
    labs(
      title    = "Hurst Exponent — Structural Trendiness of Each Ticker",
      subtitle = "H > 0.5 → momentum / persistence → 200DMA works\nH < 0.5 → mean-reversion → 200DMA costs money",
      x = "Hurst Exponent (H)", y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(face = "bold", size = 13),
      plot.subtitle      = element_text(colour = "grey50", size = 9),
      axis.text.y        = element_text(face = "bold", size = 10)
    )
}

# ==============================================================================
# 4. RUN
# ==============================================================================

message("📐 Scoring trend quality across SAA universe...")
trend_quality_scores <- score_trend_quality(trend_signals, SAA_60_40)

message("\n📋 Trend Quality Summary:\n")
trend_quality_scores %>%
  mutate(
    trendability  = sprintf("%.0f",   trendability),
    hit_rate_diff = sprintf("%+.1f%%", hit_rate_diff * 100),
    capture_ratio = sprintf("%.2f",   capture_ratio),
    whipsaw_rate  = sprintf("%.0f%%", whipsaw_rate  * 100),
    avg_ep_days   = sprintf("%.0f",   avg_ep_days),
    hurst         = sprintf("%.2f",   hurst)
  ) %>%
  select(ticker, saa_bucket, verdict, trendability,
         hit_rate_diff, capture_ratio, whipsaw_rate, avg_ep_days, hurst) %>%
  print(n = 30)

write_rds(trend_quality_scores, here("02_data_processed/trend_quality_scores.rds"))
message("💾 Saved: trend_quality_scores")

if (!isTRUE(getOption("knitr.in.progress"))) {
  p_trend_scorecard <- plot_trend_scorecard(trend_quality_scores)
  p_trend_ranking   <- plot_trend_ranking(trend_quality_scores)
  p_trend_hurst     <- plot_trend_hurst(trend_quality_scores)

  print(p_trend_scorecard)
  print(p_trend_ranking)
  print(p_trend_hurst)
}

message("✅ Stage DAA-06 complete: trendability scores computed.")
