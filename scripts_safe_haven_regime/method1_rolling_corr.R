################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : scripts_safe_haven_regime/method1_rolling_corr.R
# Purpose : Method 1 — Regime-Conditional Rolling Correlation Analysis
#
# NORTH STAR: SPY regimes (Fall / Recovery / Consolidation, t_fall=10%)
#             All analysis is anchored to SPY regime phases.
#
# QUESTION: Do sensitivity groups encode early signal about SPY regime?
#
# SENSITIVITY GROUPS (hierarchy)
#   1. Credit    : HYG  (anti-safe-haven / risk-on canary)
#   2. Equities  : XLF, XLY, XLK
#   3. FX        : FXF (CHF), FXY (JPY), EMLC (EM local vs USD)
#   4. Safe Haven: GLD, IEF, TLT, UUP
#
# METRICS
#   - 60-day rolling correlation vs SPY
#   - First derivative  (daily change in rolling corr)
#   - Second derivative (acceleration of corr change)
#   - Jumps             (|first deriv| > 2 * rolling sd of first deriv)
################################################################################

library(tidyverse)
library(xts)
library(PerformanceAnalytics)
library(scales)
library(patchwork)
library(here)

# ── Dependencies ──────────────────────────────────────────────────────────────
if (!exists("build_regime_table"))
  source(here("scripts_spy_dd_regime/spy_dd_regime.R"))

if (!exists("xts_ret"))
  xts_ret <- readRDS(here("02_data_processed/xts_ret_returns.rds"))

# ── Parameters ────────────────────────────────────────────────────────────────
MASTER       <- "SPY"
T_FALL       <- 0.10
T_CRUISE     <- 0.05
ROLL_WIN     <- 60      # rolling window in days
JUMP_MULT    <- 2.0     # jump = |d1| > JUMP_MULT * sd(d1)

# ── Sensitivity groups ────────────────────────────────────────────────────────
groups <- list(
  "Credit"     = "HYG",
  "Equities"   = c("XLF", "XLY", "XLK"),
  "FX"         = c("FXF", "FXY", "EMLC"),
  "Safe Haven" = c("GLD", "IEF", "TLT", "UUP")
)

all_tickers <- unlist(groups)

# ── Regime table (SPY is the North Star) ──────────────────────────────────────
spy_ret <- xts_ret[, MASTER]
rt      <- build_regime_table(spy_ret, T_FALL, T_CRUISE)

# ── Build rolling correlation + derivatives ───────────────────────────────────
compute_roll_corr <- function(ticker, win = ROLL_WIN) {
  tk_r  <- xts_ret[, ticker]
  dates <- index(spy_ret)

  # 60-day rolling correlation vs SPY
  roll_corr <- rollapply(
    merge(spy_ret, tk_r),
    width   = win,
    FUN     = function(m) cor(m[, 1], m[, 2], use = "complete.obs"),
    by.column = FALSE,
    align   = "right",
    fill    = NA
  )

  corr_vec <- as.numeric(roll_corr)

  # First derivative (day-over-day change)
  d1 <- c(NA, diff(corr_vec))

  # Second derivative (acceleration)
  d2 <- c(NA, diff(d1))

  # Jump detection: |d1| > JUMP_MULT * rolling sd of d1
  d1_sd  <- as.numeric(rollapply(d1, width = win, FUN = sd, na.rm = TRUE,
                                  align = "right", fill = NA))
  is_jump <- abs(d1) > JUMP_MULT * d1_sd

  tibble(
    date    = dates,
    ticker  = ticker,
    corr    = corr_vec,
    d1      = d1,
    d2      = d2,
    d1_sd   = d1_sd,
    is_jump = is_jump
  )
}

roll_df <- map_dfr(all_tickers, compute_roll_corr) %>%
  left_join(
    rt %>% select(regime, xmin, xmax) %>%
      rowwise() %>%
      mutate(date = list(seq(as.Date(xmin), as.Date(xmax), by = "day"))) %>%
      unnest(date) %>%
      select(date, regime),
    by = "date"
  ) %>%
  mutate(
    group = case_when(
      ticker %in% groups$Credit      ~ "Credit",
      ticker %in% groups$Equities    ~ "Equities",
      ticker %in% groups$FX          ~ "FX",
      ticker %in% groups$`Safe Haven`~ "Safe Haven"
    ),
    group  = factor(group, levels = names(groups)),
    ticker = factor(ticker, levels = all_tickers),
    regime = factor(regime, levels = c("Fall", "Recovery", "Consolidation"))
  )

# ── Regime palette (consistent with project) ─────────────────────────────────
REGIME_COLORS <- c(Fall = "#D90429", Recovery = "#F77F00", Consolidation = "#2D6A4F")

# ==============================================================================
# PLOT 1: Rolling correlation — all tickers, faceted by group, regime shaded
# ==============================================================================
plot_rolling_corr <- function() {

  shade_df <- rt %>%
    filter(regime == "Fall") %>%
    select(xmin, xmax)

  ggplot(roll_df %>% filter(!is.na(corr)),
         aes(x = date, y = corr, color = ticker)) +

    # Fall shading
    geom_rect(data = shade_df,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = "#D90429", alpha = 0.08, inherit.aes = FALSE) +

    geom_hline(yintercept = 0,  color = "grey40", linewidth = 0.5) +
    geom_hline(yintercept = -0.5, color = "#D90429", linetype = "dashed",
               alpha = 0.4, linewidth = 0.4) +
    geom_hline(yintercept =  0.5, color = "#2D6A4F", linetype = "dashed",
               alpha = 0.4, linewidth = 0.4) +

    geom_line(linewidth = 0.7, alpha = 0.9) +

    # Jump markers
    geom_point(data = roll_df %>% filter(is_jump == TRUE, !is.na(corr)),
               aes(x = date, y = corr),
               shape = 21, fill = "white", size = 1.8, stroke = 0.8) +

    facet_wrap(~ group, ncol = 1, scales = "free_y") +

    scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5),
                       labels = number_format(accuracy = 0.1)) +
    scale_x_date(expand = expansion(mult = c(0.01, 0.01))) +

    labs(
      title    = sprintf("%d-Day Rolling Correlation vs %s — by Sensitivity Group",
                         ROLL_WIN, MASTER),
      subtitle = "Red shading = SPY Fall regime  |  Circles = correlation jumps  |  Dashed lines = ±0.5",
      x = NULL, y = "Rolling Correlation", color = "Ticker"
    ) +

    theme_minimal(base_size = 11) +
    theme(
      strip.text         = element_text(face = "bold", size = 10),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position    = "right",
      plot.title         = element_text(face = "bold", size = 13),
      plot.subtitle      = element_text(color = "grey50", size = 9)
    )
}

# ==============================================================================
# PLOT 2: First derivative — rate of change of correlation, jumps highlighted
# ==============================================================================
plot_corr_d1 <- function() {

  shade_df <- rt %>% filter(regime == "Fall") %>% select(xmin, xmax)

  ggplot(roll_df %>% filter(!is.na(d1)),
         aes(x = date, y = d1, color = ticker)) +

    geom_rect(data = shade_df,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = "#D90429", alpha = 0.08, inherit.aes = FALSE) +

    geom_hline(yintercept = 0, color = "grey40", linewidth = 0.5) +
    geom_line(linewidth = 0.5, alpha = 0.7) +

    # Jump markers on d1
    geom_point(data = roll_df %>% filter(is_jump == TRUE, !is.na(d1)),
               aes(x = date, y = d1),
               shape = 21, fill = "white", size = 2, stroke = 0.9) +

    facet_wrap(~ group, ncol = 1, scales = "free_y") +

    scale_x_date(expand = expansion(mult = c(0.01, 0.01))) +

    labs(
      title    = "First Derivative of Rolling Correlation vs SPY",
      subtitle = "Daily change in 60-day rolling corr  |  Circles = jumps (> 2σ move)",
      x = NULL, y = "Δ Correlation / day", color = "Ticker"
    ) +

    theme_minimal(base_size = 11) +
    theme(
      strip.text         = element_text(face = "bold", size = 10),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position    = "right",
      plot.title         = element_text(face = "bold", size = 13),
      plot.subtitle      = element_text(color = "grey50", size = 9)
    )
}

# ==============================================================================
# PLOT 3: Regime-conditional correlation distribution — boxplot per regime
# ==============================================================================
plot_corr_by_regime <- function() {

  ggplot(roll_df %>% filter(!is.na(corr), !is.na(regime)),
         aes(x = regime, y = corr, fill = regime)) +

    geom_hline(yintercept = 0, color = "grey40", linewidth = 0.5) +
    geom_boxplot(alpha = 0.8, outlier.size = 0.8, width = 0.6) +

    scale_fill_manual(values = REGIME_COLORS, guide = "none") +

    facet_wrap(~ group + ticker, ncol = 4, scales = "free_y") +

    scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +

    labs(
      title    = "Correlation vs SPY — Distribution by Regime Phase",
      subtitle = sprintf("North Star: SPY Fall ≥%.0f%%  |  60-day rolling window", T_FALL * 100),
      x = NULL, y = "Rolling Correlation"
    ) +

    theme_minimal(base_size = 10) +
    theme(
      strip.text       = element_text(face = "bold", size = 8),
      panel.grid.minor = element_blank(),
      axis.text.x      = element_text(angle = 30, hjust = 1, size = 8),
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(color = "grey50", size = 9)
    )
}

# ==============================================================================
# PLOT 4: Jump frequency by regime — do jumps cluster in Fall?
# ==============================================================================
plot_jump_freq_by_regime <- function() {

  jump_df <- roll_df %>%
    filter(!is.na(regime), !is.na(is_jump)) %>%
    group_by(group, ticker, regime) %>%
    summarise(
      n_days  = n(),
      n_jumps = sum(is_jump, na.rm = TRUE),
      jump_pct = n_jumps / n_days,
      .groups = "drop"
    )

  ggplot(jump_df, aes(x = ticker, y = jump_pct, fill = regime)) +
    geom_col(position = "dodge", width = 0.7) +
    scale_fill_manual(values = REGIME_COLORS) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    facet_wrap(~ group, scales = "free_x", nrow = 1) +
    labs(
      title    = "Correlation Jump Frequency by SPY Regime",
      subtitle = "Jump = |Δcorr| > 2σ  |  If Falls cluster jumps → early warning signal",
      x = NULL, y = "Jump frequency (% of days)", fill = "SPY Regime"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.text       = element_text(face = "bold", size = 10),
      axis.text.x      = element_text(angle = 30, hjust = 1),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(color = "grey50", size = 9)
    )
}

# ==============================================================================
# RUN ALL
# ==============================================================================
print(plot_rolling_corr())
print(plot_corr_d1())
print(plot_corr_by_regime())
print(plot_jump_freq_by_regime())

################################################################################
