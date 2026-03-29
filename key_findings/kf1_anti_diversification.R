# ==============================================================================
# KEY FINDING 1 — Anti-Diversification: Low-Correlation Tickers Have WORSE
#                 Empirical MaxDD Than Theory Predicts
# ==============================================================================
#
# FINDING IN ONE SENTENCE:
#   Magdon-Ismail theory predicts that a ticker with near-zero correlation vs SPY
#   should produce a shallow spread-DD when held alongside SPY. Empirically it is
#   the opposite: low-ρ tickers produce larger-than-predicted drawdowns, while
#   high-ρ tickers track theory closely.
#
# WHY IT MATTERS:
#   Conventional portfolio theory says "add a low-correlation asset to reduce
#   portfolio risk." This finding shows that for a long-only blended portfolio,
#   the *spread* (ticker − SPY) with a near-zero ρ ticker behaves erratically —
#   the ticker goes up and down for entirely different reasons than SPY, so when
#   SPY falls hard the ticker may ALSO fall, giving you no protection and a large
#   empirical drawdown that theory did not anticipate.
#
# PLOT 1 — Scatter: Theoretical vs Empirical MaxDD, coloured by ρ band
# PLOT 2 — Residual bar chart: (empirical − theory), sorted by ρ, showing the
#           systematic bias for low-ρ tickers
#
# Data requirements:
#   xts_ret      — loaded from 02_data_processed/xts_ret_returns.rds
#   etf_metadata — loaded from scripts/00_init_universe.R
# ==============================================================================

library(tidyverse)
library(xts)
library(scales)
library(ggrepel)
library(here)
library(patchwork)

source(here("scripts/00_init_universe.R"))

xts_ret <- readRDS(here("02_data_processed/xts_ret_returns.rds"))

# ── Magdon-Ismail formula for expected MaxDD of a random walk ─────────────────
# sigma = annualised vol of the series, mu = annualised drift, T = years
#
# Edge-case handling:
#   1. mu <= 0  → pure random walk approximation: sigma * sqrt(T)
#   2. S * sqrt(T) <= exp(-0.35) ≈ 0.705 → log term goes negative,
#      making the formula meaningless. Fall back to sigma * sqrt(T).
#   3. Result floored at 0 to prevent sign inversions from numerical noise.
magdon_mdd <- function(sigma, mu, T) {
  if (is.na(mu) || is.na(sigma) || sigma <= 0) return(NA_real_)
  if (mu <= 0) return(sigma * sqrt(T))
  S <- mu / sigma
  log_term <- log(S * sqrt(T)) + 0.35
  if (log_term <= 0) return(sigma * sqrt(T))   # formula breaks down here
  result <- (sigma / S) * log_term
  max(result, 0)
}

# ── Build cost-benefit table for the full Sovereign Universe ──────────────────
T_yrs    <- as.numeric(diff(range(index(xts_ret)))) / 365
MASTER   <- "SPY"
universe <- setdiff(colnames(xts_ret), MASTER)

cb_tbl <- map_dfr(universe, function(tk) {
  r_tk  <- as.numeric(na.omit(xts_ret[, tk]))
  r_spy <- as.numeric(na.omit(xts_ret[, MASTER]))
  n     <- min(length(r_tk), length(r_spy))
  if (n < 252) return(NULL)          # need at least 1 year of data
  r_tk  <- tail(r_tk,  n)
  r_spy <- tail(r_spy, n)

  spread   <- r_tk - r_spy
  mu_spr   <- mean(spread, na.rm = TRUE) * 252
  sig_spr  <- sd(spread,   na.rm = TRUE) * sqrt(252)
  mdd_emp  <- {
    w <- cumprod(1 + spread)
    min((w - cummax(w)) / cummax(w), na.rm = TRUE)
  }
  mdd_th   <- magdon_mdd(sig_spr, mu_spr, T_yrs)
  rho      <- cor(r_tk, r_spy, use = "complete.obs")

  tibble(
    ticker  = tk,
    mu_spr  = mu_spr,
    sig_spr = sig_spr,
    mdd_emp = mdd_emp,    # negative number (drawdown)
    mdd_th  = -mdd_th,    # make negative to match mdd_emp sign
    rho     = rho,
    ir      = mu_spr / sig_spr
  )
}) %>%
  left_join(
    etf_metadata %>% dplyr::select(ticker, asset_class, sub_block),
    by = "ticker"
  ) %>%
  filter(!is.na(mdd_th)) %>%
  mutate(
    residual = mdd_emp - mdd_th,   # negative = empirical WORSE than theory
    rho_band = cut(rho,
      breaks = c(-Inf, 0.2, 0.5, 0.75, Inf),
      labels = c("ρ < 0.2", "0.2–0.5", "0.5–0.75", "ρ > 0.75")
    )
  )

# ── Palette ───────────────────────────────────────────────────────────────────
RHO_PAL <- c(
  "ρ < 0.2"  = "#d73027",
  "0.2–0.5"  = "#fc8d59",
  "0.5–0.75" = "#4575b4",
  "ρ > 0.75" = "#1a1a6e"
)

# ── PLOT 1: Scatter — Theoretical vs Empirical MaxDD ──────────────────────────
# Points clustered near the diagonal are well-predicted by theory.
# Points BELOW the diagonal (y < x) have empirical DD worse than predicted.
# Colour = ρ band: the pattern reveals that red/orange (low ρ) cluster below.

p1 <- ggplot(cb_tbl, aes(x = mdd_th, y = mdd_emp, colour = rho_band)) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", colour = "grey50", linewidth = 0.8) +
  annotate("text",
           x = min(cb_tbl$mdd_th, na.rm = TRUE) * 0.55,
           y = min(cb_tbl$mdd_emp, na.rm = TRUE) * 0.97,
           label = "Empirical DD worse\nthan theory predicts",
           hjust = 0, vjust = 1, size = 4, colour = "grey35", fontface = "italic") +
  annotate("text",
           x = max(cb_tbl$mdd_th, na.rm = TRUE) * 0.97,
           y = max(cb_tbl$mdd_th, na.rm = TRUE) * 0.55,
           label = "Theory overstates DD",
           hjust = 1, vjust = 0, size = 4, colour = "grey35", fontface = "italic") +
  geom_point(aes(size = abs(rho)), alpha = 0.78) +
  geom_text_repel(
    data = cb_tbl %>%
      group_by(rho_band) %>%
      slice_min(residual, n = 3) %>%
      ungroup(),
    aes(label = ticker),
    size = 3.8, fontface = "bold", max.overlaps = 40,
    segment.color = "grey60", show.legend = FALSE
  ) +
  scale_colour_manual(values = RHO_PAL, name = "ρ vs SPY") +
  scale_size_continuous(range = c(2, 6), guide = "none") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor  = element_blank(),
    legend.position   = "right",
    legend.text       = element_text(size = 12),
    legend.title      = element_text(size = 12, face = "bold"),
    axis.title        = element_text(size = 13),
    axis.text         = element_text(size = 12),
    plot.title        = element_text(face = "bold", size = 15),
    plot.subtitle     = element_text(size = 12, colour = "grey40")
  ) +
  labs(
    title    = "KF1-A: Theoretical vs Empirical MaxDD of Spread vs SPY",
    subtitle = paste0(
      "Below diagonal = empirical DD worse than Magdon-Ismail predicts\n",
      "Low-ρ tickers (red/orange) cluster below the diagonal — anti-diversification in action"
    ),
    x = "Theoretical MaxDD of spread (Magdon-Ismail)",
    y = "Empirical MaxDD of spread (historical)"
  )

# ── PLOT 2: Box plot by ρ band ─────────────────────────────────────────────────
# Shows the distribution of residuals within each band.
# Tells the same story as a ticker-by-ticker bar chart but is readable at any size.
# Residual = empirical − theory  (negative = empirical WORSE than theory)

p2 <- ggplot(cb_tbl, aes(x = rho_band, y = residual, fill = rho_band)) +
  geom_hline(yintercept = 0, linewidth = 0.8, colour = "grey30") +
  geom_boxplot(alpha = 0.75, outlier.shape = NA, width = 0.5) +
  geom_jitter(aes(colour = rho_band), width = 0.18, size = 2.5, alpha = 0.6) +
  geom_text_repel(
    data = cb_tbl %>%
      group_by(rho_band) %>%
      slice_min(residual, n = 2) %>%
      ungroup(),
    aes(label = ticker, colour = rho_band),
    size = 3.5, fontface = "bold", max.overlaps = 20,
    segment.color = "grey60", show.legend = FALSE,
    nudge_x = 0.3
  ) +
  scale_fill_manual(values  = RHO_PAL, guide = "none") +
  scale_colour_manual(values = RHO_PAL, guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.title         = element_text(size = 13),
    axis.text          = element_text(size = 13),
    plot.title         = element_text(face = "bold", size = 15),
    plot.subtitle      = element_text(size = 12, colour = "grey40")
  ) +
  labs(
    title    = "KF1-B: Residual (Empirical − Theory) MaxDD by ρ Band",
    subtitle = paste0(
      "Below zero = empirical DD worse than theory predicts\n",
      "Low-ρ bands have deeper and more consistent negative residuals"
    ),
    x = "Correlation band vs SPY",
    y = "Residual MaxDD (empirical − theory)"
  )

# ── Print ──────────────────────────────────────────────────────────────────────
if (!isTRUE(getOption("knitr.in.progress"))) {
  print(p1)
  print(p2)

  # Summary statistics by ρ band
  cat("\n── Residual by ρ band (empirical − theory, negative = empirical worse) ────\n")
  cb_tbl %>%
    group_by(rho_band) %>%
    summarise(
      n            = n(),
      median_resid = median(residual, na.rm = TRUE),
      mean_resid   = mean(residual,   na.rm = TRUE),
      pct_worse    = mean(residual < 0, na.rm = TRUE)
    ) %>%
    mutate(
      median_resid = scales::percent(median_resid, accuracy = 0.1),
      mean_resid   = scales::percent(mean_resid,   accuracy = 0.1),
      pct_worse    = scales::percent(pct_worse,    accuracy = 1)
    ) %>%
    print()
}
