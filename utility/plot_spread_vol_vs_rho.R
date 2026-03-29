################################################################################
# FILE    : utility/plot_spread_vol_vs_rho.R
# Purpose : Visualise how correlation ρ drives spread volatility AND MaxDD
#           via Monte Carlo simulation + Magdon-Ismail analytical overlay
#
# QUESTION: Given two assets (GLD-like, XLK-like), how does the L/S spread
#           MaxDD distribution shift as ρ moves from -1 to +1?
#
# TWO PANELS
#   Panel 1 : Boxplot of simulated MaxDD across ρ grid
#              + Magdon-Ismail E[MaxDD] line overlay
#   Panel 2 : Analytical spread vol curve σ_spread(ρ) with key annotations
#              (deterministic — no simulation needed)
################################################################################

library(MASS)          # mvrnorm — load before tidyverse
library(tidyverse)
library(scales)
library(patchwork)

# ── Parameters (GLD vs XLK empirical) ────────────────────────────────────────
MU1    <- 0.191          # GLD ann drift
MU2    <- 0.158          # XLK ann drift
SIG1   <- 0.142          # GLD ann vol
SIG2   <- 0.229          # XLK ann vol
T_YRS  <- 8.17           # horizon in years
T_DAYS <- round(T_YRS * 252)
N_SIM  <- 1000           # MC paths per ρ

# Daily params
mu1_d  <- MU1  / 252
mu2_d  <- MU2  / 252
sig1_d <- SIG1 / sqrt(252)
sig2_d <- SIG2 / sqrt(252)

# ── ρ grid ────────────────────────────────────────────────────────────────────
# Avoid ±1 exactly (singular covariance); use ±0.995 as stand-ins
rho_grid <- c(-0.995, -0.75, -0.50, -0.25, 0, 0.07, 0.25, 0.50, 0.75, 0.995)
rho_labels <- c("ρ→-1", "ρ=-0.75", "ρ=-0.5", "ρ=-0.25",
                 "ρ=0",  "ρ=0.07\n(actual)", "ρ=0.25", "ρ=0.5",
                 "ρ=0.75", "ρ→+1")

# ── Magdon-Ismail E[MaxDD] ────────────────────────────────────────────────────
# For GBM with ann drift μ and ann vol σ over T years
# Handles μ ≤ 0 (random walk regime) and μ > 0 (drift regime)
magdon_mdd <- function(sigma, mu, T_years) {
  if (mu <= 0 || is.nan(mu))
    return(sigma * sqrt(T_years))           # zero/negative drift → σ√T
  S <- mu / sigma
  (sigma / S) * (log(S * sqrt(T_years)) + 0.35)
}

# ── Running MaxDD helper ──────────────────────────────────────────────────────
max_dd_from_returns <- function(r) {
  w  <- cumprod(1 + r)
  dd <- (w - cummax(w)) / cummax(w)
  min(dd, na.rm = TRUE)
}

# ── Monte Carlo simulation ────────────────────────────────────────────────────
cat("Running MC simulation...\n")

mc_df <- map_dfr(seq_along(rho_grid), function(i) {
  rho <- rho_grid[i]

  Sigma_mat <- matrix(c(sig1_d^2,          rho * sig1_d * sig2_d,
                         rho * sig1_d * sig2_d, sig2_d^2), 2, 2)

  # Annualised spread vol (analytical)
  sig_spread_ann <- sqrt(SIG1^2 + SIG2^2 - 2 * rho * SIG1 * SIG2)

  # Spread ann drift (arithmetic) and geometric drift (for Magdon-Ismail)
  mu_spread_arith <- MU1 - MU2
  mu_spread_geo   <- mu_spread_arith - 0.5 * sig_spread_ann^2

  # Magdon-Ismail expected MaxDD
  mdd_theory <- magdon_mdd(sig_spread_ann, mu_spread_geo, T_YRS)

  # Simulated MaxDD paths
  sim_mdd <- map_dbl(seq_len(N_SIM), function(j) {
    ret    <- mvrnorm(T_DAYS, mu = c(mu1_d, mu2_d), Sigma = Sigma_mat)
    r_spr  <- ret[, 1] - ret[, 2]
    max_dd_from_returns(r_spr)
  })

  tibble(
    rho            = rho,
    rho_label      = rho_labels[i],
    max_dd_sim     = sim_mdd,
    mdd_theory     = mdd_theory,
    sig_spread_ann = sig_spread_ann
  )
}, .progress = TRUE)

cat("Done.\n")

# Factor order for x-axis
mc_df <- mc_df %>%
  mutate(rho_label = factor(rho_label, levels = rho_labels))

# ── Analytical spread vol curve (no simulation) ───────────────────────────────
rho_fine <- seq(-1, 1, length.out = 300)
vol_curve_df <- tibble(
  rho            = rho_fine,
  sig_spread     = sqrt(SIG1^2 + SIG2^2 - 2 * rho_fine * SIG1 * SIG2) * 100,
  mu_spread_geo  = (MU1 - MU2) - 0.5 * (SIG1^2 + SIG2^2 - 2 * rho_fine * SIG1 * SIG2),
  mdd_theory     = map2_dbl(sig_spread / 100, mu_spread_geo,
                             ~ magdon_mdd(.x, .y, T_YRS)) * 100
)

# Key annotation points
key_pts <- tibble(
  rho   = c(-0.995,  0,     0.07,  0.995),
  label = c("ρ→-1", "ρ=0", "actual\nρ=0.07", "ρ→+1"),
  sig_spread = sqrt(SIG1^2 + SIG2^2 - 2 * rho * SIG1 * SIG2) * 100
) %>%
  mutate(sig_spread = sqrt(SIG1^2 + SIG2^2 - 2 * rho * SIG1 * SIG2) * 100)

# Threshold lines
thresh_df <- tibble(
  yintercept = c(abs(SIG1 - SIG2), sqrt(SIG1^2 + SIG2^2), SIG1 + SIG2) * 100,
  label      = c(sprintf("|σ₁-σ₂| = %.1f%%", abs(SIG1-SIG2)*100),
                 sprintf("√(σ₁²+σ₂²) = %.1f%%", sqrt(SIG1^2+SIG2^2)*100),
                 sprintf("σ₁+σ₂ = %.1f%%", (SIG1+SIG2)*100))
)

# ── PLOT 1: MaxDD boxplot + theory line ───────────────────────────────────────
theory_pts <- mc_df %>%
  distinct(rho_label, mdd_theory) %>%
  mutate(mdd_theory = -mdd_theory * 100)   # convert to % negative

p1 <- ggplot(mc_df, aes(x = rho_label, y = max_dd_sim * 100)) +

  # Boxplot distribution of simulated MaxDD
  geom_boxplot(aes(fill = rho), width = 0.6, outlier.size = 0.6,
               outlier.alpha = 0.4, alpha = 0.75) +

  # Magdon-Ismail E[MaxDD] line
  geom_line(data = theory_pts,
            aes(x = rho_label, y = mdd_theory, group = 1),
            colour = "#D90429", linewidth = 0.9, linetype = "dashed") +
  geom_point(data = theory_pts,
             aes(x = rho_label, y = mdd_theory),
             colour = "#D90429", size = 2.5) +

  # Actual GLD/XLK MaxDD reference lines
  geom_hline(yintercept = -20.3, linetype = "dotted",
             colour = "#2D6A4F", linewidth = 0.6) +
  geom_hline(yintercept = -37.1, linetype = "dotted",
             colour = "#457B9D", linewidth = 0.6) +
  annotate("text", x = 0.6, y = -19,   label = "GLD actual MaxDD -20.3%",
           colour = "#2D6A4F", size = 3, hjust = 0) +
  annotate("text", x = 0.6, y = -38.5, label = "XLK actual MaxDD -37.1%",
           colour = "#457B9D", size = 3, hjust = 0) +

  scale_fill_gradient2(low = "#D90429", mid = "grey80", high = "#2D6A4F",
                       midpoint = 0, guide = "none") +
  scale_y_continuous(labels = percent_format(scale = 1, accuracy = 1),
                     breaks = seq(-100, 0, 10)) +
  labs(
    title    = "MaxDD Distribution vs Correlation ρ  (Monte Carlo, 1000 paths each)",
    subtitle = paste0("Red dashed = Magdon-Ismail E[MaxDD]  |  Dotted = actual GLD/XLK MaxDD\n",
                      sprintf("Parameters: σ₁=%.1f%% (GLD), σ₂=%.1f%% (XLK), T=%.1f yrs, N=%d",
                              SIG1*100, SIG2*100, T_YRS, N_SIM)),
    x = "Correlation ρ", y = "Max Drawdown"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor  = element_blank(),
    axis.text.x       = element_text(size = 8),
    plot.title        = element_text(face = "bold", size = 13),
    plot.subtitle     = element_text(colour = "grey50", size = 8)
  )

# ── PLOT 2: Analytical spread vol + E[MaxDD] curve ───────────────────────────
p2 <- ggplot(vol_curve_df, aes(x = rho)) +

  # Spread vol curve
  geom_line(aes(y = sig_spread, colour = "Spread Vol σ(ρ)"),
            linewidth = 1.0) +

  # Magdon-Ismail MaxDD curve
  geom_line(aes(y = mdd_theory, colour = "E[MaxDD] Magdon-Ismail"),
            linewidth = 1.0, linetype = "dashed") +

  # Threshold reference lines
  geom_hline(data = thresh_df,
             aes(yintercept = yintercept),
             linetype = "dotted", colour = "grey50", linewidth = 0.5) +
  geom_text(data = thresh_df,
            aes(x = -0.98, y = yintercept, label = label),
            hjust = 0, vjust = -0.4, size = 2.8, colour = "grey40") +

  # Actual ρ marker
  geom_vline(xintercept = 0.07, linetype = "dashed",
             colour = "grey30", linewidth = 0.5) +
  annotate("text", x = 0.09, y = max(vol_curve_df$sig_spread) * 0.95,
           label = "actual ρ=0.07", hjust = 0, size = 3, colour = "grey30") +

  scale_colour_manual(
    values = c("Spread Vol σ(ρ)" = "#457B9D",
               "E[MaxDD] Magdon-Ismail" = "#D90429"),
    name = NULL
  ) +
  scale_x_continuous(breaks = seq(-1, 1, 0.25)) +
  scale_y_continuous(labels = percent_format(scale = 1, accuracy = 1)) +
  labs(
    title    = "Analytical: Spread Vol and E[MaxDD] as a Function of ρ",
    subtitle = "For Long GLD / Short XLK  |  Dotted = |σ₁-σ₂|, √(σ₁²+σ₂²), σ₁+σ₂ thresholds",
    x = "Correlation ρ", y = "Annualised %"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(colour = "grey50", size = 8)
  )

# ── Combined ──────────────────────────────────────────────────────────────────
combined <- (p1 / p2) +
  plot_annotation(
    title    = "How Correlation ρ Drives L/S Spread Risk",
    subtitle = "Higher ρ → spread vol collapses to |σ₁-σ₂| → MaxDD shrinks dramatically",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(colour = "grey50", size = 9)
    )
  )

print(combined)
invisible(list(p_boxplot = p1, p_curve = p2, combined = combined, mc_df = mc_df))
