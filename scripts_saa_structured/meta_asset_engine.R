# ==============================================================================
# META-ASSET ENGINE — Regime-Aware Pair Scoring (Sector ETF Universe)
# ==============================================================================
#
# IDEA: Combine m=2 sector ETFs into a synthetic "meta-asset" that acts as a
# building block for higher-level portfolio construction.
#
# APPROACH B — Complementarity-based:
#   A good pair has components whose REGIME ALPHAS complement each other:
#   one protects in Fall, the other participates in Consolidation.
#   The blend inherits smoothed regime exposure.
#
# SCORING (per pair, at optimal blend weight):
#   S1 — Blend Sharpe ratio
#   S2 — MaxDD improvement vs SPY (blend DD < SPY DD)
#   S3 — Regime complementarity: ρ(alpha_i, alpha_j) across regime episodes
#        (negative = they offset each other = good)
#   S4 — Blend regime alpha variance (low = smoother across regimes = good)
#
# OUTPUT:
#   - score_tbl    : full pair × weight scoring table
#   - best_pairs   : top pair per (i,j) at optimal weight
#   - plot_heatmap : pair scoring heatmap
#   - plot_ranked  : top 15 pairs ranked by composite score
#   - plot_regime_profile : regime alpha bars for top 6 pairs
# ==============================================================================

library(tidyverse)
library(xts)
library(PerformanceAnalytics)
library(scales)
library(ggrepel)
library(patchwork)
library(here)

source(here("scripts/00_init_universe.R"))
source(here("scripts_spy_dd_regime/spy_dd_regime.R"))
source(here("scripts_spy_dd_regime/spy_dd_regime_rel.R"))

xts_ret <- readRDS(here("02_data_processed/xts_ret_returns.rds"))

# ── Parameters ────────────────────────────────────────────────────────────────
MASTER       <- "SPY"
T_FALL       <- 0.10
RF           <- 0.045
WEIGHT_GRID  <- seq(0.10, 0.90, by = 0.10)   # weight on asset 1 in the pair

SECTOR_TICKERS <- c("XLK", "XLC", "XLV", "XLF", "XLI",
                     "XLP", "XLY", "XLE", "XLB", "XLRE",
                     "XLU", "VHT", "ITA", "ITB", "KRE")

# ── Build regime table ────────────────────────────────────────────────────────
spy_ret <- xts_ret[, MASTER]
rt      <- build_regime_table(spy_ret, T_FALL)

# ── Per-regime alpha helper ───────────────────────────────────────────────────
# Returns a named vector: avg alpha per regime type (Fall / Recovery / Consolidation)
regime_alphas <- function(ret_series, spy_ret, rt) {
  map_dfr(seq_len(nrow(rt)), function(i) {
    ep   <- rt[i, ]
    idx  <- paste0(ep$xmin, "/", ep$xmax)
    r_tk <- ret_series[idx]
    r_sp <- spy_ret[idx]
    if (length(r_tk) < 5) return(NULL)
    tibble(
      regime = as.character(ep$regime),
      alpha  = as.numeric(Return.cumulative(r_tk)) -
               as.numeric(Return.cumulative(r_sp))
    )
  }) %>%
    group_by(regime) %>%
    summarise(avg_alpha = mean(alpha, na.rm = TRUE), .groups = "drop")
}

# ── MaxDD helper ──────────────────────────────────────────────────────────────
max_dd <- function(ret_vec) {
  w <- cumprod(1 + ret_vec)
  min((w - cummax(w)) / cummax(w), na.rm = TRUE)
}

# SPY reference MaxDD
spy_vec   <- as.numeric(na.omit(xts_ret[, MASTER]))
spy_mdd   <- max_dd(spy_vec)
spy_mu    <- mean(spy_vec) * 252
spy_sig   <- sd(spy_vec)   * sqrt(252)
spy_sharpe <- (spy_mu - RF) / spy_sig

# ── Pre-compute per-ticker regime alphas ──────────────────────────────────────
cat("Computing per-ticker regime alphas...\n")
ticker_regime_alphas <- map(SECTOR_TICKERS, function(tk) {
  regime_alphas(xts_ret[, tk], spy_ret, rt)
}) %>% setNames(SECTOR_TICKERS)

# ── Score all pairs at all weights ────────────────────────────────────────────
cat("Scoring all pairs...\n")

pairs_raw <- combn(SECTOR_TICKERS, 2, simplify = FALSE)

score_tbl <- map_dfr(pairs_raw, function(pair) {
  tk1 <- pair[1]; tk2 <- pair[2]

  r1 <- as.numeric(na.omit(xts_ret[, tk1]))
  r2 <- as.numeric(na.omit(xts_ret[, tk2]))
  rs <- as.numeric(na.omit(spy_ret))
  n  <- min(length(r1), length(r2), length(rs))
  r1 <- tail(r1, n); r2 <- tail(r2, n); rs <- tail(rs, n)

  rho_pair <- cor(r1, r2)

  # Regime alpha correlation across episodes (complementarity)
  a1 <- ticker_regime_alphas[[tk1]]
  a2 <- ticker_regime_alphas[[tk2]]
  alpha_both <- inner_join(a1, a2, by = "regime", suffix = c("_1", "_2"))
  regime_corr <- if (nrow(alpha_both) >= 2)
    cor(alpha_both$avg_alpha_1, alpha_both$avg_alpha_2) else NA_real_

  map_dfr(WEIGHT_GRID, function(w) {
    blend <- w * r1 + (1 - w) * r2
    mu_b  <- mean(blend) * 252
    sig_b <- sd(blend)   * sqrt(252)
    mdd_b <- max_dd(blend)
    sharpe_b <- (mu_b - RF) / sig_b

    # Blend regime alphas across all episodes
    blend_xts   <- xts(blend, order.by = tail(index(spy_ret), n))
    ba           <- regime_alphas(blend_xts, spy_ret, rt)
    alpha_var    <- var(ba$avg_alpha, na.rm = TRUE)
    fall_alpha   <- ba$avg_alpha[ba$regime == "Fall"]
    consol_alpha <- ba$avg_alpha[ba$regime == "Consolidation"]

    tibble(
      tk1          = tk1,
      tk2          = tk2,
      w1           = w,
      w2           = 1 - w,
      rho_pair     = rho_pair,
      sharpe_blend = sharpe_b,
      mdd_blend    = mdd_b,
      dd_delta     = mdd_b - spy_mdd,     # positive = blend less severe than SPY
      mu_blend     = mu_b,
      sig_blend    = sig_b,
      regime_corr  = regime_corr,         # neg = complementary
      alpha_var    = alpha_var,           # low = smooth across regimes
      fall_alpha   = if (length(fall_alpha))   fall_alpha   else NA_real_,
      consol_alpha = if (length(consol_alpha)) consol_alpha else NA_real_
    )
  })
})

# ── Composite score ───────────────────────────────────────────────────────────
# Normalise each component to [0, 1] then weight:
#   40% Sharpe  |  25% DD improvement  |  25% Regime complementarity  |  10% Alpha smoothness

score_tbl <- score_tbl %>%
  mutate(
    # normalise (higher = better for all after sign flip where needed)
    z_sharpe  = (sharpe_blend - min(sharpe_blend, na.rm=TRUE)) /
                (max(sharpe_blend, na.rm=TRUE) - min(sharpe_blend, na.rm=TRUE)),
    z_dd      = (dd_delta - min(dd_delta, na.rm=TRUE)) /
                (max(dd_delta, na.rm=TRUE) - min(dd_delta, na.rm=TRUE)),
    z_compl   = (-regime_corr - min(-regime_corr, na.rm=TRUE)) /
                (max(-regime_corr, na.rm=TRUE) - min(-regime_corr, na.rm=TRUE)),
    z_smooth  = (-alpha_var - min(-alpha_var, na.rm=TRUE)) /
                (max(-alpha_var, na.rm=TRUE) - min(-alpha_var, na.rm=TRUE)),
    composite = 0.40 * z_sharpe + 0.25 * z_dd + 0.25 * z_compl + 0.10 * z_smooth
  )

# ── Best weight per pair ───────────────────────────────────────────────────────
best_pairs <- score_tbl %>%
  group_by(tk1, tk2) %>%
  slice_max(composite, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    pair_label  = paste0(tk1, " + ", tk2),
    weight_label = paste0(round(w1 * 100), "/", round(w2 * 100))
  ) %>%
  arrange(desc(composite))

cat(sprintf("\nTop 10 meta-asset pairs:\n"))
best_pairs %>%
  dplyr::select(pair_label, weight_label, sharpe_blend, mdd_blend,
                regime_corr, composite) %>%
  mutate(across(c(sharpe_blend, regime_corr, composite), ~ round(.x, 3)),
         mdd_blend = percent(mdd_blend, accuracy = 0.1)) %>%
  print(n = 10)

# ── PLOT 1: Scoring heatmap ───────────────────────────────────────────────────
# Upper triangle: composite score at optimal weight
heatmap_df <- best_pairs %>%
  dplyr::select(tk1, tk2, composite, weight_label) %>%
  bind_rows(
    best_pairs %>% dplyr::rename(tk1 = tk2, tk2 = tk1)
  ) %>%
  mutate(
    tk1 = factor(tk1, levels = SECTOR_TICKERS),
    tk2 = factor(tk2, levels = SECTOR_TICKERS)
  )

p_heatmap <- ggplot(heatmap_df, aes(x = tk1, y = tk2, fill = composite)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = weight_label), size = 2.8, colour = "white", fontface = "bold") +
  scale_fill_gradient2(
    low = "#d73027", mid = "#ffffbf", high = "#1a1a6e",
    midpoint = 0.5, name = "Composite\nScore", limits = c(0, 1)
  ) +
  scale_x_discrete(position = "top") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x    = element_text(angle = 45, hjust = 0, face = "bold", size = 10),
    axis.text.y    = element_text(face = "bold", size = 10),
    axis.title     = element_blank(),
    panel.grid     = element_blank(),
    plot.title     = element_text(face = "bold", size = 14),
    plot.subtitle  = element_text(size = 10, colour = "grey40"),
    legend.position = "right"
  ) +
  labs(
    title    = "Meta-Asset Pair Scoring Heatmap — Sector ETF Universe",
    subtitle = "Colour = composite score (Sharpe 40% + DD 25% + Regime complementarity 25% + Smoothness 10%)\nCell label = optimal blend weight (w1/w2)"
  )

# ── PLOT 2: Ranked bar — top 20 pairs ─────────────────────────────────────────
top_n_pairs <- 20

p_ranked <- best_pairs %>%
  slice_head(n = top_n_pairs) %>%
  mutate(
    pair_label = fct_reorder(paste0(tk1, "+", tk2, "\n(", weight_label, ")"), composite),
    sharpe_col = if_else(sharpe_blend > spy_sharpe, "#1a1a6e", "#d73027")
  ) %>%
  ggplot(aes(x = pair_label, y = composite)) +
  geom_col(aes(fill = sharpe_blend > spy_sharpe), alpha = 0.85, width = 0.7) +
  geom_text(aes(label = sprintf("IR=%.2f", sharpe_blend)),
            hjust = -0.1, size = 3, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(
    values = c("TRUE" = "#1a1a6e", "FALSE" = "#d73027"),
    labels = c("TRUE" = paste0("Sharpe > SPY (", round(spy_sharpe, 2), ")"),
               "FALSE" = paste0("Sharpe < SPY (", round(spy_sharpe, 2), ")")),
    name = NULL
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), limits = c(0, 1)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position    = "bottom",
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(size = 10, colour = "grey40")
  ) +
  labs(
    title    = paste0("Top ", top_n_pairs, " Meta-Asset Pairs — Ranked by Composite Score"),
    subtitle = "Label = Information Ratio of the blend  |  Blue = beats SPY Sharpe",
    x = NULL, y = "Composite Score"
  )

# ── PLOT 3: Regime alpha profile for top 6 pairs ──────────────────────────────
top6 <- best_pairs %>% slice_head(n = 6)

regime_profile_df <- map_dfr(seq_len(nrow(top6)), function(i) {
  p   <- top6[i, ]
  r1  <- as.numeric(na.omit(xts_ret[, p$tk1]))
  r2  <- as.numeric(na.omit(xts_ret[, p$tk2]))
  rs  <- as.numeric(na.omit(spy_ret))
  n   <- min(length(r1), length(r2), length(rs))
  r1  <- tail(r1, n); r2 <- tail(r2, n); rs <- tail(rs, n)

  blend     <- p$w1 * r1 + p$w2 * r2
  blend_xts <- xts(blend, order.by = tail(index(spy_ret), n))
  ba        <- regime_alphas(blend_xts, spy_ret, rt)
  a1        <- ticker_regime_alphas[[p$tk1]]
  a2        <- ticker_regime_alphas[[p$tk2]]

  bind_rows(
    ba  %>% mutate(series = paste0(p$tk1, "+", p$tk2, "\n(", p$weight_label, ")")),
    a1  %>% mutate(series = p$tk1),
    a2  %>% mutate(series = p$tk2)
  ) %>%
    mutate(
      pair      = paste0(p$tk1, "+", p$tk2, "\n(", p$weight_label, ")"),
      is_blend  = series == paste0(p$tk1, "+", p$tk2, "\n(", p$weight_label, ")")
    )
}) %>%
  mutate(
    regime = factor(regime, levels = c("Fall", "Recovery", "Consolidation")),
    fill_col = case_when(
      is_blend & avg_alpha >= 0  ~ "#1a1a6e",
      is_blend & avg_alpha <  0  ~ "#d73027",
      !is_blend & avg_alpha >= 0 ~ "#7EB8E8",
      TRUE                       ~ "#fc8d59"
    )
  )

REGIME_PAL <- c(Fall = "#D90429", Recovery = "#F77F00", Consolidation = "#2D6A4F")

p_regime_profile <- ggplot(
    regime_profile_df,
    aes(x = series, y = avg_alpha, fill = fill_col)
  ) +
  geom_col(aes(alpha = is_blend), width = 0.65) +
  geom_hline(yintercept = 0, linewidth = 0.6, colour = "grey30") +
  geom_text(
    aes(label = sprintf("%+.1f%%", avg_alpha * 100),
        vjust = if_else(avg_alpha >= 0, -0.3, 1.2)),
    size = 2.8, fontface = "bold"
  ) +
  scale_fill_identity() +
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.55), guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  facet_grid(pair ~ regime, scales = "free_x") +
  theme_minimal(base_size = 11) +
  theme(
    strip.text.x     = element_text(face = "bold", size = 10,
                                    colour = "white"),
    strip.background.x = element_rect(
      fill = c(Fall = "#D90429", Recovery = "#F77F00", Consolidation = "#2D6A4F"),
      colour = NA
    ),
    strip.text.y     = element_text(face = "bold", size = 9, angle = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x      = element_text(size = 8, face = "bold"),
    axis.title       = element_blank(),
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 10, colour = "grey40")
  ) +
  labs(
    title    = "Regime Alpha Profile — Top 6 Meta-Asset Pairs",
    subtitle = "Dark = blend alpha vs SPY  |  Light = individual component alpha  |  Blend should smooth the regime profile"
  )

# ── PLOT 4: Risk-Return scatter of meta-assets vs individual tickers ──────────
indiv_pts <- tibble(
  label    = SECTOR_TICKERS,
  mu       = map_dbl(SECTOR_TICKERS, ~ mean(as.numeric(na.omit(xts_ret[, .x]))) * 252),
  vol      = map_dbl(SECTOR_TICKERS, ~ sd(as.numeric(na.omit(xts_ret[, .x]))) * sqrt(252)),
  type     = "Individual"
)

blend_pts <- best_pairs %>%
  slice_head(n = 15) %>%
  transmute(
    label = paste0(tk1, "+", tk2),
    mu    = mu_blend,
    vol   = sig_blend,
    type  = "Meta-Asset (top 15)"
  )

spy_pt <- tibble(label = "SPY", mu = spy_mu, vol = spy_sig, type = "Benchmark")

scatter_df <- bind_rows(indiv_pts, blend_pts, spy_pt)

p_scatter <- ggplot(scatter_df, aes(x = vol, y = mu, colour = type, size = type)) +
  geom_point(alpha = 0.80) +
  geom_text_repel(aes(label = label), size = 2.8, max.overlaps = 20,
                  segment.color = "grey70", show.legend = FALSE) +
  geom_hline(yintercept = spy_mu, linetype = "dashed", colour = "grey50", linewidth = 0.6) +
  scale_colour_manual(
    values = c("Individual" = "#4A90D9", "Meta-Asset (top 15)" = "#1a1a6e",
               "Benchmark" = "#D90429"),
    name = NULL
  ) +
  scale_size_manual(
    values = c("Individual" = 2.5, "Meta-Asset (top 15)" = 3.5, "Benchmark" = 4),
    guide  = "none"
  ) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 10, colour = "grey40")
  ) +
  labs(
    title    = "Risk-Return: Individual Sectors vs Top Meta-Asset Blends",
    subtitle = "Meta-assets (dark) should cluster differently from individual sectors",
    x = "Annualised Volatility", y = "Annualised Return"
  )

# ── Auto-run guard ─────────────────────────────────────────────────────────────
if (!isTRUE(getOption("knitr.in.progress"))) {
  print(p_heatmap)
  print(p_ranked)
  print(p_regime_profile)
  print(p_scatter)
}
