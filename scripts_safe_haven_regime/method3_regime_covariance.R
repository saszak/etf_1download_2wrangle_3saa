################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : scripts_safe_haven_regime/method3_regime_covariance.R
# Purpose : Method 3 — Regime-Difference Covariance Eigenanalysis
#
# NORTH STAR: SPY regimes (Fall / Recovery / Consolidation, t_fall=10%)
#
# QUESTION: Which linear combinations of ticker returns change their covariance
#           structure most dramatically at regime transitions?
#
# THREE OPERATORS
#   I.  Delta = Sigma_fall - Sigma_normal  (regime-difference covariance)
#         → eigenvectors: directions of maximal regime-structural change
#         → most negative eigenvector  = optimal data-derived safe-haven composite
#         → most positive eigenvector  = optimal risk-dispersal detector
#
#  II.  c_h = Cov(X_t, y_{t+h})           (lagged cross-covariance)
#         → scalar vector: which ticker return best leads Fall by h days
#         → no model needed — pure linear algebra
#
# III.  Precision matrix  Sigma^{-1}       (conditional independence)
#         → eigenvectors: directions of maximum *conditional* separation
#         → helps prune redundant features before LASSO
#
# OUTPUTS
#   plot_delta_heatmap()       Sigma_fall, Sigma_normal, Delta side-by-side
#   plot_eigenvectors()        Top +/- eigenvectors of Delta as bar charts
#   plot_composite_series()    Data-derived SH composite vs SPY regimes
#   plot_lagged_xcov()         Lagged cross-covariance vs horizon h
#   plot_precision_heatmap()   Precision matrix structure
################################################################################

library(MASS)          # load BEFORE tidyverse — prevents MASS::select() masking dplyr::select()
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
MASTER    <- "SPY"
T_FALL    <- 0.10
T_CRUISE  <- 0.05
HORIZON   <- 63        # ~3 months (trading days) — matches Method 2
MAX_LAG   <- 120       # max lag for cross-covariance sweep

# ── Sensitivity groups (mirror Method 1 & 2) ─────────────────────────────────
groups <- list(
  Credit     = "HYG",
  Equities   = c("XLF", "XLY", "XLK"),
  FX         = c("FXF", "FXY", "EMLC"),
  Safe_Haven = c("GLD", "IEF", "TLT", "UUP")
)
all_tickers <- unlist(groups)

# Group colour palette (one colour per group, recycled to tickers)
group_colours <- c(
  Credit     = "#E63946",
  Equities   = "#457B9D",
  FX         = "#F4A261",
  Safe_Haven = "#2D6A4F"
)
ticker_colours <- setNames(
  rep(group_colours, lengths(groups)),
  all_tickers
)

# ── Regime labels ─────────────────────────────────────────────────────────────
spy_ret <- xts_ret[, MASTER]
rt      <- build_regime_table(spy_ret, T_FALL, T_CRUISE)

regime_daily <- rt %>%
  rowwise() %>%
  mutate(date = list(seq(as.Date(xmin), as.Date(xmax), by = "day"))) %>%
  unnest(date) %>%
  dplyr::select(date, regime) %>%
  ungroup()

# ── Return matrix (tickers only, aligned dates) ───────────────────────────────
ret_mat   <- as.matrix(xts_ret[, all_tickers])
ret_dates <- as.Date(index(xts_ret))

# Regime membership by date
fall_dates   <- regime_daily %>% filter(regime == "Fall")   %>% pull(date)
normal_dates <- regime_daily %>% filter(regime != "Fall")   %>% pull(date)

idx_fall   <- ret_dates %in% fall_dates
idx_normal <- ret_dates %in% normal_dates

cat(sprintf("Fall observations   : %d\n", sum(idx_fall)))
cat(sprintf("Non-Fall observations: %d\n", sum(idx_normal)))

# ==============================================================================
# OPERATOR I — REGIME-DIFFERENCE COVARIANCE  Delta = Sigma_fall - Sigma_normal
# ==============================================================================
Sigma_fall   <- cov(ret_mat[idx_fall,   ], use = "complete.obs")
Sigma_normal <- cov(ret_mat[idx_normal, ], use = "complete.obs")
Delta        <- Sigma_fall - Sigma_normal

eig <- eigen(Delta)   # eigenvalues sorted descending

cat(sprintf("\nDelta eigenvalues (top 3 positive): %s\n",
            paste(round(eig$values[1:3],  4), collapse = ", ")))
cat(sprintf("Delta eigenvalues (top 3 negative): %s\n",
            paste(round(tail(eig$values, 3), 4), collapse = ", ")))

# Eigenvectors of interest
n_tickers  <- length(all_tickers)
ev_pos_idx <- which.max(eig$values)            # most positive eigenvalue
ev_neg_idx <- which.min(eig$values)            # most negative eigenvalue

ev_pos <- eig$vectors[, ev_pos_idx]            # risk-dispersal direction
ev_neg <- eig$vectors[, ev_neg_idx]            # safe-haven convergence direction

# Sign convention: safe-haven composite should load positively on GLD/TLT/IEF
sh_tickers  <- groups$Safe_Haven
sign_correct <- sign(mean(ev_neg[all_tickers %in% sh_tickers]))
ev_neg_signed <- ev_neg * sign_correct

# ── Data-derived SH composite: project returns onto ev_neg ───────────────────
sh_composite_derived <- as.numeric(ret_mat %*% ev_neg_signed)
sh_composite_xts     <- xts(sh_composite_derived, order.by = ret_dates)

# Rolling 60-day z-score of the composite
sh_roll_mu  <- as.numeric(rollapply(sh_composite_xts, 60, mean, fill = NA, align = "right"))
sh_roll_sd  <- as.numeric(rollapply(sh_composite_xts, 60, sd,   fill = NA, align = "right"))
sh_z        <- (sh_composite_derived - sh_roll_mu) / sh_roll_sd

# ==============================================================================
# OPERATOR II — LAGGED CROSS-COVARIANCE  c_h = Cov(X_t, y_{t+h})
# ==============================================================================
# y_{t+h} = 1 if SPY in Fall at t+h
y_fall <- as.integer(ret_dates %in% fall_dates)   # contemporaneous flag

xcov_df <- map_dfr(seq(0, MAX_LAG, by = 5), function(h) {
  # Features at time t, target at time t+h  →  shift y back by h
  y_lagged <- c(y_fall[(h + 1):length(y_fall)], rep(NA, h))
  keep      <- !is.na(y_lagged)
  map_dfr(seq_along(all_tickers), function(i) {
    tibble(
      lag    = h,
      ticker = all_tickers[i],
      xcov   = cov(ret_mat[keep, i], y_lagged[keep])
    )
  })
})

# ==============================================================================
# OPERATOR III — PRECISION MATRIX  Sigma^{-1}
# ==============================================================================
# Use full-sample covariance; regularise with small ridge if near-singular
Sigma_full <- cov(ret_mat, use = "complete.obs")
ridge_eps  <- 1e-6 * mean(diag(Sigma_full))
Precision  <- solve(Sigma_full + diag(ridge_eps, n_tickers))
rownames(Precision) <- colnames(Precision) <- all_tickers

# ==============================================================================
# PLOTS
# ==============================================================================

# ── Helper: covariance matrix → long tibble ───────────────────────────────────
cov_to_long <- function(mat, label) {
  as_tibble(mat, rownames = "row") %>%
    pivot_longer(-row, names_to = "col", values_to = "value") %>%
    mutate(panel = label,
           row   = factor(row, levels = all_tickers),
           col   = factor(col, levels = all_tickers))
}

# ── PLOT 1: Sigma_fall | Sigma_normal | Delta heatmaps ────────────────────────
plot_delta_heatmap <- function() {

  long_df <- bind_rows(
    cov_to_long(Sigma_fall,   "Sigma[Fall]"),
    cov_to_long(Sigma_normal, "Sigma[Normal]"),
    cov_to_long(Delta,        "Delta == Sigma[Fall] - Sigma[Normal]")
  )

  # Symmetric colour limit per panel
  lim_df <- long_df %>%
    group_by(panel) %>%
    summarise(lim = max(abs(value)), .groups = "drop")

  long_df <- long_df %>% left_join(lim_df, by = "panel")

  ggplot(long_df, aes(x = col, y = fct_rev(row), fill = value)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    facet_wrap(~ panel, ncol = 3, labeller = label_parsed) +
    scale_fill_gradient2(
      low      = "#2D6A4F",
      mid      = "white",
      high     = "#D90429",
      midpoint = 0,
      labels   = scales::label_scientific(digits = 2)
    ) +
    labs(
      title    = "Covariance Structure: Fall vs Normal vs Difference",
      subtitle = "Green = more negative in Delta (converge in Fall)  |  Red = more dispersed in Fall",
      x = NULL, y = NULL, fill = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y      = element_text(size = 8),
      strip.text       = element_text(face = "bold", size = 9),
      panel.grid       = element_blank(),
      legend.position  = "bottom",
      legend.key.width = unit(2, "cm"),
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(colour = "grey50", size = 9)
    )
}

# ── PLOT 2: Top eigenvectors of Delta ─────────────────────────────────────────
plot_eigenvectors <- function(n_show = 3) {

  # Build a tidy df for top-n positive and top-n negative eigenvalues
  pos_idx <- order(eig$values, decreasing = TRUE)[seq_len(n_show)]
  neg_idx <- order(eig$values, decreasing = FALSE)[seq_len(n_show)]
  idx     <- unique(c(pos_idx, neg_idx))

  ev_df <- map_dfr(idx, function(i) {
    lam  <- eig$values[i]
    sign <- if_else(lam > 0, "Dispersal (+λ)", "Safe-Haven Convergence (−λ)")
    tibble(
      ticker   = all_tickers,
      loading  = eig$vectors[, i],
      lambda   = round(lam, 5),
      ev_label = sprintf("EV %d  λ=%.4f", i, lam),
      type     = sign,
      group    = rep(names(groups), lengths(groups))
    )
  }) %>%
    mutate(
      ticker   = factor(ticker, levels = all_tickers),
      ev_label = fct_reorder(ev_label, lambda)
    )

  ggplot(ev_df, aes(x = ticker, y = loading, fill = group)) +
    geom_col(width = 0.7) +
    geom_hline(yintercept = 0, colour = "grey30") +
    facet_wrap(~ ev_label, ncol = 2,
               labeller = labeller(ev_label = label_value)) +
    scale_fill_manual(values = group_colours) +
    labs(
      title    = "Eigenvectors of Delta = Sigma_Fall - Sigma_Normal",
      subtitle = "Negative-eigenvalue vectors → safe-haven convergence directions  |  Positive → risk dispersal",
      x = NULL, y = "Loading", fill = "Group"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold", size = 9),
      legend.position  = "bottom",
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(colour = "grey50", size = 9)
    )
}

# ── PLOT 3: Data-derived SH composite z-score vs SPY regimes ─────────────────
plot_composite_series <- function() {

  shade_fall <- rt %>% filter(regime == "Fall")
  shade_rec  <- rt %>% filter(regime == "Recovery")

  composite_df <- tibble(
    date = ret_dates,
    z    = sh_z
  ) %>% drop_na()

  ggplot(composite_df, aes(x = date, y = z)) +

    geom_rect(data = shade_fall,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = "#D90429", alpha = 0.12, inherit.aes = FALSE) +
    geom_rect(data = shade_rec,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = "#F4A261", alpha = 0.10, inherit.aes = FALSE) +

    geom_hline(yintercept = c(-1, 0, 1), linetype = "dashed",
               colour = c("grey60", "grey30", "grey60"), linewidth = 0.4) +
    geom_line(colour = "#2D6A4F", linewidth = 0.7) +
    geom_ribbon(aes(ymin = 0, ymax = z), fill = "#2D6A4F", alpha = 0.15) +

    scale_x_date(expand = expansion(mult = 0.01)) +
    scale_y_continuous(breaks = seq(-4, 4, 1)) +
    labs(
      title    = "Data-Derived Safe-Haven Composite (Delta eigenvector)",
      subtitle = paste0(
        "Weights: ", paste(sprintf("%s=%.2f", all_tickers, ev_neg_signed), collapse = " | "),
        "\nRed shading = SPY Fall  |  Orange = Recovery  |  Series = 60d rolling z-score"
      ),
      x = NULL, y = "Z-score"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(colour = "grey50", size = 8)
    )
}

# ── PLOT 4: Lagged cross-covariance sweep ─────────────────────────────────────
plot_lagged_xcov <- function() {

  xcov_df %>%
    mutate(
      group  = rep(names(groups), lengths(groups))[match(ticker, all_tickers)],
      ticker = factor(ticker, levels = all_tickers)
    ) %>%
    ggplot(aes(x = lag, y = xcov, colour = group, group = ticker)) +
    geom_hline(yintercept = 0, colour = "grey40") +
    geom_vline(xintercept = HORIZON, linetype = "dashed",
               colour = "grey50", linewidth = 0.5) +
    geom_line(linewidth = 0.7, alpha = 0.85) +
    annotate("text", x = HORIZON + 2, y = Inf, label = sprintf("h=%d", HORIZON),
             hjust = 0, vjust = 1.5, size = 3, colour = "grey40") +
    facet_wrap(~ group, ncol = 2, scales = "free_y") +
    scale_colour_manual(values = group_colours, guide = "none") +
    scale_x_continuous(breaks = seq(0, MAX_LAG, 20)) +
    labs(
      title    = sprintf("Lagged Cross-Covariance: Cov(ticker_t, Fall_{t+h})"),
      subtitle = "How strongly does each ticker return today predict SPY Fall h days ahead?\nDashed = 63-day horizon used in Method 2",
      x = "Lead horizon h (trading days)", y = "Cross-covariance"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold", size = 9),
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(colour = "grey50", size = 9)
    )
}

# ── PLOT 5: Precision matrix heatmap ──────────────────────────────────────────
plot_precision_heatmap <- function() {

  prec_long <- as_tibble(Precision, rownames = "row") %>%
    pivot_longer(-row, names_to = "col", values_to = "value") %>%
    mutate(
      row = factor(row, levels = all_tickers),
      col = factor(col, levels = all_tickers)
    )

  lim <- max(abs(prec_long$value[prec_long$row != prec_long$col]))

  ggplot(prec_long, aes(x = col, y = fct_rev(row), fill = value)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    geom_text(
      data = prec_long %>% filter(row == col),
      aes(label = round(value, 0)), size = 2.5, colour = "grey20"
    ) +
    scale_fill_gradient2(
      low      = "#2D6A4F",
      mid      = "white",
      high     = "#D90429",
      midpoint = 0,
      limits   = c(-lim, lim),
      oob      = scales::squish,
      labels   = scales::label_scientific(digits = 1)
    ) +
    labs(
      title    = "Precision Matrix  Sigma^{-1}  (Conditional Independence)",
      subtitle = "Off-diagonal red = conditionally positively linked  |  Green = conditionally negatively linked\nDiagonal shows partial variance (clamped for colour scale)",
      x = NULL, y = NULL, fill = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y      = element_text(size = 8),
      panel.grid       = element_blank(),
      legend.position  = "bottom",
      legend.key.width = unit(2, "cm"),
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(colour = "grey50", size = 9)
    )
}

# ── PLOT 6: Eigenspectrum of Delta ────────────────────────────────────────────
plot_eigenspectrum <- function() {

  spec_df <- tibble(
    index    = seq_along(eig$values),
    lambda   = eig$values,
    sign_lab = if_else(lambda >= 0, "Dispersal (+)", "Safe-Haven (−)")
  )

  ggplot(spec_df, aes(x = index, y = lambda, fill = sign_lab)) +
    geom_col(width = 0.7) +
    geom_hline(yintercept = 0, colour = "grey30") +
    scale_fill_manual(values = c("Dispersal (+)" = "#D90429",
                                  "Safe-Haven (−)" = "#2D6A4F")) +
    scale_x_continuous(breaks = seq_along(eig$values),
                       labels = sprintf("EV%d", seq_along(eig$values))) +
    labs(
      title    = "Eigenspectrum of Delta",
      subtitle = "Positive eigenvalues → directions more volatile in Fall (risk dispersal)\nNegative eigenvalues → directions that converge in Fall (safe-haven clustering)",
      x = NULL, y = "Eigenvalue", fill = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(colour = "grey50", size = 9)
    )
}

# ==============================================================================
# SUMMARY TABLE — derived composite weights vs hand-crafted sh_composite
# ==============================================================================
weight_comparison <- tibble(
  ticker           = all_tickers,
  group            = rep(names(groups), lengths(groups)),
  derived_weight   = round(ev_neg_signed, 4),
  hand_crafted     = if_else(all_tickers %in% c("GLD", "IEF", "TLT"), 1/3, 0)
) %>%
  arrange(desc(abs(derived_weight)))

cat("\n── Weight comparison: derived eigenvector vs hand-crafted sh_composite ──\n")
print(weight_comparison)

# ==============================================================================
# VERIFICATION PLOT — XLK/XLF spread vs hand-crafted sh_composite
#
# CLAIM TO TEST: the most regime-sensitive convergence signal is the
#   Tech vs Financials spread (XLK - XLF), NOT bonds + gold surging.
#
# METHOD:
#   - Compute 60d momentum z-score for XLK, XLF, GLD, IEF, TLT
#   - Signal A: XLK_z - XLF_z  (eigenvector-dominant spread)
#   - Signal B: (GLD_z + IEF_z + TLT_z) / 3  (hand-crafted sh_composite)
#   - Panel 1 & 2: time series of each vs SPY Fall shading
#   - Panel 3: regime-conditional distributions — does Signal A separate
#              Fall vs Normal better than Signal B?
# ==============================================================================

roll_mom_vec <- function(tk, win)
  as.numeric(rollapply(xts_ret[, tk], win,
    FUN = function(r) prod(1 + r) - 1,
    align = "right", fill = NA))

roll_z_vec <- function(x, win) {
  mu  <- as.numeric(rollapply(x, win, mean, na.rm = TRUE, align = "right", fill = NA))
  sig <- as.numeric(rollapply(x, win, sd,   na.rm = TRUE, align = "right", fill = NA))
  (x - mu) / sig
}

xlk_z  <- roll_z_vec(roll_mom_vec("XLK", 60), 120)
xlf_z  <- roll_z_vec(roll_mom_vec("XLF", 60), 120)
gld_z  <- roll_z_vec(roll_mom_vec("GLD", 60), 120)
ief_z  <- roll_z_vec(roll_mom_vec("IEF", 60), 120)
tlt_z  <- roll_z_vec(roll_mom_vec("TLT", 60), 120)

signal_df <- tibble(
  date      = ret_dates,
  sig_a     = xlk_z - xlf_z,                    # Tech vs Financials spread
  sig_b     = (gld_z + ief_z + tlt_z) / 3,      # hand-crafted sh_composite
  xlk_z     = xlk_z,
  xlf_z     = xlf_z
) %>%
  left_join(regime_daily, by = "date") %>%
  drop_na()

plot_verification <- function() {

  shade_fall <- rt %>% filter(regime == "Fall")
  shade_rec  <- rt %>% filter(regime == "Recovery")

  # ── Shared shading layers ──────────────────────────────────────────────────
  shading <- list(
    geom_rect(data = shade_fall,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = "#D90429", alpha = 0.12, inherit.aes = FALSE),
    geom_rect(data = shade_rec,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = "#F4A261", alpha = 0.10, inherit.aes = FALSE),
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40",
               linewidth = 0.4)
  )

  base_theme <- theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 12),
      plot.subtitle    = element_text(colour = "grey50", size = 8),
      axis.title.x     = element_blank()
    )

  # ── Panel 1: Signal A — XLK_z minus XLF_z ─────────────────────────────────
  p1 <- ggplot(signal_df, aes(x = date, y = sig_a)) +
    shading +
    geom_ribbon(aes(ymin = 0, ymax = sig_a), fill = "#457B9D", alpha = 0.20) +
    geom_line(colour = "#457B9D", linewidth = 0.7) +
    scale_y_continuous(breaks = seq(-4, 4, 1)) +
    labs(
      title    = "Signal A: XLK_z − XLF_z  (Tech vs Financials spread)",
      subtitle = "Eigenvector-dominant term | High = Tech outperforming Financials | Drops sharply into Fall"
    ) +
    base_theme

  # ── Panel 2: Signal B — hand-crafted sh_composite ─────────────────────────
  p2 <- ggplot(signal_df, aes(x = date, y = sig_b)) +
    shading +
    geom_ribbon(aes(ymin = 0, ymax = sig_b), fill = "#2D6A4F", alpha = 0.20) +
    geom_line(colour = "#2D6A4F", linewidth = 0.7) +
    scale_y_continuous(breaks = seq(-4, 4, 1)) +
    labs(
      title    = "Signal B: (GLD_z + IEF_z + TLT_z) / 3  (hand-crafted sh_composite)",
      subtitle = "Traditional safe-haven composite | Spikes during Fall but also during non-Fall stress episodes"
    ) +
    base_theme

  # ── Panel 3: XLK and XLF individually — see them diverge ─────────────────
  p3 <- signal_df %>%
    dplyr::select(date, XLK = xlk_z, XLF = xlf_z) %>%
    pivot_longer(-date, names_to = "ticker", values_to = "z") %>%
    ggplot(aes(x = date, y = z, colour = ticker)) +
    shading +
    geom_line(linewidth = 0.65, alpha = 0.9) +
    scale_colour_manual(values = c(XLK = "#1D3557", XLF = "#E63946")) +
    scale_y_continuous(breaks = seq(-4, 4, 1)) +
    labs(
      title    = "XLK and XLF individually — the divergence during Fall",
      subtitle = "In Fall: XLF (red) collapses faster/deeper than XLK (navy) → spread widens",
      colour   = NULL
    ) +
    base_theme +
    theme(legend.position = "bottom")

  # ── Panel 4: Regime-conditional distributions ─────────────────────────────
  dist_df <- signal_df %>%
    filter(!is.na(regime)) %>%
    mutate(regime = factor(regime, levels = c("Fall", "Recovery", "Consolidation"))) %>%
    dplyr::select(regime, `A: XLK-XLF` = sig_a, `B: sh_composite` = sig_b) %>%
    pivot_longer(-regime, names_to = "signal", values_to = "value")

  # Compute regime means per signal for annotation
  means_df <- dist_df %>%
    group_by(signal, regime) %>%
    summarise(m = mean(value, na.rm = TRUE), .groups = "drop")

  p4 <- ggplot(dist_df, aes(x = regime, y = value, fill = regime)) +
    geom_violin(alpha = 0.35, colour = NA, trim = TRUE) +
    geom_boxplot(width = 0.22, outlier.size = 0.5, alpha = 0.8) +
    geom_text(data = means_df,
              aes(x = regime, y = m, label = sprintf("μ=%.2f", m)),
              vjust = -0.6, size = 3, fontface = "bold", inherit.aes = FALSE) +
    facet_wrap(~ signal, ncol = 2) +
    scale_fill_manual(values = c(Fall         = "#D90429",
                                  Recovery     = "#F4A261",
                                  Consolidation = "#457B9D"),
                      guide = "none") +
    labs(
      title    = "Regime-Conditional Distributions: Signal A vs Signal B",
      subtitle = "The signal that separates Fall (red) from others most cleanly is the stronger regime indicator",
      x = NULL, y = "Z-score"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold", size = 10),
      plot.title       = element_text(face = "bold", size = 12),
      plot.subtitle    = element_text(colour = "grey50", size = 8)
    )

  # ── Assemble with patchwork ────────────────────────────────────────────────
  (p1 / p2 / p3 / p4) +
    plot_annotation(
      title   = "Verification: Is XLK/XLF spread a better regime signal than GLD+IEF+TLT?",
      subtitle = "Red shading = SPY Fall  |  Orange = Recovery  |  Panel 4 is the statistical verdict",
      theme = theme(
        plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(colour = "grey50", size = 9)
      )
    )
}

# ==============================================================================
# RUN ALL
# ==============================================================================
print(plot_delta_heatmap())
print(plot_eigenvectors())
print(plot_composite_series())
print(plot_lagged_xcov())
print(plot_precision_heatmap())
print(plot_eigenspectrum())
print(plot_verification())

################################################################################
