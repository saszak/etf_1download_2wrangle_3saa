################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : scripts_safe_haven_regime/method2_logistic_regime.R
# Purpose : Method 2 — Logistic Regression on SPY Regime Transitions
#
# NORTH STAR: SPY regimes (Fall / Recovery / Consolidation, t_fall=10%)
#
# QUESTION: Can sensitivity group signals today predict P(SPY Fall) in 63 days?
#
# FEATURE FAMILIES (creative — simplify after first run)
#   A. Rolling correlation vs SPY (from Method 1)
#   B. Momentum z-scores (20/60/120-day)
#   C. Momentum divergence (ticker vs SPY momentum)
#   D. Volatility regime (ticker vol / SPY vol ratio)
#   E. Cross-asset stress composite (PCA-free risk-off index)
#   F. Credit spread proxy (HYG - IEF spread)
#   G. Regime memory (current duration vs historical avg)
#   H. Safe haven convergence (GLD + IEF + TLT composite z-score)
#
# TARGET
#   Binary: 1 = SPY in Fall regime at t + HORIZON, 0 = otherwise
#   (also compute multinomial for Fall / Recovery / Consolidation)
#
# VALIDATION
#   Time-series walk-forward CV — no lookahead bias
################################################################################

library(tidyverse)
library(xts)
library(PerformanceAnalytics)
library(scales)
library(patchwork)
library(here)
library(glmnet)       # regularised logistic regression
library(pROC)         # ROC / AUC

# ── Dependencies ──────────────────────────────────────────────────────────────
if (!exists("build_regime_table"))
  source(here("scripts_spy_dd_regime/spy_dd_regime.R"))

if (!exists("xts_ret"))
  xts_ret <- readRDS(here("02_data_processed/xts_ret_returns.rds"))

# ── Parameters ────────────────────────────────────────────────────────────────
MASTER      <- "SPY"
T_FALL      <- 0.10
T_CRUISE    <- 0.05
HORIZON     <- 63     # ~3 months (trading days)
ROLL_WIN    <- 60
JUMP_MULT   <- 2.0
CV_FOLDS    <- 5      # walk-forward CV folds

# ── Feature families reference ────────────────────────────────────────────────
#
#   Family                 | Features built                          | Hypothesis
#   -----------------------|-----------------------------------------|------------------------------
#   A. Rolling Corr        | 60d/20d corr vs SPY, Δcorr, jump count | Corr breaks down before Fall
#   B. Momentum Z-score    | 20/60/120d momentum z-scored            | Exhaustion signals regime end
#   C. Mom Divergence      | ticker mom - SPY mom, z-scored          | Safe havens decouple early
#   D. Vol Ratio           | ticker vol / SPY vol, z-scored          | Vol regime shift precedes Fall
#   E. Credit Spread       | HYG - IEF momentum spread               | Credit stress leads equity Fall
#   F. SH Composite        | GLD + IEF + TLT convergence z-score     | Multi-asset flight-to-safety
#   G. Regime Memory       | current duration vs historical avg      | Regimes mean-revert in length
#
# ── Sensitivity groups ────────────────────────────────────────────────────────
groups <- list(
  Credit      = "HYG",
  Equities    = c("XLF", "XLY", "XLK"),
  FX          = c("FXF", "FXY", "EMLC"),
  Safe_Haven  = c("GLD", "IEF", "TLT", "UUP")
)
all_tickers <- unlist(groups)

# ── Ticker economic rationale (rc60 lens) ────────────────────────────────────
#
#  For each ticker, high rc60 = moving tightly WITH SPY.
#  Low / falling rc60 = decoupling from SPY — the "canary" moment.
#
#  CREDIT
#  HYG  (High Yield Corp Bonds)
#    rc60 HIGH  → credit spreads compressed, junk bonds rallying with equities
#                 → "the credit cycle is healthy" — borrowers are fine
#    rc60 DROPS → HYG lags or inverts vs SPY → credit stress building
#                 → credit markets usually LEAD equity drawdowns by weeks
#
#  EQUITIES
#  XLF  (Financials)
#    rc60 HIGH  → banks and brokers in sync with market → lending conditions
#                 normal, yield curve supportive, no balance-sheet stress
#    rc60 DROPS → financials underperforming → credit tightening, yield curve
#                 flattening, or hidden bank stress — classic pre-recession signal
#
#  XLY  (Consumer Discretionary)
#    rc60 HIGH  → consumers still spending, cyclicals leading → "everything is
#                 fine" signal; no stress in household balance sheets
#    rc60 DROPS → XLY decouples → consumer confidence cracking, rotation out
#                 of cyclicals into defensives — early risk-off behaviour
#
#  XLK  (Technology)
#    rc60 HIGH  → growth expectations intact, rate environment benign, tech
#                 multiples holding → broad market participation is healthy
#    rc60 DROPS → tech decouples → often rate-driven (long-duration pain) or
#                 earnings-growth fears; tech tends to LEAD market at turns
#
#  FX
#  FXF  (Swiss Franc ETF)
#    rc60 HIGH  → CHF moving WITH equities → unusual, implies risk-on and
#                 no safe-haven bid for CHF; carry trades intact
#    rc60 DROPS → CHF appreciating vs USD → capital fleeing to Switzerland
#                 → one of the cleanest flight-to-safety signals in FX
#
#  FXY  (Japanese Yen ETF)
#    rc60 HIGH  → JPY moving WITH equities → carry trades (borrow JPY, buy
#                 risk assets) are intact → global risk appetite healthy
#    rc60 DROPS → JPY strengthening → carry unwind → leveraged positions
#                 unwinding globally; historically precedes sharp drawdowns
#
#  EMLC (EM Local Currency Bonds)
#    rc60 HIGH  → EM debt rallying with SPY → global risk appetite strong,
#                 USD stable, EM fundamentals not under pressure
#    rc60 DROPS → EM debt decoupling → USD strengthening, capital flight from
#                 EM, global liquidity tightening → broad risk-off stress signal
#
#  SAFE HAVEN
#  GLD  (Gold)
#    rc60 HIGH  → gold moving WITH equities → inflation / reflation trade;
#                 NOT yet a stress signal — both assets bid for different reasons
#    rc60 DROPS / goes negative → gold decoupling upward from SPY → classic
#                 flight-to-safety; historically activates as Fall approaches
#
#  IEF  (7–10 Yr Treasury ETF)
#    rc60 HIGH  → bonds and equities rising together → "Goldilocks" / falling
#                 rates with growth; unusual and often precedes a reversal
#    rc60 DROPS → Treasuries bid while equities lag → rate cut expectations
#                 building, or risk-off positioning in sovereign bonds
#
#  TLT  (20+ Yr Treasury ETF)
#    rc60 HIGH  → long bonds in sync with SPY → duration risk not being
#                 rewarded yet; risk-on, rates not a concern
#    rc60 DROPS → TLT surging while SPY weakens → the textbook flight-to-
#                 safety trade; most sensitive duration asset in the model
#
#  UUP  (USD Index ETF)
#    rc60 HIGH  → USD moving WITH equities → "dollar smile" risk-on leg;
#                 global growth lifting all boats including the reserve currency
#    rc60 DROPS → USD decoupling → could be USD STRENGTHENING (risk-off
#                 dollar demand) or weakening (inflation/fiscal fears);
#                 direction of the decoupling determines the signal sign
# ─────────────────────────────────────────────────────────────────────────────

# ── Regime labels ─────────────────────────────────────────────────────────────
spy_ret <- xts_ret[, MASTER]
rt      <- build_regime_table(spy_ret, T_FALL, T_CRUISE)

# Daily regime label for every date
regime_daily <- rt %>%
  rowwise() %>%
  mutate(date = list(seq(as.Date(xmin), as.Date(xmax), by = "day"))) %>%
  unnest(date) %>%
  select(date, regime) %>%
  ungroup()

# ── Helper: rolling z-score ───────────────────────────────────────────────────
roll_zscore <- function(x, win) {
  mu  <- as.numeric(rollapply(x, win, mean, na.rm = TRUE, align = "right", fill = NA))
  sig <- as.numeric(rollapply(x, win, sd,   na.rm = TRUE, align = "right", fill = NA))
  (as.numeric(x) - mu) / sig
}

# ── Helper: momentum (cumulative return over window) ──────────────────────────
roll_mom <- function(x, win) {
  as.numeric(rollapply(x, win,
    FUN = function(r) prod(1 + r) - 1,
    align = "right", fill = NA))
}

# ── Helper: rolling correlation vs SPY ───────────────────────────────────────
roll_corr_spy <- function(tk_r, win = ROLL_WIN) {
  as.numeric(rollapply(
    merge(spy_ret, tk_r), width = win,
    FUN = function(m) cor(m[,1], m[,2], use = "complete.obs"),
    by.column = FALSE, align = "right", fill = NA
  ))
}

# ==============================================================================
# FEATURE ENGINEERING
# ==============================================================================
dates <- as.Date(index(spy_ret))

feature_list <- list()

for (tk in all_tickers) {

  tk_r <- as.numeric(xts_ret[, tk])

  # ── A. Rolling correlation vs SPY ─────────────────────────────────────────
  rc60 <- roll_corr_spy(xts_ret[, tk], 60)
  rc20 <- roll_corr_spy(xts_ret[, tk], 20)

  # First derivative of 60d corr
  d1_rc60 <- c(NA, diff(rc60))

  # Jump indicator (|d1| > 2σ)
  d1_sd   <- as.numeric(rollapply(d1_rc60, ROLL_WIN, sd, na.rm = TRUE,
                                   align = "right", fill = NA))
  jump_60 <- as.numeric(abs(d1_rc60) > JUMP_MULT * d1_sd)

  # Cumulative jump count over 20 days
  jump_sum20 <- as.numeric(rollapply(jump_60, 20, sum, na.rm = TRUE,
                                      align = "right", fill = NA))

  # ── B. Momentum z-scores ──────────────────────────────────────────────────
  mom20  <- roll_mom(xts_ret[, tk], 20)
  mom60  <- roll_mom(xts_ret[, tk], 60)
  mom120 <- roll_mom(xts_ret[, tk], 120)

  mz20  <- roll_zscore(mom20,  60)   # z-score of 20d mom over 60d window
  mz60  <- roll_zscore(mom60,  120)
  mz120 <- roll_zscore(mom120, 252)

  # ── C. Momentum divergence (ticker mom - SPY mom) ─────────────────────────
  spy_mom20  <- roll_mom(spy_ret, 20)
  spy_mom60  <- roll_mom(spy_ret, 60)
  div_mom20  <- mom20  - spy_mom20
  div_mom60  <- mom60  - spy_mom60

  # Z-score of divergence
  div_z20 <- roll_zscore(div_mom20, 60)
  div_z60 <- roll_zscore(div_mom60, 120)

  # ── D. Volatility ratio (ticker vol / SPY vol) ────────────────────────────
  vol_tk  <- as.numeric(rollapply(xts_ret[, tk], 20, sd, na.rm = TRUE,
                                   align = "right", fill = NA)) * sqrt(252)
  vol_spy <- as.numeric(rollapply(spy_ret,         20, sd, na.rm = TRUE,
                                   align = "right", fill = NA)) * sqrt(252)
  vol_ratio <- vol_tk / vol_spy

  # Z-score of vol ratio
  vol_ratio_z <- roll_zscore(vol_ratio, 60)

  # ── Store all features for this ticker ────────────────────────────────────
  feature_list[[tk]] <- tibble(
    date          = dates,
    ticker        = tk,
    rc60          = rc60,
    rc20          = rc20,
    d1_rc60       = d1_rc60,
    jump_sum20    = jump_sum20,
    mz20          = mz20,
    mz60          = mz60,
    mz120         = mz120,
    div_z20       = div_z20,
    div_z60       = div_z60,
    vol_ratio_z   = vol_ratio_z
  )
}

features_long <- bind_rows(feature_list)

# ── E. Credit spread proxy: HYG momentum - IEF momentum (daily series) ───────
hyg_mom60 <- roll_mom(xts_ret[, "HYG"], 60)
ief_mom60 <- roll_mom(xts_ret[, "IEF"], 60)
credit_spread_z <- roll_zscore(hyg_mom60 - ief_mom60, 120)

# ── F. Safe haven convergence composite ───────────────────────────────────────
# Average z-score of GLD + IEF + TLT momentum — when all surge together = stress
sh_composite <- (
  roll_zscore(roll_mom(xts_ret[, "GLD"], 60), 120) +
  roll_zscore(roll_mom(xts_ret[, "IEF"], 60), 120) +
  roll_zscore(roll_mom(xts_ret[, "TLT"], 60), 120)
) / 3

# ── G. Regime memory: current regime duration z-score ─────────────────────────
regime_dur_avg <- rt %>%
  group_by(regime) %>%
  summarise(avg_days = mean(days), sd_days = sd(days), .groups = "drop")

regime_dur_daily <- regime_daily %>%
  left_join(
    rt %>% mutate(start = as.Date(xmin)) %>%
      select(start, regime, days),
    by = "regime"
  ) %>%
  group_by(date) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(
    current_dur = as.numeric(date - as.Date(
      rt$xmin[findInterval(date, as.Date(rt$xmin))]
    ))
  ) %>%
  left_join(regime_dur_avg, by = "regime") %>%
  mutate(dur_z = (current_dur - avg_days) / sd_days) %>%
  select(date, regime, dur_z)

# ── Pivot features wide (one row per date) ────────────────────────────────────
features_wide <- features_long %>%
  pivot_wider(
    id_cols     = date,
    names_from  = ticker,
    values_from = c(rc60, rc20, d1_rc60, jump_sum20,
                    mz20, mz60, mz120, div_z20, div_z60, vol_ratio_z),
    names_glue  = "{ticker}_{.value}"
  ) %>%
  mutate(
    credit_spread_z = credit_spread_z,
    sh_composite    = sh_composite
  ) %>%
  left_join(regime_dur_daily %>% select(date, dur_z), by = "date")

# ── Build model dataset ───────────────────────────────────────────────────────
# Target: is SPY in Fall regime HORIZON days ahead?
target_df <- regime_daily %>%
  mutate(
    future_date  = date - HORIZON,   # lag: feature at t, target at t+HORIZON
    in_fall      = as.integer(regime == "Fall")
  ) %>%
  select(future_date, in_fall) %>%
  rename(date = future_date)

model_df <- features_wide %>%
  inner_join(target_df, by = "date") %>%
  drop_na() %>%
  arrange(date)

cat(sprintf("Model dataset: %d rows | %d features | %.1f%% Fall observations\n",
            nrow(model_df),
            ncol(model_df) - 2,   # minus date + target
            mean(model_df$in_fall) * 100))

# ==============================================================================
# LOGISTIC REGRESSION — LASSO regularised (glmnet)
# ==============================================================================
X <- model_df %>% select(-date, -in_fall) %>% as.matrix()
y <- model_df$in_fall
dates_model <- model_df$date

# ── Walk-forward cross-validation ─────────────────────────────────────────────
n       <- nrow(X)
fold_sz <- floor(n / (CV_FOLDS + 1))

cv_results <- map_dfr(seq_len(CV_FOLDS), function(k) {
  train_end <- k * fold_sz
  test_idx  <- (train_end + 1):min(train_end + fold_sz, n)

  X_train <- X[1:train_end, ]
  y_train <- y[1:train_end]
  X_test  <- X[test_idx, ]
  y_test  <- y[test_idx]

  # LASSO with cross-validated lambda
  cv_fit  <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 1,
                        nfolds = 5, type.measure = "auc")
  pred    <- as.numeric(predict(cv_fit, X_test, s = "lambda.min", type = "response"))

  tibble(
    date     = dates_model[test_idx],
    prob     = pred,
    actual   = y_test,
    fold     = k
  )
})

# ── Full model for coefficients ────────────────────────────────────────────────
cv_full  <- cv.glmnet(X, y, family = "binomial", alpha = 1,
                       nfolds = 10, type.measure = "auc")
coef_mat <- coef(cv_full, s = "lambda.min")

coef_df <- tibble(
  feature = rownames(coef_mat)[-1],
  coef    = as.numeric(coef_mat)[-1]
) %>%
  filter(coef != 0) %>%
  arrange(desc(abs(coef))) %>%
  mutate(
    direction = if_else(coef > 0, "Increases Fall P", "Decreases Fall P"),
    feature   = fct_reorder(feature, abs(coef))
  )

cat(sprintf("\nActive features (non-zero LASSO coefs): %d / %d\n",
            nrow(coef_df), ncol(X)))

# ==============================================================================
# PLOT 1: Predicted P(Fall) over time vs actual regimes
# ==============================================================================
plot_prob_series <- function() {

  shade_df <- rt %>% filter(regime == "Fall") %>% select(xmin, xmax)

  ggplot(cv_results, aes(x = date, y = prob)) +

    geom_rect(data = shade_df,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = "#D90429", alpha = 0.12, inherit.aes = FALSE) +

    geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50") +
    geom_line(color = "#1D3557", linewidth = 0.8) +
    geom_ribbon(aes(ymin = 0, ymax = prob), fill = "#1D3557", alpha = 0.15) +

    facet_wrap(~ fold, ncol = 1,
               labeller = labeller(fold = ~ paste("CV Fold", .x))) +

    scale_y_continuous(labels = percent_format(accuracy = 1),
                       limits = c(0, 1)) +
    scale_x_date(expand = expansion(mult = c(0.01, 0.01))) +

    labs(
      title    = sprintf("P(SPY Fall in %d days) — Walk-Forward CV", HORIZON),
      subtitle = "Red shading = actual SPY Fall  |  Line = predicted probability  |  Dashed = 0.5 threshold",
      x = NULL, y = "P(Fall)"
    ) +

    theme_minimal(base_size = 11) +
    theme(
      strip.text       = element_text(face = "bold", size = 9),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(color = "grey50", size = 9)
    )
}

# ==============================================================================
# PLOT 2: Feature importance (non-zero LASSO coefficients)
# ==============================================================================
plot_feature_importance <- function() {

  ggplot(coef_df, aes(x = feature, y = coef, fill = direction)) +
    geom_col(width = 0.7) +
    geom_hline(yintercept = 0, color = "grey30") +
    coord_flip() +
    scale_fill_manual(values = c("Increases Fall P" = "#D90429",
                                  "Decreases Fall P" = "#2D6A4F")) +
    labs(
      title    = "LASSO Feature Importance — P(SPY Fall)",
      subtitle = sprintf("Horizon: %d days | Only non-zero coefficients shown", HORIZON),
      x = NULL, y = "Coefficient", fill = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position    = "bottom",
      plot.title         = element_text(face = "bold", size = 13),
      plot.subtitle      = element_text(color = "grey50", size = 9)
    )
}

# ==============================================================================
# PLOT 3: ROC curve per fold + overall AUC
# ==============================================================================
plot_roc <- function() {

  roc_df <- cv_results %>%
    group_by(fold) %>%
    group_map(~ {
      r   <- roc(.x$actual, .x$prob, quiet = TRUE)
      auc <- as.numeric(auc(r))
      tibble(
        fpr  = 1 - r$specificities,
        tpr  = r$sensitivities,
        fold = unique(.x$fold),
        auc  = auc
      )
    }) %>%
    bind_rows()

  auc_labels <- roc_df %>%
    distinct(fold, auc) %>%
    mutate(label = sprintf("Fold %d  AUC=%.2f", fold, auc))

  ggplot(roc_df, aes(x = fpr, y = tpr, color = factor(fold))) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
    geom_line(linewidth = 0.9) +
    scale_color_brewer(palette = "Set1",
                       labels  = auc_labels$label,
                       name    = NULL) +
    labs(
      title    = "ROC Curves — Walk-Forward CV Folds",
      subtitle = sprintf("Target: SPY Fall regime in %d trading days (~3 months)", HORIZON),
      x = "False Positive Rate", y = "True Positive Rate"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      plot.title      = element_text(face = "bold", size = 13),
      plot.subtitle   = element_text(color = "grey50", size = 9)
    )
}

# ==============================================================================
# PLOT 4: Signal strength by feature family
# ==============================================================================
plot_family_importance <- function() {

  family_df <- coef_df %>%
    mutate(
      family = case_when(
        str_detect(feature, "rc60|rc20|d1_rc|jump") ~ "A. Rolling Corr",
        str_detect(feature, "mz20|mz60|mz120")      ~ "B. Momentum Z",
        str_detect(feature, "div_z")                 ~ "C. Mom Divergence",
        str_detect(feature, "vol_ratio")             ~ "D. Vol Ratio",
        str_detect(feature, "credit_spread")         ~ "E. Credit Spread",
        str_detect(feature, "sh_composite")          ~ "F. SH Composite",
        str_detect(feature, "dur_z")                 ~ "G. Regime Memory",
        TRUE                                          ~ "Other"
      )
    ) %>%
    group_by(family) %>%
    summarise(total_abs_coef = sum(abs(coef)), .groups = "drop") %>%
    arrange(desc(total_abs_coef)) %>%
    mutate(family = fct_reorder(family, total_abs_coef))

  ggplot(family_df, aes(x = family, y = total_abs_coef)) +
    geom_col(fill = "#1D3557", width = 0.7) +
    coord_flip() +
    labs(
      title    = "Feature Family Importance — Sum of |LASSO Coefs|",
      subtitle = "Which signal family matters most for predicting SPY Fall?",
      x = NULL, y = "Total |coefficient|"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.title         = element_text(face = "bold", size = 13),
      plot.subtitle      = element_text(color = "grey50", size = 9)
    )
}

# ==============================================================================
# RUN ALL
# ==============================================================================
print(plot_prob_series())
print(plot_feature_importance())
print(plot_roc())
print(plot_family_importance())

################################################################################
