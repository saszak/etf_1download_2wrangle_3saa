################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : scripts_daa_saa_taa/05_dd_stabilizer.R
# Purpose : Phase 2.1 — Identify and rank DD Stabilizers to overlay on 6040Classic.
#
# PHASE 2.1 — STABILIZER OVERLAY
#   Baseline (Phase 1.0): 6040Classic = 60% SPY + 40% IEF (buy-and-hold)
#   A stabilizer at weight w means: (1-w) × 6040Classic + w × TICKER
#   Regime detection always uses SPY via build_regime_table() — unchanged.
#
# QUESTION ANSWERED
#   "Given 6040Classic as our SAA, which ETFs most efficiently reduce MaxDD
#    RIGHT NOW — with minimum return sacrifice?"
#
# THREE-STAGE PROCESS
#   Stage 1  build_stabilizer_candidates()  — loose screen, run quarterly
#            Admits any ticker with dd_reduction > 0 at w = SCREEN_WEIGHT.
#            Assigns hedge_type based on Fall correlation with 6040Classic:
#              FULL        — fall_corr <= -0.10  (negative Fall correlation = true hedge)
#              PARTIAL     — -0.10 < fall_corr <= 0.30  (uncorrelated but not negative)
#              CONDITIONAL — fall_corr > 0.30  (reduces DD but still positive Fall corr)
#            Returns a stable watchlist of ~15–25 tickers.
#
#   Stage 2  score_stabilizer_context()     — rolling + regime-conditional, run monthly
#            For each candidate computes four current signals:
#              s1  63-day rolling correlation vs baseline (lower = better hedge NOW)
#              s2  historical Fall DD reduction at SCREEN_WEIGHT
#              s3  own 200DMA trend signal (asset trending = may continue to protect)
#              s4  own 63d return momentum (positive = stabilizer is "working" currently)
#            Composite = equal-weight rank sum → stabilizer_score 0–100.
#
#   Stage 3  select_stabilizers()           — top-N with correlation guard
#            Greedy forward selection: highest stabilizer_score first,
#            reject if pairwise ρ (among selected) > RHO_MAX.
#
# OUTPUTS
#   stabilizer_candidates   tibble — Stage 1 watchlist + full-period metrics + hedge_type
#   stabilizer_scores       tibble — Stage 2 scores + hedge_type
#   selected_stabilizers    tibble — Stage 3 final selection
#
# PLOTS
#   Insurance quadrant: x=return_drag, y=dd_reduction, colour=fall_corr
#   DD frontier: MaxDD path as w sweeps 0→25% for top candidates
#   Sleeve wealth chart: selected tickers vs 6040Classic baseline
#   Ticker fingerprints: full plot_xts_pair() per selected ticker, saved to ticker_fingerprints/
#
# PARAMETERS (top of script)
#   BMK            SPY — used ONLY for regime detection (never changes)
#   W_EQ           equity weight in 6040Classic baseline (default 0.60)
#   W_FI           fixed income weight in 6040Classic baseline (default 0.40)
#   BASELINE_LABEL human-readable label for the baseline (default "6040Classic")
#   SCREEN_WEIGHT  overlay weight used for Stage 1 candidate screen (default 0.10)
#   ROLL_WIN       rolling window for correlation and return (days, default 63)
#   N_SELECT       number of tickers to select in Stage 3 (default 6)
#   RHO_MAX        max pairwise correlation allowed between selected (default 0.80)
#   FULL_CORR_THR  fall_corr threshold for FULL hedge type (default -0.10)
#   PART_CORR_THR  fall_corr threshold between FULL and CONDITIONAL (default 0.30)
################################################################################

library(tidyverse)
library(xts)
library(zoo)
library(PerformanceAnalytics)
library(patchwork)
library(scales)
library(here)

if (!exists("project_tree"))       source(here("project_tree.R"))
if (!exists("etf_metadata"))       source(here(project_tree$scripts$init))
if (!exists("xts_ret"))            source(here(project_tree$scripts$wrangle))
if (!exists("build_regime_table")) source(here("scripts_spy_dd_regime/spy_dd_regime.R"))
if (!exists("rt") || !is.data.frame(rt))
  rt <- build_regime_table(xts_ret[, "SPY"])
source(here("utility/plot_xts_pair.R"))     # always reload
source(here("key_plots/chart_dd_frontier.R"))

# ── Parameters ────────────────────────────────────────────────────────────────
BMK            <- "SPY"          # regime detection ONLY — never the spread benchmark
W_EQ           <- 0.60           # 6040Classic equity weight (SPY)
W_FI           <- 0.40           # 6040Classic fixed income weight (IEF)
BASELINE_LABEL <- "6040Classic"  # label used in all plots and reports
SCREEN_WEIGHT  <- 0.10           # Stage 1 evaluation weight
ROLL_WIN       <- 63L            # rolling window for correlation and return
N_SELECT       <- 6L             # max tickers to select in Stage 3
RHO_MAX        <- 0.80           # pairwise correlation cap between selected stabilizers
FULL_CORR_THR  <- -0.10          # fall_corr <= this → FULL hedge type
PART_CORR_THR  <-  0.30          # fall_corr <= this → PARTIAL; above → CONDITIONAL

# ==============================================================================
# HELPERS
# ==============================================================================

.ann     <- function(r) mean(r, na.rm = TRUE) * 252
.vol     <- function(r) sd(r,   na.rm = TRUE) * sqrt(252)
.ann_ret <- function(r) {
  r <- as.numeric(r[!is.na(r)])
  prod(1 + r)^(252 / length(r)) - 1
}
.max_dd  <- function(r) {
  w <- cumprod(1 + as.numeric(r[!is.na(r)]))
  min((w - cummax(w)) / cummax(w))
}

# Build the 6040Classic baseline as an xts of daily returns.
.build_baseline <- function(xts_ret,
                             w_eq           = W_EQ,
                             w_fi           = W_FI,
                             baseline_label = BASELINE_LABEL) {
  stopifnot("SPY" %in% colnames(xts_ret), "IEF" %in% colnames(xts_ret))
  common <- merge(xts_ret[, "SPY"], xts_ret[, "IEF"], join = "inner")
  bl     <- w_eq * common[, "SPY"] + w_fi * common[, "IEF"]
  colnames(bl) <- baseline_label
  bl
}

# Regime daily lookup: date → regime label
.regime_daily <- function(rt) {
  rt %>%
    rowwise() %>%
    mutate(date = list(seq(as.Date(xmin), as.Date(xmax), by = "day"))) %>%
    unnest(date) %>%
    select(date, regime) %>%
    ungroup()
}

# Current regime = regime of the most recent trading day in xts_ret
.current_regime <- function(rt, xts_ret) {
  last_date   <- as.Date(last(index(xts_ret)))
  regime_lkup <- .regime_daily(rt)
  found       <- regime_lkup %>% filter(date <= last_date) %>% slice_tail(n = 1)
  if (nrow(found) == 0) return("Consolidation")
  found$regime
}

# Blended portfolio: (1-w) × baseline + w × instrument
.blend <- function(baseline_r, instrument_r, w) {
  common  <- merge(baseline_r, instrument_r, join = "inner")
  bl_r    <- as.numeric(common[, 1])
  inst_r  <- as.numeric(common[, 2])
  blended <- xts((1 - w) * bl_r + w * inst_r, order.by = index(common))
  colnames(blended) <- "Blended"
  blended
}

# Per-ticker stabilizer metrics vs 6040Classic baseline at a given overlay weight.
.stabilizer_stats <- function(tk, baseline_xts, xts_ret, regime_daily,
                               screen_weight = SCREEN_WEIGHT) {
  if (!(tk %in% colnames(xts_ret))) return(NULL)

  common <- merge(baseline_xts, xts_ret[, tk], join = "inner")
  if (nrow(common) < 252) return(NULL)

  bl_r   <- common[, 1]
  tk_r   <- common[, 2]
  blend  <- .blend(bl_r, tk_r, screen_weight)

  dates  <- as.Date(index(common))
  p_num  <- as.numeric(bl_r)
  x_num  <- as.numeric(tk_r)

  # Regime indices
  lkup      <- tibble(date = dates) %>% left_join(regime_daily, by = "date")
  fall_idx  <- which(lkup$regime == "Fall")
  recov_idx <- which(lkup$regime == "Recovery")

  # DD reduction (positive = stabilizer helped)
  dd_baseline    <- .max_dd(bl_r)
  dd_blended     <- .max_dd(blend)
  dd_reduction   <- dd_baseline - dd_blended

  # Fall-specific DD reduction
  fall_dd_reduction <- if (length(fall_idx) > 20) {
    bl_fall  <- bl_r[fall_idx]
    tk_fall  <- tk_r[fall_idx]
    bld_fall <- .blend(bl_fall, tk_fall, screen_weight)
    .max_dd(bl_fall) - .max_dd(bld_fall)
  } else NA_real_

  # Return drag (positive = stabilizer costs return)
  return_drag <- .ann_ret(bl_r) - .ann_ret(blend)

  # Insurance ratio
  insurance_ratio <- dd_reduction / max(abs(return_drag), 0.001)

  # Correlations with baseline
  fall_corr <- if (length(fall_idx) > 20)
    cor(p_num[fall_idx], x_num[fall_idx], use = "complete.obs") else NA_real_
  full_corr <- cor(p_num, x_num, use = "complete.obs")

  # Standalone metrics
  own_maxdd <- .max_dd(tk_r)
  own_ret   <- .ann_ret(tk_r)
  own_vol   <- .vol(x_num)

  tibble(
    ticker            = tk,
    dd_reduction      = dd_reduction,
    fall_dd_reduction = fall_dd_reduction,
    return_drag       = return_drag,
    insurance_ratio   = insurance_ratio,
    fall_corr         = fall_corr,
    full_corr         = full_corr,
    own_maxdd         = own_maxdd,
    own_ret           = own_ret,
    own_vol           = own_vol,
    n_days            = nrow(common)
  )
}

# ==============================================================================
# STAGE 1 — BUILD STABILIZER CANDIDATES (stable watchlist, run quarterly)
# ==============================================================================
# Admission: dd_reduction > 0 at SCREEN_WEIGHT.
# Hedge type assigned from Fall correlation with baseline:
#   FULL        — fall_corr <= FULL_CORR_THR  (true crisis hedge)
#   PARTIAL     — FULL_CORR_THR < fall_corr <= PART_CORR_THR
#   CONDITIONAL — fall_corr > PART_CORR_THR   (reduces DD through portfolio math only)

build_stabilizer_candidates <- function(
    xts_ret,
    rt,
    bmk            = BMK,
    w_eq           = W_EQ,
    w_fi           = W_FI,
    baseline_label = BASELINE_LABEL,
    screen_weight  = SCREEN_WEIGHT,
    full_corr_thr  = FULL_CORR_THR,
    part_corr_thr  = PART_CORR_THR
) {
  baseline_xts <- .build_baseline(xts_ret, w_eq, w_fi, baseline_label)

  message(sprintf("▶ Stage 1 — screening %d tickers vs %s (%s%% SPY + %s%% IEF) at w=%.0f%%...",
                  ncol(xts_ret) - 1L, baseline_label,
                  round(w_eq * 100), round(w_fi * 100),
                  screen_weight * 100))

  regime_daily <- .regime_daily(rt)

  candidates <- purrr::map_dfr(
    setdiff(colnames(xts_ret), c(bmk, "IEF")),
    ~ .stabilizer_stats(.x, baseline_xts, xts_ret, regime_daily, screen_weight)
  ) %>%
    filter(dd_reduction > 0) %>%
    # ── hedge_type assignment ──────────────────────────────────────────────────
    mutate(
      hedge_type = case_when(
        is.na(fall_corr)              ~ "PARTIAL",     # no Fall data — assume partial hedge
        fall_corr <= full_corr_thr   ~ "FULL",
        fall_corr <= part_corr_thr   ~ "PARTIAL",
        TRUE                          ~ "CONDITIONAL"
      ),
      hedge_label = case_when(
        hedge_type == "FULL"        ~ "FULL — negative Fall correlation",
        hedge_type == "PARTIAL"     ~ "PARTIAL — uncorrelated in Fall",
        hedge_type == "CONDITIONAL" ~ "CONDITIONAL — positive Fall corr, portfolio effect only"
      )
    ) %>%
    left_join(
      etf_metadata %>% select(ticker, name, asset_class, pf_function),
      by = "ticker"
    ) %>%
    arrange(hedge_type, desc(insurance_ratio))

  n_full  <- sum(candidates$hedge_type == "FULL",        na.rm = TRUE)
  n_part  <- sum(candidates$hedge_type == "PARTIAL",     na.rm = TRUE)
  n_cond  <- sum(candidates$hedge_type == "CONDITIONAL", na.rm = TRUE)

  message(sprintf("✅ Stage 1 complete: %d candidates (%d FULL, %d PARTIAL, %d CONDITIONAL).",
                  nrow(candidates), n_full, n_part, n_cond))
  candidates
}

# ==============================================================================
# STAGE 2 — SCORE IN CURRENT CONTEXT (run monthly)
# ==============================================================================
# Four signals, equal-weight rank sum → stabilizer_score 0–100.
#
# s1  rolling_corr_63d   — Rolling 63d correlation vs baseline (LOWER = better)
# s2  fall_dd_reduction  — Historical Fall DD reduction at screen weight (HIGHER = better)
# s3  trend_signal       — Own 200DMA trend signal (HIGHER = better momentum)
# s4  own_momentum_63d   — Own annualised return last 63d (HIGHER = asset is working)
#
# trend_signals tibble is optional; if not available s3 is skipped.

score_stabilizer_context <- function(
    candidates,
    xts_ret,
    rt,
    bmk            = BMK,
    w_eq           = W_EQ,
    w_fi           = W_FI,
    baseline_label = BASELINE_LABEL,
    screen_weight  = SCREEN_WEIGHT,
    roll_win       = ROLL_WIN,
    trend_signals  = NULL    # tibble from 03_taa_rules.R: date, symbol, signal
) {
  baseline_xts <- .build_baseline(xts_ret, w_eq, w_fi, baseline_label)
  cur_regime   <- .current_regime(rt, xts_ret)

  message(sprintf("▶ Stage 2 — scoring %d candidates vs %s | current regime: %s",
                  nrow(candidates), baseline_label, cur_regime))

  scores <- purrr::map_dfr(candidates$ticker, function(tk) {
    if (!(tk %in% colnames(xts_ret))) return(NULL)

    common <- merge(baseline_xts, xts_ret[, tk], join = "inner")
    if (nrow(common) < roll_win + 10L) return(NULL)

    bl_r  <- as.numeric(common[, 1])
    tk_r  <- as.numeric(common[, 2])
    n     <- length(bl_r)

    # s1: rolling 63d correlation vs baseline (lower = better hedge RIGHT NOW)
    recent_bl <- tail(bl_r, roll_win)
    recent_tk <- tail(tk_r, roll_win)
    rolling_corr <- if (sd(recent_bl) > 0 && sd(recent_tk) > 0)
      cor(recent_bl, recent_tk, use = "complete.obs") else NA_real_

    # s2: Fall DD reduction from Stage 1 (already computed, join from candidates)
    fall_dd_red <- candidates %>%
      filter(ticker == tk) %>%
      pull(fall_dd_reduction) %>%
      first()

    # s3: 200DMA trend signal (last date, from trend_signals if available)
    trend_on <- if (!is.null(trend_signals) && tk %in% trend_signals$symbol) {
      trend_signals %>%
        filter(symbol == tk) %>%
        slice_max(date, n = 1) %>%
        pull(signal) %>%
        as.numeric()
    } else NA_real_

    # s4: own 63d annualised return momentum (positive = asset trending up now)
    own_momentum_63d <- .ann(tail(tk_r, roll_win))

    tibble(
      ticker           = tk,
      cur_regime       = cur_regime,
      rolling_corr_63d = rolling_corr,
      fall_dd_red      = fall_dd_red,
      trend_on         = trend_on,
      own_momentum_63d = own_momentum_63d
    )
  })

  # Rank-sum composite.
  # s1 = rolling_corr: LOWER is better → rank by NEGATIVE corr (invert before rank)
  # s2 = fall_dd_red:  HIGHER is better → rank directly
  # s3 = trend_on:     HIGHER is better → rank directly
  # s4 = own_momentum: HIGHER is better → rank directly
  scores <- scores %>%
    mutate(
      r_corr     = rank(-rolling_corr_63d, na.last = "keep", ties.method = "average"),
      r_fall_dd  = rank(fall_dd_red,       na.last = "keep", ties.method = "average"),
      r_trend    = rank(trend_on,           na.last = "keep", ties.method = "average"),
      r_momentum = rank(own_momentum_63d,   na.last = "keep", ties.method = "average")
    ) %>%
    rowwise() %>%
    mutate(
      # Average only non-NA components so missing trend_signal doesn't penalise
      stabilizer_score = mean(c(r_corr, r_fall_dd, r_trend, r_momentum), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    # Rescale 0–100
    mutate(stabilizer_score = (stabilizer_score - min(stabilizer_score, na.rm = TRUE)) /
             (max(stabilizer_score, na.rm = TRUE) - min(stabilizer_score, na.rm = TRUE)) * 100) %>%
    arrange(desc(stabilizer_score)) %>%
    left_join(
      candidates %>% select(ticker, dd_reduction, fall_dd_reduction, return_drag,
                            insurance_ratio, fall_corr, own_maxdd,
                            asset_class, name, hedge_type, hedge_label),
      by = "ticker"
    )

  type_counts <- table(scores$hedge_type)
  message(sprintf("✅ Stage 2 complete. Hedge types: %s",
                  paste(names(type_counts), type_counts, sep = "=", collapse = " | ")))
  scores
}

# ==============================================================================
# STAGE 3 — SELECT STABILIZERS (correlation guard)
# ==============================================================================
# Greedy forward: add highest-scoring stabilizer not too correlated with already-selected.

select_stabilizers <- function(
    stabilizer_scores,
    xts_ret,
    n_select = N_SELECT,
    rho_max  = RHO_MAX
) {
  message(sprintf("▶ Stage 3 — selecting up to %d stabilizers (ρ_max = %.2f)...",
                  n_select, rho_max))

  ranked   <- stabilizer_scores %>% arrange(desc(stabilizer_score)) %>% pull(ticker)
  selected <- character(0)

  for (tk in ranked) {
    if (length(selected) >= n_select) break
    if (!(tk %in% colnames(xts_ret))) next

    if (length(selected) == 0) {
      selected <- c(selected, tk)
      next
    }

    # Check pairwise correlation with all already-selected
    corrs <- sapply(selected, function(sel) {
      common <- merge(xts_ret[, tk], xts_ret[, sel], join = "inner")
      cor(as.numeric(common[, 1]), as.numeric(common[, 2]),
          use = "complete.obs")
    })

    if (all(abs(corrs) <= rho_max)) {
      selected <- c(selected, tk)
    } else {
      message(sprintf("  ↷ Skipped %s — ρ=%.2f with %s",
                      tk, max(abs(corrs)), selected[which.max(abs(corrs))]))
    }
  }

  result <- stabilizer_scores %>%
    filter(ticker %in% selected) %>%
    arrange(desc(stabilizer_score)) %>%
    mutate(rank = row_number())

  message(sprintf("✅ Stage 3 complete: selected %d stabilizers.", nrow(result)))
  result
}

# ==============================================================================
# CONSOLE REPORT
# ==============================================================================

.print_stab_report <- function(selected_stabilizers,
                                bmk            = BMK,
                                baseline_label = BASELINE_LABEL) {
  cur <- unique(selected_stabilizers$cur_regime)
  message(sprintf("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"))
  message(sprintf("🛡️   STABILIZER SELECTION vs %s  |  Current regime: %s", baseline_label, cur))
  message(sprintf("    (Regime detection: %s  |  Overlay weight: %.0f%%)", bmk, SCREEN_WEIGHT * 100))
  message(sprintf("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"))

  selected_stabilizers %>%
    mutate(
      score_f        = sprintf("%.1f",   stabilizer_score),
      dd_red_f       = sprintf("%+.1f pp", dd_reduction    * 100),
      fall_dd_f      = sprintf("%+.1f pp", fall_dd_reduction * 100),
      drag_f         = sprintf("%+.1f pp", return_drag     * 100),
      ins_f          = sprintf("%.2f",   insurance_ratio),
      fall_corr_f    = sprintf("%+.2f",  fall_corr),
      roll_corr_f    = sprintf("%+.2f",  rolling_corr_63d),
      trend_f        = if_else(is.na(trend_on), "n/a", if_else(trend_on == 1, "ON", "OFF")),
      momentum_f     = sprintf("%+.1f%%",  own_momentum_63d * 100)
    ) %>%
    select(
      `#`           = rank,
      Ticker        = ticker,
      `Hedge type`  = hedge_type,
      `Stab Score`  = score_f,
      `DD Red`      = dd_red_f,
      `Fall DD Red` = fall_dd_f,
      `Drag`        = drag_f,
      `Ins Ratio`   = ins_f,
      `Fall ρ`      = fall_corr_f,
      `Roll ρ 63d`  = roll_corr_f,
      `200DMA`      = trend_f,
      `Own Mom`     = momentum_f,
      `Asset class` = asset_class
    ) %>%
    print(n = Inf)
  message("")
}

# ==============================================================================
# PLOT A — INSURANCE QUADRANT
#    x = return_drag (cost),  y = dd_reduction (benefit)
#    colour = fall_corr (blue = negative = true hedge, red = positive = no hedge)
#    bubble size = insurance_ratio
# ==============================================================================

plot_stabilizer_quad <- function(candidates, baseline_label = BASELINE_LABEL) {
  df <- candidates %>%
    filter(!is.na(fall_corr), !is.na(dd_reduction)) %>%
    mutate(asset_class = coalesce(asset_class, "Unknown"))

  baseline_dd  <- .max_dd(.build_baseline(xts_ret))
  baseline_ret <- .ann_ret(.build_baseline(xts_ret))

  ggplot(df, aes(x = return_drag * 100, y = dd_reduction * 100)) +
    geom_hline(yintercept = 0, colour = "#6b7280", linewidth = 0.4, linetype = "dashed") +
    geom_vline(xintercept = 0, colour = "#6b7280", linewidth = 0.4, linetype = "dashed") +
    annotate("text", x =  3,  y =  max(df$dd_reduction * 100) * 0.85,
             label = "Best: cuts DD, costs some return",
             colour = "#166534", size = 3.2, fontface = "italic") +
    annotate("text", x = -2,  y =  max(df$dd_reduction * 100) * 0.85,
             label = "Ideal: cuts DD, free or adds return",
             colour = "#14532d", size = 3.2, fontface = "bold") +
    geom_point(aes(size  = insurance_ratio,
                   fill  = fall_corr,
                   shape = asset_class),
               alpha = 0.82, colour = "white", stroke = 0.3) +
    scale_fill_gradient2(
      low      = "#1d4ed8",   # blue = negative fall_corr = true hedge
      mid      = "#d1d5db",
      high     = "#dc2626",   # red  = positive fall_corr = sells off with market
      midpoint = 0,
      name     = "Fall ρ\nvs baseline",
      limits   = c(-1, 1)
    ) +
    scale_size_continuous(range = c(2, 10), name = "Insurance ratio") +
    scale_shape_manual(
      values = c("Equity"       = 21, "FixedIncome" = 22, "Alternative" = 23,
                 "RealAsset"    = 24, "MultiAsset"  = 25, "Commodity"   = 21,
                 "Unknown"      = 21),
      name = "Asset class"
    ) +
    ggrepel::geom_text_repel(
      data = df %>% filter(dd_reduction > 0.02 | insurance_ratio > 1.5),
      aes(label = ticker),
      size = 3, fontface = "bold", max.overlaps = 15,
      box.padding = 0.3, colour = "#111827"
    ) +
    scale_x_continuous(labels = function(x) paste0(x, " pp"),
                       name   = sprintf("Return drag at %.0f%% allocation (pp/yr)\n← free      costs you →",
                                        SCREEN_WEIGHT * 100)) +
    scale_y_continuous(labels = function(x) paste0(x, " pp"),
                       name   = "MaxDD reduction at 10% allocation (pp)\n↑ more protection") +
    labs(
      title    = sprintf("DD Stabilizer Quadrant — Cost vs Benefit at %.0f%% Overlay", SCREEN_WEIGHT * 100),
      subtitle = sprintf(
        "%s baseline — MaxDD: %.1f%%  AnnRet: %+.1f%%\nBlue = negative Fall ρ (true hedge)  |  Bubble size = insurance ratio",
        baseline_label, baseline_dd * 100, baseline_ret * 100
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(colour = "grey50", size = 9),
      legend.position  = "right"
    )
}

# ==============================================================================
# PLOT B — DD FRONTIER  (chart_dd_frontier.R already sourced above)
# ==============================================================================
# Build sweep data for top candidates to feed into plot_dd_frontier()

build_dd_frontier_data <- function(candidates, xts_ret, baseline_xts,
                                    top_n = 8L,
                                    sweep_weights = seq(0, 0.25, by = 0.02)) {
  top_picks <- candidates %>%
    filter(!is.na(fall_corr)) %>%
    slice_head(n = top_n) %>%
    pull(ticker)

  purrr::map_dfr(top_picks, function(tk) {
    if (!(tk %in% colnames(xts_ret))) return(NULL)
    common <- merge(baseline_xts, xts_ret[, tk], join = "inner")
    bl_r   <- common[, 1]
    tk_r   <- common[, 2]

    purrr::map_dfr(sweep_weights, function(w) {
      bld <- .blend(bl_r, tk_r, w)
      tibble(
        ticker  = tk,
        weight  = w,
        maxdd   = .max_dd(bld),
        ann_ret = .ann_ret(bld),
        ann_vol = .vol(as.numeric(bld))
      )
    })
  })
}

# ==============================================================================
# PLOT C — SLEEVE WEALTH CHART
# ==============================================================================

plot_stabilizer_sleeves <- function(
    selected_stabilizers,
    xts_ret,
    rt,
    baseline_label      = BASELINE_LABEL,
    w_eq                = W_EQ,
    w_fi                = W_FI,
    show_candidates     = FALSE,
    stabilizer_candidates = NULL
) {
  baseline_xts <- .build_baseline(xts_ret, w_eq, w_fi, baseline_label)
  xts_ret      <- merge(xts_ret, baseline_xts, join = "left")
  mangled      <- paste0("X", baseline_label)
  if (mangled %in% colnames(xts_ret) && !baseline_label %in% colnames(xts_ret))
    colnames(xts_ret)[colnames(xts_ret) == mangled] <- baseline_label

  regime_rect <- rt %>%
    filter(regime %in% c("Fall", "Recovery")) %>%
    mutate(
      fill = if_else(regime == "Fall", "#fca5a5", "#bbf7d0"),
      xmin = as.Date(xmin),
      xmax = as.Date(xmax)
    )

  .wealth_tbl <- function(tickers, xts_ret, bmk_label) {
    all_tks <- c(bmk_label, tickers)
    avail   <- all_tks[all_tks %in% colnames(xts_ret)]
    if (length(avail) < 2) return(NULL)
    combined <- Reduce(function(a, b) merge(a, b, join = "inner"),
                       lapply(avail, function(tk) xts_ret[, tk]))
    colnames(combined) <- avail
    as.data.frame(combined) %>%
      rownames_to_column("date") %>%
      mutate(date = as.Date(date)) %>%
      pivot_longer(-date, names_to = "ticker", values_to = "ret") %>%
      group_by(ticker) %>%
      arrange(date) %>%
      mutate(wealth = cumprod(1 + ret)) %>%
      ungroup()
  }

  .end_labels <- function(wealth_df) {
    wealth_df %>%
      group_by(ticker) %>%
      slice_max(date, n = 1) %>%
      left_join(
        wealth_df %>%
          group_by(ticker) %>%
          summarise(ann_ret = last(wealth)^(252 / n()) - 1, .groups = "drop"),
        by = "ticker"
      ) %>%
      mutate(end_label = sprintf("%s\n%+.1f%% p.a.", ticker, ann_ret * 100)) %>%
      ungroup()
  }

  default_palette <- c("#7c3aed", "#3b82f6", "#f59e0b",
                       "#22c55e", "#ef4444", "#0ea5e9", "#a78bfa")

  sleeves <- unique(selected_stabilizers$asset_class)
  sleeves <- sleeves[!is.na(sleeves)]

  purrr::walk(sleeves, function(sleeve) {
    sel_tks <- selected_stabilizers %>%
      filter(asset_class == sleeve) %>%
      arrange(desc(stabilizer_score)) %>%
      pull(ticker)

    if (length(sel_tks) == 0) return(invisible(NULL))

    bg_tks <- character(0)
    if (isTRUE(show_candidates) && !is.null(stabilizer_candidates)) {
      bg_tks <- stabilizer_candidates %>%
        filter(asset_class == sleeve,
               !ticker %in% sel_tks,
               ticker %in% colnames(xts_ret)) %>%
        pull(ticker)
    }

    all_plot_tks <- c(sel_tks, bg_tks)
    wdf          <- .wealth_tbl(all_plot_tks, xts_ret, baseline_label)
    if (is.null(wdf)) return(invisible(NULL))

    wdf_tickers  <- wdf %>% filter(ticker != baseline_label)
    wdf_baseline <- wdf %>% filter(ticker == baseline_label)
    last_pts     <- .end_labels(wdf)

    n_sel       <- length(sel_tks)
    sel_colours <- setNames(default_palette[seq_len(n_sel)], sel_tks)
    bg_colours  <- setNames(rep("#d1d5db", length(bg_tks)), bg_tks)
    all_colours <- c(sel_colours, bg_colours)

    lw_map <- c(setNames(rep(0.6, n_sel), sel_tks),
                setNames(rep(0.3, length(bg_tks)), bg_tks))
    alpha_map <- c(setNames(rep(1.0, n_sel), sel_tks),
                   setNames(rep(0.35, length(bg_tks)), bg_tks))

    level_order  <- c(bg_tks, sel_tks)
    wdf_tickers  <- wdf_tickers %>% mutate(ticker = factor(ticker, levels = level_order))
    last_pts     <- last_pts %>% mutate(ticker = as.character(ticker))

    score_str <- selected_stabilizers %>%
      filter(asset_class == sleeve) %>%
      arrange(desc(stabilizer_score)) %>%
      mutate(s = sprintf("%s(%.0f)", ticker, stabilizer_score)) %>%
      pull(s) %>%
      paste(collapse = "  ")

    cur_regime <- unique(selected_stabilizers$cur_regime)[1]

    p <- ggplot(wdf_tickers, aes(date, wealth,
                                  colour    = ticker,
                                  linewidth = ticker,
                                  alpha     = ticker)) +
      geom_rect(data        = regime_rect,
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = fill),
                inherit.aes = FALSE, alpha = 0.18) +
      scale_fill_identity() +
      geom_line() +
      geom_line(data        = wdf_baseline,
                aes(date, wealth),
                inherit.aes = FALSE,
                colour      = "#1d3461",
                linewidth   = 1.8) +
      geom_text(
        data      = last_pts %>% filter(ticker != baseline_label),
        aes(label = end_label),
        hjust = 0, nudge_x = 60, size = 2.9, fontface = "bold", lineheight = 0.85
      ) +
      geom_text(
        data        = last_pts %>% filter(ticker == baseline_label),
        aes(date, wealth, label = end_label),
        inherit.aes = FALSE,
        hjust = 0, nudge_x = 60, size = 2.9, fontface = "bold",
        colour = "#1d3461", lineheight = 0.85
      ) +
      scale_colour_manual(values = all_colours, guide = "none") +
      scale_linewidth_manual(values = lw_map,   guide = "none") +
      scale_alpha_manual(values = alpha_map,    guide = "none") +
      scale_y_continuous(labels = dollar_format(prefix = "$"),
                         limits = c(0.5, NA),
                         expand = expansion(mult = c(0, 0.05))) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      coord_cartesian(clip = "off") +
      labs(
        title    = sprintf("%s — Selected Stabilizers vs %s", sleeve, baseline_label),
        subtitle = sprintf(
          "Regime: %s  |  Scores: %s\nPink = Fall  |  Green = Recovery  |  Navy = %s baseline",
          cur_regime, score_str, baseline_label
        ),
        x = NULL, y = "Wealth ($1 invested)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.margin      = margin(5, 160, 5, 5),
        panel.grid.minor = element_blank(),
        plot.title       = element_text(face = "bold", size = 13),
        plot.subtitle    = element_text(colour = "grey50", size = 9),
        axis.text        = element_text(colour = "#1B3A6B")
      )

    print(p)
  })
  invisible(NULL)
}

# ==============================================================================
# PLOT D — TICKER FINGERPRINTS (plot_xts_pair per selected stabilizer)
# ==============================================================================

plot_stabilizer_pairs <- function(
    selected_stabilizers,
    xts_ret,
    rt,
    bmk            = BMK,
    w_eq           = W_EQ,
    w_fi           = W_FI,
    baseline_label = BASELINE_LABEL,
    save_png       = TRUE,
    fig_width      = 13,
    fig_height     = 21,
    dpi            = 150
) {
  baseline_xts <- .build_baseline(xts_ret, w_eq, w_fi, baseline_label)

  if (isTRUE(save_png)) {
    out_dir <- here("ticker_fingerprints")
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  }

  purrr::walk(seq_len(nrow(selected_stabilizers)), function(i) {
    row   <- selected_stabilizers[i, ]
    tk    <- row$ticker
    hedge <- if ("hedge_label" %in% names(row)) row$hedge_label else ""
    pfx   <- sprintf("[#%d | Score %.1f | %s | %s regime]",
                     row$rank, row$stabilizer_score, hedge, row$cur_regime)

    message(sprintf("\n── Stabilizer fingerprint %d/%d: %s vs %s ─────────────────────",
                    i, nrow(selected_stabilizers), tk, baseline_label))

    tk_name <- if (exists("etf_metadata"))
      etf_metadata %>% filter(ticker == tk) %>% pull(name) %>% first()
    else NULL
    if (length(tk_name) == 0) tk_name <- NULL

    result <- plot_xts_pair(
      xts1         = xts_ret[, tk],
      xts2         = baseline_xts,
      label1       = tk,
      label2       = baseline_label,
      name1        = tk_name,
      regime_tbl   = rt,
      title_prefix = pfx,
      role         = "Stabilizer",
      print_plot   = !save_png
    )

    if (isTRUE(save_png)) {
      fname <- file.path(out_dir,
                         sprintf("stab_%02d_%s_vs_%s.png", i, tk,
                                 gsub("[^A-Za-z0-9]", "", baseline_label)))
      ggplot2::ggsave(fname, plot = result$combined,
                      width = fig_width, height = fig_height, dpi = dpi)
      message(sprintf("   💾 Saved: ticker_fingerprints/%s", basename(fname)))
    }
  })

  if (isTRUE(save_png))
    message(sprintf("\n✅ %d stabilizer fingerprints saved to ticker_fingerprints/",
                    nrow(selected_stabilizers)))

  invisible(NULL)
}

# ==============================================================================
# MASTER WRAPPER — find_stabilizers()
# ==============================================================================
# Single entry point.  Returns a named list with all pipeline outputs.
# Can be sourced and called from Rmd or interactive session.

find_stabilizers <- function(
    xts_ret,
    rt,
    bmk            = BMK,
    w_eq           = W_EQ,
    w_fi           = W_FI,
    baseline_label = BASELINE_LABEL,
    screen_weight  = SCREEN_WEIGHT,
    roll_win       = ROLL_WIN,
    n_select       = N_SELECT,
    rho_max        = RHO_MAX,
    full_corr_thr  = FULL_CORR_THR,
    part_corr_thr  = PART_CORR_THR,
    trend_signals  = NULL
) {
  baseline_xts <- .build_baseline(xts_ret, w_eq, w_fi, baseline_label)

  candidates <- build_stabilizer_candidates(
    xts_ret        = xts_ret,
    rt             = rt,
    bmk            = bmk,
    w_eq           = w_eq,
    w_fi           = w_fi,
    baseline_label = baseline_label,
    screen_weight  = screen_weight,
    full_corr_thr  = full_corr_thr,
    part_corr_thr  = part_corr_thr
  )

  scores <- score_stabilizer_context(
    candidates     = candidates,
    xts_ret        = xts_ret,
    rt             = rt,
    bmk            = bmk,
    w_eq           = w_eq,
    w_fi           = w_fi,
    baseline_label = baseline_label,
    screen_weight  = screen_weight,
    roll_win       = roll_win,
    trend_signals  = trend_signals
  )

  selected <- select_stabilizers(
    stabilizer_scores = scores,
    xts_ret           = xts_ret,
    n_select          = n_select,
    rho_max           = rho_max
  )

  .print_stab_report(selected, bmk = bmk, baseline_label = baseline_label)

  list(
    candidates = candidates,
    scores     = scores,
    selected   = selected,
    baseline   = baseline_xts,
    params     = list(
      bmk            = bmk,
      w_eq           = w_eq,
      w_fi           = w_fi,
      baseline_label = baseline_label,
      screen_weight  = screen_weight,
      roll_win       = roll_win,
      n_select       = n_select,
      rho_max        = rho_max,
      full_corr_thr  = full_corr_thr,
      part_corr_thr  = part_corr_thr
    )
  )
}

# ==============================================================================
# RUN WHEN SOURCED DIRECTLY (not inside Rmd)
# ==============================================================================

if (!isTRUE(getOption("knitr.in.progress"))) {

  stab <- find_stabilizers(xts_ret, rt)

  if (!requireNamespace("ggrepel", quietly = TRUE))
    install.packages("ggrepel", repos = "https://cloud.r-project.org")

  # Build DD frontier data for plot_dd_frontier()
  stab_frontier <- build_dd_frontier_data(
    stab$candidates,
    xts_ret,
    stab$baseline
  )

  p_quad     <- plot_stabilizer_quad(stab$candidates)
  p_frontier <- plot_dd_frontier(stab_frontier)

  print(p_quad)
  print(p_frontier)

  plot_stabilizer_sleeves(stab$selected, xts_ret, rt)

  plot_stabilizer_pairs(stab$selected, xts_ret, rt)

  # Persist outputs
  write_rds(stab$candidates, here("02_data_processed/dd_stabilizer_screen.rds"))
  write_rds(stab_frontier,   here("02_data_processed/dd_frontier_data.rds"))
  message("\n💾 Saved: dd_stabilizer_screen.rds, dd_frontier_data.rds")
}

message("✅ Phase 2.1 complete: DD stabilizer pipeline loaded.")
