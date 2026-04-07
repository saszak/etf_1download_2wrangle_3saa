################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : scripts_daa_saa_taa/08_enhancer_screen.R
# Purpose : Identify and rank ETF enhancers vs the 6040Classic baseline in three stages.
#
# PHASE 2.2 — OVERLAY ENHANCER
#   Baseline (Phase 1.0): 6040Classic = 60% SPY + 40% IEF (buy-and-hold)
#   An enhancer at weight w means: (1-w) × 6040Classic + w × TICKER
#   Regime detection always uses SPY via build_regime_table() — unchanged.
#
# QUESTION ANSWERED
#   "Given 6040Classic as our SAA, which ETFs are the best enhancers
#    RIGHT NOW — historically credible AND currently 'on'?"
#
# THREE-STAGE PROCESS
#   Stage 1  build_enhancer_candidates()  — loose OR-screen, run quarterly
#            Admits any ticker that shows enhancer signal in AT LEAST ONE regime.
#            Assigns trigger_type (A/B) based on Fall behaviour vs 6040Classic:
#              A — Unconditional   fall_excess >= -0.02  (hold always)
#              B — Regime-cond.    fall_excess <  -0.02  (hold in non-Fall only)
#            Returns a stable watchlist of ~15–25 tickers.
#
#   Stage 2  score_current_context()      — rolling + regime-conditional, run monthly
#            Refines trigger_type B → C when 200DMA signal is available:
#              C — 200DMA-signal   Type B ticker with active trend_signals data
#            For each candidate computes four current signals:
#              s1  63-day rolling IR of the spread vs 6040Classic (alpha present NOW?)
#              s2  historical excess return in the CURRENT regime vs 6040Classic
#              s3  200DMA trend signal (ticker "on" today?)
#              s4  spread momentum (alpha accelerating or fading?)
#            Composite = equal-weight rank sum of the four signals.
#
#   Stage 3  select_enhancers()           — top-N with correlation guard
#            Greedy forward selection: highest context_score first,
#            reject if pairwise ρ > RHO_MAX with any already-selected ticker.
#
# OUTPUTS
#   enhancer_candidates   tibble — Stage 1 watchlist + full-period stats + trigger_type
#   enhancer_scores       tibble — Stage 2 context scores + trigger_type_final
#   selected_enhancers    tibble — Stage 3 final selection
#
# PLOTS
#   For each selected enhancer: full plot_xts_pair() vs 6040Classic
#   title_prefix carries context score + rank + trigger_type into every panel title.
#
# PARAMETERS (top of script)
#   BMK            SPY — used ONLY for regime detection (never changes)
#   W_EQ           equity weight in 6040Classic baseline (default 0.60)
#   W_FI           fixed income weight in 6040Classic baseline (default 0.40)
#   BASELINE_LABEL human-readable label for the baseline (default "6040Classic")
#   IR_FLOOR       minimum full-period IR vs baseline for Stage 1 admission (OR logic)
#   ROLL_WIN       rolling window for IR and return (days, default 63)
#   MOMENTUM_FAST  short spread-momentum window (days, default 20)
#   N_SELECT       number of tickers to select in Stage 3 (default 5)
#   RHO_MAX        max pairwise correlation allowed between selected (default 0.70)
#   FALL_TOLERANCE max acceptable annualised loss vs baseline in Fall regime for Type A
#                  (default -0.02, i.e. 2pp worse than 6040Classic in Fall is still OK)
################################################################################

library(tidyverse)
library(xts)
library(zoo)
library(patchwork)
library(scales)
library(here)

if (!exists("project_tree"))       source(here("project_tree.R"))
if (!exists("etf_metadata"))       source(here(project_tree$scripts$init))
if (!exists("xts_ret"))            source(here(project_tree$scripts$wrangle))
if (!exists("build_regime_table")) source(here("scripts_spy_dd_regime/spy_dd_regime.R"))
if (!exists("rt") || !is.data.frame(rt))
  rt <- build_regime_table(xts_ret[, "SPY"])
source(here("utility/plot_xts_pair.R"))   # always reload — function signature may have changed

# ── Parameters ────────────────────────────────────────────────────────────────
BMK            <- "SPY"          # regime detection ONLY — never the spread benchmark
W_EQ           <- 0.60           # 6040Classic equity weight (SPY)
W_FI           <- 0.40           # 6040Classic fixed income weight (IEF)
BASELINE_LABEL <- "6040Classic"  # label used in all plots and reports
IR_FLOOR       <- 0.20           # Stage 1: full-period IR threshold vs baseline (OR gate)
ROLL_WIN       <- 63L            # rolling window for IR and return (1 quarter)
MOMENTUM_FAST  <- 20L            # short window for spread momentum
N_SELECT       <- 8L             # max tickers to select in Stage 3
RHO_MAX        <- 1.00           # pairwise correlation cap between selected tickers
                                  # 1.0 = off (enhancers are ranked independently of each other)
                                  # set to e.g. 0.80 if you want sleeve diversification enforced
FALL_TOLERANCE <- -0.02          # Type A threshold: fall_excess >= this → hold always
                                  # tickers losing >2pp vs baseline in Fall → Type B

# ==============================================================================
# HELPERS
# ==============================================================================

.ann <- function(r)  mean(r, na.rm = TRUE) * 252
.vol <- function(r)  sd(r,   na.rm = TRUE) * sqrt(252)
.ir  <- function(r)  if (.vol(r) == 0) NA_real_ else .ann(r) / .vol(r)

# Build the 6040Classic baseline as an xts of daily returns.
# Requires "SPY" and "IEF" in xts_ret.
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

# Per-ticker spread stats vs 6040Classic baseline.
# baseline_xts: single-column xts of daily baseline returns (output of .build_baseline())
.spread_stats <- function(tk, baseline_xts, xts_ret, regime_daily) {
  if (!(tk %in% colnames(xts_ret))) return(NULL)

  common <- merge(xts_ret[, tk], baseline_xts, join = "inner")
  if (nrow(common) < 252) return(NULL)

  r_tk  <- as.numeric(common[, 1])
  r_bl  <- as.numeric(common[, 2])
  r_spr <- r_tk - r_bl
  dates <- as.Date(index(common))

  # Regime membership (always SPY-based)
  lkup  <- tibble(date = dates) %>%
    left_join(regime_daily, by = "date")

  fall_idx  <- which(lkup$regime == "Fall")
  recov_idx <- which(lkup$regime == "Recovery")
  cons_idx  <- which(lkup$regime == "Consolidation")

  tibble(
    ticker          = tk,
    full_ir         = .ir(r_spr),
    full_excess_ann = .ann(r_spr),
    fall_excess_ann = if (length(fall_idx)  > 20) .ann(r_spr[fall_idx])  else NA_real_,
    recov_excess_ann= if (length(recov_idx) > 20) .ann(r_spr[recov_idx]) else NA_real_,
    cons_excess_ann = if (length(cons_idx)  > 20) .ann(r_spr[cons_idx])  else NA_real_,
    n_days          = nrow(common)
  )
}

# ==============================================================================
# STAGE 1 — BUILD ENHANCER CANDIDATES (stable watchlist, run quarterly)
# ==============================================================================
# Admission: OR logic — pass if ANY of the three regime conditions is met.
# This keeps the watchlist inclusive; Stage 2 sorts out who is "on" now.

build_enhancer_candidates <- function(
    xts_ret,
    rt,
    bmk            = BMK,
    w_eq           = W_EQ,
    w_fi           = W_FI,
    baseline_label = BASELINE_LABEL,
    ir_floor       = IR_FLOOR,
    fall_tolerance = FALL_TOLERANCE
) {
  baseline_xts <- .build_baseline(xts_ret, w_eq, w_fi, baseline_label)

  message(sprintf("▶ Stage 1 — screening %d tickers vs %s (%s%% SPY + %s%% IEF)...",
                  ncol(xts_ret) - 1L, baseline_label,
                  round(w_eq * 100), round(w_fi * 100)))

  regime_daily <- .regime_daily(rt)

  candidates <- purrr::map_dfr(
    setdiff(colnames(xts_ret), c(bmk, "IEF")),
    ~ .spread_stats(.x, baseline_xts, xts_ret, regime_daily)
  ) %>%
    filter(
      !is.na(full_ir),
      full_ir        > ir_floor  |   # consistent full-period alpha vs baseline, OR
      (!is.na(fall_excess_ann)  & fall_excess_ann  > 0) |  # useful in Fall vs baseline, OR
      (!is.na(recov_excess_ann) & recov_excess_ann > 0)    # useful in Recovery vs baseline
    ) %>%
    # ── trigger_type assignment ───────────────────────────────────────────────
    # A — Unconditional : Fall excess >= fall_tolerance → hold always
    # B — Regime-conditional: Fall excess <  fall_tolerance → hold in non-Fall only
    mutate(
      trigger_type = case_when(
        is.na(fall_excess_ann)          ~ "A",   # no Fall data — assume always-on
        fall_excess_ann >= fall_tolerance ~ "A",
        TRUE                             ~ "B"
      )
    ) %>%
    left_join(
      etf_metadata %>% select(ticker, name, asset_class, pf_function),
      by = "ticker"
    ) %>%
    arrange(trigger_type, desc(full_ir))

  n_a <- sum(candidates$trigger_type == "A", na.rm = TRUE)
  n_b <- sum(candidates$trigger_type == "B", na.rm = TRUE)
  message(sprintf("✅ Stage 1 complete: %d candidates (%d Type A unconditional, %d Type B regime-conditional).",
                  nrow(candidates), n_a, n_b))
  candidates
}

# ==============================================================================
# STAGE 2 — SCORE IN CURRENT CONTEXT (run monthly)
# ==============================================================================
# Four signals, equal-weight rank sum → context_score 0–100.
#
# s1  rolling_ir_63d    — Is the alpha present in the last 63 trading days?
# s2  regime_excess     — Historical excess return in TODAY's SPY regime?
# s3  trend_signal      — Is the ticker above its 200DMA today? (1/0)
# s4  spread_momentum   — Is the spread accelerating? (20d mean > 63d mean)
#
# trend_signals tibble is optional; if not available s3 is skipped.

score_current_context <- function(
    candidates,
    xts_ret,
    rt,
    bmk            = BMK,
    w_eq           = W_EQ,
    w_fi           = W_FI,
    baseline_label = BASELINE_LABEL,
    roll_win       = ROLL_WIN,
    momentum_fast  = MOMENTUM_FAST,
    trend_signals  = NULL    # tibble from 03_taa_rules.R: date, symbol, signal
) {
  baseline_xts <- .build_baseline(xts_ret, w_eq, w_fi, baseline_label)
  cur_regime   <- .current_regime(rt, xts_ret)
  regime_daily <- .regime_daily(rt)

  message(sprintf("▶ Stage 2 — scoring %d candidates vs %s | current regime: %s",
                  nrow(candidates), baseline_label, cur_regime))

  scores <- purrr::map_dfr(candidates$ticker, function(tk) {
    common <- merge(xts_ret[, tk], baseline_xts, join = "inner")
    if (nrow(common) < roll_win + 10L) return(NULL)

    r_tk  <- as.numeric(common[, 1])
    r_bl  <- as.numeric(common[, 2])
    r_spr <- r_tk - r_bl          # spread vs 6040Classic baseline
    dates <- as.Date(index(common))
    n     <- length(r_spr)

    # s1: rolling IR in the last roll_win days
    if (n < roll_win) return(NULL)
    recent    <- tail(r_spr, roll_win)
    rolling_ir <- .ir(recent)

    # s2: regime excess — historical mean spread during current regime
    lkup         <- tibble(date = dates) %>% left_join(regime_daily, by = "date")
    regime_idx   <- which(lkup$regime == cur_regime)
    regime_excess <- if (length(regime_idx) > 10)
      .ann(r_spr[regime_idx]) else NA_real_

    # s3: 200DMA trend signal (last date, from trend_signals if available)
    trend_on <- if (!is.null(trend_signals) && tk %in% trend_signals$symbol) {
      trend_signals %>%
        filter(symbol == tk) %>%
        slice_max(date, n = 1) %>%
        pull(signal) %>%
        as.numeric()
    } else NA_real_

    # s4: spread momentum — 20d mean vs 63d mean of spread
    spr_20  <- mean(tail(r_spr, momentum_fast), na.rm = TRUE)
    spr_63  <- mean(tail(r_spr, roll_win),      na.rm = TRUE)
    spread_momentum <- spr_20 - spr_63   # positive = alpha accelerating

    tibble(
      ticker          = tk,
      cur_regime      = cur_regime,
      rolling_ir      = rolling_ir,
      regime_excess   = regime_excess,
      trend_on        = trend_on,
      spread_momentum = spread_momentum
    )
  })

  # Rank-sum composite (higher rank = better signal)
  scores <- scores %>%
    mutate(
      r_ir       = rank(rolling_ir,      na.last = "keep", ties.method = "average"),
      r_regime   = rank(regime_excess,   na.last = "keep", ties.method = "average"),
      r_trend    = rank(trend_on,        na.last = "keep", ties.method = "average"),
      r_momentum = rank(spread_momentum, na.last = "keep", ties.method = "average")
    ) %>%
    rowwise() %>%
    mutate(
      # Average only the non-NA components so missing trend_signal doesn't penalise
      context_score = mean(c(r_ir, r_regime, r_trend, r_momentum), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    # Rescale 0–100
    mutate(context_score = (context_score - min(context_score, na.rm = TRUE)) /
             (max(context_score, na.rm = TRUE) - min(context_score, na.rm = TRUE)) * 100) %>%
    arrange(desc(context_score)) %>%
    left_join(candidates %>% select(ticker, full_ir, fall_excess_ann,
                                    recov_excess_ann, asset_class, name,
                                    trigger_type),
              by = "ticker") %>%
    # ── trigger_type refinement: B → C when 200DMA signal data is available ──
    # Type C — 200DMA-signal: use trend filter instead of hard regime switch
    mutate(
      trigger_type_final = case_when(
        trigger_type == "B" & !is.na(trend_on) ~ "C",
        TRUE                                    ~ trigger_type
      ),
      trigger_label = case_when(
        trigger_type_final == "A" ~ "A — Unconditional",
        trigger_type_final == "B" ~ "B — Regime-conditional (non-Fall)",
        trigger_type_final == "C" ~ "C — 200DMA signal"
      )
    )

  type_counts <- table(scores$trigger_type_final)
  message(sprintf("✅ Stage 2 complete. Trigger types: %s",
                  paste(names(type_counts), type_counts, sep = "=", collapse = " | ")))
  scores
}

# ==============================================================================
# STAGE 3 — SELECT ENHANCERS (correlation guard)
# ==============================================================================
# Greedy forward: add highest-scoring ticker that is not too correlated
# with any already-selected ticker.

select_enhancers <- function(
    enhancer_scores,
    xts_ret,
    n_select = N_SELECT,
    rho_max  = RHO_MAX
) {
  message(sprintf("▶ Stage 3 — selecting up to %d enhancers (ρ_max = %.2f)...",
                  n_select, rho_max))

  ranked   <- enhancer_scores %>% arrange(desc(context_score)) %>% pull(ticker)
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

  result <- enhancer_scores %>%
    filter(ticker %in% selected) %>%
    arrange(desc(context_score)) %>%
    mutate(rank = row_number())

  message(sprintf("✅ Stage 3 complete: selected %d enhancers.", nrow(result)))
  result
}

# ==============================================================================
# CONSOLE REPORT
# ==============================================================================

.print_report <- function(selected_enhancers,
                          bmk            = BMK,
                          baseline_label = BASELINE_LABEL) {
  cur <- unique(selected_enhancers$cur_regime)
  message(sprintf("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"))
  message(sprintf("🎯  ENHANCER SELECTION vs %s  |  Current regime: %s", baseline_label, cur))
  message(sprintf("    (Regime detection: %s  |  Spread computed vs %s)", bmk, baseline_label))
  message(sprintf("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"))

  selected_enhancers %>%
    mutate(
      score      = sprintf("%.1f",  context_score),
      full_ir_f  = sprintf("%.2f",  full_ir),
      fall_f     = sprintf("%+.1f%%", fall_excess_ann  * 100),
      recov_f    = sprintf("%+.1f%%", recov_excess_ann * 100),
      roll_ir_f  = sprintf("%.2f",  rolling_ir),
      regime_f   = sprintf("%+.1f%%", regime_excess * 100),
      trend_f    = if_else(is.na(trend_on), "n/a", if_else(trend_on == 1, "ON", "OFF")),
      momentum_f = sprintf("%+.4f", spread_momentum)
    ) %>%
    select(
      `#`          = rank,
      Ticker       = ticker,
      Trigger      = trigger_label,
      `Ctx Score`  = score,
      `Full IR`    = full_ir_f,
      `Fall α`     = fall_f,
      `Recov α`    = recov_f,
      `Roll IR 63d`= roll_ir_f,
      `Regime α`   = regime_f,
      `200DMA`     = trend_f,
      Momentum     = momentum_f,
      `Asset class`= asset_class
    ) %>%
    print(n = Inf)
  message("")
}

# ==============================================================================
# XTS PAIR PLOTS — one per selected enhancer vs bmk
# ==============================================================================

plot_enhancer_pairs <- function(
    selected_enhancers,
    xts_ret,
    rt,
    bmk            = BMK,
    w_eq           = W_EQ,
    w_fi           = W_FI,
    baseline_label = BASELINE_LABEL,
    save_png       = TRUE,   # TRUE  = save PNG to key_plots/pair_analysis/ (readable size)
                              # FALSE = print to RStudio Plots pane (size = pane size)
    fig_width      = 13,
    fig_height     = 21,
    dpi            = 150
) {
  baseline_xts <- .build_baseline(xts_ret, w_eq, w_fi, baseline_label)

  if (isTRUE(save_png)) {
    out_dir <- here("ticker_fingerprints")
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  }

  purrr::walk(seq_len(nrow(selected_enhancers)), function(i) {
    row  <- selected_enhancers[i, ]
    tk   <- row$ticker
    trig <- if ("trigger_label" %in% names(row)) row$trigger_label else ""
    pfx  <- sprintf("[#%d | Score %.1f | %s | %s regime]",
                    row$rank, row$context_score, trig, row$cur_regime)

    message(sprintf("\n── Pair plot %d/%d: %s vs %s ─────────────────────────",
                    i, nrow(selected_enhancers), tk, baseline_label))

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
      role         = "Enhancer",
      print_plot   = !save_png   # suppress internal print when saving to PNG
    )

    if (isTRUE(save_png)) {
      fname <- file.path(out_dir,
                         sprintf("enhancer_%02d_%s_vs_%s.png", i, tk,
                                 gsub("[^A-Za-z0-9]", "", baseline_label)))
      ggplot2::ggsave(fname, plot = result$combined,
                      width = fig_width, height = fig_height, dpi = dpi)
      message(sprintf("   💾 Saved: ticker_fingerprints/%s", basename(fname)))
    }
  })

  if (isTRUE(save_png))
    message(sprintf("\n✅ %d ticker fingerprints saved to ticker_fingerprints/  — open in Files pane to view.",
                    nrow(selected_enhancers)))

  invisible(NULL)
}

# ==============================================================================
# SLEEVE WEALTH CHART
# ==============================================================================
# For each asset_class group in selected_enhancers, plot cumulative wealth of
# all tickers in the sleeve + BMK always as a grey reference line.
# End-of-line labels carry ann return.  Regime shading from rt.
#
# show_candidates  TRUE  = show ALL Stage-1 candidates from the sleeve (grey thin)
#                          with selected ones highlighted on top (coloured thick)
#                  FALSE = show only selected tickers (default)

plot_enhancer_sleeves <- function(
    selected_enhancers,
    xts_ret,
    rt,
    bmk                 = BMK,
    w_eq                = W_EQ,
    w_fi                = W_FI,
    baseline_label      = BASELINE_LABEL,
    show_candidates     = FALSE,
    enhancer_candidates = NULL   # required when show_candidates = TRUE
) {
  # Add baseline as a column so it plots as the reference line.
  # R prefixes digit-starting names with "X" on merge (e.g. "6040Classic" → "X6040Classic").
  # Rename back immediately so all downstream filter(ticker == baseline_label) work.
  baseline_xts <- .build_baseline(xts_ret, w_eq, w_fi, baseline_label)
  xts_ret      <- merge(xts_ret, baseline_xts, join = "left")
  mangled      <- paste0("X", baseline_label)
  if (mangled %in% colnames(xts_ret) && !baseline_label %in% colnames(xts_ret))
    colnames(xts_ret)[colnames(xts_ret) == mangled] <- baseline_label
  library(scales)

  # Regime shading rectangles
  regime_rect <- rt %>%
    filter(regime %in% c("Fall", "Recovery")) %>%
    mutate(
      fill = if_else(regime == "Fall", "#fca5a5", "#bbf7d0"),
      xmin = as.Date(xmin),
      xmax = as.Date(xmax)
    )

  # Helper: build wealth tibble for a vector of tickers + bmk
  .wealth_tbl <- function(tickers, xts_ret, bmk) {
    all_tks <- c(bmk, tickers)
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

  # Helper: end-of-line label with ann return
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

  # Colour palette: BMK always grey, selected get canonical palette
  default_palette <- c("#7c3aed", "#3b82f6", "#f59e0b",
                       "#22c55e", "#ef4444", "#0ea5e9", "#a78bfa")

  sleeves <- unique(selected_enhancers$asset_class)
  sleeves <- sleeves[!is.na(sleeves)]

  purrr::walk(sleeves, function(sleeve) {

    sel_tks <- selected_enhancers %>%
      filter(asset_class == sleeve) %>%
      arrange(desc(context_score)) %>%
      pull(ticker)

    if (length(sel_tks) == 0) return(invisible(NULL))

    # Optionally add unselected candidates from same sleeve (thin background lines)
    bg_tks <- character(0)
    if (isTRUE(show_candidates) && !is.null(enhancer_candidates)) {
      bg_tks <- enhancer_candidates %>%
        filter(asset_class == sleeve,
               !ticker %in% sel_tks,
               ticker %in% colnames(xts_ret)) %>%
        pull(ticker)
    }

    # Build wealth data
    all_plot_tks <- c(sel_tks, bg_tks)
    wdf          <- .wealth_tbl(all_plot_tks, xts_ret, baseline_label)
    if (is.null(wdf)) return(invisible(NULL))

    # Split: baseline row separate from ticker rows so it always draws on top
    wdf_tickers  <- wdf %>% filter(ticker != baseline_label)
    wdf_baseline <- wdf %>% filter(ticker == baseline_label)
    last_pts     <- .end_labels(wdf)

    # Colour / width / alpha maps — baseline excluded (it has its own layer)
    n_sel       <- length(sel_tks)
    sel_colours <- setNames(default_palette[seq_len(n_sel)], sel_tks)
    bg_colours  <- setNames(rep("#d1d5db", length(bg_tks)), bg_tks)
    all_colours <- c(sel_colours, bg_colours)

    lw_map <- c(
      setNames(rep(0.6, n_sel), sel_tks),
      setNames(rep(0.3, length(bg_tks)), bg_tks)
    )

    alpha_map <- c(
      setNames(rep(1.0, n_sel), sel_tks),
      setNames(rep(0.35, length(bg_tks)), bg_tks)
    )

    # Factor order for ticker lines: bg draws first (bottom), selected on top
    level_order <- c(bg_tks, sel_tks)
    wdf_tickers <- wdf_tickers %>%
      mutate(ticker = factor(ticker, levels = level_order))
    last_pts <- last_pts %>%
      mutate(ticker = as.character(ticker))

    # Score annotation for subtitle
    score_str <- selected_enhancers %>%
      filter(asset_class == sleeve) %>%
      arrange(desc(context_score)) %>%
      mutate(s = sprintf("%s(%.0f)", ticker, context_score)) %>%
      pull(s) %>%
      paste(collapse = "  ")

    cur_regime <- unique(selected_enhancers$cur_regime)[1]

    p <- ggplot(wdf_tickers, aes(date, wealth,
                                  colour    = ticker,
                                  linewidth = ticker,
                                  alpha     = ticker)) +
      # Regime shading
      geom_rect(data        = regime_rect,
                aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf,
                    fill = fill),
                inherit.aes = FALSE, alpha = 0.18) +
      scale_fill_identity() +
      # Ticker wealth lines (bg + selected)
      geom_line() +
      # Baseline drawn last — always on top, dark navy, clearly visible
      geom_line(data      = wdf_baseline,
                aes(date, wealth),
                inherit.aes = FALSE,
                colour      = "#1d3461",
                linewidth   = 1.8) +
      # End-of-line labels for selected tickers
      geom_text(
        data      = last_pts %>% filter(ticker != baseline_label),
        aes(label = end_label),
        hjust     = 0, nudge_x = 60,
        size      = 2.9, fontface = "bold", lineheight = 0.85
      ) +
      # Baseline end label — offset slightly lower to avoid overlap
      geom_text(
        data      = last_pts %>% filter(ticker == baseline_label),
        aes(date, wealth, label = end_label),
        inherit.aes = FALSE,
        hjust     = 0, nudge_x = 60,
        size      = 2.9, fontface = "bold", colour = "#1d3461", lineheight = 0.85
      ) +
      # Manual scales (tickers only — baseline has its own layer)
      scale_colour_manual(values = all_colours, guide = "none") +
      scale_linewidth_manual(values = lw_map,   guide = "none") +
      scale_alpha_manual(values = alpha_map,    guide = "none") +
      scale_y_continuous(labels = dollar_format(prefix = "$"),
                         limits = c(0.5, NA),
                         expand = expansion(mult = c(0, 0.05))) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      coord_cartesian(clip = "off") +
      labs(
        title    = sprintf("%s — Selected Enhancers vs %s", sleeve, baseline_label),
        subtitle = sprintf(
          "Regime: %s  |  Scores: %s\nPink = Fall  |  Green = Recovery  |  Navy = %s baseline  |  Ann return at line end",
          cur_regime, score_str, baseline_label
        ),
        x = NULL, y = "Wealth ($1 invested)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.margin      = margin(5, 160, 5, 5),
        panel.grid.minor = element_blank(),
        plot.title       = element_text(face = "bold", size = 13),
        plot.subtitle    = element_text(colour = "grey50", size = 9)
      )

    print(p)
  })

  invisible(NULL)
}

# ==============================================================================
# WRAPPER — find_enhancers()
# ==============================================================================
# Single entry point: given a baseline (SAA), run all three stages and return
# candidates, scores, and selected enhancers in one list.
#
# Usage:
#   result <- find_enhancers(xts_ret, rt)                        # default 6040Classic
#   result <- find_enhancers(xts_ret, rt, w_eq=0.70, w_fi=0.30) # 70/30 baseline
#
#   result$candidates   — Stage 1 watchlist + trigger_type A/B
#   result$scores       — Stage 2 context scores + trigger_type_final A/B/C
#   result$selected     — Stage 3 final selection
#   result$baseline     — xts of baseline daily returns
#   result$params       — list of parameters used

find_enhancers <- function(
    xts_ret,
    rt,
    w_eq           = W_EQ,
    w_fi           = W_FI,
    baseline_label = BASELINE_LABEL,
    bmk            = BMK,
    ir_floor       = IR_FLOOR,
    fall_tolerance = FALL_TOLERANCE,
    roll_win       = ROLL_WIN,
    momentum_fast  = MOMENTUM_FAST,
    n_select       = N_SELECT,
    rho_max        = RHO_MAX,
    trend_signals  = NULL
) {
  message(sprintf("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"))
  message(sprintf("🔍  find_enhancers()  |  Baseline: %s = %.0f%% SPY + %.0f%% IEF",
                  baseline_label, w_eq * 100, w_fi * 100))
  message(sprintf("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"))

  baseline_xts <- .build_baseline(xts_ret, w_eq, w_fi, baseline_label)

  candidates <- build_enhancer_candidates(
    xts_ret, rt,
    bmk            = bmk,
    w_eq           = w_eq,
    w_fi           = w_fi,
    baseline_label = baseline_label,
    ir_floor       = ir_floor,
    fall_tolerance = fall_tolerance
  )

  scores <- score_current_context(
    candidates, xts_ret, rt,
    bmk            = bmk,
    w_eq           = w_eq,
    w_fi           = w_fi,
    baseline_label = baseline_label,
    roll_win       = roll_win,
    momentum_fast  = momentum_fast,
    trend_signals  = trend_signals
  )

  selected <- select_enhancers(scores, xts_ret,
                                n_select = n_select,
                                rho_max  = rho_max)

  list(
    candidates = candidates,
    scores     = scores,
    selected   = selected,
    baseline   = baseline_xts,
    params     = list(
      w_eq           = w_eq,
      w_fi           = w_fi,
      baseline_label = baseline_label,
      ir_floor       = ir_floor,
      fall_tolerance = fall_tolerance,
      roll_win       = roll_win,
      n_select       = n_select,
      rho_max        = rho_max
    )
  )
}

# ==============================================================================
# AUTO-RUN
# ==============================================================================

# Load trend_signals if available (optional — enhances Stage 2 with s3)
trend_signals <- NULL
ts_path <- here("02_data_processed/trend_signals.rds")
if (file.exists(ts_path)) {
  trend_signals <- readRDS(ts_path)
  message("📥 trend_signals loaded — 200DMA component active in Stage 2.")
} else {
  message("ℹ️  trend_signals not found — Stage 2 runs on 3 signals (s1/s2/s4).")
}

# Build baseline once (used inside all three stages)
baseline_6040 <- .build_baseline(xts_ret, W_EQ, W_FI, BASELINE_LABEL)
message(sprintf("📐 Baseline: %s = %.0f%% SPY + %.0f%% IEF",
                BASELINE_LABEL, W_EQ * 100, W_FI * 100))

# Run pipeline
enhancer_candidates  <- build_enhancer_candidates(xts_ret, rt)
enhancer_scores      <- score_current_context(enhancer_candidates, xts_ret, rt,
                                               trend_signals = trend_signals)
selected_enhancers   <- select_enhancers(enhancer_scores, xts_ret)

# Save
write_rds(enhancer_candidates, here("02_data_processed/enhancer_candidates.rds"))
write_rds(enhancer_scores,     here("02_data_processed/enhancer_scores.rds"))
write_rds(selected_enhancers,  here("02_data_processed/selected_enhancers.rds"))
message("💾 Saved: enhancer_candidates, enhancer_scores, selected_enhancers")

if (!isTRUE(getOption("knitr.in.progress"))) {
  .print_report(selected_enhancers)
  plot_enhancer_sleeves(selected_enhancers, xts_ret, rt,
                        show_candidates     = TRUE,
                        enhancer_candidates = enhancer_candidates)
  plot_enhancer_pairs(selected_enhancers, xts_ret, rt)
}

message("✅ Stage DAA-08 complete: enhancer screen finished.")
