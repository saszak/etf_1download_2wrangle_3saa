################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE: scripts_saa_structured/fractal_saa_engine.R
# Purpose: Pure Structural Hierarchical/Fractal SAA Engine
#
# DESIGN PHILOSOPHY:
#   No time series required. Weights are defined top-down in 3 layers:
#   Layer 1: Sleeve split         (EQ vs FI, e.g. 50/50)
#   Layer 2: Core vs Enhancer     (how much of sleeve stays in anchor)
#   Layer 3: Bucket → ETF         (how the enhancer pool is sub-divided)
################################################################################

library(tidyverse)
library(plotly)

# ==============================================================================
# CORE FUNCTION: build_fractal_saa()
# ==============================================================================
#
# INPUTS:
#   sleeve_weights   : named numeric vector. How the total portfolio splits
#                      across sleeves. Must sum to 1.
#                      e.g., c(EQ = 0.50, FI = 0.50)
#
#   core_anchors     : named character vector. The single anchor ETF per sleeve.
#                      e.g., c(EQ = "SPY", FI = "AGG")
#
#   enhancement_rates: named numeric vector (0 to 1). What fraction of each
#                      sleeve is given to Enhancers/Stabilizers (the universe).
#                      The anchor retains (1 - rate) of the sleeve.
#                      e.g., c(EQ = 0.40, FI = 0.30)
#
#   buckets_df       : tibble defining the universe ETFs with columns:
#                        sleeve         : "EQ" or "FI"
#                        bucket         : bucket name (e.g., "Growth", "Duration")
#                        ticker         : ETF ticker
#                        bucket_pct     : this bucket's share of the sleeve's
#                                         enhancer pool (per sleeve, sums to 1)
#                        etf_pct        : this ETF's share within its bucket
#                                         (per bucket, sums to 1)
#
# OUTPUTS: a list with
#   $weights_df    : full weight table (ticker, weight, sleeve, bucket, type)
#   $w_vec         : named numeric vector ready for calc_saa_portfolio()
#   $sunburst_data : tibble ready for plot_ly sunburst
#   $summary       : sleeve-level and total weight summary
#
# ==============================================================================

build_fractal_saa <- function(sleeve_weights,
                               core_anchors,
                               enhancement_rates,
                               buckets_df) {

  # ── Validation ──────────────────────────────────────────────────────────────
  sleeves <- names(sleeve_weights)

  if (abs(sum(sleeve_weights) - 1) > 0.001)
    stop("sleeve_weights must sum to 1.0")

  if (!all(sleeves %in% names(core_anchors)))
    stop("core_anchors must have an entry for every sleeve")

  if (!all(sleeves %in% names(enhancement_rates)))
    stop("enhancement_rates must have an entry for every sleeve")

  if (any(enhancement_rates < 0) | any(enhancement_rates > 1))
    stop("enhancement_rates must be between 0 and 1")

  required_cols <- c("sleeve", "bucket", "ticker", "bucket_pct", "etf_pct")
  if (!all(required_cols %in% names(buckets_df)))
    stop(paste("buckets_df must contain columns:", paste(required_cols, collapse = ", ")))

  # ── Layer 1 & 2: Anchor weights ─────────────────────────────────────────────
  anchor_rows <- map_dfr(sleeves, function(s) {
    anchor_weight <- sleeve_weights[s] * (1 - enhancement_rates[s])
    tibble(
      sleeve  = s,
      bucket  = paste0(s, "_Core"),
      ticker  = core_anchors[s],
      weight  = anchor_weight,
      type    = "Core_Anchor"
    )
  })

  # ── Layer 3: Enhancer / Stabilizer weights ───────────────────────────────────
  enhancer_rows <- buckets_df %>%
    mutate(
      sleeve_weight     = sleeve_weights[sleeve],
      enhancer_pool     = sleeve_weight * enhancement_rates[sleeve],
      weight            = enhancer_pool * bucket_pct * etf_pct,
      type              = if_else(sleeve == "EQ", "Enhancer", "Stabilizer")
    ) %>%
    select(sleeve, bucket, ticker, weight, type)

  # ── Combine ──────────────────────────────────────────────────────────────────
  weights_df <- bind_rows(anchor_rows, enhancer_rows) %>%
    arrange(sleeve, type, bucket, ticker)

  # ── Named weight vector (for calc_saa_portfolio) ────────────────────────────
  w_vec <- setNames(weights_df$weight, weights_df$ticker)

  total <- sum(w_vec)
  if (abs(total - 1) > 0.001)
    warning(sprintf("Total weight = %.4f (expected 1.0). Check bucket_pct/etf_pct inputs.", total))

  # ── Sunburst data ────────────────────────────────────────────────────────────
  # Level 0: root
  root <- tibble(
    ids     = "Portfolio",
    labels  = "<b>Portfolio</b>",
    parents = "",
    values  = 1
  )

  # Level 1: sleeves
  sleeve_nodes <- tibble(
    ids     = sleeves,
    labels  = paste0(sleeves, " (", scales::percent(sleeve_weights), ")"),
    parents = "Portfolio",
    values  = sleeve_weights
  )

  # Level 2: core + buckets (one node per distinct sleeve+bucket combo)
  bucket_nodes <- weights_df %>%
    group_by(sleeve, bucket) %>%
    summarise(values = sum(weight), .groups = "drop") %>%
    mutate(
      ids     = paste0(sleeve, "_", bucket),
      labels  = paste0(bucket, "<br>", scales::percent(values, accuracy = 0.1)),
      parents = sleeve
    ) %>%
    select(ids, labels, parents, values)

  # Level 3: individual ETFs
  etf_nodes <- weights_df %>%
    mutate(
      ids     = ticker,
      labels  = paste0(ticker, "<br>", scales::percent(weight, accuracy = 0.1)),
      parents = paste0(sleeve, "_", bucket),
      values  = weight
    ) %>%
    select(ids, labels, parents, values)

  sunburst_data <- bind_rows(root, sleeve_nodes, bucket_nodes, etf_nodes)

  # ── Summary ──────────────────────────────────────────────────────────────────
  summary_tbl <- weights_df %>%
    group_by(sleeve, type) %>%
    summarise(
      n_etfs  = n(),
      weight  = sum(weight),
      .groups = "drop"
    ) %>%
    mutate(pct = scales::percent(weight, accuracy = 0.1))

  message("── Fractal SAA Weight Summary ───────────────────────────")
  message(sprintf("   Total Weight : %.4f", total))
  print(summary_tbl)
  message("─────────────────────────────────────────────────────────")

  list(
    weights_df    = weights_df,
    w_vec         = w_vec,
    sunburst_data = sunburst_data,
    summary       = summary_tbl
  )
}


# ==============================================================================
# HELPER: plot_fractal_sunburst()
# ==============================================================================

plot_fractal_sunburst <- function(fractal_obj, title = "Fractal SAA Structure") {
  plot_ly(
    data         = fractal_obj$sunburst_data,
    ids          = ~ids,
    labels       = ~labels,
    parents      = ~parents,
    values       = ~values,
    type         = "sunburst",
    branchvalues = "total",
    marker       = list(colorscale = "Portland")
  ) %>%
    layout(title = paste0("<b>", title, "</b>"))
}


# ==============================================================================
# EXAMPLE: 50/50 Core with Enhancers and Stabilizers
# ==============================================================================

# ── Step 1: Define Sleeves ────────────────────────────────────────────────────
my_sleeves     <- c(EQ = 0.50, FI = 0.50)
my_anchors     <- c(EQ = "SPY", FI = "AGG")
my_enh_rates   <- c(EQ = 0.40, FI = 0.30)
# Interpretation:
#   EQ sleeve: 60% stays in SPY, 40% goes to Equity Enhancers
#   FI sleeve: 70% stays in AGG, 30% goes to FI Stabilizers

# ── Step 2: Define Universe (Buckets → ETFs) ──────────────────────────────────
my_buckets <- tribble(
  ~sleeve, ~bucket,          ~ticker, ~bucket_pct, ~etf_pct,

  # ── EQUITY ENHANCERS (bucket_pct sums to 1.0 within EQ) ──
  "EQ",   "Growth",          "QQQ",   0.40,        0.60,
  "EQ",   "Growth",          "XLK",    0.40,        0.40,
  "EQ",   "International",   "IEFA",   0.35,        1.00,
  "EQ",   "Cyclical",        "XLF",    0.25,        0.50,
  "EQ",   "Cyclical",        "XLI",    0.25,        0.50,

  # ── FI STABILIZERS (bucket_pct sums to 1.0 within FI) ───
  "FI",   "Investment_Grade","LQD",    0.50,        1.00,
  "FI",   "Duration",        "IEF",    0.30,        0.60,
  "FI",   "Duration",        "TLT",    0.30,        0.40,
  "FI",   "Inflation_Credit","TIP",    0.20,        0.50,
  "FI",   "Inflation_Credit","HYG",    0.20,        0.50
)

# ── Step 3: Build the Fractal SAA ─────────────────────────────────────────────
fractal_saa <- build_fractal_saa(
  sleeve_weights    = my_sleeves,
  core_anchors      = my_anchors,
  enhancement_rates = my_enh_rates,
  buckets_df        = my_buckets
)

# ── Step 4: Visualize ─────────────────────────────────────────────────────────
plot_fractal_sunburst(fractal_saa, title = "Fractal SAA: 50/50 Core + Universe")

# ── Step 5: View weight table ─────────────────────────────────────────────────
fractal_saa$weights_df

# ==============================================================================
# HELPER: apply_signal_layer()
# ==============================================================================
#
# Adjusts ETF weights WITHIN each bucket using a data signal.
# The sleeve split and bucket allocations remain structurally fixed.
# Only Layer 3 (ETF weights inside each bucket) is modified.
#
# INPUTS:
#   fractal_obj : output of build_fractal_saa()
#   xts_data    : xts of returns (use xts_ret for inv_vol, xts_rel for momentum)
#   method      : "inv_vol"  — weight by 1/volatility within bucket
#                 "momentum" — weight by cumulative return within bucket
#                 "equal"    — reset to equal weight within bucket (default)
#   lookback    : integer, number of trading days for signal calculation (default 63 = 1 quarter)
#
# OUTPUT: updated fractal_obj with signal-adjusted $w_vec and $weights_df
#
# ==============================================================================

apply_signal_layer <- function(fractal_obj,
                                xts_data,
                                method   = "inv_vol",
                                lookback = 63) {

  valid_methods <- c("inv_vol", "momentum", "equal")
  if (!(method %in% valid_methods))
    stop(paste("method must be one of:", paste(valid_methods, collapse = ", ")))

  weights_df <- fractal_obj$weights_df

  # Only re-weight Enhancers and Stabilizers (not Core Anchors)
  universe_df  <- weights_df %>% filter(type != "Core_Anchor")
  anchor_df    <- weights_df %>% filter(type == "Core_Anchor")

  # Use the most recent `lookback` rows of xts_data
  xts_window <- tail(xts_data, lookback)

  # For each bucket, compute signal scores and re-weight ETFs proportionally
  universe_adj <- universe_df %>%
    group_by(sleeve, bucket) %>%
    mutate(
      bucket_total = sum(weight),   # preserve the total $ allocated to this bucket
      score = map_dbl(ticker, function(tk) {
        if (!(tk %in% colnames(xts_window))) return(NA_real_)
        col <- xts_window[, tk]
        if (method == "inv_vol") {
          vol <- as.numeric(StdDev.annualized(col, scale = 252))
          if (is.na(vol) | vol == 0) return(NA_real_)
          return(1 / vol)
        }
        if (method == "momentum") {
          cum_ret <- as.numeric(Return.cumulative(col))
          return(cum_ret)
        }
        if (method == "equal") {
          return(1)
        }
      })
    ) %>%
    mutate(
      # If any score is NA or negative (momentum can go negative), fall back to equal
      score        = if_else(is.na(score) | score <= 0, mean(score[score > 0], na.rm = TRUE), score),
      score        = if_else(is.na(score), 1, score),   # full fallback
      score_norm   = score / sum(score),
      weight       = bucket_total * score_norm
    ) %>%
    select(-bucket_total, -score, -score_norm) %>%
    ungroup()

  # Rebuild full weights_df
  new_weights_df <- bind_rows(anchor_df, universe_adj) %>%
    arrange(sleeve, type, bucket, ticker)

  # Updated w_vec
  new_w_vec <- setNames(new_weights_df$weight, new_weights_df$ticker)

  total <- sum(new_w_vec)
  message(sprintf("── Signal Layer Applied: %s (lookback = %d days) ──", method, lookback))
  message(sprintf("   Total Weight: %.4f", total))

  # Return updated fractal object
  fractal_obj$weights_df <- new_weights_df
  fractal_obj$w_vec      <- new_w_vec
  fractal_obj$signal     <- list(method = method, lookback = lookback)
  fractal_obj
}


# ==============================================================================
# STEP 6: Performance Audit vs 50/50 Benchmark (requires xts_ret)
# ==============================================================================

source("scripts_saa_taa/05_saa_portfolio.R")

# --- Guard: filter w_vec to tickers available in xts_ret ---
safe_w_vec <- function(w_vec, xts_data) {
  avail  <- intersect(names(w_vec), colnames(xts_data))
  missing <- setdiff(names(w_vec), avail)
  if (length(missing) > 0)
    warning("Tickers not found in xts_ret, excluded: ", paste(missing, collapse = ", "))
  w_out <- w_vec[avail]
  w_out / sum(w_out)   # re-normalize
}

# --- Pure Structural Fractal ---
fractal_result <- calc_saa_portfolio(
  xts_returns    = xts_ret,
  weights_vector = safe_w_vec(fractal_saa$w_vec, xts_ret),
  rebalance_freq = "quarters"
)

# --- Signal-Enhanced Fractal (Inverse Volatility within buckets) ---
fractal_signal <- apply_signal_layer(
  fractal_obj = fractal_saa,
  xts_data    = xts_ret,
  method      = "inv_vol",
  lookback    = 63
)

fractal_signal_result <- calc_saa_portfolio(
  xts_returns    = xts_ret,
  weights_vector = safe_w_vec(fractal_signal$w_vec, xts_ret),
  rebalance_freq = "quarters"
)

# --- 50/50 Benchmark ---
bmk_5050 <- calc_saa_portfolio(
  xts_returns    = xts_ret,
  weights_vector = c(SPY = 0.50, AGG = 0.50),
  rebalance_freq = "quarters"
)

# --- Combine for Comparison ---
audit_xts <- merge(
  fractal_result$returns,
  fractal_signal_result$returns,
  bmk_5050$returns
)
colnames(audit_xts) <- c("Fractal_Structural", "Fractal_InvVol", "Benchmark_50_50")

# --- Performance Summary Chart ---
charts.PerformanceSummary(
  audit_xts,
  main       = "Fractal SAA: Structural vs Signal-Enhanced vs 50/50 Benchmark",
  colorset   = c("#2c3e50", "#27ae60", "#e74c3c"),
  lwd        = c(3, 3, 1),
  legend.loc = "topleft"
)

# --- Annualized Stats Table ---
table.AnnualizedReturns(audit_xts, scale = 252)

# ==============================================================================
# ATTRIBUTION: fractal_attribution()
# ==============================================================================
#
# Decomposes the fractal portfolio's outperformance vs the 50/50 benchmark
# into each position's L/S contribution, using xts_rel (returns vs SPY).
#
# LOGIC:
#   Core Anchors (SPY, AGG) : relative return = 0 by definition (they are base)
#   EQ Enhancers            : contribution = weight × xts_rel[ticker]
#   FI Stabilizers          : contribution = weight × xts_rel[ticker] vs SPY
#                             (note: vs SPY, not AGG — directionally useful)
#
# INPUTS:
#   fractal_obj : output of build_fractal_saa() or apply_signal_layer()
#   xts_rel     : xts of relative returns vs SPY (from your pipeline)
#   xts_ret     : xts of absolute returns (for absolute stats)
#
# OUTPUTS: a list with
#   $table      : ticker-level attribution table
#   $total_alpha: total portfolio alpha (sum of weighted relative returns)
#   $chart      : ggplot bar chart of contributions
#
# ==============================================================================

fractal_attribution <- function(fractal_obj, xts_rel, xts_ret) {

  weights_df <- fractal_obj$weights_df

  # ── Per-ticker stats ─────────────────────────────────────────────────────────
  attr_df <- weights_df %>%
    mutate(
      # Annualized alpha (relative return vs SPY)
      ann_alpha = map_dbl(ticker, function(tk) {
        if (tk %in% c("SPY", "AGG") | !(tk %in% colnames(xts_rel))) return(0)
        as.numeric(Return.annualized(xts_rel[, tk], scale = 252))
      }),

      # Tracking error (vol of relative return)
      tracking_error = map_dbl(ticker, function(tk) {
        if (tk %in% c("SPY", "AGG") | !(tk %in% colnames(xts_rel))) return(NA_real_)
        as.numeric(StdDev.annualized(xts_rel[, tk], scale = 252))
      }),

      # Information ratio
      IR = if_else(is.na(tracking_error) | tracking_error == 0,
                   NA_real_,
                   ann_alpha / tracking_error),

      # Weighted alpha contribution to portfolio
      contribution = weight * ann_alpha,

      # Absolute annualized return (for reference)
      ann_ret_abs = map_dbl(ticker, function(tk) {
        if (!(tk %in% colnames(xts_ret))) return(NA_real_)
        as.numeric(Return.annualized(xts_ret[, tk], scale = 252))
      })
    ) %>%
    arrange(desc(abs(contribution)))

  total_alpha <- sum(attr_df$contribution)

  # ── Print summary ────────────────────────────────────────────────────────────
  message("── Fractal Attribution Report ───────────────────────────────")
  message(sprintf("   Total Portfolio Alpha vs SPY: %+.2f%%", total_alpha * 100))
  message("─────────────────────────────────────────────────────────────")

  print(
    attr_df %>%
      select(sleeve, bucket, ticker, type, weight, ann_alpha, tracking_error, IR, contribution) %>%
      mutate(
        across(c(weight, ann_alpha, tracking_error, contribution),
               ~scales::percent(., accuracy = 0.01)),
        IR = round(IR, 2)
      )
  )

  # ── Bar chart: contribution per ticker ───────────────────────────────────────
  chart <- attr_df %>%
    filter(type != "Core_Anchor") %>%
    mutate(
      ticker    = forcats::fct_reorder(ticker, contribution),
      direction = if_else(contribution >= 0, "Positive", "Negative")
    ) %>%
    ggplot(aes(x = ticker, y = contribution, fill = direction)) +
    geom_col(width = 0.6) +
    geom_text(aes(
      label = paste0(ticker, "  ", scales::percent(contribution, accuracy = 0.01)),
      hjust = if_else(contribution >= 0, -0.1, 1.1)
    ), size = 3.2, fontface = "bold", show.legend = FALSE) +
    coord_flip() +
    scale_fill_manual(values = c("Positive" = "#27ae60", "Negative" = "#e74c3c")) +
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = 0.25)) +
    facet_grid(sleeve ~ ., scales = "free_y", space = "free") +
    theme_minimal(base_size = 12) +
    theme(
      legend.position  = "none",
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold")
    ) +
    labs(
      title    = "Fractal SAA: L/S Alpha Contribution by Position",
      subtitle = sprintf("Total Portfolio Alpha vs SPY: %+.2f%%", total_alpha * 100),
      x        = NULL,
      y        = "Weighted Alpha Contribution (ann.)"
    )

  print(chart)

  invisible(list(table = attr_df, total_alpha = total_alpha, chart = chart))
}


# ==============================================================================
# STEP 7: Run Attribution
# ==============================================================================

fractal_attr <- fractal_attribution(
  fractal_obj = fractal_saa,
  xts_rel     = xts_rel,
  xts_ret     = xts_ret
)

################################################################################
