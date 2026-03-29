################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : scripts_daa_saa_taa/02_daa_classifier.R
# Purpose : Classify every ticker in the Sovereign Universe into one of three
#           DAA roles based on regime-conditional relative performance:
#
#   EQ-Enhancer   — ALPHA vs SPY in Recovery + Consolidation
#                   (add to equity sleeve when regime is constructive)
#   EQ-Stabilizer — HEDGE or STABLE vs SPY in Fall
#                   (protect equity sleeve when regime deteriorates)
#   FI-Enhancer   — Outperforms AGG in Recovery (positive spread, risk-adjusted)
#                   (add to FI sleeve as yield / carry / duration tilt)
#
# DEPENDS ON
#   01_saa_baseline.R   (SAA_60_40, rt)
#   spy_dd_regime_rel.R (.outperf_type)
#
# OUTPUTS
#   episode_stats      tibble — per ticker × regime episode stats
#   daa_class_tbl      tibble — one row per ticker, DAA role classification
#   p_eq_enhancer              — dot plot: EQ-Enhancers ranked by alpha score
#   p_eq_stabilizer            — dot plot: EQ-Stabilizers ranked by hedge score
#   p_fi_enhancer              — dot plot: FI-Enhancers ranked by spread vs AGG
#
# FUNCTIONS
#   compute_episode_stats(xts_ret, rt, benchmark)
#   classify_daa_universe(episode_stats, eq_bmk, fi_bmk)
#   plot_daa_role(daa_class_tbl, role, title)
################################################################################

library(tidyverse)
library(xts)
library(PerformanceAnalytics)
library(scales)
library(here)

if (!exists("project_tree"))     source(here("project_tree.R"))
if (!exists("etf_metadata"))     source(here(project_tree$scripts$init))
if (!exists("xts_ret"))          source(here(project_tree$scripts$wrangle))
if (!exists("build_regime_table"))
  source(here("scripts_spy_dd_regime/spy_dd_regime.R"))
if (!exists("rt") || !is.data.frame(rt))
  rt <- build_regime_table(xts_ret[, "SPY"])
if (!exists("SAA_60_40"))
  source(here("scripts_daa_saa_taa/01_saa_baseline.R"))

# Pull .outperf_type from spy_dd_regime_rel.R
if (!exists(".outperf_type"))
  source(here("scripts_spy_dd_regime/spy_dd_regime_rel.R"))

# ==============================================================================
# 1. EPISODE STATS ENGINE
# ==============================================================================
# For each regime episode × ticker:
#   - cum return of the ticker over the episode
#   - cum return of the benchmark (SPY or AGG) over the same episode
#   - relative alpha = ticker − benchmark
#   - outperf type (.outperf_type)
#
# Returns a long tibble: one row per (ticker × episode).

compute_episode_stats <- function(xts_ret, rt, benchmark = "SPY") {
  stopifnot(benchmark %in% colnames(xts_ret))

  bmk_xts  <- xts_ret[, benchmark]
  tickers  <- setdiff(colnames(xts_ret), benchmark)

  purrr::map_dfr(seq_len(nrow(rt)), function(i) {
    ep       <- rt[i, ]
    date_seq <- seq(as.Date(ep$xmin), as.Date(ep$xmax), by = "day")
    idx      <- index(xts_ret) %in% date_seq

    if (sum(idx) < 3) return(NULL)   # skip trivially short episodes

    ep_ret   <- xts_ret[idx, ]
    bmk_ret  <- as.numeric(prod(1 + as.numeric(bmk_xts[idx])) - 1)

    purrr::map_dfr(tickers, function(tk) {
      if (!tk %in% colnames(ep_ret)) return(NULL)
      tk_ret   <- as.numeric(prod(1 + as.numeric(ep_ret[, tk])) - 1)
      alpha    <- tk_ret - bmk_ret
      type_tag <- .outperf_type(tk_ret, bmk_ret, alpha)

      tibble(
        ticker    = tk,
        episode   = i,
        regime    = as.character(ep$regime),
        xmin      = as.Date(ep$xmin),
        xmax      = as.Date(ep$xmax),
        days      = as.numeric(ep$xmax - ep$xmin),
        tk_ret    = tk_ret,
        bmk_ret   = bmk_ret,
        alpha     = alpha,
        type      = type_tag,
        benchmark = benchmark
      )
    })
  })
}

# ==============================================================================
# 2. DAA CLASSIFICATION ENGINE
# ==============================================================================
# Aggregates episode_stats to produce one row per ticker with:
#   - dominant type per regime (mode)
#   - mean alpha per regime
#   - n_episodes contributing
#   - DAA role assignment
#
# Classification rules (applied in order):
#
#   EQ-Enhancer   : avg_alpha_recovery  > +2%  AND
#                   dominant type in Recovery ∈ {ALPHA}  AND
#                   NOT dominant type in Fall ∈ {HEDGE, STABLE}
#
#   EQ-Stabilizer : dominant type in Fall ∈ {HEDGE, STABLE}  AND
#                   avg_alpha_fall > 0
#
#   FI-Enhancer   : computed against AGG benchmark (separate call)
#                   avg_spread_recovery vs AGG > +1.5%  AND
#                   sharpe_spread > 0.3
#
#   Neutral       : all others (may still be useful as core SAA holdings)

.mode_type <- function(types) {
  if (length(types) == 0) return(NA_character_)
  tbl <- sort(table(types), decreasing = TRUE)
  names(tbl)[1]
}

classify_daa_universe <- function(episode_stats_spy, episode_stats_agg) {

  # ── EQ classification (vs SPY) ──────────────────────────────────────────────
  eq_summary <- episode_stats_spy %>%
    group_by(ticker, regime) %>%
    summarise(
      mean_alpha   = mean(alpha,   na.rm = TRUE),
      median_alpha = median(alpha, na.rm = TRUE),
      dominant_type = .mode_type(type),
      n_episodes   = n(),
      pct_positive = mean(alpha > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from  = regime,
      values_from = c(mean_alpha, median_alpha, dominant_type, n_episodes, pct_positive),
      names_glue  = "{regime}_{.value}"
    )

  # ── FI classification (vs AGG) ──────────────────────────────────────────────
  fi_summary <- episode_stats_agg %>%
    group_by(ticker, regime) %>%
    summarise(
      mean_spread   = mean(alpha,   na.rm = TRUE),
      pct_positive  = mean(alpha > 0, na.rm = TRUE),
      vol_spread    = sd(alpha, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      sharpe_spread = if_else(vol_spread > 0, mean_spread / vol_spread, 0)
    ) %>%
    pivot_wider(
      names_from  = regime,
      values_from = c(mean_spread, pct_positive, sharpe_spread),
      names_glue  = "fi_{regime}_{.value}"
    )

  # ── Join and classify ────────────────────────────────────────────────────────
  all_tickers <- union(eq_summary$ticker, fi_summary$ticker)

  result <- tibble(ticker = all_tickers) %>%
    left_join(eq_summary, by = "ticker") %>%
    left_join(fi_summary,  by = "ticker") %>%
    left_join(etf_metadata %>% select(ticker, name, asset_class, pf_function,
                                       tree_level, sub_block),
              by = "ticker") %>%
    mutate(
      # Safe accessors (pivot_wider may produce missing cols for sparse data)
      fall_alpha    = coalesce(Fall_mean_alpha,         0),
      rec_alpha     = coalesce(Recovery_mean_alpha,     0),
      consol_alpha  = coalesce(Consolidation_mean_alpha,0),
      fall_type     = coalesce(Fall_dominant_type,      "LOSS"),
      rec_type      = coalesce(Recovery_dominant_type,  "LAG"),
      consol_type   = coalesce(Consolidation_dominant_type, "LAG"),
      fall_pct_pos  = coalesce(Fall_pct_positive,       0),
      rec_pct_pos   = coalesce(Recovery_pct_positive,   0),

      fi_rec_spread = coalesce(fi_Recovery_mean_spread, 0),
      fi_rec_sharpe = coalesce(fi_Recovery_sharpe_spread, 0),
      fi_fall_spread= coalesce(fi_Fall_mean_spread,     0),

      # ── EQ-Enhancer: ALPHA in Recovery, positive in Consolidation ─────────
      is_eq_enhancer = (rec_alpha  > 0.02) &
                       (rec_type   %in% c("ALPHA")) &
                       (rec_pct_pos > 0.55) &
                       !(fall_type %in% c("HEDGE", "STABLE")),

      # ── EQ-Stabilizer: HEDGE/STABLE in Fall, positive alpha in Fall ────────
      is_eq_stabilizer = (fall_type %in% c("HEDGE", "STABLE")) &
                         (fall_alpha > 0) &
                         (fall_pct_pos > 0.55),

      # ── FI-Enhancer: better than AGG in Recovery, decent Sharpe ────────────
      # Exclude pure Equity asset class — any equity beats AGG trivially in Recovery
      is_fi_enhancer = (fi_rec_spread > 0.015) &
                       (fi_rec_sharpe > 0.30) &
                       !is_eq_enhancer &
                       !(asset_class %in% c("Equity")),

      # ── DAA role (priority: Stabilizer > Enhancer > FI-Enhancer > Neutral) ─
      daa_role = case_when(
        is_eq_stabilizer & is_eq_enhancer ~ "EQ-Dual",         # rare: good both ways
        is_eq_stabilizer                  ~ "EQ-Stabilizer",
        is_eq_enhancer                    ~ "EQ-Enhancer",
        is_fi_enhancer                    ~ "FI-Enhancer",
        TRUE                              ~ "Neutral"
      ),

      # ── Composite DAA score (0–100) ─────────────────────────────────────────
      eq_enh_score  = pmax(rec_alpha * 100  * rec_pct_pos, 0),
      eq_stab_score = pmax(fall_alpha * 100 * fall_pct_pos, 0),
      fi_enh_score  = pmax(fi_rec_sharpe * 50, 0),

      daa_score = round(case_when(
        daa_role == "EQ-Enhancer"   ~ pmin(eq_enh_score  * 10, 100),
        daa_role == "EQ-Stabilizer" ~ pmin(eq_stab_score * 10, 100),
        daa_role == "EQ-Dual"       ~ pmin((eq_enh_score + eq_stab_score) * 5, 100),
        daa_role == "FI-Enhancer"   ~ pmin(fi_enh_score,  100),
        TRUE                        ~ 0
      ), 1)
    ) %>%
    arrange(daa_role, desc(daa_score)) %>%
    select(ticker, name, asset_class, pf_function, sub_block,
           daa_role, daa_score,
           fall_type, fall_alpha, fall_pct_pos,
           rec_type,  rec_alpha,  rec_pct_pos,
           consol_type, consol_alpha,
           fi_rec_spread, fi_rec_sharpe)

  result
}

# ==============================================================================
# 3. PLOT FUNCTIONS
# ==============================================================================

#' plot_daa_role
#' Dot + bar chart for a given DAA role, ranked by daa_score.
plot_daa_role <- function(daa_class_tbl, role, title = NULL) {
  df <- daa_class_tbl %>%
    filter(daa_role == role) %>%
    arrange(desc(daa_score)) %>%
    mutate(ticker = fct_reorder(ticker, daa_score))

  if (nrow(df) == 0) {
    message("plot_daa_role: no tickers found for role = ", role)
    return(NULL)
  }

  if (is.null(title)) title <- paste0("DAA Role: ", role)

  role_colour <- switch(role,
    "EQ-Enhancer"   = "#3b82f6",
    "EQ-Stabilizer" = "#22c55e",
    "EQ-Dual"       = "#a855f7",
    "FI-Enhancer"   = "#f59e0b",
    "#6b7280"
  )

  # Select the relevant alpha column
  alpha_col <- switch(role,
    "EQ-Enhancer"   = "rec_alpha",
    "EQ-Stabilizer" = "fall_alpha",
    "EQ-Dual"       = "rec_alpha",
    "FI-Enhancer"   = "fi_rec_spread",
    "rec_alpha"
  )
  regime_label <- switch(role,
    "EQ-Enhancer"   = "Recovery α vs SPY",
    "EQ-Stabilizer" = "Fall α vs SPY",
    "EQ-Dual"       = "Recovery α vs SPY",
    "FI-Enhancer"   = "Recovery spread vs AGG",
    "α"
  )

  df <- df %>%
    mutate(alpha_plot = .data[[alpha_col]])

  ggplot(df, aes(x = alpha_plot * 100, y = ticker)) +
    geom_vline(xintercept = 0, colour = "#6b7280", linewidth = 0.4, linetype = "dashed") +
    geom_segment(aes(x = 0, xend = alpha_plot * 100, yend = ticker),
                 colour = role_colour, linewidth = 0.7, alpha = 0.6) +
    geom_point(aes(size = daa_score), colour = role_colour, alpha = 0.9) +
    geom_text(aes(label = sprintf("%+.1f%%", alpha_plot * 100)),
              hjust = -0.3, size = 3, colour = "#374151") +
    scale_size_continuous(range = c(2, 8), guide = "none") +
    scale_x_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0.05, 0.2))) +
    labs(
      title    = title,
      subtitle = paste0("x-axis = avg ", regime_label, " per episode | dot size = DAA score"),
      x = regime_label, y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(face = "bold", size = 12),
      axis.text.y        = element_text(face = "bold", size = 9)
    )
}

# ==============================================================================
# 4. RUN CLASSIFICATION
# ==============================================================================
message("⚙️  Computing episode stats vs SPY...")
episode_stats_spy <- compute_episode_stats(xts_ret, rt, benchmark = "SPY")

message("⚙️  Computing episode stats vs AGG...")
episode_stats_agg <- compute_episode_stats(xts_ret, rt, benchmark = "AGG")

message("🏷️  Classifying DAA universe...")
daa_class_tbl <- classify_daa_universe(episode_stats_spy, episode_stats_agg)

# Console summary
role_counts <- daa_class_tbl %>% count(daa_role, sort = TRUE)
message("📋 DAA Classification Summary:")
walk2(role_counts$daa_role, role_counts$n,
      ~message(sprintf("  %-18s  %d tickers", .x, .y)))

message("\n🔷 EQ-Enhancers:")
daa_class_tbl %>%
  filter(daa_role == "EQ-Enhancer") %>%
  select(ticker, daa_score, rec_alpha, rec_type, consol_type) %>%
  print(n = 20)

message("\n🛡️  EQ-Stabilizers:")
daa_class_tbl %>%
  filter(daa_role %in% c("EQ-Stabilizer", "EQ-Dual")) %>%
  select(ticker, daa_score, fall_alpha, fall_type, rec_type) %>%
  print(n = 20)

message("\n📈 FI-Enhancers:")
daa_class_tbl %>%
  filter(daa_role == "FI-Enhancer") %>%
  select(ticker, daa_score, fi_rec_spread, fi_rec_sharpe) %>%
  print(n = 20)

# ==============================================================================
# 5. PERSIST
# ==============================================================================
write_rds(daa_class_tbl,    here("02_data_processed/daa_class_tbl.rds"))
write_rds(episode_stats_spy, here("02_data_processed/daa_episode_stats_spy.rds"))
write_rds(episode_stats_agg, here("02_data_processed/daa_episode_stats_agg.rds"))
message("💾 Classifier outputs saved to 02_data_processed/")

# ==============================================================================
# 6. PLOTS (guarded)
# ==============================================================================
if (!isTRUE(getOption("knitr.in.progress"))) {
  p_eq_enhancer   <- plot_daa_role(daa_class_tbl, "EQ-Enhancer",
                                    "EQ-Enhancers — ALPHA in Recovery vs SPY")
  p_eq_stabilizer <- plot_daa_role(daa_class_tbl, "EQ-Stabilizer",
                                    "EQ-Stabilizers — HEDGE/STABLE in Fall vs SPY")
  p_fi_enhancer   <- plot_daa_role(daa_class_tbl, "FI-Enhancer",
                                    "FI-Enhancers — Spread vs AGG in Recovery")
  p_eq_dual       <- plot_daa_role(daa_class_tbl, "EQ-Dual",
                                    "EQ-Dual — ALPHA in Recovery & HEDGE in Fall")

  if (!is.null(p_eq_enhancer))   print(p_eq_enhancer)
  if (!is.null(p_eq_stabilizer)) print(p_eq_stabilizer)
  if (!is.null(p_fi_enhancer))   print(p_fi_enhancer)
  if (!is.null(p_eq_dual))       print(p_eq_dual)
}

message("✅ Stage DAA-02 complete: universe classified into EQ-Enhancer / EQ-Stabilizer / FI-Enhancer.")
