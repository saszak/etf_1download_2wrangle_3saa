################################################################################
# SUBPROJECT : What Has Happened
# FILE       : scripts_what_happened/regime_rank.R
#
# PURPOSE
#   Given a set of candidate ETFs, rank each one by usefulness in every regime
#   (Fall / Recovery / Consolidation) relative to a benchmark (default SPY).
#
# RANKING LOGIC
#   Fall        → rank by mean_alpha DESC  (HEDGE > STABLE >> LOSS)
#   Recovery    → rank by mean_abs_ret DESC (capture max upside)
#   Consolidation → rank by mean_alpha DESC + consistency (steady alpha)
#
#   consistency = % of episodes where alpha > 0  (robustness across cycles)
#
# OUTPERF TYPE (from .outperf_type in spy_dd_regime_rel.R)
#   HEDGE  : alpha>0, abs>0, spy<0  — rose while SPY fell
#   STABLE : alpha>0, abs<0, spy<0  — both fell, ticker less
#   ALPHA  : alpha>0, spy>0         — both rose, ticker more
#   LAG    : alpha<0, spy>0         — underperformed rising SPY
#   LOSS   : alpha<0, spy<0         — both fell, ticker worse
#
# FUNCTIONS
#   rank_regime_etfs(tickers, xts_ret, rt, master, n_top)
#     → tibble: ticker | regime | mean_alpha | mean_abs_ret | consistency |
#               outperf_type | score | rank
#
#   plot_regime_rank(ranking_tbl, n_top, title)
#     → lollipop chart: top N per regime, coloured by outperf_type
#
# INVOKE
#   source(here("scripts_spy_dd_regime/spy_dd_regime_rel.R"))
#   source(here("scripts_what_happened/regime_rank.R"))
#
#   rk <- rank_regime_etfs(colnames(xts_ret), xts_ret, rt)
#   plot_regime_rank(rk)
#   plot_regime_rank(rk %>% filter(regime == "Fall"), n_top = 15)
################################################################################

library(tidyverse)
library(scales)
library(here)

# ── Outperf type colours ──────────────────────────────────────────────────────
TYPE_COL <- c(
  HEDGE  = "#16a34a",   # green
  STABLE = "#4ade80",   # light green
  ALPHA  = "#3b82f6",   # blue
  LAG    = "#f97316",   # orange
  LOSS   = "#dc2626"    # red
)

TYPE_ORDER <- c("HEDGE", "STABLE", "ALPHA", "LAG", "LOSS")

# ── rank_regime_etfs ──────────────────────────────────────────────────────────
rank_regime_etfs <- function(
    tickers,
    xts_ret,
    rt,
    master       = "SPY",
    n_top        = NULL,       # NULL = return all; integer = top N per regime
    etf_metadata = NULL,       # pass to enable asset_class / pf_function filter
    asset_class  = NULL,       # e.g. "Equity"  — filters on etf_metadata$asset_class
    pf_function  = NULL        # e.g. c("Anchor","Core-Growth") — additional filter
) {
  # Requires .build_episode_alpha() from spy_dd_regime_rel.R
  if (!exists(".build_episode_alpha", mode = "function"))
    source(here("scripts_spy_dd_regime/spy_dd_regime_rel.R"))
  if (!exists(".outperf_type", mode = "function"))
    source(here("scripts_spy_dd_regime/spy_dd_regime_rel.R"))

  # Apply metadata filters if provided
  if (!is.null(etf_metadata)) {
    meta <- etf_metadata
    if (!is.null(asset_class)) meta <- meta %>% filter(asset_class %in% !!asset_class)
    if (!is.null(pf_function)) meta <- meta %>% filter(pf_function %in% !!pf_function)
    tickers <- intersect(tickers, meta$ticker)
  }
  tickers <- setdiff(tickers[tickers %in% colnames(xts_ret)], master)

  ep <- .build_episode_alpha(xts_ret, tickers, rt, master)

  # Aggregate per ticker × regime
  agg <- ep %>%
    group_by(ticker, regime) %>%
    summarise(
      n_episodes   = n(),
      mean_alpha   = mean(alpha,   na.rm = TRUE) * 100,   # in %
      mean_abs_ret = mean(abs_ret, na.rm = TRUE) * 100,
      mean_spy_ret = mean(spy_ret, na.rm = TRUE) * 100,
      consistency  = mean(alpha > 0, na.rm = TRUE),       # 0–1
      .groups = "drop"
    ) %>%
    mutate(
      outperf_type = .outperf_type(
        mean_abs_ret / 100,
        mean_spy_ret / 100,
        mean_alpha   / 100
      ),
      outperf_type = factor(outperf_type, levels = TYPE_ORDER),
      # Composite score per regime
      score = case_when(
        regime == "Fall"          ~ mean_alpha * 0.6 + consistency * 10 * 0.4,
        regime == "Recovery"      ~ mean_abs_ret * 0.7 + mean_alpha * 0.3,
        regime == "Consolidation" ~ mean_alpha * 0.5 + consistency * 10 * 0.5,
        TRUE                      ~ mean_alpha
      )
    ) %>%
    group_by(regime) %>%
    mutate(rank = rank(-score, ties.method = "min")) %>%
    ungroup() %>%
    arrange(regime, rank)

  if (!is.null(n_top))
    agg <- agg %>% group_by(regime) %>% slice_head(n = n_top) %>% ungroup()

  agg
}

# ── plot_regime_rank ──────────────────────────────────────────────────────────
plot_regime_rank <- function(
    ranking_tbl,
    n_top = 15,
    title = "ETF Regime Ranking — Usefulness by Market Phase"
) {
  df <- ranking_tbl %>%
    group_by(regime) %>%
    slice_head(n = n_top) %>%
    ungroup() %>%
    mutate(
      regime = factor(regime, levels = c("Fall", "Recovery", "Consolidation")),
      # x-axis metric depends on regime
      x_val  = case_when(
        regime == "Fall"          ~ mean_alpha,
        regime == "Recovery"      ~ mean_abs_ret,
        regime == "Consolidation" ~ mean_alpha
      ),
      x_label = case_when(
        regime == "Fall"          ~ "Mean alpha vs SPY (%)",
        regime == "Recovery"      ~ "Mean absolute return (%)",
        regime == "Consolidation" ~ "Mean alpha vs SPY (%)"
      )
    )

  # Ticker order within each facet: by score descending
  df <- df %>%
    group_by(regime) %>%
    mutate(ticker = fct_reorder(ticker, score)) %>%
    ungroup()

  ggplot(df, aes(x = x_val, y = ticker, colour = outperf_type)) +

    # Zero reference
    geom_vline(xintercept = 0, colour = "#6b7280", linewidth = 0.4,
               linetype = "dashed") +

    # Lollipop stem
    geom_segment(aes(x = 0, xend = x_val, yend = ticker),
                 linewidth = 0.8, alpha = 0.7) +

    # Dot
    geom_point(size = 3.5) +

    # Consistency annotation (right side)
    geom_text(
      aes(x = max(df$x_val, na.rm = TRUE) * 1.05,
          label = sprintf("%.0f%% cons.", consistency * 100)),
      hjust = 0, size = 2.4, colour = "#6b7280"
    ) +

    # Value label
    geom_text(
      aes(label = sprintf("%+.1f%%", x_val)),
      hjust = -0.3, size = 2.6, fontface = "bold"
    ) +

    scale_colour_manual(
      values = TYPE_COL,
      limits = TYPE_ORDER,
      name   = "Type",
      drop   = FALSE
    ) +
    scale_x_continuous(
      labels = function(x) sprintf("%+.0f%%", x),
      expand = expansion(mult = c(0.05, 0.22))
    ) +
    facet_wrap(
      ~ regime, scales = "free",
      labeller = labeller(regime = c(
        Fall          = "FALL  (x-axis = mean alpha)",
        Recovery      = "RECOVERY  (x-axis = mean abs return)",
        Consolidation = "CONSOLIDATION  (x-axis = mean alpha)"
      ))
    ) +
    labs(
      title    = title,
      subtitle = sprintf(
        "Top %d per regime  |  Consistency = %% of episodes with positive alpha  |  Sorted by composite score",
        n_top
      ),
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(face = "bold"),
      plot.subtitle      = element_text(size = 8.5, colour = "#555"),
      strip.text         = element_text(face = "bold", size = 10),
      legend.position    = "bottom"
    )
}

# ── Quick-run (guarded) ───────────────────────────────────────────────────────
if (!isTRUE(getOption("knitr.in.progress"))) {
  library(here)

  if (!exists("xts_ret"))
    xts_ret <- readRDS(here("02_data_processed/xts_ret_returns.rds"))
  if (!exists("rt") || !is.data.frame(rt))
    rt <- build_regime_table(xts_ret[, "SPY"])
  if (!exists("etf_metadata"))
    source(here("scripts/00_init_universe.R"))

  tk <- intersect(etf_metadata$ticker, colnames(xts_ret))

  rk <- rank_regime_etfs(tk, xts_ret, rt)
  print(rk)

  print(plot_regime_rank(rk, n_top = 12))

  # Fall only — who are the hedges?
  print(plot_regime_rank(rk %>% filter(regime == "Fall"), n_top = 15,
                         title = "Best ETFs in a Fall — Ranked by Alpha + Consistency"))
}
