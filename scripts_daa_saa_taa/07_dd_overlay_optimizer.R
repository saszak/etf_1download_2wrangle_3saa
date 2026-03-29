################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : scripts_daa_saa_taa/07_dd_overlay_optimizer.R
# Purpose : Find the MINIMUM-COST overlay that reduces MaxDD from ~23% to ~15%.
#
# QUESTION ANSWERED
#   "Given we accept SPY/IEF 60/40 return, what is the cheapest combination
#    of stabilizer instruments that reduces MaxDD from ~23% to ~15%?"
#
# APPROACH
#   1. Load top stabilizers ranked by insurance ratio (from 05_dd_stabilizer.R)
#   2. Grid-search all weight combinations across the top N instruments
#      — total overlay weight constrained to ≤ 25% of portfolio
#      — funded from SPY/IEF proportionally (same as 05_dd_stabilizer.R blend logic)
#   3. For every combination compute: MaxDD, AnnRet, return_drag vs passive
#   4. Find the Pareto frontier: minimum return_drag for each MaxDD level
#   5. Solve for each target MaxDD (15%, 17%, 19%, 21%) → cheapest combination
#
# OUTPUTS (tibbles)
#   overlay_grid       all combinations evaluated (MaxDD + AnnRet per combo)
#   pareto_frontier    Pareto-efficient combos (min return_drag at each MaxDD)
#   target_solutions   recommended overlay per DD target
#
# PLOTS
#   p_overlay_frontier  scatter: all combos + Pareto frontier + target lines
#   p_optimal_bars      bar chart: instrument weights for each DD target
#   p_overlay_wealth    wealth + DD comparison: passive vs each DD-target overlay
#
# DEPENDENCIES
#   05_dd_stabilizer.R must be sourced first (provides stabilizer_screen, passive_ret)
#   or its outputs must be present in 02_data_processed/
################################################################################

library(tidyverse)
library(xts)
library(patchwork)
library(scales)
library(here)

if (!exists("project_tree"))       source(here("project_tree.R"))
if (!exists("etf_metadata"))       source(here(project_tree$scripts$init))
if (!exists("xts_ret"))            source(here(project_tree$scripts$wrangle))
if (!exists("build_regime_table")) source(here("scripts_spy_dd_regime/spy_dd_regime.R"))
if (!exists("rt") || !is.data.frame(rt))
  rt <- build_regime_table(xts_ret[, "SPY"])

# Load screen output from 05 (run it if not yet in memory)
if (!exists("stabilizer_screen")) {
  screen_path <- here("02_data_processed/dd_stabilizer_screen.rds")
  if (file.exists(screen_path)) {
    stabilizer_screen <- read_rds(screen_path)
    message("📥 Loaded dd_stabilizer_screen from disk.")
  } else {
    message("⚙️  Running 05_dd_stabilizer.R to build screen...")
    source(here("scripts_daa_saa_taa/05_dd_stabilizer.R"))
  }
}

if (!exists("passive_ret")) {
  passive_ret <- xts(
    as.numeric(xts_ret[, "SPY"]) * 0.60 +
    as.numeric(xts_ret[, "IEF"]) * 0.40,
    order.by = index(xts_ret)
  )
  colnames(passive_ret) <- "Passive_60_40"
}

# ── Utility helpers ────────────────────────────────────────────────────────────
.ann_ret <- function(r) { r <- as.numeric(r[!is.na(r)]); prod(1+r)^(252/length(r))-1 }
.max_dd  <- function(r) { w <- cumprod(1+as.numeric(r[!is.na(r)])); min((w-cummax(w))/cummax(w)) }

passive_ann_ret <- .ann_ret(passive_ret)
passive_max_dd  <- .max_dd(passive_ret)

message(sprintf("📊 Passive baseline — MaxDD: %.1f%%  AnnRet: %+.1f%%",
                passive_max_dd * 100, passive_ann_ret * 100))

# ==============================================================================
# 1. SELECT CANDIDATE STABILIZERS
# ==============================================================================
# Use top instruments by insurance_ratio that actually reduce DD

N_CANDIDATES  <- 8L    # how many instruments to consider in grid search
MAX_OVERLAY   <- 0.25  # total overlay budget (funded from passive)
WEIGHT_STEP   <- 0.05  # weight increments per instrument
MAX_PER_INSTR <- 0.20  # cap per single instrument

candidates <- stabilizer_screen %>%
  filter(!is.na(fall_corr), dd_reduction > 0) %>%
  slice_head(n = N_CANDIDATES) %>%
  pull(ticker)

message(sprintf("🎯 Candidates (%d): %s", length(candidates), paste(candidates, collapse=", ")))

# Pre-compute aligned returns for all candidates vs passive
cand_ret_list <- lapply(candidates, function(tk) {
  common <- merge(passive_ret, xts_ret[, tk], join = "inner")
  list(
    p = as.numeric(common[, 1]),
    x = as.numeric(common[, 2]),
    dates = index(common),
    n = nrow(common)
  )
})
names(cand_ret_list) <- candidates

# ==============================================================================
# 2. GRID SEARCH — ALL SINGLE + PAIRWISE + TRIPLEWISE COMBINATIONS
# ==============================================================================
# Strategy:
#   — Single instruments: weight = 5%, 10%, 15%, 20%
#   — Pairs:    each instrument 5%–15% with 5% step, sum ≤ MAX_OVERLAY
#   — Triples:  each 5%–10%, sum ≤ MAX_OVERLAY
# This keeps the grid tractable (< 5 000 evals) while covering the useful space.

weight_steps_single <- seq(WEIGHT_STEP, MAX_PER_INSTR, by = WEIGHT_STEP)
weight_steps_pair   <- seq(WEIGHT_STEP, 0.15,          by = WEIGHT_STEP)
weight_steps_triple <- seq(WEIGHT_STEP, 0.10,          by = WEIGHT_STEP)

# Helper: blend n instruments at given weights onto passive
.blend_multi <- function(weights_named) {
  # weights_named: named numeric vector, e.g. c(GLD=0.10, TLT=0.05)
  tks <- names(weights_named)
  w   <- as.numeric(weights_named)

  # Use common date range across passive + all instruments
  all_ret <- lapply(tks, function(tk) cand_ret_list[[tk]])
  min_n   <- min(sapply(all_ret, `[[`, "n"))

  # Align all to shortest series (inner join already done via cand_ret_list)
  # Find common indices by merging xts objects
  xts_list <- c(
    list(passive_ret),
    lapply(tks, function(tk) xts(cand_ret_list[[tk]]$x,
                                  order.by = cand_ret_list[[tk]]$dates))
  )
  common <- Reduce(function(a, b) merge(a, b, join = "inner"), xts_list)

  p_r   <- as.numeric(common[, 1])
  x_mat <- as.matrix(common[, -1, drop = FALSE])

  # Portfolio: (1 - sum(w)) × passive + sum_i(w_i × ticker_i)
  stab_component <- as.numeric(x_mat %*% w)
  blended        <- (1 - sum(w)) * p_r + stab_component

  list(r = blended, dates = index(common))
}

.eval_combo <- function(weights_named) {
  bl      <- .blend_multi(weights_named)
  mdd     <- .max_dd(bl$r)
  ann_ret <- .ann_ret(bl$r)
  drag    <- passive_ann_ret - ann_ret
  tibble(
    instruments   = paste(names(weights_named), collapse = "+"),
    weights_str   = paste(sprintf("%.0f%%", weights_named * 100), collapse = "+"),
    n_instruments = length(weights_named),
    total_weight  = sum(weights_named),
    maxdd         = mdd,
    ann_ret       = ann_ret,
    return_drag   = drag,
    dd_reduction  = passive_max_dd - mdd   # negative = DD got worse (exclude these)
  )
}

message("⚙️  Running grid search...")

# — Singles ——————————————————————————————————————————————————————————————————
message("  Singles...")
single_rows <- purrr::map_dfr(candidates, function(tk) {
  purrr::map_dfr(weight_steps_single, function(w) {
    tryCatch(.eval_combo(setNames(w, tk)), error = function(e) NULL)
  })
})

# — Pairs ────────────────────────────────────────────────────────────────────
message("  Pairs...")
cand_pairs <- combn(candidates, 2, simplify = FALSE)
pair_rows  <- purrr::map_dfr(cand_pairs, function(pair) {
  tk1 <- pair[1]; tk2 <- pair[2]
  purrr::map_dfr(weight_steps_pair, function(w1) {
    purrr::map_dfr(weight_steps_pair, function(w2) {
      if (w1 + w2 > MAX_OVERLAY) return(NULL)
      tryCatch(.eval_combo(c(setNames(w1, tk1), setNames(w2, tk2))),
               error = function(e) NULL)
    })
  })
})

# — Triples ──────────────────────────────────────────────────────────────────
message("  Triples (top 5 candidates only)...")
top5 <- candidates[seq_len(min(5, length(candidates)))]
cand_triples <- combn(top5, 3, simplify = FALSE)
triple_rows  <- purrr::map_dfr(cand_triples, function(triple) {
  tk1 <- triple[1]; tk2 <- triple[2]; tk3 <- triple[3]
  purrr::map_dfr(weight_steps_triple, function(w1) {
    purrr::map_dfr(weight_steps_triple, function(w2) {
      purrr::map_dfr(weight_steps_triple, function(w3) {
        if (w1 + w2 + w3 > MAX_OVERLAY) return(NULL)
        tryCatch(.eval_combo(c(setNames(w1, tk1), setNames(w2, tk2), setNames(w3, tk3))),
                 error = function(e) NULL)
      })
    })
  })
})

overlay_grid <- bind_rows(single_rows, pair_rows, triple_rows) %>%
  filter(!is.na(maxdd), !is.na(ann_ret)) %>%
  arrange(maxdd)

message(sprintf("✅ Grid complete: %d combinations evaluated.", nrow(overlay_grid)))

# ==============================================================================
# 3. PARETO FRONTIER
#    For each MaxDD bucket, which combination has minimum return drag?
#    "Pareto" here: no other combo achieves the same or better DD reduction
#    at equal or lower return cost.
# ==============================================================================

# Discretise MaxDD to 0.5pp buckets for comparison
pareto_frontier <- overlay_grid %>%
  filter(dd_reduction > 0) %>%                 # only combos that actually reduce DD
  mutate(dd_bucket = round(maxdd * 200) / 2) %>%  # 0.5pp buckets
  group_by(dd_bucket) %>%
  slice_min(return_drag, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(dd_bucket)

# ==============================================================================
# 4. SOLVE FOR DD TARGETS
#    For targets 15%, 17%, 19%, 21%: find cheapest combination that gets there.
# ==============================================================================

DD_TARGETS <- c(0.15, 0.17, 0.19, 0.21)

target_solutions <- purrr::map_dfr(DD_TARGETS, function(tgt) {
  # Must achieve MaxDD ≤ target (i.e. |maxdd| ≤ tgt, remembering maxdd is negative)
  feasible <- overlay_grid %>%
    filter(abs(maxdd) <= tgt, dd_reduction > 0) %>%
    slice_min(return_drag, n = 1, with_ties = FALSE)

  if (nrow(feasible) == 0) {
    message(sprintf("  ⚠️  No combo achieves MaxDD ≤ %.0f%% — best available shown.", tgt * 100))
    feasible <- overlay_grid %>%
      slice_min(maxdd, n = 1, with_ties = FALSE)
    feasible$target_label <- sprintf("≤%.0f%% — unreachable", tgt * 100)
  } else {
    feasible$target_label <- sprintf("≤%.0f%%", tgt * 100)
  }

  feasible %>%
    mutate(dd_target = tgt) %>%
    select(dd_target, target_label, instruments, weights_str, n_instruments,
           total_weight, maxdd, ann_ret, return_drag, dd_reduction)
})

# ==============================================================================
# 5. CONSOLE REPORT
# ==============================================================================

message("\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
message("🎯  MINIMUM-COST OVERLAY SOLUTIONS")
message("    Passive 60/40 baseline:")
message(sprintf("    MaxDD: %.1f%%  |  AnnRet: %+.1f%%", passive_max_dd*100, passive_ann_ret*100))
message("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

target_solutions %>%
  mutate(
    maxdd_achieved = sprintf("%.1f%%",    abs(maxdd) * 100),
    dd_saved       = sprintf("%+.1f pp",  dd_reduction * 100),
    ann_ret_fmt    = sprintf("%+.1f%%",   ann_ret * 100),
    drag_fmt       = sprintf("%+.1f pp",  return_drag * 100),
    total_w_fmt    = sprintf("%.0f%%",    total_weight * 100)
  ) %>%
  select(`DD Target` = target_label, Instruments = instruments,
         Weights = weights_str, `Total w` = total_w_fmt,
         MaxDD = maxdd_achieved, `DD Saved` = dd_saved,
         AnnRet = ann_ret_fmt, `Return Drag` = drag_fmt) %>%
  print(n = Inf)

# ==============================================================================
# 6. PLOT A — OVERLAY EFFICIENCY FRONTIER
#    All combos as grey points; Pareto frontier highlighted;
#    Passive anchor + DD target lines
# ==============================================================================

plot_overlay_frontier <- function(overlay_grid, pareto_frontier, target_solutions) {

  # Jitter slightly to avoid overplotting
  set.seed(42)

  # Colour combos by number of instruments
  combo_colours <- c("1" = "#9ca3af", "2" = "#60a5fa", "3" = "#a78bfa")

  ggplot(overlay_grid %>% filter(dd_reduction > 0),
         aes(x = abs(maxdd) * 100, y = ann_ret * 100)) +

    # All evaluated combos (background cloud)
    geom_point(aes(colour = factor(n_instruments)),
               alpha = 0.35, size = 1.2) +
    scale_colour_manual(values = combo_colours,
                        name   = "# Instruments",
                        labels = c("1" = "Single", "2" = "Pair", "3" = "Triple")) +

    # Pareto frontier path
    geom_path(data = pareto_frontier %>% arrange(dd_bucket),
              aes(x = abs(dd_bucket) * 100, y = ann_ret * 100),
              inherit.aes = FALSE,
              colour = "#7c3aed", linewidth = 1.3, alpha = 0.9) +
    geom_point(data = pareto_frontier,
               aes(x = abs(dd_bucket) * 100, y = ann_ret * 100),
               inherit.aes = FALSE,
               colour = "#7c3aed", size = 2.5) +

    # Passive baseline anchor
    annotate("point", x = abs(passive_max_dd) * 100, y = passive_ann_ret * 100,
             colour = "#111827", size = 6, shape = 18) +
    annotate("text",  x = abs(passive_max_dd) * 100 - 0.3,
             y = passive_ann_ret * 100,
             label = sprintf("Passive 60/40\nMaxDD %.1f%%", abs(passive_max_dd)*100),
             hjust = 1, size = 3, colour = "#111827", fontface = "bold") +

    # DD target vertical lines
    geom_vline(data = tibble(tgt = DD_TARGETS * 100),
               aes(xintercept = tgt),
               linetype = "dashed", colour = "#ef4444", linewidth = 0.5,
               alpha = 0.7, inherit.aes = FALSE) +
    annotate("text",
             x     = DD_TARGETS * 100,
             y     = rep(max(overlay_grid$ann_ret, na.rm=TRUE) * 100 + 0.1, 4),
             label = paste0(DD_TARGETS * 100, "%"),
             size  = 2.8, colour = "#ef4444", fontface = "bold", vjust = 0) +

    # Optimal solution points per target
    geom_point(data = target_solutions %>% filter(dd_reduction > 0),
               aes(x = abs(maxdd) * 100, y = ann_ret * 100),
               inherit.aes = FALSE,
               colour = "#dc2626", size = 4, shape = 8, stroke = 1.5) +

    scale_x_reverse(labels = function(x) paste0(x, "%"),
                    name   = "MaxDD (absolute, lower = better) →") +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       name   = "Annual Return (higher = better) ↑") +
    labs(
      title    = "Overlay Efficiency Frontier — All Combinations",
      subtitle = sprintf(
        "Grey cloud = all combos evaluated (%d total)  |  Purple path = Pareto frontier\nRed dashed lines = MaxDD targets  |  Red star = optimal combo per target",
        nrow(overlay_grid %>% filter(dd_reduction > 0))
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(colour = "grey50", size = 9),
      legend.position  = "bottom"
    )
}

# ==============================================================================
# 7. PLOT B — OPTIMAL OVERLAY COMPOSITION
#    For each DD target: bar chart showing which instruments and at what weight.
#    Sorted by target (most aggressive reduction first).
# ==============================================================================

plot_optimal_bars <- function(target_solutions) {

  # Parse back into long form: instruments + weights → tidy rows
  tidy_combos <- target_solutions %>%
    filter(dd_reduction > 0) %>%
    mutate(row_id = row_number()) %>%
    rowwise() %>%
    mutate(
      tickers = list(str_split(instruments, "\\+")[[1]]),
      wts     = list(as.numeric(str_extract_all(weights_str, "[0-9.]+")[[1]]) / 100)
    ) %>%
    ungroup() %>%
    select(dd_target, target_label, maxdd, return_drag, tickers, wts) %>%
    mutate(data = map2(tickers, wts, ~ tibble(ticker = .x, weight = .y))) %>%
    unnest(data) %>%
    mutate(
      label = sprintf("MaxDD %.1f%%\nDrag %+.1f pp",
                      abs(maxdd) * 100, return_drag * 100),
      target_label = factor(target_label,
                            levels = unique(target_label[order(dd_target)]))
    )

  ggplot(tidy_combos, aes(x = ticker, y = weight * 100, fill = ticker)) +
    geom_col(width = 0.65, colour = "white") +
    geom_text(aes(label = sprintf("%.0f%%", weight * 100)),
              vjust = -0.4, size = 3.2, fontface = "bold") +
    facet_wrap(~ target_label, nrow = 1) +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.15))) +
    scale_fill_brewer(palette = "Set2", guide = "none") +
    labs(
      title    = "Optimal Overlay Composition per DD Target",
      subtitle = "Each panel = cheapest combination achieving that MaxDD target\nOverlay funded from SPY/IEF proportionally — passive 60/40 shrinks accordingly",
      x = NULL, y = "Allocation (%)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor  = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title        = element_text(face = "bold", size = 13),
      plot.subtitle     = element_text(colour = "grey50", size = 9),
      strip.text        = element_text(face = "bold", size = 10),
      axis.text.x       = element_text(angle = 30, hjust = 1)
    )
}

# ==============================================================================
# 8. PLOT C — WEALTH + DD COMPARISON
#    Passive vs the overlay that achieves the 15% MaxDD target
# ==============================================================================

plot_overlay_comparison <- function(target_solutions, passive_ret, xts_ret, rt) {

  # Use the 15% target (or closest available)
  sol <- target_solutions %>%
    arrange(abs(dd_target - 0.15)) %>%
    slice(1)

  # Parse instruments and weights
  tickers_vec <- str_split(sol$instruments, "\\+")[[1]]
  weights_vec <- as.numeric(str_extract_all(sol$weights_str, "[0-9.]+")[[1]]) / 100
  names(weights_vec) <- tickers_vec

  # Build blended return series
  xts_list <- c(
    list(passive_ret),
    lapply(tickers_vec, function(tk) xts_ret[, tk])
  )
  common <- Reduce(function(a, b) merge(a, b, join = "inner"), xts_list)
  p_r    <- as.numeric(common[, 1])
  x_mat  <- as.matrix(common[, -1, drop = FALSE])
  bl_r   <- (1 - sum(weights_vec)) * p_r + as.numeric(x_mat %*% weights_vec)

  dates  <- as.Date(index(common))
  w1     <- cumprod(1 + p_r)
  w2     <- cumprod(1 + bl_r)
  dd1    <- (w1 - cummax(w1)) / cummax(w1)
  dd2    <- (w2 - cummax(w2)) / cummax(w2)

  wealth_df <- tibble(date = dates, Passive = w1, Stabilized = w2) %>%
    pivot_longer(-date, names_to = "portfolio", values_to = "wealth")
  dd_df     <- tibble(date = dates, Passive = dd1, Stabilized = dd2) %>%
    pivot_longer(-date, names_to = "portfolio", values_to = "dd")

  regime_rect <- rt %>%
    filter(regime == "Fall") %>%
    mutate(xmin = as.Date(xmin), xmax = as.Date(xmax))

  colours <- c("Passive" = "#9ca3af", "Stabilized" = "#7c3aed")

  stats_tbl <- tibble(
    portfolio = c("Passive", "Stabilized"),
    ret = c(.ann_ret(p_r), .ann_ret(bl_r)),
    mdd = c(.max_dd(p_r),  .max_dd(bl_r)),
    vol = c(sd(p_r)*sqrt(252), sd(bl_r)*sqrt(252))
  ) %>%
    mutate(lbl = sprintf("%s\n%+.1f%% p.a.  DD %.1f%%",
                         portfolio, ret*100, mdd*100))

  last_w <- wealth_df %>%
    group_by(portfolio) %>% slice_max(date, n=1) %>%
    left_join(stats_tbl %>% select(portfolio, lbl), by="portfolio")

  overlay_label <- sprintf(
    "%s\n(%s)",
    paste(tickers_vec, collapse=" + "),
    paste(sprintf("%.0f%%", weights_vec * 100), collapse=" + ")
  )

  pW <- ggplot(wealth_df, aes(date, wealth, colour = portfolio)) +
    geom_rect(data = regime_rect,
              aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf),
              inherit.aes = FALSE, fill = "#fca5a5", alpha = 0.18) +
    geom_line(linewidth = 0.9) +
    geom_text(data = last_w, aes(label = lbl),
              hjust = 0, nudge_x = 80, size = 2.8,
              fontface = "bold", lineheight = 0.85) +
    scale_colour_manual(values = colours, guide = "none") +
    scale_y_continuous(labels = dollar_format(prefix = "$"),
                       limits = c(0.7, NA),
                       expand = expansion(mult = c(0, 0.05))) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    coord_cartesian(clip = "off") +
    labs(
      title    = sprintf("Passive 60/40  vs  Optimal Overlay for ≤%.0f%% MaxDD Target",
                         abs(sol$dd_target) * 100),
      subtitle = sprintf("Overlay: %s  |  Return drag: %+.1f pp/yr  |  Pink = Fall regime",
                         overlay_label, sol$return_drag * 100),
      x = NULL, y = "Wealth ($)"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.margin = margin(5, 180, 5, 5), panel.grid.minor = element_blank(),
          plot.title  = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(colour = "grey50", size = 9))

  pDD <- ggplot(dd_df, aes(date, dd * 100, colour = portfolio)) +
    geom_rect(data = regime_rect,
              aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=0),
              inherit.aes = FALSE, fill = "#fca5a5", alpha = 0.15) +
    geom_hline(yintercept = 0,          colour = "grey40", linewidth = 0.3) +
    geom_hline(yintercept = -15,        colour = "#7c3aed", linewidth = 0.5,
               linetype = "dashed", alpha = 0.7) +
    geom_hline(yintercept = passive_max_dd * 100,
               colour = "#9ca3af", linewidth = 0.5, linetype = "dashed", alpha = 0.6) +
    geom_line(linewidth = 0.8) +
    annotate("text", x = min(dates) + 100, y = -14.5,
             label = "Target −15%", size = 2.8, colour = "#7c3aed", hjust = 0) +
    annotate("text", x = min(dates) + 100,
             y = passive_max_dd * 100 + 0.5,
             label = sprintf("Passive MaxDD %.1f%%", passive_max_dd*100),
             size = 2.8, colour = "#9ca3af", hjust = 0) +
    scale_colour_manual(values = colours, name = NULL) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(title = "Drawdown Comparison",
         subtitle = "Purple = stabilized  |  Grey = passive  |  Dashed lines = key DD levels",
         x = NULL, y = "Drawdown (%)") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          legend.position  = "top",
          plot.title       = element_text(face = "bold", size = 12),
          plot.subtitle    = element_text(colour = "grey50", size = 9))

  pW / pDD
}

# ==============================================================================
# 9. SAVE + RENDER
# ==============================================================================

write_rds(overlay_grid,      here("02_data_processed/overlay_grid.rds"))
write_rds(pareto_frontier,   here("02_data_processed/pareto_frontier.rds"))
write_rds(target_solutions,  here("02_data_processed/overlay_target_solutions.rds"))
message("💾 Saved: overlay_grid, pareto_frontier, overlay_target_solutions")

if (!isTRUE(getOption("knitr.in.progress"))) {
  if (!requireNamespace("ggrepel", quietly = TRUE))
    install.packages("ggrepel", repos = "https://cloud.r-project.org")

  p_overlay_frontier <- plot_overlay_frontier(overlay_grid, pareto_frontier, target_solutions)
  p_optimal_bars     <- plot_optimal_bars(target_solutions)
  p_overlay_wealth   <- plot_overlay_comparison(target_solutions, passive_ret, xts_ret, rt)

  print(p_overlay_frontier)
  print(p_optimal_bars)
  print(p_overlay_wealth)
}

message("✅ Stage DAA-07 complete: overlay optimizer finished.")
