################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : scripts_spy_dd_regime/spy_statemachine_diagram_rel.R
# Purpose : Regime State Machine Diagram — Relative Mode
#           Plot 1 : SPY 4-state flow diagram with empirical stats + transition probs
#           Plot 2 : Ticker × Regime avg alpha heatmap  (SPY absolute, others vs SPY)
#           Plot 3 : 4×4 empirical transition probability heatmap
#
# DEPENDS ON
#   scripts_spy_dd_regime/spy_dd_regime.R        — build_regime_table(), REGIME_PAL
#   scripts_spy_dd_regime/regime_multi_ticker.R  — .period_ret()
#
# FUNCTIONS
#   plot_sm_diagram()              Diamond flow chart: nodes + curved arrows
#   plot_sm_alpha_heatmap()        Avg alpha per ticker × regime type
#   plot_sm_transition_matrix()    4×4 transition count + probability heatmap
#   run_sm_diagram_rel()           Wrapper — all three from one call
################################################################################

library(tidyverse)
library(xts)
library(PerformanceAnalytics)
library(scales)
library(patchwork)

if (!exists("build_regime_table"))
  source(here::here("scripts_spy_dd_regime/spy_dd_regime.R"))

if (!exists(".period_ret"))
  source(here::here("scripts_spy_dd_regime/regime_multi_ticker.R"))


# ==============================================================================
# INTERNAL HELPERS
# ==============================================================================

# Per-regime empirical stats (n, avg duration, avg return, % time)
.sm_stats <- function(rt) {
  total_days <- sum(rt$days, na.rm = TRUE)
  rt %>%
    as_tibble() %>%
    mutate(regime = as.character(regime)) %>%
    group_by(regime) %>%
    summarise(
      n_periods   = n(),
      avg_days    = round(mean(days,         na.rm = TRUE)),
      avg_ret     = mean(period_return,      na.rm = TRUE),
      avg_ann_ret = mean(ann_return,         na.rm = TRUE),
      pct_time    = sum(days, na.rm = TRUE) / total_days,
      .groups     = "drop"
    )
}

# Empirical (from → to) transition counts + probabilities from consecutive periods
.sm_transitions <- function(rt) {
  rt_ord <- rt %>% arrange(xmin)
  tibble(
    from = as.character(head(rt_ord$regime, -1)),
    to   = as.character(tail(rt_ord$regime, -1))
  ) %>%
    count(from, to, name = "n_obs") %>%
    group_by(from) %>%
    mutate(prob = n_obs / sum(n_obs)) %>%
    ungroup()
}

# Mixed-mode per-period display return (same logic as spy_dd_regime_rel.R)
.disp_ret_sm <- function(tk, raw_ret, spy_ret, master) {
  if (tk == master) raw_ret else raw_ret - spy_ret
}


# ==============================================================================
# 1. plot_sm_diagram()
# ==============================================================================
# Triangle layout — 3 states only:
#
#            [CONSOLIDATION]       ← top
#           ↗               ↘
#     [FALL]  ─────────────  [RECOVERY]
#
# Each node   : regime name, N periods, avg duration, avg return, % time
# Each arrow  : transition condition (threshold rule) + empirical p=XX% (n=Y)
# Active node : highlighted with an outer glow ring
# ==============================================================================

plot_sm_diagram <- function(xts_ret,
                             master   = "SPY",
                             t_fall   = 0.10,
                             t_cruise = 0.05) {

  master_r <- xts_ret[, master]
  rt       <- build_regime_table(master_r, t_fall, t_cruise)
  stats    <- .sm_stats(rt)
  trans    <- .sm_transitions(rt)

  current_regime <- as.character(tail(rt$regime, 1))
  current_since  <- format(tail(rt$xmin, 1), "%d %b %Y")
  date_range     <- sprintf("%s – %s",
                            format(min(index(master_r)), "%b %Y"),
                            format(max(index(master_r)), "%b %Y"))

  p_fall_lbl <- percent(t_fall, accuracy = 1)

  # ── Node positions: triangle ─────────────────────────────────────────────
  node_pos <- tibble(
    regime = c("Fall", "Recovery", "Consolidation"),
    x      = c(1.0,    4.0,        2.5),
    y      = c(1.0,    1.0,        3.8)
  )

  # Guarantee all 3 regimes exist
  stats <- tibble(regime = c("Fall", "Recovery", "Consolidation")) %>%
    left_join(stats, by = "regime") %>%
    replace_na(list(n_periods = 0, avg_days = 0, avg_ret = 0, avg_ann_ret = 0, pct_time = 0))

  nodes <- stats %>%
    left_join(node_pos, by = "regime") %>%
    mutate(
      color     = REGIME_PAL[regime],
      is_active = regime == current_regime,
      node_text = if_else(
        n_periods == 0,
        sprintf("%s\n(not observed)", toupper(regime)),
        sprintf(
          "%s%s\nN=%d · avg %d days\nAvg: %s  Speed: %s/yr\n%s of time",
          if_else(is_active, "▶ ", ""),
          toupper(regime),
          n_periods, avg_days,
          percent(avg_ret,     accuracy = 0.1),
          percent(avg_ann_ret, accuracy = 0.1),
          percent(pct_time,    accuracy = 0.1)
        )
      )
    )

  # ── Edge definitions — 3-edge cycle ──────────────────────────────────────
  # lx / ly = offset from edge midpoint for label placement
  edge_def <- tribble(
    ~from,           ~to,               ~condition,               ~curv,  ~lx,   ~ly,
    "Fall",          "Recovery",        "Trough reached",         -0.20,   0.00, -0.45,
    "Recovery",      "Consolidation",   "Recovery complete",      -0.30,   0.65,  0.20,
    "Consolidation", "Fall",            paste0("DD ≥ ", p_fall_lbl), -0.30, -0.65,  0.20
  ) %>%
    left_join(node_pos %>% rename(from = regime, x0 = x, y0 = y), by = "from") %>%
    left_join(node_pos %>% rename(to   = regime, x1 = x, y1 = y), by = "to") %>%
    left_join(trans %>% select(from, to, n_obs, prob), by = c("from", "to")) %>%
    mutate(
      mid_x      = (x0 + x1) / 2,
      mid_y      = (y0 + y1) / 2,
      lx_abs     = mid_x + lx,
      ly_abs     = mid_y + ly,
      prob_label = if_else(
        !is.na(prob),
        sprintf("p=%.0f%%  (n=%d)", prob * 100, n_obs),
        NA_character_
      ),
      px_abs = mid_x + lx * 0.45,
      py_abs = mid_y + ly * 0.45
    )

  # ── Build plot — one geom_curve per edge (scalar curvature required) ───────
  p <- ggplot() +
    coord_cartesian(xlim = c(-0.8, 5.8), ylim = c(-0.5, 5.2)) +
    theme_void(base_size = 11) +
    theme(plot.margin = margin(8, 8, 8, 8))

  for (i in seq_len(nrow(edge_def))) {
    e <- edge_def[i, ]
    p <- p + geom_curve(
      data       = e,
      aes(x = x0, y = y0, xend = x1, yend = y1),
      curvature  = e$curv,
      arrow      = arrow(length = unit(0.13, "in"), type = "closed"),
      color      = "grey42",
      linewidth  = 0.70,
      show.legend = FALSE
    )
  }

  p <- p +

    # Condition labels on arrows
    geom_label(
      data          = edge_def,
      aes(x = lx_abs, y = ly_abs, label = condition),
      size          = 2.6,
      color         = "grey28",
      fill          = "white",
      label.size    = 0.25,
      label.padding = unit(0.20, "lines"),
      fontface      = "italic",
      show.legend   = FALSE
    ) +

    # Empirical transition probability labels
    geom_text(
      data        = edge_def %>% filter(!is.na(prob_label)),
      aes(x = px_abs, y = py_abs, label = prob_label),
      size        = 2.2,
      color       = "grey55",
      fontface    = "bold",
      show.legend = FALSE
    ) +

    # Active-node outer glow ring
    geom_label(
      data = nodes %>% filter(is_active),
      aes(x = x, y = y, label = node_text, fill = color),
      color         = "white",
      size          = 3.4,
      fontface      = "bold",
      alpha         = 0.22,
      label.size    = 3.2,
      label.padding = unit(0.72, "lines"),
      label.r       = unit(0.50, "lines"),
      show.legend   = FALSE
    ) +

    # Node labels (all 3 regimes)
    geom_label(
      data = nodes,
      aes(x = x, y = y, label = node_text, fill = color),
      color         = "white",
      size          = 3.2,
      fontface      = "bold",
      label.size    = 0.65,
      label.padding = unit(0.62, "lines"),
      label.r       = unit(0.44, "lines"),
      show.legend   = FALSE
    ) +
    scale_fill_identity() +

    # Title bar
    annotate(
      "text", x = 2.5, y = 4.95,
      label   = sprintf("%s  Regime State Machine   |   Fall ≥%s   |   %s",
                        master, p_fall_lbl, date_range),
      size     = 3.5,
      fontface = "bold",
      color    = "grey18",
      hjust    = 0.5
    ) +

    # Current-regime badge
    annotate(
      "label",
      x = 2.5, y = 4.62,
      label         = sprintf("NOW: %s  (since %s)", current_regime, current_since),
      size          = 3.0,
      fontface      = "bold",
      color         = REGIME_PAL[current_regime],
      fill          = "white",
      label.size    = 0.5,
      label.padding = unit(0.25, "lines"),
      hjust         = 0.5
    )

  p
}


# ==============================================================================
# 2. plot_sm_alpha_heatmap()
# ==============================================================================
# Ticker × Regime TYPE avg return heatmap (relative mode):
#   SPY row  : avg absolute return per regime type
#   Others   : avg alpha (ticker − SPY) per regime type
# Columns are the 4 regime types (not individual periods).
# SPY row distinguished with navy border.
# ==============================================================================

plot_sm_alpha_heatmap <- function(xts_ret,
                                   tickers,
                                   master   = "SPY",
                                   t_fall   = 0.10,
                                   t_cruise = 0.05) {

  missing_tk <- setdiff(c(master, tickers), colnames(xts_ret))
  if (length(missing_tk) > 0)
    stop("Tickers not in xts_ret: ", paste(missing_tk, collapse = ", "))

  master_r <- xts_ret[, master]
  rt       <- build_regime_table(master_r, t_fall, t_cruise)
  all_tkrs <- c(master, tickers)

  # Per-ticker, per-period display returns
  period_df <- map_dfr(all_tkrs, function(tk) {
    tk_r <- xts_ret[, tk]
    rt %>%
      rowwise() %>%
      mutate(
        ticker   = tk,
        raw_ret  = .period_ret(tk_r, xmin, xmax),
        spy_ret  = period_return,
        disp_ret = .disp_ret_sm(tk, raw_ret, spy_ret, master)
      ) %>%
      ungroup() %>%
      select(ticker, regime, disp_ret)
  })

  # Average per ticker per regime TYPE
  avg_df <- period_df %>%
    mutate(regime = as.character(regime)) %>%
    group_by(ticker, regime) %>%
    summarise(avg_disp = mean(disp_ret, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      label_str = if_else(
        ticker == master,
        percent(avg_disp, accuracy = 0.1),
        sprintf("%s%.1f%%", if_else(avg_disp >= 0, "+", ""), avg_disp * 100)
      ),
      ticker    = factor(ticker, levels = rev(all_tkrs)),
      regime    = factor(regime, levels = c("Fall", "Recovery", "Consolidation")),
      is_master = ticker == master,
      txt_color = if_else(abs(avg_disp) > 0.06, "white", "grey20")
    )

  ggplot(avg_df, aes(x = regime, y = ticker, fill = avg_disp)) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_text(aes(label = label_str, color = txt_color),
              size = 3.0, fontface = "bold", show.legend = FALSE) +

    # Navy outline on SPY row
    geom_tile(
      data = avg_df %>% filter(is_master),
      aes(x = regime, y = ticker),
      fill = NA, color = "#1D3557", linewidth = 1.3,
      show.legend = FALSE
    ) +

    scale_fill_gradient2(
      low      = "#D90429",
      mid      = "white",
      high     = "#2D6A4F",
      midpoint = 0,
      name     = "Avg Return / Alpha",
      labels   = percent_format(accuracy = 1)
    ) +
    scale_color_identity() +
    scale_x_discrete(
      labels = c(
        Fall          = paste0("FALL\n≥", percent(t_fall, accuracy = 1)),
        Recovery      = "RECOVERY",
        Consolidation = "CONSOLIDATION"
      )
    ) +

    theme_minimal(base_size = 11) +
    theme(
      panel.grid    = element_blank(),
      axis.text.x   = element_text(face = "bold", size = 9.5, color = "grey25"),
      axis.text.y   = element_text(face = "bold", size = 9),
      axis.title    = element_blank(),
      legend.position = "right",
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "grey50", size = 9)
    ) +
    labs(
      title    = sprintf("Avg Return per Regime  —  %s: Absolute  |  Others: α vs %s",
                         master, master),
      subtitle = "Navy border = SPY (absolute avg)  |  All other rows = avg alpha vs SPY per regime type  |  + prefix = outperformed SPY"
    )
}


# ==============================================================================
# 3. plot_sm_transition_matrix()
# ==============================================================================
# 4×4 heatmap of empirical transition probabilities between regime types.
# Cell label: "XX% (n=Y)". Diagonal cells = self-loops (if any).
# Row = From, Column = To. Colour intensity = empirical probability.
# ==============================================================================

plot_sm_transition_matrix <- function(xts_ret,
                                       master   = "SPY",
                                       t_fall   = 0.10,
                                       t_cruise = 0.05) {

  master_r <- xts_ret[, master]
  rt       <- build_regime_table(master_r, t_fall, t_cruise)
  trans    <- .sm_transitions(rt)

  all_regimes <- c("Fall", "Recovery", "Consolidation")

  # Full 4×4 grid — fill missing pairs with 0
  full_grid <- expand.grid(
    from = all_regimes,
    to   = all_regimes,
    stringsAsFactors = FALSE
  ) %>%
    left_join(trans, by = c("from", "to")) %>%
    replace_na(list(n_obs = 0, prob = 0)) %>%
    mutate(
      from  = factor(from, levels = rev(all_regimes)),
      to    = factor(to,   levels = all_regimes),
      label = if_else(n_obs > 0,
                      sprintf("%.0f%%\n(n=%d)", prob * 100, n_obs),
                      "—"),
      txt_color = if_else(prob > 0.45, "white", "grey25"),
      is_diag   = as.character(from) == as.character(to)
    )

  ggplot(full_grid, aes(x = to, y = from, fill = prob)) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(aes(label = label, color = txt_color),
              size = 3.2, fontface = "bold", lineheight = 1.2,
              show.legend = FALSE) +

    # Diagonal: thick grey border to mark self-loops
    geom_tile(
      data = full_grid %>% filter(is_diag),
      aes(x = to, y = from),
      fill = NA, color = "grey55", linewidth = 1.2,
      show.legend = FALSE
    ) +

    scale_fill_gradient(
      low  = "grey96",
      high = "#1D3557",
      name = "Transition\nProbability",
      labels = percent_format(accuracy = 1),
      limits = c(0, 1)
    ) +
    scale_color_identity() +
    scale_x_discrete(
      labels = setNames(toupper(all_regimes), all_regimes),
      position = "top"
    ) +
    scale_y_discrete(labels = setNames(toupper(rev(all_regimes)), rev(all_regimes))) +

    theme_minimal(base_size = 11) +
    theme(
      panel.grid   = element_blank(),
      axis.text    = element_text(face = "bold", size = 9.5, color = "grey22"),
      axis.title.x = element_text(face = "bold", size = 10, color = "grey30",
                                   margin = margin(t = 4)),
      axis.title.y = element_text(face = "bold", size = 10, color = "grey30",
                                   margin = margin(r = 4)),
      legend.position = "right",
      plot.title   = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "grey50", size = 9)
    ) +
    labs(
      title    = sprintf("%s Regime Transition Matrix  (Empirical)", master),
      subtitle = sprintf(
        "Based on %d observed regime periods  |  Diagonal = self-loop  |  Missing cells = never observed",
        nrow(rt)
      ),
      x = "To →",
      y = "← From"
    )
}


# ==============================================================================
# 4. run_sm_diagram_rel()   Wrapper
# ==============================================================================

run_sm_diagram_rel <- function(xts_ret,
                                tickers,
                                master     = "SPY",
                                t_fall     = 0.10,
                                t_cruise   = 0.05) {

  cat(sprintf(
    "\n── Regime State Machine Diagram: %s  |  Fall ≥%.0f%%  Cruise <%.0f%% ──\n",
    master, t_fall * 100, t_cruise * 100
  ))

  rt <- build_regime_table(xts_ret[, master], t_fall, t_cruise)

  cat(sprintf("   Regime periods : %d\n", nrow(rt)))
  cat(sprintf("   Tickers        : %s\n", paste(tickers, collapse = ", ")))
  cat(sprintf("   %s row         : Absolute return per regime\n", master))
  cat("   Others         : Alpha vs SPY per regime\n")
  cat("─────────────────────────────────────────────────────────────────────\n")

  p1 <- plot_sm_diagram(xts_ret, master, t_fall, t_cruise)
  p2 <- plot_sm_alpha_heatmap(xts_ret, tickers, master, t_fall, t_cruise)
  p3 <- plot_sm_transition_matrix(xts_ret, master, t_fall, t_cruise)

  print(p1)
  print(p2)
  print(p3)

  invisible(rt)
}


# ==============================================================================
# MAIN
# ==============================================================================

if (!exists("xts_ret"))
  xts_ret <- readRDS(here::here("02_data_processed/xts_ret_returns.rds"))

core_universe <- c(
  "IEF", "HYG",
  "IEFA",
  "XLK", "XLF", "XLI",
  "GLD"
)

run_sm_diagram_rel(
  xts_ret  = xts_ret,
  tickers  = core_universe,
  master   = "SPY",
  t_fall   = 0.10,
  t_cruise = 0.05
)

################################################################################
