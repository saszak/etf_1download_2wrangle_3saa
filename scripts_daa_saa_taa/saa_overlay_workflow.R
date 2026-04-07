################################################################################
# FILE    : scripts_daa_saa_taa/saa_overlay_workflow.R
# Purpose : End-to-end workflow — build binary SAA → classify universe →
#           rank top enhancers / stabilizers → fingerprint → L/S overlay backtest
#
# DESIGN CHOICES (v1)
# ┌─────────────────────────┬────────────────────────────────────────────────┐
# │ Dimension               │ v1 Choice                                      │
# ├─────────────────────────┼────────────────────────────────────────────────┤
# │ SAA type                │ Binary (two tickers + target weights)          │
# │ SAA rebalancing         │ Daily (r_saa = w1·r1 + w2·r2, constant weight)│
# │ SAA injection           │ Added to xts_ret as "saa{W1}{W2}" e.g."saa6040│
# │ Enhancer ranking        │ full_ir  (information ratio over full period)  │
# │ Stabilizer ranking      │ dd_reduction (MaxDD reduction in pp)           │
# │ Screening universe      │ Full Sovereign Universe (default); custom vec  │
# │ Overlay formula         │ Option B: r_ov = r_SAA + α·(r_cand − r_SAA)  │
# │ Backtest type           │ Long overlay-SAA / Short original-SAA (spread) │
# │ Fingerprint             │ screen_ticker() per ticker + summary heatmap   │
# └─────────────────────────┴────────────────────────────────────────────────┘
#
# OVERLAY FORMULA
#   r_overlay_t = r_SAA_t + α × (r_candidate_t − r_SAA_t)
#   L/S spread  = α × (r_candidate_t − r_SAA_t)   ← daily PnL of overlay vs SAA
#
# MAIN FUNCTION
#   run_saa_overlay_workflow(ticker1, ticker2, w1, enh_pct, stab_pct, ...)
#     → list: saa_name, enhancers, stabilizers, summary_fig,
#             enh_screens, stab_screens, enh_backtest, stab_backtest
#
# EXPORTED HELPERS
#   build_saa_ret(ticker1, ticker2, w1, name)    → modifies global xts_ret
#   rank_enhancers(baseline, universe, top_n)     → tibble, ranked by full_ir
#   rank_stabilizers(baseline, universe, top_n)   → tibble, ranked by dd_reduction
#   fingerprint_candidates(tickers, baseline)     → named list of screen results
#   overlay_summary_heatmap(enh_tbl, stab_tbl)   → patchwork heatmap
#   backtest_ls_overlay(saa_name, tickers, pct, type)
#                                                 → list(fig, scatter, stats, ret, spread)
#
# USAGE
#   source(here("scripts_daa_saa_taa/saa_overlay_workflow.R"))
#   res <- run_saa_overlay_workflow(
#     ticker1  = "SPY", ticker2 = "IEF", w1 = 0.60,
#     enh_pct  = 0.10,  stab_pct = 0.10
#   )
#   res$summary_fig          # heatmap: top 5 enhancers + top 5 stabilizers
#   res$enh_backtest$fig     # wealth + spread + stats for enhancer overlays
#   res$stab_backtest$fig    # wealth + spread + stats for stabilizer overlays
#   res$enh_screens[["GLD"]]$plot   # fingerprint for a specific candidate
################################################################################

library(tidyverse)
library(xts)
library(patchwork)
library(ggrepel)
library(here)

# ── Auto-source dependencies ──────────────────────────────────────────────────
if (!exists("project_tree"))       source(here("project_tree.R"))
if (!exists("xts_ret"))            source(here(project_tree$scripts$wrangle))
if (!exists("rt")) {
  source(here(project_tree$scripts$spy_dd_regime))
  rt <- build_regime_table(xts_ret[, "SPY"])
}
if (!exists("classify_universe"))  source(here("scripts_daa_saa_taa/screen_ticker.R"))

# ── Palette ───────────────────────────────────────────────────────────────────
.COL_SAA  <- "#7c3aed"   # purple — SAA baseline (consistent with project palette)
.COL_ENH  <- c("#3b82f6","#10b981","#f59e0b","#f97316","#dc2626")   # 5 enhancers
.COL_STAB <- c("#0891b2","#059669","#d97706","#9333ea","#be123c")   # 5 stabilizers

# ── Shared helpers ────────────────────────────────────────────────────────────

# Regime shading: add annotate("rect") for Fall + Recovery episodes
.add_shading <- function(p, rt) {
  falls <- rt[rt$regime == "Fall",     ]
  recvs <- rt[rt$regime == "Recovery", ]
  for (i in seq_len(nrow(falls)))
    p <- p + annotate("rect",
      xmin = falls$xmin[i], xmax = falls$xmax[i],
      ymin = -Inf, ymax = Inf, fill = "#fca5a5", alpha = 0.18)
  for (i in seq_len(nrow(recvs)))
    p <- p + annotate("rect",
      xmin = recvs$xmin[i], xmax = recvs$xmax[i],
      ymin = -Inf, ymax = Inf, fill = "#bbf7d0", alpha = 0.18)
  p
}

# Single-series performance stats (from log return vector)
.port_stats <- function(log_ret_vec, label) {
  r   <- as.numeric(log_ret_vec[!is.na(log_ret_vec)])
  cum <- exp(cumsum(r))
  ann_ret <- tail(cum, 1)^(252 / length(r)) - 1
  ann_vol <- sd(r) * sqrt(252)
  peak    <- cummax(cum)
  max_dd  <- min((cum - peak) / peak)
  tibble(
    Strategy  = label,
    `Ann Ret` = round(ann_ret, 4),
    `Ann Vol` = round(ann_vol, 4),
    `Max DD`  = round(max_dd,  4),
    Sharpe    = round(ann_ret / ann_vol,     3),
    Calmar    = round(ann_ret / abs(max_dd), 3)
  )
}

################################################################################
# 1.  build_saa_ret
#     Builds daily-rebalanced (constant-weight) SAA return series and injects
#     it into the global xts_ret.
#
#     Single-ticker SAA (ticker2 = NULL  OR  w1 = 1):
#       If ticker1 already exists in xts_ret, it is used directly as the SAA
#       (no new column created). This is the cleanest path for e.g. SPY 100%.
#       If ticker1 does NOT exist, an error is raised.
#
#     Two-ticker SAA (default):
#       r_saa_t = w1 * r1_t + w2 * r2_t  (constant-weight daily blend)
#       New column "saa{W1}{W2}" e.g. "saa6040" is added to xts_ret.
#
#     Name override: pass `name` explicitly to use a custom column name, e.g.
#       build_saa_ret("SPY","IEF", w1=0.60, name="my_saa")
################################################################################

build_saa_ret <- function(
    ticker1,
    ticker2 = NULL,  # NULL or omitted → single-ticker SAA (100% ticker1)
    w1,
    w2   = 1 - w1,
    name = NULL
) {
  xts_ret <- get("xts_ret", envir = globalenv())

  single <- is.null(ticker2) || abs(w2) < 1e-9

  # ── Single-ticker path ────────────────────────────────────────────────────
  if (single) {
    if (!ticker1 %in% colnames(xts_ret))
      stop(sprintf("'%s' not found in xts_ret", ticker1))

    # If no custom name requested, use ticker1 directly (already in xts_ret)
    if (is.null(name)) {
      message(sprintf(
        "\u2713 SAA = 100%% %s  \u2192 using existing xts_ret column directly",
        ticker1
      ))
      return(invisible(ticker1))
    }

    # Custom name requested — create a named copy if not already present
    if (name %in% colnames(xts_ret)) {
      message(sprintf("  '%s' already in xts_ret — skipping build.", name))
      return(invisible(name))
    }
    saa_ret <- xts_ret[, ticker1, drop = FALSE]
    colnames(saa_ret) <- name
    assign("xts_ret", merge(xts_ret, saa_ret), envir = globalenv())
    message(sprintf(
      "\u2713 '%s' = 100%% %s  [%s \u2192 %s, %d days]  \u2192 added to xts_ret",
      name, ticker1,
      format(min(index(saa_ret)), "%Y-%m-%d"),
      format(max(index(saa_ret)), "%Y-%m-%d"),
      nrow(saa_ret)
    ))
    return(invisible(name))
  }

  # ── Two-ticker path ───────────────────────────────────────────────────────
  if (is.null(name))
    name <- paste0("saa", round(w1 * 100), round(w2 * 100))

  if (name %in% colnames(xts_ret)) {
    message(sprintf("  '%s' already in xts_ret — skipping build.", name))
    return(invisible(name))
  }

  if (!ticker1 %in% colnames(xts_ret))
    stop(sprintf("'%s' not found in xts_ret", ticker1))
  if (!ticker2 %in% colnames(xts_ret))
    stop(sprintf("'%s' not found in xts_ret", ticker2))

  common  <- na.omit(merge(xts_ret[, ticker1], xts_ret[, ticker2]))
  saa_ret <- w1 * common[, ticker1] + w2 * common[, ticker2]
  colnames(saa_ret) <- name

  assign("xts_ret", merge(xts_ret, saa_ret), envir = globalenv())

  message(sprintf(
    "\u2713 '%s' = %.0f%% %s + %.0f%% %s  [%s \u2192 %s, %d days]  \u2192 added to xts_ret",
    name, w1*100, ticker1, w2*100, ticker2,
    format(min(index(saa_ret)), "%Y-%m-%d"),
    format(max(index(saa_ret)), "%Y-%m-%d"),
    nrow(saa_ret)
  ))

  invisible(name)
}

################################################################################
# 2.  rank_enhancers
#     Classifies universe vs SAA baseline; returns top N by full_ir.
#     Classification filter: "Enhancer" or "Dual".
################################################################################

rank_enhancers <- function(
    baseline,           # character — SAA column name in xts_ret (e.g. "saa6040")
    universe  = NULL,   # character vector; NULL = all cols in xts_ret except baseline
    top_n     = 5L,
    overlay_w = 0.10,   # overlay weight passed to screen_ticker() inside classify_universe
    verbose   = FALSE
) {
  xts_ret <- get("xts_ret", envir = globalenv())
  rt      <- get("rt",      envir = globalenv())

  tickers <- if (is.null(universe))
    colnames(xts_ret)[!colnames(xts_ret) %in% baseline]
  else
    universe[!universe %in% baseline]

  cls <- classify_universe(
    tickers   = tickers,
    xts_ret   = xts_ret,
    rt        = rt,
    baseline  = baseline,
    overlay_w = overlay_w,
    verbose   = verbose
  )

  out <- cls %>%
    dplyr::filter(classification %in% c("Enhancer", "Dual"), !is.na(full_ir)) %>%
    dplyr::arrange(dplyr::desc(full_ir)) %>%
    dplyr::slice_head(n = as.integer(top_n))

  message(sprintf("  %d enhancer(s) selected (top %d by full_ir vs '%s')",
                  nrow(out), top_n, baseline))
  out
}

################################################################################
# 3.  rank_stabilizers
#     Classifies universe vs SAA baseline; returns top N by dd_reduction.
#     Classification filter: "Stabilizer" or "Dual".
#     Note: dd_reduction is in percentage points (pp), e.g. 3.2 = 3.2pp MaxDD reduction.
################################################################################

rank_stabilizers <- function(
    baseline,
    universe  = NULL,
    top_n     = 5L,
    overlay_w = 0.10,
    verbose   = FALSE
) {
  xts_ret <- get("xts_ret", envir = globalenv())
  rt      <- get("rt",      envir = globalenv())

  tickers <- if (is.null(universe))
    colnames(xts_ret)[!colnames(xts_ret) %in% baseline]
  else
    universe[!universe %in% baseline]

  cls <- classify_universe(
    tickers   = tickers,
    xts_ret   = xts_ret,
    rt        = rt,
    baseline  = baseline,
    overlay_w = overlay_w,
    verbose   = verbose
  )

  out <- cls %>%
    dplyr::filter(classification %in% c("Stabilizer", "Dual"), !is.na(dd_reduction)) %>%
    dplyr::arrange(dplyr::desc(dd_reduction)) %>%
    dplyr::slice_head(n = as.integer(top_n))

  message(sprintf("  %d stabilizer(s) selected (top %d by dd_reduction vs '%s')",
                  nrow(out), top_n, baseline))
  out
}

################################################################################
# 4.  fingerprint_candidates
#     Runs screen_ticker() for each candidate vs the SAA baseline.
#     Returns a named list; each element = screen_ticker() result list with
#       $enhancer, $stabilizer, $baseline, $plot, $ticker
################################################################################

fingerprint_candidates <- function(
    tickers,
    baseline,
    overlay_w  = 0.10,
    print_plot = TRUE,
    ...
) {
  xts_ret <- get("xts_ret", envir = globalenv())
  rt      <- get("rt",      envir = globalenv())

  results <- lapply(tickers, function(tk) {
    message(sprintf("  Fingerprint: %s vs %s ...", tk, baseline))
    screen_ticker(
      ticker     = tk,
      xts_ret    = xts_ret,
      rt         = rt,
      baseline   = baseline,
      overlay_w  = overlay_w,
      print_plot = print_plot,
      ...
    )
  })
  names(results) <- tickers
  invisible(results)
}

################################################################################
# 5.  overlay_summary_heatmap
#     Condensed two-panel ranking view:
#       Left:  top enhancers   — Rank | Ticker | Class | Trigger | Full IR
#       Right: top stabilizers — Rank | Ticker | Class | Hedge | DD Red | Ins Ratio | Fall ρ
#     Ranking column (Full IR / DD Red) traffic-light coloured.
################################################################################

overlay_summary_heatmap <- function(enh_tbl, stab_tbl, baseline = "") {

  # ── Enhancer panel ─────────────────────────────────────────────────────────
  enh_cols <- c("Rank","Ticker","Class","Trigger","Full IR")
  enh_df <- enh_tbl %>%
    dplyr::mutate(
      row       = dplyr::row_number(),
      Rank      = as.character(row),
      Ticker    = ticker,
      Class     = dplyr::coalesce(classification, "—"),
      Trigger   = dplyr::coalesce(trigger_type, "—"),
      `Full IR` = sprintf("%.2f", full_ir)
    ) %>%
    dplyr::select(row, dplyr::all_of(enh_cols)) %>%
    tidyr::pivot_longer(-row, names_to = "col", values_to = "val") %>%
    dplyr::mutate(
      col  = factor(col, levels = enh_cols),
      fill = dplyr::case_when(
        col == "Full IR" & row == 1            ~ "#bbf7d0",
        col == "Full IR" & row == nrow(enh_tbl) ~ "#fecaca",
        col == "Full IR"                       ~ "#fef9c3",
        TRUE                                   ~ "#f8fafc"
      )
    )

  p_enh <- ggplot(enh_df, aes(col, -row)) +
    geom_tile(aes(fill = fill), colour = "white", linewidth = 0.5) +
    geom_text(aes(label = val), size = 3.2, colour = "#1e293b") +
    scale_fill_identity() +
    scale_x_discrete(position = "top") +
    labs(
      title    = "Top Enhancers",
      subtitle = sprintf("ranked by Full IR vs %s", baseline)
    ) +
    theme_void(base_size = 11) +
    theme(
      axis.text.x.top = element_text(face = "bold", colour = "#1e293b", size = 9),
      plot.title      = element_text(face = "bold", colour = .COL_ENH[1], size = 12),
      plot.subtitle   = element_text(colour = "#64748b", size = 9),
      plot.margin     = margin(8, 16, 8, 8)
    )

  # ── Stabilizer panel ───────────────────────────────────────────────────────
  stab_cols <- c("Rank","Ticker","Class","Hedge","DD Red (pp)","Ins Ratio","Fall ρ")
  stab_df <- stab_tbl %>%
    dplyr::mutate(
      row           = dplyr::row_number(),
      Rank          = as.character(row),
      Ticker        = ticker,
      Class         = dplyr::coalesce(classification, "—"),
      Hedge         = dplyr::coalesce(hedge_type, "—"),
      `DD Red (pp)` = sprintf("%.1f", dd_reduction),
      `Ins Ratio`   = sprintf("%.1f", ins_ratio),
      `Fall ρ`      = sprintf("%.2f", fall_corr)
    ) %>%
    dplyr::select(row, dplyr::all_of(stab_cols)) %>%
    tidyr::pivot_longer(-row, names_to = "col", values_to = "val") %>%
    dplyr::mutate(
      col  = factor(col, levels = stab_cols),
      fill = dplyr::case_when(
        col == "DD Red (pp)" & row == 1              ~ "#bbf7d0",
        col == "DD Red (pp)" & row == nrow(stab_tbl) ~ "#fecaca",
        col == "DD Red (pp)"                         ~ "#fef9c3",
        TRUE                                         ~ "#f8fafc"
      )
    )

  p_stab <- ggplot(stab_df, aes(col, -row)) +
    geom_tile(aes(fill = fill), colour = "white", linewidth = 0.5) +
    geom_text(aes(label = val), size = 3.2, colour = "#1e293b") +
    scale_fill_identity() +
    scale_x_discrete(position = "top") +
    labs(
      title    = "Top Stabilizers",
      subtitle = sprintf("ranked by DD Reduction (pp) vs %s", baseline)
    ) +
    theme_void(base_size = 11) +
    theme(
      axis.text.x.top = element_text(face = "bold", colour = "#1e293b", size = 9),
      plot.title      = element_text(face = "bold", colour = .COL_STAB[1], size = 12),
      plot.subtitle   = element_text(colour = "#64748b", size = 9),
      plot.margin     = margin(8, 8, 8, 16)
    )

  p_enh + p_stab +
    patchwork::plot_annotation(
      title = sprintf("SAA Overlay Candidates — baseline: %s", baseline),
      theme = theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
      )
    )
}

################################################################################
# 6.  backtest_ls_overlay
#     Overlay formula: r_overlay_t = r_SAA_t + α*(r_candidate_t − r_SAA_t)
#     L/S spread PnL : α*(r_candidate_t − r_SAA_t)
#
#     Output figures:
#       $fig     — three panels (patchwork):
#                    P1 (tall)   Wealth curves: SAA (purple) + each overlay
#                    P2 (medium) Cumulative L/S spread PnL (zero = SAA baseline)
#                    P3 (short)  Stats table: Ann Ret / Ann Vol / Max DD / Sharpe / Calmar
#       $scatter — two side-by-side scatter plots (patchwork):
#                    Left  Absolute risk-return: Ann Vol vs Ann Ret (iso-Sharpe lines)
#                    Right Relative risk-return: tracking error vs excess return (iso-IR lines)
################################################################################

backtest_ls_overlay <- function(
    saa_name,                          # SAA column name in xts_ret
    tickers,                           # candidate tickers (up to 5)
    pct,                               # overlay fraction α
    type = c("enhancer", "stabilizer") # controls colour palette and title
) {
  type    <- match.arg(type)
  xts_ret <- get("xts_ret", envir = globalenv())
  rt      <- get("rt",      envir = globalenv())
  colours <- (if (type == "enhancer") .COL_ENH else .COL_STAB)[seq_along(tickers)]

  saa_ret <- xts_ret[, saa_name]

  # ── Build overlay series ──────────────────────────────────────────────────
  overlay_list <- lapply(tickers, function(tk) {
    common <- na.omit(merge(saa_ret, xts_ret[, tk]))
    ov     <- common[, saa_name] + pct * (common[, tk] - common[, saa_name])
    colnames(ov) <- tk
    ov
  })
  names(overlay_list) <- tickers

  # Align all series to common date range
  all_xts <- do.call(merge, c(list(saa_ret), overlay_list))
  all_xts <- na.omit(all_xts)
  colnames(all_xts)[1] <- saa_name
  dates <- as.Date(index(all_xts))

  # ── Panel 1: Wealth ───────────────────────────────────────────────────────
  cum_mat <- apply(all_xts, 2, function(r) {
    w <- exp(cumsum(as.numeric(r)))
    w / w[1]
  })

  wealth_df <- data.frame(date = dates, cum_mat, check.names = FALSE) %>%
    tidyr::pivot_longer(-date, names_to = "strategy", values_to = "wealth") %>%
    dplyr::mutate(
      is_saa  = strategy == saa_name,
      colour  = ifelse(is_saa, .COL_SAA,
                        colours[match(strategy, tickers)])
    )

  # End-of-series labels (ticker + ann return)
  ann_rets <- c(
    setNames(.port_stats(all_xts[, saa_name], saa_name)$`Ann Ret`, saa_name),
    setNames(
      sapply(tickers, function(tk) .port_stats(all_xts[, tk], tk)$`Ann Ret`),
      tickers
    )
  )
  end_labels <- wealth_df %>%
    dplyr::group_by(strategy) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(lbl = sprintf("%s\n%.1f%%", strategy, ann_rets[strategy] * 100))

  p1 <- ggplot(wealth_df, aes(date, wealth, group = strategy, colour = colour)) +
    geom_line(data = dplyr::filter(wealth_df, !is_saa),  linewidth = 0.65, alpha = 0.85) +
    geom_line(data = dplyr::filter(wealth_df,  is_saa),  linewidth = 1.10, alpha = 1.00) +
    geom_text(data = end_labels, aes(label = lbl, colour = colour),
              hjust = 0, nudge_x = 50, size = 2.7, lineheight = 0.85) +
    scale_colour_identity() +
    scale_x_date(expand = expansion(mult = c(0.01, 0.13))) +
    labs(
      y        = "Cumulative wealth ($1 start)",
      x        = NULL,
      title    = sprintf("Long %s Overlay / Short SAA \u2014 Wealth Comparison",
                         ifelse(type == "enhancer", "Enhancer", "Stabilizer")),
      subtitle = sprintf("\u03b1 = %.0f%%  |  SAA baseline = %s (purple)  |  overlay = SAA + \u03b1\u00b7(candidate \u2212 SAA)",
                         pct * 100, saa_name)
    ) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(), legend.position = "none",
          plot.title = element_text(face = "bold"), plot.subtitle = element_text(colour = "#64748b"))
  p1 <- .add_shading(p1, rt)

  # ── Panel 2: Cumulative L/S spread ────────────────────────────────────────
  spread_df <- dplyr::bind_rows(lapply(seq_along(tickers), function(j) {
    tk <- tickers[j]
    sp <- pct * (as.numeric(all_xts[, tk]) - as.numeric(all_xts[, saa_name]))
    data.frame(
      date   = dates,
      ticker = tk,
      cum_sp = exp(cumsum(sp)),
      colour = colours[j],
      stringsAsFactors = FALSE
    )
  }))

  p2 <- ggplot(spread_df, aes(date, cum_sp, group = ticker, colour = colour)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "#94a3b8", linewidth = 0.4) +
    geom_line(linewidth = 0.7, alpha = 0.9) +
    scale_colour_identity() +
    labs(
      y     = "Cumulative spread ($1)",
      x     = NULL,
      title = sprintf("L/S Spread PnL  [\u03b1\u00b7(candidate \u2212 SAA),  \u03b1=%.0f%%]", pct * 100)
    ) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(), legend.position = "none",
          plot.title = element_text(face = "bold"))
  p2 <- .add_shading(p2, rt)

  # ── Panel 3: Stats table ──────────────────────────────────────────────────
  stats_tbl <- dplyr::bind_rows(
    .port_stats(all_xts[, saa_name], paste0(saa_name, " (baseline)")),
    purrr::map2_dfr(tickers, paste0(tickers, " +", round(pct*100), "%"),
                     ~ .port_stats(all_xts[, .x], .y))
  )
  n_rows    <- nrow(stats_tbl)
  row_cols  <- c(.COL_SAA, colours)   # one colour per row
  met_cols  <- c("Ann Ret","Ann Vol","Max DD","Sharpe","Calmar")
  hib       <- c(TRUE, FALSE, FALSE, TRUE, TRUE)   # higher-is-better per metric

  # Numeric ranks for traffic-light colouring
  rank_df <- stats_tbl %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    tidyr::pivot_longer(-c(row, Strategy), names_to = "metric", values_to = "val_num") %>%
    dplyr::group_by(metric) %>%
    dplyr::mutate(
      best_row  = {
        m <- dplyr::cur_group()$metric
        if (m %in% c("Ann Ret","Sharpe","Calmar")) which.max(val_num)
        else which.min(val_num)
      },
      worst_row = {
        m <- dplyr::cur_group()$metric
        if (m %in% c("Ann Ret","Sharpe","Calmar")) which.min(val_num)
        else which.max(val_num)
      },
      fill = dplyr::case_when(
        row == best_row  ~ "#bbf7d0",
        row == worst_row ~ "#fecaca",
        TRUE             ~ "#fef9c3"
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(row, metric, fill)

  # Formatted display values
  fmt_tbl <- stats_tbl %>%
    dplyr::mutate(
      `Ann Ret` = sprintf("%.1f%%", `Ann Ret` * 100),
      `Ann Vol` = sprintf("%.1f%%", `Ann Vol` * 100),
      `Max DD`  = sprintf("%.1f%%", `Max DD`  * 100),
      Sharpe    = sprintf("%.2f",   Sharpe),
      Calmar    = sprintf("%.2f",   Calmar),
      row       = dplyr::row_number()
    )

  tbl_long <- fmt_tbl %>%
    tidyr::pivot_longer(-c(row, Strategy), names_to = "metric", values_to = "val") %>%
    dplyr::left_join(rank_df, by = c("row","metric")) %>%
    dplyr::mutate(
      metric     = factor(metric, levels = met_cols),
      row_colour = row_cols[row],
      fill       = dplyr::coalesce(fill, "#fef9c3")
    )

  strat_long <- fmt_tbl %>%
    dplyr::transmute(
      row       = row,
      metric    = factor("Strategy", levels = c("Strategy", met_cols)),
      val       = Strategy,
      fill      = "#f1f5f9",
      row_colour = row_cols[row]
    )

  full_tbl <- dplyr::bind_rows(strat_long, tbl_long) %>%
    dplyr::mutate(metric = factor(metric, levels = c("Strategy", met_cols)))

  p3 <- ggplot(full_tbl, aes(metric, -row)) +
    geom_tile(aes(fill = fill), colour = "white", linewidth = 0.4) +
    geom_text(aes(label = val, colour = row_colour), size = 2.9) +
    scale_fill_identity() +
    scale_colour_identity() +
    scale_x_discrete(position = "top") +
    labs(title = "Performance comparison", x = NULL, y = NULL) +
    theme_void(base_size = 10) +
    theme(
      axis.text.x.top = element_text(face = "bold", colour = "#1e293b", size = 8.5),
      plot.title      = element_text(face = "bold", size = 10),
      plot.margin     = margin(4, 8, 4, 8)
    )

  # ── Assemble figure ───────────────────────────────────────────────────────
  fig <- (p1 / p2 / p3) +
    patchwork::plot_layout(heights = c(3, 2, 1)) +
    patchwork::plot_annotation(
      caption = paste0(
        "Regime shading: Fall (#fca5a5 \u03b1=0.18) | Recovery (#bbf7d0 \u03b1=0.18)  |  ",
        sprintf("Overlay: r_SAA + %.0f%%\u00b7(r_candidate \u2212 r_SAA)", pct * 100)
      )
    )

  # ── Scatter plots ─────────────────────────────────────────────────────────
  scatter <- .overlay_scatter(all_xts, stats_tbl, saa_name, tickers, colours, pct, type)

  invisible(list(
    ret     = all_xts,
    spread  = spread_df,
    stats   = stats_tbl,
    fig     = fig,
    scatter = scatter
  ))
}

# ── .overlay_scatter ──────────────────────────────────────────────────────────
#   Private helper — called by backtest_ls_overlay().
#   Produces two side-by-side scatter plots:
#     Left  — Absolute: Ann Vol (x) vs Ann Ret (y); SAA + overlays; iso-Sharpe lines
#     Right — Relative: tracking error (x) vs excess return vs SAA (y); iso-IR lines
# ─────────────────────────────────────────────────────────────────────────────

.overlay_scatter <- function(all_xts, stats_tbl, saa_name, tickers, colours, pct, type) {

  n         <- length(tickers)
  col_vec   <- colours                              # one per candidate (already trimmed)
  all_cols  <- c(.COL_SAA, col_vec)                # SAA + candidates
  lbl_vec   <- c(saa_name, tickers)

  # Convenience: % formatter for axis labels
  .pct <- function(x) sprintf("%.1f%%", x * 100)

  # ── Absolute scatter ───────────────────────────────────────────────────────
  abs_df <- stats_tbl %>%
    dplyr::mutate(
      colour = all_cols,
      pt_size = c(4.5, rep(3.5, n)),
      lbl     = lbl_vec
    )

  vol_lo <- min(abs_df$`Ann Vol`) * 0.80
  vol_hi <- max(abs_df$`Ann Vol`) * 1.25
  ret_lo <- min(abs_df$`Ann Ret`) * 1.10
  ret_hi <- max(abs_df$`Ann Ret`) * 1.25

  # Iso-Sharpe reference lines through origin
  sr_vals  <- c(0.5, 1.0, 1.5)
  iso_sr   <- do.call(rbind, lapply(sr_vals, function(sr) {
    data.frame(sr = sr,
               vol = c(vol_lo, vol_hi),
               ret = c(vol_lo * sr, vol_hi * sr),
               stringsAsFactors = FALSE)
  }))
  iso_sr_lbl <- iso_sr %>%
    dplyr::group_by(sr) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::mutate(lbl = sprintf("SR %.1f", sr))

  p_abs <- ggplot(abs_df, aes(`Ann Vol`, `Ann Ret`)) +
    geom_line(data = iso_sr, aes(vol, ret, group = sr),
              colour = "#cbd5e1", linetype = "dashed", linewidth = 0.35) +
    geom_text(data = iso_sr_lbl, aes(vol, ret, label = lbl),
              colour = "#94a3b8", size = 2.6, hjust = 0, nudge_x = 0.002) +
    geom_hline(yintercept = 0, colour = "#e2e8f0", linewidth = 0.3) +
    geom_point(aes(colour = colour, size = pt_size)) +
    ggrepel::geom_text_repel(aes(label = lbl, colour = colour),
                              size = 3.0, fontface = "bold",
                              box.padding = 0.35, show.legend = FALSE) +
    scale_colour_identity() +
    scale_size_identity() +
    scale_x_continuous(labels = .pct, limits = c(vol_lo, vol_hi)) +
    scale_y_continuous(labels = .pct) +
    labs(
      x        = "Ann Volatility",
      y        = "Ann Return (CAGR)",
      title    = "Absolute Risk-Return",
      subtitle = sprintf("SAA baseline: %s (purple)  |  \u03b1 = %.0f%%", saa_name, pct * 100)
    ) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor  = element_blank(),
          legend.position   = "none",
          plot.title        = element_text(face = "bold"),
          plot.subtitle     = element_text(colour = "#64748b", size = 8))

  # ── Relative / L/S scatter ─────────────────────────────────────────────────
  #   Excess return  = overlay Ann Ret − SAA Ann Ret
  #   Tracking error = Ann Vol of (r_overlay − r_SAA) = Ann Vol of α*(r_cand − r_SAA)
  saa_ret_v <- as.numeric(all_xts[, saa_name])
  saa_ann   <- stats_tbl$`Ann Ret`[1]

  rel_rows <- lapply(seq_len(n), function(j) {
    tk     <- tickers[j]
    spread <- as.numeric(all_xts[, tk]) - saa_ret_v   # α*(r_cand − r_SAA)
    exc    <- stats_tbl$`Ann Ret`[j + 1] - saa_ann
    te     <- sd(spread, na.rm = TRUE) * sqrt(252)
    ir     <- if (te > 0) exc / te else NA_real_
    data.frame(ticker  = tk,
               exc_ret = exc,
               te      = te,
               ir      = ir,
               colour  = col_vec[j],
               stringsAsFactors = FALSE)
  })
  rel_df <- dplyr::bind_rows(rel_rows)

  te_lo  <- min(rel_df$te) * 0.70
  te_hi  <- max(rel_df$te) * 1.30
  exc_lo <- min(c(rel_df$exc_ret, 0)) * 1.20
  exc_hi <- max(c(rel_df$exc_ret, 0)) * 1.30

  ir_vals  <- c(-0.5, 0.5, 1.0, 1.5)
  iso_ir   <- do.call(rbind, lapply(ir_vals, function(ir) {
    data.frame(ir  = ir,
               te  = c(te_lo, te_hi),
               exc = c(te_lo * ir, te_hi * ir),
               stringsAsFactors = FALSE)
  }))
  iso_ir_lbl <- iso_ir %>%
    dplyr::group_by(ir) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::mutate(lbl = sprintf("IR %.1f", ir))

  p_rel <- ggplot(rel_df, aes(te, exc_ret)) +
    geom_line(data = iso_ir, aes(te, exc, group = ir),
              colour = "#cbd5e1", linetype = "dashed", linewidth = 0.35) +
    geom_text(data = iso_ir_lbl, aes(te, exc, label = lbl),
              colour = "#94a3b8", size = 2.6, hjust = 0, nudge_x = te_hi * 0.01) +
    geom_hline(yintercept = 0, colour = "#94a3b8", linewidth = 0.5) +
    geom_vline(xintercept = 0, colour = "#e2e8f0", linewidth = 0.3) +
    geom_point(aes(colour = colour), size = 3.5) +
    ggrepel::geom_text_repel(aes(label = sprintf("%s\nIR %.2f", ticker, ir), colour = colour),
                              size = 2.8, fontface = "bold",
                              box.padding = 0.35, lineheight = 0.85,
                              show.legend = FALSE) +
    scale_colour_identity() +
    scale_x_continuous(labels = .pct, limits = c(te_lo, te_hi)) +
    scale_y_continuous(labels = .pct) +
    labs(
      x        = "Tracking Error  (Ann Vol of overlay \u2212 SAA)",
      y        = "Excess Return vs SAA (Ann)",
      title    = "Relative Risk-Return (L/S)",
      subtitle = sprintf("Information ratio = excess return / tracking error  |  \u03b1 = %.0f%%",
                         pct * 100)
    ) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor  = element_blank(),
          legend.position   = "none",
          plot.title        = element_text(face = "bold"),
          plot.subtitle     = element_text(colour = "#64748b", size = 8))

  # ── Combine ────────────────────────────────────────────────────────────────
  p_abs + p_rel +
    patchwork::plot_annotation(
      title   = sprintf("%s Overlay — Risk-Return Scatter  (\u03b1 = %.0f%%)",
                        if (type == "enhancer") "Enhancer" else "Stabilizer",
                        pct * 100),
      caption = sprintf(
        "Left: absolute Ann Vol vs CAGR; dashed = iso-Sharpe lines  |  Right: tracking error vs excess return vs %s; dashed = iso-IR lines",
        saa_name
      ),
      theme   = theme(
        plot.title   = element_text(face = "bold", size = 12, hjust = 0.5),
        plot.caption = element_text(colour = "#94a3b8", size = 7.5)
      )
    )
}

################################################################################
# 7.  run_saa_overlay_workflow
#     Master orchestrator — runs all steps in sequence.
#
#     Steps:
#       1. build_saa_ret()          → adds "saa{W1}{W2}" to global xts_ret
#       2. rank_enhancers()         → top N by full_ir
#          rank_stabilizers()       → top N by dd_reduction
#       3. overlay_summary_heatmap()→ two-panel ranking table
#       4. fingerprint_candidates() → screen_ticker() per candidate (optional)
#       5. backtest_ls_overlay()    → L/S wealth + spread + stats (enhancers)
#          backtest_ls_overlay()    → L/S wealth + spread + stats (stabilizers)
################################################################################

run_saa_overlay_workflow <- function(
    ticker1,
    ticker2            = NULL,   # NULL → single-ticker SAA (100% ticker1)
    w1,
    w2                 = 1 - w1,
    saa_name           = NULL,    # NULL → auto "saa{W1}{W2}"
    universe           = NULL,    # NULL → full Sovereign Universe
    top_n              = 5L,
    enh_pct            = 0.10,    # α for enhancer overlay
    stab_pct           = 0.10,    # α for stabilizer overlay
    overlay_w          = 0.10,    # screen_ticker overlay weight for classification
    print_fingerprints = TRUE,    # set FALSE to skip screen_ticker() calls (faster)
    verbose            = FALSE    # classify_universe verbosity
) {
  saa_desc <- if (is.null(ticker2) || abs(1 - w1) < 1e-9) {
    sprintf("100%% %s", ticker1)
  } else {
    sprintf("%.0f%% %s + %.0f%% %s", w1*100, ticker1, w2*100, ticker2)
  }

  cat(sprintf(
    "\n%s\n  SAA Overlay Workflow  (v1)\n  SAA: %s  |  enh\u03b1=%.0f%%  stab\u03b1=%.0f%%\n%s\n\n",
    strrep("\u2550", 70),
    saa_desc, enh_pct*100, stab_pct*100,
    strrep("\u2550", 70)
  ))

  # Step 1 ───────────────────────────────────────────────────────────────────
  cat("\u2500\u2500 Step 1: Build SAA return series\n")
  saa_name <- build_saa_ret(ticker1, ticker2, w1, w2, name = saa_name)

  # Step 2 ───────────────────────────────────────────────────────────────────
  cat("\n\u2500\u2500 Step 2a: Rank enhancers vs", saa_name, "\n")
  enh_tbl  <- rank_enhancers(saa_name, universe, top_n, overlay_w, verbose)

  cat("\n\u2500\u2500 Step 2b: Rank stabilizers vs", saa_name, "\n")
  stab_tbl <- rank_stabilizers(saa_name, universe, top_n, overlay_w, verbose)

  cat(sprintf("\n  Enhancers:   %s\n", paste(enh_tbl$ticker,  collapse = "  ")))
  cat(sprintf("  Stabilizers: %s\n",  paste(stab_tbl$ticker, collapse = "  ")))

  # Step 3 ───────────────────────────────────────────────────────────────────
  cat("\n\u2500\u2500 Step 3: Summary heatmap\n")
  summary_fig <- overlay_summary_heatmap(enh_tbl, stab_tbl, baseline = saa_name)

  # Step 4 ───────────────────────────────────────────────────────────────────
  if (print_fingerprints) {
    cat("\n\u2500\u2500 Step 4a: Enhancer fingerprints\n")
    enh_screens  <- fingerprint_candidates(enh_tbl$ticker,  saa_name,
                                            overlay_w = overlay_w, print_plot = TRUE)
    cat("\n\u2500\u2500 Step 4b: Stabilizer fingerprints\n")
    stab_screens <- fingerprint_candidates(stab_tbl$ticker, saa_name,
                                            overlay_w = overlay_w, print_plot = TRUE)
  } else {
    enh_screens  <- NULL
    stab_screens <- NULL
  }

  # Step 5 ───────────────────────────────────────────────────────────────────
  cat("\n\u2500\u2500 Step 5a: Enhancer L/S backtest  (alpha =", enh_pct*100, "%)\n")
  enh_backtest  <- backtest_ls_overlay(saa_name, enh_tbl$ticker,  enh_pct,  "enhancer")

  cat("\n\u2500\u2500 Step 5b: Stabilizer L/S backtest  (alpha =", stab_pct*100, "%)\n")
  stab_backtest <- backtest_ls_overlay(saa_name, stab_tbl$ticker, stab_pct, "stabilizer")

  cat(sprintf("\n%s\n  Done.\n%s\n\n", strrep("\u2550", 70), strrep("\u2550", 70)))

  # Print figures (suppressed inside Rmd render)
  if (!isTRUE(getOption("knitr.in.progress"))) {
    print(summary_fig)
    print(enh_backtest$fig)
    print(enh_backtest$scatter)
    print(stab_backtest$fig)
    print(stab_backtest$scatter)
  }

  invisible(list(
    saa_name      = saa_name,
    enhancers     = enh_tbl,
    stabilizers   = stab_tbl,
    summary_fig   = summary_fig,
    enh_screens   = enh_screens,
    stab_screens  = stab_screens,
    enh_backtest  = enh_backtest,
    stab_backtest = stab_backtest
  ))
}
