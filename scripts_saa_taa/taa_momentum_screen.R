################################################################################
# scripts_saa_taa/taa_momentum_screen.R
#
# Purpose : TAA momentum screen — find ETFs significantly outperforming SPY
#           in the short term (6M / 12M / 2Y) even if NOT structurally
#           superior over the full period.
#
# LOGIC
#   TAA candidate  = rel_Nm  > REL_THRESHOLD  AND  ir_full < IR_SAA_THRESHOLD
#   SAA + TAA      = rel_Nm  > REL_THRESHOLD  AND  ir_full >= IR_SAA_THRESHOLD
#   SAA cold       = rel_Nm <= REL_THRESHOLD  AND  ir_full >= IR_SAA_THRESHOLD
#   Avoid          = rel_Nm <= REL_THRESHOLD  AND  ir_full <  IR_SAA_THRESHOLD
#
# FUNCTIONS
#   build_taa_screen(xts_ret, meta, ...)   → screen_tbl (one row per ticker)
#   plot_taa_scatter(screen_tbl, horizon)  → 2×2 quadrant scatter
#   plot_taa_ranking(screen_tbl, n)        → ranked bar chart (top n by horizon)
#   plot_taa_consistency(screen_tbl)       → top-quartile frequency dot plot
#   run_taa_screen(xts_ret, meta, ...)     → builds + prints all three plots
#
# USAGE
#   source(here("scripts_saa_taa/taa_momentum_screen.R"))
#   scr <- build_taa_screen(xts_ret, etf_metadata)
#   plot_taa_scatter(scr, "rel_6m")
#   plot_taa_ranking(scr)
#   plot_taa_consistency(scr)
#   # or all at once:
#   run_taa_screen(xts_ret, etf_metadata)
################################################################################

library(tidyverse)
library(xts)
library(scales)
library(patchwork)
if (requireNamespace("ggrepel", quietly = TRUE)) library(ggrepel)

# ── Constants ─────────────────────────────────────────────────────────────────
TAA_WINDOWS        <- c(rel_6m = 126L, rel_12m = 252L, rel_2y = 504L)
IR_SAA_THRESHOLD   <- 0.10   # full-period IR below this = not SAA-grade
REL_THRESHOLD      <- 0.05   # trailing relative return above this = recently hot
CONSIST_WINDOW     <- 126L   # rolling window for consistency (6M)
TOP_N_LABEL        <- 8L     # max ticker labels per quadrant in scatter

# ── Shared theme ──────────────────────────────────────────────────────────────
.taa_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor  = element_blank(),
    plot.title        = element_text(face = "bold", size = 12),
    plot.subtitle     = element_text(colour = "grey50", size = 8),
    legend.position   = "bottom",
    legend.key.height = unit(0.4, "cm")
  )

# ── Asset class colour palette ────────────────────────────────────────────────
.AC_COLS <- c(
  Equity      = "#1D3557",
  FixedIncome = "#E63946",
  Commodity   = "#F4A261",
  RealAsset   = "#2D6A4F",
  Alternative = "#9B59B6",
  MultiAsset  = "#1E6E7A",
  FX          = "#8B6510",
  Signal      = "#888888"
)

################################################################################
# build_taa_screen()
################################################################################

#' Build the TAA momentum screen table.
#'
#' @param xts_ret          xts — daily returns matrix (all tickers incl. master)
#' @param meta             data.frame — etf_metadata with ticker / name / asset_class
#' @param master           character — benchmark ticker (default "SPY")
#' @param ir_saa_threshold numeric — full-period IR below this = not SAA-grade
#' @param rel_threshold    numeric — trailing rel return above this = recently hot
#' @param windows          named int vector — trading-day windows (default 6M/12M/2Y)
#' @param consist_window   integer — rolling window for consistency score
#' @return tibble with one row per ticker and columns:
#'   ticker, name, asset_class, pf_function,
#'   rel_6m, rel_12m, rel_2y,          (trailing cumulative relative returns)
#'   ir_full, ir_1y,                   (annualised information ratio)
#'   consist_pct,                      (% of rolling windows with positive rel ret)
#'   quadrant, taa_flag
build_taa_screen <- function(xts_ret,
                              meta,
                              master           = "SPY",
                              ir_saa_threshold = IR_SAA_THRESHOLD,
                              rel_threshold    = REL_THRESHOLD,
                              windows          = TAA_WINDOWS,
                              consist_window   = CONSIST_WINDOW) {

  stopifnot(master %in% colnames(xts_ret))

  tickers <- setdiff(colnames(xts_ret), master)

  master_r <- as.numeric(xts_ret[, master])

  rows <- purrr::map(tickers, function(tk) {

    tk_r  <- as.numeric(xts_ret[, tk])
    valid <- !is.na(tk_r) & !is.na(master_r)
    r_rel <- tk_r[valid] - master_r[valid]
    n     <- length(r_rel)

    if (n < consist_window) return(NULL)

    # ── Full-period IR ──────────────────────────────────────────────────────
    ir_full <- if (sd(r_rel) > 0) mean(r_rel) / sd(r_rel) * sqrt(252) else NA_real_

    # ── Trailing 1Y IR ─────────────────────────────────────────────────────
    r_1y    <- tail(r_rel, 252L)
    ir_1y   <- if (length(r_1y) >= 63 && sd(r_1y) > 0)
                 mean(r_1y) / sd(r_1y) * sqrt(252) else NA_real_

    # ── Trailing cumulative relative returns ───────────────────────────────
    rel_vals <- purrr::map_dbl(windows, function(w) {
      seg <- tail(r_rel, w)
      if (length(seg) < w * 0.8) return(NA_real_)   # require 80% history
      prod(1 + seg) - 1
    })

    # ── Consistency: % of rolling consist_window windows with positive rel ret
    if (n >= consist_window * 2) {
      roll_rel <- zoo::rollapply(r_rel, consist_window,
                                 function(x) prod(1 + x) - 1,
                                 align = "right", fill = NA)
      consist_pct <- mean(roll_rel > 0, na.rm = TRUE)
    } else {
      consist_pct <- NA_real_
    }

    tibble(
      ticker      = tk,
      ir_full     = ir_full,
      ir_1y       = ir_1y,
      rel_6m      = rel_vals[["rel_6m"]],
      rel_12m     = rel_vals[["rel_12m"]],
      rel_2y      = rel_vals[["rel_2y"]],
      consist_pct = consist_pct
    )
  }) %>%
    purrr::compact() %>%
    bind_rows()

  # ── Join metadata ──────────────────────────────────────────────────────────
  meta_slim <- meta %>%
    select(ticker, name, asset_class, pf_function) %>%
    distinct(ticker, .keep_all = TRUE)

  rows <- rows %>%
    left_join(meta_slim, by = "ticker") %>%
    mutate(
      asset_class = replace_na(asset_class, "Unknown"),
      # ── Quadrant classification (based on rel_6m vs ir_full) ─────────────
      quadrant = case_when(
        rel_6m  >  rel_threshold & ir_full >= ir_saa_threshold ~ "SAA + TAA",
        rel_6m  >  rel_threshold & ir_full <  ir_saa_threshold ~ "TAA Only",
        rel_6m  <= rel_threshold & ir_full >= ir_saa_threshold ~ "SAA (cold)",
        TRUE                                                    ~ "Avoid"
      ),
      taa_flag = rel_6m > rel_threshold & ir_full < ir_saa_threshold
    ) %>%
    arrange(desc(rel_6m))

  rows
}

################################################################################
# plot_taa_scatter()
################################################################################

#' 2×2 quadrant scatter: recent relative return vs full-period IR.
#'
#' @param screen_tbl  output of build_taa_screen()
#' @param horizon     column name for y-axis: "rel_6m", "rel_12m", or "rel_2y"
#' @param top_n_label max tickers to label per quadrant
plot_taa_scatter <- function(screen_tbl,
                              horizon     = "rel_6m",
                              top_n_label = TOP_N_LABEL) {

  horizon_label <- switch(horizon,
    rel_6m  = "Trailing 6-Month",
    rel_12m = "Trailing 12-Month",
    rel_2y  = "Trailing 2-Year",
    horizon
  )

  df <- screen_tbl %>%
    filter(!is.na(.data[[horizon]]), !is.na(ir_full)) %>%
    mutate(
      y        = .data[[horizon]],
      ac_col   = .AC_COLS[asset_class],
      ac_col   = replace_na(ac_col, "#888888"),
      # Label top-n per quadrant by |y|
      do_label = quadrant %in% c("TAA Only", "SAA + TAA") &
                 rank(-abs(y)) <= top_n_label
    )

  # Quadrant background rectangles
  x_rng <- range(df$ir_full, na.rm = TRUE)
  y_rng <- range(df$y, na.rm = TRUE)
  pad_x <- diff(x_rng) * 0.05
  pad_y <- diff(y_rng) * 0.05

  quads <- tibble(
    xmin  = c(x_rng[1] - pad_x, IR_SAA_THRESHOLD,
              x_rng[1] - pad_x, IR_SAA_THRESHOLD),
    xmax  = c(IR_SAA_THRESHOLD,  x_rng[2] + pad_x,
              IR_SAA_THRESHOLD,  x_rng[2] + pad_x),
    ymin  = c(REL_THRESHOLD,     REL_THRESHOLD,
              y_rng[1] - pad_y,  y_rng[1] - pad_y),
    ymax  = c(y_rng[2] + pad_y,  y_rng[2] + pad_y,
              REL_THRESHOLD,     REL_THRESHOLD),
    fill  = c("#FFF5E6", "#EAF4EA", "#FFF0F0", "#F0F4FF"),
    label = c("TAA Only", "SAA + TAA", "Avoid", "SAA (cold)")
  )

  p <- ggplot(df, aes(x = ir_full, y = y)) +

    # Quadrant backgrounds
    geom_rect(data = quads,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                  fill = label),
              inherit.aes = FALSE, alpha = 0.35) +
    scale_fill_manual(
      values = c("TAA Only"  = "#FFE0B2",
                 "SAA + TAA" = "#C8E6C9",
                 "Avoid"     = "#FFCDD2",
                 "SAA (cold)"= "#BBDEFB"),
      guide = "none"
    ) +

    # Reference lines
    geom_hline(yintercept = REL_THRESHOLD,    linetype = "dashed",
               colour = "grey50", linewidth = 0.5) +
    geom_vline(xintercept = IR_SAA_THRESHOLD, linetype = "dashed",
               colour = "grey50", linewidth = 0.5) +
    geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.3) +
    geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.3) +

    # Points
    geom_point(aes(colour = asset_class, size = abs(y)),
               alpha = 0.75) +
    scale_colour_manual(values = .AC_COLS, name = NULL,
                        na.value = "#888888") +
    scale_size_continuous(range = c(1.5, 5), guide = "none") +

    # Quadrant labels
    annotate("text", x = IR_SAA_THRESHOLD - diff(x_rng) * 0.03,
             y = y_rng[2] + pad_y * 0.6,
             label = "TAA Only", hjust = 1, size = 3,
             fontface = "bold", colour = "#E65100") +
    annotate("text", x = IR_SAA_THRESHOLD + diff(x_rng) * 0.03,
             y = y_rng[2] + pad_y * 0.6,
             label = "SAA + TAA", hjust = 0, size = 3,
             fontface = "bold", colour = "#2E7D32") +
    annotate("text", x = IR_SAA_THRESHOLD - diff(x_rng) * 0.03,
             y = y_rng[1] - pad_y * 0.6,
             label = "Avoid", hjust = 1, size = 3,
             fontface = "bold", colour = "#C62828") +
    annotate("text", x = IR_SAA_THRESHOLD + diff(x_rng) * 0.03,
             y = y_rng[1] - pad_y * 0.6,
             label = "SAA (cold)", hjust = 0, size = 3,
             fontface = "bold", colour = "#1565C0") +

    # Ticker labels
    { if (requireNamespace("ggrepel", quietly = TRUE))
        ggrepel::geom_text_repel(
          data = df[df$do_label, ],
          aes(label = ticker), size = 2.8, max.overlaps = 20,
          segment.colour = "grey60", segment.linewidth = 0.3
        )
      else
        geom_text(
          data = df[df$do_label, ],
          aes(label = ticker), size = 2.5, vjust = -0.7
        )
    } +

    scale_x_continuous(labels = number_format(accuracy = 0.01)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title    = paste0("TAA Momentum Screen — ", horizon_label, " Relative Return vs Full-Period IR"),
      subtitle = paste0(
        "x-axis: full-period annualised IR vs SPY  |  ",
        "y-axis: trailing relative return vs SPY  |  ",
        "Dashed lines: IR=", IR_SAA_THRESHOLD, " · rel=",
        percent(REL_THRESHOLD, accuracy = 1)
      ),
      x = "Full-Period Information Ratio (vs SPY)",
      y = paste0(horizon_label, " Cumulative Relative Return")
    ) +
    .taa_theme

  p
}

################################################################################
# plot_taa_ranking()
################################################################################

#' Ranked bar chart — top N tickers by each horizon, coloured by quadrant.
#'
#' @param screen_tbl output of build_taa_screen()
#' @param n          number of top tickers to show per horizon (default 15)
plot_taa_ranking <- function(screen_tbl, n = 15L) {

  horizons <- c("rel_6m", "rel_12m", "rel_2y")
  labels   <- c("6 Month", "12 Month", "2 Year")

  QUAD_COLS <- c(
    "TAA Only"   = "#E65100",
    "SAA + TAA"  = "#2E7D32",
    "SAA (cold)" = "#1565C0",
    "Avoid"      = "#C62828"
  )

  plot_list <- purrr::map2(horizons, labels, function(h, lab) {
    df <- screen_tbl %>%
      filter(!is.na(.data[[h]])) %>%
      slice_max(order_by = .data[[h]], n = n) %>%
      mutate(
        ticker = fct_reorder(ticker, .data[[h]]),
        val    = .data[[h]]
      )

    ggplot(df, aes(x = ticker, y = val, fill = quadrant)) +
      geom_col(width = 0.7, alpha = 0.85) +
      geom_hline(yintercept = REL_THRESHOLD, linetype = "dashed",
                 colour = "grey40", linewidth = 0.4) +
      geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.3) +
      geom_text(aes(label = percent(val, accuracy = 1),
                    vjust = ifelse(val >= 0, -0.3, 1.2)),
                size = 2.5) +
      coord_flip() +
      scale_fill_manual(values = QUAD_COLS, name = NULL) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(title = lab, x = NULL, y = NULL) +
      .taa_theme +
      theme(legend.position = "none",
            axis.text.y = element_text(size = 8))
  })

  wrap_plots(plot_list, nrow = 1) +
    plot_annotation(
      title    = paste0("Top ", n, " Tickers by Trailing Relative Return vs SPY"),
      subtitle = paste0(
        "Orange = TAA Only (hot now, not durable)  |  ",
        "Green = SAA + TAA  |  ",
        "Blue = SAA (cold)  |  ",
        "Dashed = ", percent(REL_THRESHOLD, accuracy = 1), " TAA threshold"
      ),
      theme = theme(plot.title    = element_text(face = "bold", size = 13),
                    plot.subtitle = element_text(colour = "grey50", size = 8))
    )
}

################################################################################
# plot_taa_consistency()
################################################################################

#' Dot plot: rolling consistency score — how often in top quartile short-term.
#' Only shows tickers with at least one horizon in TAA Only or SAA+TAA quadrant.
#'
#' @param screen_tbl output of build_taa_screen()
#' @param n          top n tickers by consist_pct to show (default 25)
plot_taa_consistency <- function(screen_tbl, n = 25L) {

  QUAD_COLS <- c(
    "TAA Only"   = "#E65100",
    "SAA + TAA"  = "#2E7D32",
    "SAA (cold)" = "#1565C0",
    "Avoid"      = "#C62828"
  )

  df <- screen_tbl %>%
    filter(!is.na(consist_pct),
           quadrant %in% c("TAA Only", "SAA + TAA")) %>%
    slice_max(order_by = consist_pct, n = n) %>%
    mutate(ticker = fct_reorder(ticker, consist_pct))

  if (nrow(df) == 0) {
    message("No TAA / SAA+TAA tickers with consistency data.")
    return(invisible(NULL))
  }

  ggplot(df, aes(x = consist_pct, y = ticker, colour = quadrant)) +
    geom_vline(xintercept = 0.5, linetype = "dashed",
               colour = "grey50", linewidth = 0.4) +
    geom_segment(aes(x = 0, xend = consist_pct, yend = ticker),
                 colour = "grey80", linewidth = 0.4) +
    geom_point(aes(size = abs(rel_6m)), alpha = 0.85) +
    geom_text(aes(label = percent(consist_pct, accuracy = 1)),
              hjust = -0.3, size = 2.6) +
    scale_colour_manual(values = QUAD_COLS, name = NULL) +
    scale_size_continuous(range = c(2, 6),
                          name = "|6M rel return|",
                          labels = percent_format(accuracy = 1)) +
    scale_x_continuous(labels = percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.12))) +
    labs(
      title    = paste0("Momentum Consistency — % of Rolling ",
                        CONSIST_WINDOW, "-Day Windows with Positive Relative Return"),
      subtitle = paste0(
        "Only TAA Only + SAA+TAA tickers shown  |  ",
        "Dot size = |trailing 6M relative return|  |  ",
        "Dashed = 50% (random)"
      ),
      x = paste0("% of rolling ", CONSIST_WINDOW,
                 "-day windows with positive relative return vs SPY"),
      y = NULL
    ) +
    .taa_theme +
    theme(legend.position = "right")
}

################################################################################
# run_taa_screen()
################################################################################

#' Build screen + print all three plots. Returns screen_tbl invisibly.
#'
#' @param xts_ret  xts daily returns matrix
#' @param meta     etf_metadata data.frame
#' @param ...      passed to build_taa_screen()
run_taa_screen <- function(xts_ret, meta, ...) {

  cat("\nBuilding TAA momentum screen...\n")
  scr <- build_taa_screen(xts_ret, meta, ...)

  # Summary to console
  cat(sprintf("\n── TAA Screen Results (%d tickers) ─────────────────────────\n",
              nrow(scr)))
  cat(sprintf("  TAA Only  (hot now, not SAA-grade) : %d tickers\n",
              sum(scr$quadrant == "TAA Only",  na.rm = TRUE)))
  cat(sprintf("  SAA + TAA (structurally good + hot): %d tickers\n",
              sum(scr$quadrant == "SAA + TAA", na.rm = TRUE)))
  cat(sprintf("  SAA cold  (good long-term, not now): %d tickers\n",
              sum(scr$quadrant == "SAA (cold)", na.rm = TRUE)))
  cat(sprintf("  Avoid                              : %d tickers\n",
              sum(scr$quadrant == "Avoid",     na.rm = TRUE)))

  cat("\nTop 10 TAA Only candidates (by trailing 6M):\n")
  top_taa <- scr %>%
    filter(quadrant == "TAA Only") %>%
    select(ticker, name, asset_class, rel_6m, rel_12m, rel_2y,
           ir_full, consist_pct) %>%
    slice_max(rel_6m, n = 10)
  print(top_taa, n = 10)

  # Plots
  p_scatter_6m  <- plot_taa_scatter(scr, "rel_6m")
  p_scatter_12m <- plot_taa_scatter(scr, "rel_12m")
  p_scatter_2y  <- plot_taa_scatter(scr, "rel_2y")
  p_ranking     <- plot_taa_ranking(scr)
  p_consistency <- plot_taa_consistency(scr)

  print(p_scatter_6m)
  print(p_scatter_12m)
  print(p_scatter_2y)
  print(p_ranking)
  if (!is.null(p_consistency)) print(p_consistency)

  invisible(scr)
}

################################################################################
# render_taa_screen_report()
################################################################################

#' Render Rmd/taa_screen_report.Rmd to 03_reports/.
#'
#' @param output_dir  directory to write the HTML (default "03_reports")
#' @param open        logical — open in browser after render (default TRUE)
render_taa_screen_report <- function(output_dir = here::here("03_reports"),
                                      open = TRUE) {
  rmd <- here::here("Rmd/taa_screen_report.Rmd")
  out <- rmarkdown::render(
    input      = rmd,
    output_dir = output_dir,
    envir      = new.env(parent = globalenv()),
    quiet      = TRUE
  )
  message("Rendered → ", out)
  if (open && interactive()) utils::browseURL(out)
  invisible(out)
}

################################################################################
# Auto-run
################################################################################
if (!isTRUE(getOption("knitr.in.progress"))) {

  if (!exists("xts_ret"))
    xts_ret <- readRDS(here::here("02_data_processed/xts_ret_returns.rds"))

  if (!exists("etf_metadata")) {
    if (!exists("build_regime_table"))
      source(here::here("scripts_spy_dd_regime/spy_dd_regime.R"))
    source(here::here("scripts/00_init_universe.R"))
  }

  taa_screen <- run_taa_screen(xts_ret, etf_metadata)
}

message("taa_momentum_screen: loaded  |  call run_taa_screen(xts_ret, etf_metadata)")
