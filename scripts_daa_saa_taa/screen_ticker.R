################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : scripts_daa_saa_taa/screen_ticker.R
# Purpose : Ad-hoc single-ticker diagnostic — does a given ticker qualify as
#           a Stabilizer, an Enhancer, or neither, for a given SAA baseline?
#
# USAGE
#   source(here("scripts_daa_saa_taa/screen_ticker.R"))
#   screen_ticker("GLD")
#   screen_ticker("SMH", overlay_w = 0.15)
#   screen_ticker("GLD", report = TRUE)   # renders a one-page HTML to scripts_daa_saa_taa/
#
#   # Custom multi-asset SAA — build it once, add as a synthetic column, then use by name:
#   my_saa  <- portfolio_maker(c("AGG","URTH","SPY","QQQ","XLK","SMH"),
#                               c(0.35, 0.25, 0.25, 0.05, 0.05, 0.05),
#                               label = "MySAA")
#   xts_ret <- merge(xts_ret, my_saa,  join = "left")   # inject as synthetic column
#   screen_ticker("GLD", baseline = "MySAA")
#   screen_ticker("GLD", baseline = "MySAA", report = TRUE)
#
# RETURNS
#   Invisible list:
#     $enhancer   — tibble with enhancer verdict + metrics
#     $stabilizer — tibble with stabilizer verdict + metrics
#     $baseline   — tibble with baseline stats
#     $plot       — plot_xts_pair() fingerprint (6-panel)
#     $ticker     — ticker string
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
if (!exists(".build_baseline"))    source(here("scripts_daa_saa_taa/08_enhancer_screen.R"))
if (!exists("find_stabilizers"))   source(here("scripts_daa_saa_taa/05_dd_stabilizer.R"))
if (!exists("plot_xts_pair"))      source(here("utility/plot_xts_pair.R"))

# ==============================================================================
# portfolio_maker  ─────────────────────────────────────────────────────────────
# Build a synthetic portfolio return series from a set of tickers and weights.
# Returns a single-column xts that can be cbind()-ed into xts_ret or used
# directly as a baseline in screen_ticker().
#
# ARGS
#   tickers  — character vector of ticker names present in xts_ret
#   weights  — numeric vector, same length as tickers; normalised internally
#   xts_ret  — the return matrix (defaults to global xts_ret)
#   label    — column name for the returned xts; auto-derived if NULL
#              (e.g. "AGG35/URTH25/SPY25" from top-3 by weight)
#
# EXAMPLE
#   my_saa  <- portfolio_maker(c("SPY","IEF"), c(0.60, 0.40), label = "6040Classic")
#   my_saa  <- portfolio_maker(c("AGG","URTH","SPY","QQQ","XLK","SMH"),
#                               c(0.35, 0.25, 0.25, 0.05, 0.05, 0.05), label = "MySAA")
#   xts_ret <- merge(xts_ret, my_saa,  join = "left")   # inject as synthetic column
# ==============================================================================

portfolio_maker <- function(tickers, weights,
                            xts_ret = get("xts_ret", envir = parent.frame()),
                            label   = NULL) {
  if (length(tickers) != length(weights))
    stop("tickers and weights must have the same length.")
  if (!is.null(label) && grepl("^[0-9]", label))
    stop(sprintf("label '%s' starts with a digit — xts will silently rename it. Use e.g. 'BL%s'.", label, label))

  missing_tks <- setdiff(tickers, colnames(xts_ret))
  if (length(missing_tks) > 0)
    stop(sprintf("Not found in xts_ret: %s", paste(missing_tks, collapse = ", ")))

  weights <- weights / sum(weights)   # normalise to 1

  common <- na.omit(xts_ret[, tickers])

  ret <- xts(
    rowSums(sweep(coredata(common), 2, weights, `*`)),
    order.by = index(common)
  )

  if (is.null(label)) {
    ord  <- order(weights, decreasing = TRUE)
    top3 <- head(ord, 3)
    label <- paste(sprintf("%s%.0f", tickers[top3], weights[top3] * 100), collapse = "/")
  }
  colnames(ret) <- label
  ret
}

# ==============================================================================
# screen_ticker  ───────────────────────────────────────────────────────────────
# ==============================================================================

screen_ticker <- function(
    ticker,
    xts_ret        = get("xts_ret",  envir = parent.frame()),
    rt             = get("rt",       envir = parent.frame()),
    baseline       = NULL,          # ticker name in xts_ret — use portfolio_maker() first
                                    # if NULL, falls back to w_eq / w_fi (SPY+IEF)
    w_eq           = 0.60,          # used only when baseline = NULL
    w_fi           = 0.40,          # used only when baseline = NULL
    baseline_label = "6040Classic", # used only when baseline = NULL
    overlay_w      = 0.10,
    ir_floor       = 0.20,
    fall_tolerance = -0.02,
    roll_win       = 63L,
    ma_win         = 200L,
    full_corr_thr  = -0.10,
    part_corr_thr  =  0.30,
    trend_signals  = NULL,
    report         = FALSE,   # TRUE → render one-page HTML to scripts_daa_saa_taa/
    print_plot     = TRUE     # print fingerprint to Plots pane
) {
  if (!(ticker %in% colnames(xts_ret)))
    stop(sprintf("'%s' not found in xts_ret.", ticker))

  # ── Resolve baseline ────────────────────────────────────────────────────────
  if (!is.null(baseline)) {
    if (!(baseline %in% colnames(xts_ret)))
      stop(sprintf("baseline '%s' not found in xts_ret. Run portfolio_maker() first.", baseline))
    baseline_xts   <- xts_ret[, baseline]
    baseline_label <- baseline
  } else {
    baseline_xts <- .build_baseline(xts_ret, w_eq, w_fi, baseline_label)
  }

  regime_daily <- .regime_daily(rt)
  cur_regime   <- .current_regime(rt, xts_ret)

  # ── Helpers ────────────────────────────────────────────────────────────────
  .ann_r <- function(r) { r <- as.numeric(r[!is.na(r)]); prod(1+r)^(252/length(r))-1 }
  .vol_r <- function(r) sd(as.numeric(r), na.rm=TRUE) * sqrt(252)
  .dd_r  <- function(r) { w <- cumprod(1+as.numeric(r[!is.na(r)])); min((w-cummax(w))/cummax(w)) }
  .ir_r  <- function(r) { v <- .vol_r(r); if(is.na(v)||v==0) NA_real_ else .ann_r(r)/v }

  # ── Baseline stats ─────────────────────────────────────────────────────────
  bl_stats <- tibble(
    series  = baseline_label,
    ann_ret = .ann_r(baseline_xts),
    max_dd  = .dd_r(baseline_xts),
    ann_vol = .vol_r(baseline_xts),
    ir      = .ir_r(baseline_xts)
  )

  # ── Align ticker to baseline ───────────────────────────────────────────────
  common <- merge(baseline_xts, xts_ret[, ticker], join = "inner")
  if (nrow(common) < 252)
    stop(sprintf("'%s' has fewer than 252 common days with baseline — too short to screen.", ticker))

  bl_r  <- common[, 1]
  tk_r  <- common[, 2]
  dates <- as.Date(index(common))
  n     <- nrow(common)

  # Regime membership
  lkup      <- tibble(date = dates) %>% left_join(regime_daily, by = "date")
  fall_idx  <- which(lkup$regime == "Fall")
  recov_idx <- which(lkup$regime == "Recovery")
  cons_idx  <- which(lkup$regime == "Consolidation")

  bl_num <- as.numeric(bl_r)
  tk_num <- as.numeric(tk_r)
  r_spr  <- tk_num - bl_num   # spread vs baseline

  # ── ENHANCER METRICS ──────────────────────────────────────────────────────
  full_ir          <- .ir_r(r_spr)
  full_excess_ann  <- mean(r_spr, na.rm=TRUE) * 252
  fall_excess_ann  <- if (length(fall_idx)  > 20) mean(r_spr[fall_idx],  na.rm=TRUE)*252 else NA_real_
  recov_excess_ann <- if (length(recov_idx) > 20) mean(r_spr[recov_idx], na.rm=TRUE)*252 else NA_real_
  cons_excess_ann  <- if (length(cons_idx)  > 20) mean(r_spr[cons_idx],  na.rm=TRUE)*252 else NA_real_

  # Stage 1 enhancer pass?
  enh_pass_s1 <- !is.na(full_ir) && (
    full_ir > ir_floor ||
    (!is.na(fall_excess_ann)  && fall_excess_ann  > 0) ||
    (!is.na(recov_excess_ann) && recov_excess_ann > 0)
  )

  trigger_type <- case_when(
    is.na(fall_excess_ann)               ~ "A",
    fall_excess_ann >= fall_tolerance    ~ "A",
    TRUE                                 ~ "B"
  )

  # Stage 2 enhancer — context signals
  recent_spr  <- tail(r_spr, roll_win)
  rolling_ir  <- .ir_r(recent_spr)

  cur_idx <- switch(as.character(cur_regime),
    "Fall"           = fall_idx,
    "Recovery"       = recov_idx,
    "Consolidation"  = cons_idx,
    integer(0)
  )
  regime_excess <- if (length(cur_idx) > 10) mean(r_spr[cur_idx], na.rm=TRUE)*252 else NA_real_

  trend_on <- if (!is.null(trend_signals) && ticker %in% trend_signals$symbol) {
    trend_signals %>% filter(symbol == ticker) %>% slice_max(date, n=1) %>% pull(signal) %>% as.numeric()
  } else NA_real_

  spr_20       <- mean(tail(r_spr, 20L), na.rm=TRUE)
  spr_63       <- mean(tail(r_spr, roll_win), na.rm=TRUE)
  spread_mom   <- spr_20 - spr_63

  # Trigger type refinement B→C
  trigger_final <- if (trigger_type == "B" && !is.na(trend_on)) "C" else trigger_type
  trigger_label <- switch(trigger_final,
    "A" = "A — Unconditional",
    "B" = "B — Regime-conditional (non-Fall)",
    "C" = "C — 200DMA signal"
  )

  enhancer_result <- tibble(
    ticker          = ticker,
    role            = "Enhancer",
    verdict         = if (enh_pass_s1) "✅ PASS" else "❌ FAIL",
    pass            = enh_pass_s1,
    trigger_type    = trigger_final,
    trigger_label   = trigger_label,
    full_ir         = full_ir,
    full_excess_ann = full_excess_ann,
    fall_excess_ann = fall_excess_ann,
    recov_excess_ann= recov_excess_ann,
    cons_excess_ann = cons_excess_ann,
    rolling_ir_63d  = rolling_ir,
    regime_excess   = regime_excess,
    trend_on        = trend_on,
    spread_momentum = spread_mom,
    cur_regime      = cur_regime,
    n_days          = n
  )

  # ── STABILIZER METRICS ────────────────────────────────────────────────────
  blend_r       <- (1 - overlay_w) * bl_r + overlay_w * tk_r
  dd_reduction  <- .dd_r(bl_r)  - .dd_r(blend_r)
  return_drag   <- .ann_r(bl_r) - .ann_r(blend_r)
  ins_ratio     <- dd_reduction / max(abs(return_drag), 0.001)

  fall_dd_red <- if (length(fall_idx) > 20) {
    bl_fall  <- bl_r[fall_idx]; tk_fall <- tk_r[fall_idx]
    bld_fall <- (1 - overlay_w) * as.numeric(bl_fall) + overlay_w * as.numeric(tk_fall)
    .dd_r(bl_fall) - min((cumprod(1+bld_fall) - cummax(cumprod(1+bld_fall)))/cummax(cumprod(1+bld_fall)))
  } else NA_real_

  fall_corr <- if (length(fall_idx) > 20)
    cor(bl_num[fall_idx], tk_num[fall_idx], use="complete.obs") else NA_real_
  full_corr <- cor(bl_num, tk_num, use="complete.obs")

  hedge_type <- case_when(
    is.na(fall_corr)              ~ "PARTIAL",
    fall_corr <= full_corr_thr   ~ "FULL",
    fall_corr <= part_corr_thr   ~ "PARTIAL",
    TRUE                          ~ "CONDITIONAL"
  )
  hedge_label <- switch(hedge_type,
    "FULL"        = "FULL — negative Fall correlation",
    "PARTIAL"     = "PARTIAL — uncorrelated in Fall",
    "CONDITIONAL" = "CONDITIONAL — positive Fall corr, portfolio effect only"
  )

  stab_pass_s1 <- dd_reduction > 0

  # Stage 2 stabilizer signals
  roll_corr_63d    <- cor(tail(bl_num, roll_win), tail(tk_num, roll_win), use="complete.obs")
  own_momentum_63d <- mean(tail(tk_num, roll_win), na.rm=TRUE) * 252

  stabilizer_result <- tibble(
    ticker           = ticker,
    role             = "Stabilizer",
    verdict          = if (stab_pass_s1) "✅ PASS" else "❌ FAIL",
    pass             = stab_pass_s1,
    hedge_type       = hedge_type,
    hedge_label      = hedge_label,
    dd_reduction     = dd_reduction,
    fall_dd_red      = fall_dd_red,
    return_drag      = return_drag,
    insurance_ratio  = ins_ratio,
    fall_corr        = fall_corr,
    full_corr        = full_corr,
    roll_corr_63d    = roll_corr_63d,
    own_maxdd        = .dd_r(tk_r),
    own_ret          = .ann_r(tk_r),
    own_momentum_63d = own_momentum_63d,
    trend_on         = trend_on,
    cur_regime       = cur_regime,
    n_days           = n
  )

  # ── CONSOLE REPORT ─────────────────────────────────────────────────────────
  .sep <- strrep("─", 62)
  tk_name <- if (exists("etf_metadata"))
    etf_metadata %>% filter(ticker == !!ticker) %>% pull(name) %>% first() else ""
  tk_name <- if (length(tk_name) == 0 || is.na(tk_name)) "" else sprintf(" (%s)", tk_name)

  message(sprintf("\n%s", .sep))
  message(sprintf("  TICKER SCREEN: %s%s  |  Baseline: %s  |  Regime: %s",
                  ticker, tk_name, baseline_label, cur_regime))
  message(sprintf("  Overlay weight: %.0f%%  |  History: %d days",
                  overlay_w * 100, n))
  message(.sep)

  message(sprintf("\n  BASELINE (%s)", baseline_label))
  message(sprintf("    Ann Ret: %+.1f%%   MaxDD: %.1f%%   Vol: %.1f%%   IR: %.2f",
                  bl_stats$ann_ret*100, bl_stats$max_dd*100,
                  bl_stats$ann_vol*100, bl_stats$ir))

  message(sprintf("\n  %s  ──  ENHANCER ASSESSMENT", enhancer_result$verdict))
  message(sprintf("    Trigger type : %s", trigger_label))
  message(sprintf("    Full IR      : %.2f  (floor: %.2f)  %s",
                  full_ir, ir_floor,
                  if (!is.na(full_ir) && full_ir > ir_floor) "✓" else "✗"))
  message(sprintf("    Excess Ann   : Full %+.1f%%  |  Fall %s  |  Recov %s  |  Cons %s",
                  full_excess_ann * 100,
                  if (is.na(fall_excess_ann))  "n/a" else sprintf("%+.1f%%", fall_excess_ann*100),
                  if (is.na(recov_excess_ann)) "n/a" else sprintf("%+.1f%%", recov_excess_ann*100),
                  if (is.na(cons_excess_ann))  "n/a" else sprintf("%+.1f%%", cons_excess_ann*100)))
  message(sprintf("    Roll IR 63d  : %.2f   Regime α: %s   Momentum: %+.4f",
                  rolling_ir,
                  if (is.na(regime_excess)) "n/a" else sprintf("%+.1f%%", regime_excess*100),
                  spread_mom))

  message(sprintf("\n  %s  ──  STABILIZER ASSESSMENT", stabilizer_result$verdict))
  message(sprintf("    Hedge type   : %s", hedge_label))
  message(sprintf("    DD reduction : %+.1f pp  (at %.0f%% overlay)",
                  dd_reduction * 100, overlay_w * 100))
  message(sprintf("    Fall DD red  : %s",
                  if (is.na(fall_dd_red)) "n/a" else sprintf("%+.1f pp", fall_dd_red * 100)))
  message(sprintf("    Return drag  : %+.1f pp/yr   Ins ratio: %.2f",
                  return_drag * 100, ins_ratio))
  message(sprintf("    Fall ρ       : %s   Full ρ: %+.2f   Roll ρ 63d: %+.2f",
                  if (is.na(fall_corr)) "n/a" else sprintf("%+.2f", fall_corr),
                  full_corr, roll_corr_63d))
  message(sprintf("%s\n", .sep))

  # ── FINGERPRINT PLOT ───────────────────────────────────────────────────────
  overall_verdict <- case_when(
    enhancer_result$pass & stabilizer_result$pass ~
      sprintf("DUAL ROLE: %s Enhancer + %s Stabilizer", trigger_final, hedge_type),
    enhancer_result$pass  ~ sprintf("ENHANCER (%s)",  trigger_label),
    stabilizer_result$pass ~ sprintf("STABILIZER (%s)", hedge_label),
    TRUE ~ "NEITHER — does not qualify as enhancer or stabilizer"
  )

  pfx <- sprintf("[%s | %s regime | %.0f%% overlay]",
                 overall_verdict, cur_regime, overlay_w * 100)

  fp <- plot_xts_pair(
    xts1         = xts_ret[, ticker],
    xts2         = baseline_xts,
    label1       = ticker,
    label2       = baseline_label,
    name1        = if (tk_name != "") trimws(gsub("\\(|\\)", "", tk_name)) else NULL,
    regime_tbl   = rt,
    title_prefix = pfx,
    role         = if (enhancer_result$pass) "Enhancer" else if (stabilizer_result$pass) "Stabilizer" else NULL,
    extended     = FALSE,
    print_plot   = print_plot
  )

  # ── OPTIONAL HTML REPORT ───────────────────────────────────────────────────
  if (isTRUE(report)) {
    rmd_path <- here("scripts_daa_saa_taa/screen_ticker_report.Rmd")
    if (!file.exists(rmd_path))
      message("⚠ screen_ticker_report.Rmd not found — skipping HTML render.")
    else {
      out_file <- here(sprintf("scripts_daa_saa_taa/%s_screen.html", ticker))
      rmarkdown::render(
        rmd_path,
        output_file = out_file,
        params      = list(
          ticker         = ticker,
          overlay_w      = overlay_w,
          baseline       = if (!is.null(baseline)) baseline else "",
          w_eq           = w_eq,
          w_fi           = w_fi,
          baseline_label = baseline_label
        ),
        envir = new.env(),
        quiet = TRUE
      )
      message(sprintf("💾 Report saved: scripts_daa_saa_taa/%s_screen.html", ticker))
    }
  }

  invisible(list(
    enhancer   = enhancer_result,
    stabilizer = stabilizer_result,
    baseline   = bl_stats,
    plot       = fp,
    ticker     = ticker
  ))
}

# ==============================================================================
# classify_ticker  ─────────────────────────────────────────────────────────────
# Fast single-ticker classification — no plot, no console output.
# Returns one of: "Dual" | "Enhancer" | "Stabilizer" | "Neither"
#
# EXAMPLE
#   classify_ticker("XLF", baseline = "6040Classic")
#   classify_ticker("GLD", baseline = "MySAA", overlay_w = 0.10)
# ==============================================================================

classify_ticker <- function(ticker, ...) {
  res <- suppressMessages(
    screen_ticker(ticker, ..., print_plot = FALSE, report = FALSE)
  )
  dplyr::case_when(
    res$enhancer$pass & res$stabilizer$pass ~ "Dual",
    res$enhancer$pass                        ~ "Enhancer",
    res$stabilizer$pass                      ~ "Stabilizer",
    TRUE                                     ~ "Neither"
  )
}

# ==============================================================================
# classify_universe  ───────────────────────────────────────────────────────────
# Batch-classify a set of tickers. Returns a tibble sorted by classification
# with key metrics — use this to decide which tickers deserve a full screen.
#
# ARGS
#   tickers  — character vector; defaults to all tickers in xts_ret except
#              SPY, IEF, and the baseline itself
#   baseline — ticker name in xts_ret (from portfolio_maker); if NULL uses
#              w_eq / w_fi (default 60/40 SPY+IEF)
#   ...      — passed through to screen_ticker() (overlay_w, w_eq, w_fi, etc.)
#
# EXAMPLE
#   classify_universe(baseline = "6040Classic")
#   classify_universe(baseline = "MySAA", overlay_w = 0.10)
#   classify_universe(tickers = c("GLD","TLT","SLV","PDBC"), baseline = "MySAA")
# ==============================================================================

classify_universe <- function(tickers  = NULL,
                               xts_ret  = get("xts_ret", envir = parent.frame()),
                               rt       = get("rt",      envir = parent.frame()),
                               baseline = NULL,
                               w_eq     = 0.60,
                               w_fi     = 0.40,
                               overlay_w = 0.10,
                               verbose  = FALSE,
                               ...) {
  # ── Resolve baseline label ─────────────────────────────────────────────────
  if (!is.null(baseline)) {
    if (!(baseline %in% colnames(xts_ret)))
      stop(sprintf(paste0("'%s' not found in xts_ret.\n",
                          "  Build it first:  xts_ret <- merge(xts_ret,\n",
                          "    portfolio_maker(tickers, weights, xts_ret, label='%s'),\n",
                          "    join='left')"), baseline, baseline))
    bl_label <- baseline
  } else {
    bl_label <- "6040Classic"   # built on-the-fly inside screen_ticker via w_eq/w_fi
  }

  if (is.null(tickers)) {
    exclude <- c("SPY", "IEF", bl_label)
    tickers <- setdiff(colnames(xts_ret), exclude)
  }

  if (verbose) message(sprintf("▶ Classifying %d tickers vs %s ...", length(tickers), bl_label))

  results <- purrr::map_dfr(tickers, function(tk) {
    tryCatch({
      invisible(capture.output(
        suppressMessages(suppressWarnings(
          res <- screen_ticker(tk,
                               xts_ret        = xts_ret,
                               rt             = rt,
                               baseline       = baseline,
                               w_eq           = w_eq,
                               w_fi           = w_fi,
                               baseline_label = bl_label,
                               overlay_w      = overlay_w,
                               print_plot     = FALSE,
                               report         = FALSE,
                               ...)
        ))
      ))
      tibble(
        ticker         = tk,
        classification = dplyr::case_when(
          res$enhancer$pass & res$stabilizer$pass ~ "Dual",
          res$enhancer$pass                        ~ "Enhancer",
          res$stabilizer$pass                      ~ "Stabilizer",
          TRUE                                     ~ "Neither"
        ),
        trigger_type   = res$enhancer$trigger_type,
        hedge_type     = res$stabilizer$hedge_type,
        full_ir        = round(res$enhancer$full_ir,        2),
        dd_reduction   = round(res$stabilizer$dd_reduction * 100, 1),
        ins_ratio      = round(res$stabilizer$insurance_ratio, 2),
        fall_corr      = round(res$stabilizer$fall_corr,    2),
        n_days         = res$enhancer$n_days
      )
    }, error = function(e) {
      message(sprintf("  ⚠ skipped %s: %s", tk, conditionMessage(e)))
      tibble(ticker = tk, classification = NA_character_,
             trigger_type = NA, hedge_type = NA,
             full_ir = NA, dd_reduction = NA, ins_ratio = NA,
             fall_corr = NA, n_days = NA_integer_)
    })
  })

  out <- results %>%
    dplyr::mutate(classification = factor(classification,
                    levels = c("Dual","Enhancer","Stabilizer","Neither",NA))) %>%
    dplyr::arrange(classification, dplyr::desc(ins_ratio)) %>%
    dplyr::mutate(classification = as.character(classification))

  if (verbose) message(sprintf("✅ Done.  Dual: %d  |  Enhancer: %d  |  Stabilizer: %d  |  Neither: %d",
    sum(out$classification == "Dual",       na.rm=TRUE),
    sum(out$classification == "Enhancer",   na.rm=TRUE),
    sum(out$classification == "Stabilizer", na.rm=TRUE),
    sum(out$classification == "Neither",    na.rm=TRUE)))

  out
}

# ==============================================================================
# RUN WHEN SOURCED DIRECTLY
# ==============================================================================

if (!isTRUE(getOption("knitr.in.progress"))) {
  message("✅ Loaded: portfolio_maker() | classify_ticker() | classify_universe() | screen_ticker()")
  message("   Step 1: cls <- classify_universe(baseline = \"BL6040\")")
  message("   Step 2: screen_ticker(\"GLD\", baseline = \"BL6040\")")
}
