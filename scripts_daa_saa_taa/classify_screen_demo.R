################################################################################
# FILE    : scripts_daa_saa_taa/classify_screen_demo.R
# Purpose : Demonstrate the two-step classify → screen workflow.
#
#   Step 1 — classify_universe() : fast batch classification, no plots
#   Step 2 — screen_ticker()     : full fingerprint + HTML report for
#                                  tickers of interest only
#
# PRE-REQUISITE : run portfolio_maker_demo.R first so BL6040 and MySAA
#                 exist as columns in xts_ret.
#
# RUN     : source this file, or step through interactively
################################################################################

library(here)
if (!exists("portfolio_maker")) source(here("scripts_daa_saa_taa/screen_ticker.R"))

stopifnot("BL6040" %in% colnames(xts_ret),
          "MySAA"  %in% colnames(xts_ret))

# ==============================================================================
# STEP 1 — CLASSIFY THE UNIVERSE
# ==============================================================================
# classify_universe() runs all tickers silently and returns a tibble.
# Use this to decide which tickers deserve deeper analysis.

cat("\n── Classifying vs BL6040 ──────────────────────────────────────────────\n")
cls_bl <- classify_universe(baseline = "BL6040", verbose = TRUE)

cat("\n── Classifying vs MySAA ───────────────────────────────────────────────\n")
cls_saa <- classify_universe(baseline = "MySAA", verbose = TRUE)

# ── Summary print ─────────────────────────────────────────────────────────────
.print_cls <- function(cls, label) {
  cat(sprintf("\n%s\n", strrep("─", 62)))
  cat(sprintf("  Classification vs %s\n", label))
  cat(sprintf("%s\n", strrep("─", 62)))

  for (grp in c("Dual", "Enhancer", "Stabilizer", "Neither")) {
    sub <- cls[cls$classification == grp & !is.na(cls$classification), ]
    if (nrow(sub) == 0) next
    cat(sprintf("\n  %s (%d)\n", grp, nrow(sub)))
    cat(sprintf("  %-8s  %-5s  %-11s  %7s  %10s  %8s\n",
                "Ticker", "Trig", "Hedge", "IR", "DD red pp", "Ins ratio"))
    for (i in seq_len(nrow(sub))) {
      r <- sub[i, ]
      cat(sprintf("  %-8s  %-5s  %-11s  %+6.2f  %+9.1f  %8.2f\n",
                  r$ticker,
                  ifelse(is.na(r$trigger_type), "—", r$trigger_type),
                  ifelse(is.na(r$hedge_type),   "—", r$hedge_type),
                  ifelse(is.na(r$full_ir),       NA,  r$full_ir),
                  ifelse(is.na(r$dd_reduction),  NA,  r$dd_reduction),
                  ifelse(is.na(r$ins_ratio),     NA,  r$ins_ratio)))
    }
  }
  cat(sprintf("%s\n\n", strrep("─", 62)))
}

.print_cls(cls_bl,  "BL6040")
.print_cls(cls_saa, "MySAA")

# ==============================================================================
# STEP 2 — FILTER, THEN SCREEN TICKERS OF INTEREST
# ==============================================================================

# Example filters — edit as needed
dual_bl   <- cls_bl  %>% dplyr::filter(classification == "Dual")
stab_saa  <- cls_saa %>% dplyr::filter(classification %in% c("Dual","Stabilizer")) %>%
               dplyr::arrange(dplyr::desc(ins_ratio))
enh_a     <- cls_bl  %>% dplyr::filter(classification %in% c("Dual","Enhancer"),
                                        trigger_type == "A") %>%
               dplyr::arrange(dplyr::desc(full_ir))

cat("Top Dual tickers vs BL6040:\n");  print(dual_bl[, c("ticker","trigger_type","hedge_type","full_ir","ins_ratio")])
cat("\nTop Stabilizers vs MySAA:\n");  print(stab_saa[1:10, c("ticker","hedge_type","dd_reduction","ins_ratio","fall_corr")])
cat("\nType-A Enhancers vs BL6040:\n"); print(enh_a[, c("ticker","full_ir","fall_corr")])

# ==============================================================================
# STEP 3 — DEEP DIVE: fingerprint + HTML report
# ==============================================================================
# Uncomment to run full screen on specific tickers.
# Each call prints the fingerprint and optionally saves an HTML report.

if (!isTRUE(getOption("knitr.in.progress"))) {

  # screen_ticker("GLD",  baseline = "BL6040")
  # screen_ticker("GLD",  baseline = "BL6040", report = TRUE)
  # screen_ticker("TAIL", baseline = "MySAA")
  # screen_ticker("ITB",  baseline = "BL6040", report = TRUE)

  # Or loop over a shortlist:
  # shortlist <- c("GLD", "TAIL", "SLV")
  # for (tk in shortlist) screen_ticker(tk, baseline = "BL6040", report = TRUE)

  message("✅ classify_screen_demo complete.")
  message("   Uncomment screen_ticker() calls above to generate fingerprints / HTML reports.")
}
