################################################################################
# FILE    : scripts_daa_saa_taa/dow30_classify_demo.R
# Purpose : Full DOW30 classify → filter → screen workflow.
#
# Self-contained — all dependencies sourced automatically if not already loaded.
# Just run: source(here("scripts_daa_saa_taa/dow30_classify_demo.R"))
################################################################################

library(here)
if (!exists("project_tree"))                          source(here("project_tree.R"))
if (!exists("etf_metadata"))                          source(here(project_tree$scripts$init))
if (!"sub_block" %in% colnames(etf_metadata) ||
    !"DOW30" %in% etf_metadata$sub_block)             source(here("scripts/00b_init_dow30.R"))
if (!exists("xts_ret"))                               source(here(project_tree$scripts$wrangle))
if (!exists("portfolio_maker"))                       source(here("scripts_daa_saa_taa/screen_ticker.R"))

if (!"BL6040" %in% colnames(xts_ret)) source(here("scripts_daa_saa_taa/portfolio_maker_demo.R"))

# ==============================================================================
# 1. TICKER LIST
# ==============================================================================

dow_tickers <- etf_metadata %>%
  dplyr::filter(sub_block == "DOW30") %>%
  dplyr::pull(ticker)

cat(sprintf("\n── DOW30 universe: %d tickers ─────────────────────────────────────────\n",
            length(dow_tickers)))
cat(paste(sort(dow_tickers), collapse = "  "), "\n")

# ==============================================================================
# 2. CLASSIFY VS BL6040
# ==============================================================================

cat("\n── Classifying vs BL6040 ──────────────────────────────────────────────\n")
cls <- classify_universe(tickers = dow_tickers, baseline = "BL6040", verbose = TRUE)

# ==============================================================================
# 3. FULL RESULTS TABLE
# ==============================================================================

cat("\n── Full classification table ───────────────────────────────────────────\n")
print(cls, n = Inf)

# ==============================================================================
# 4. FILTERED SHORTLISTS
# ==============================================================================

cat("\n── Best stabilizers  (dd_reduction > 1.5pp  &  ins_ratio > 5) ─────────\n")
stab <- cls %>%
  dplyr::filter(classification %in% c("Dual","Stabilizer"),
                dd_reduction > 1.5,
                ins_ratio    > 5) %>%
  dplyr::arrange(dplyr::desc(ins_ratio))
print(stab[, c("ticker","classification","hedge_type","dd_reduction","ins_ratio","fall_corr")])

cat("\n── Best enhancers  (full_ir > 0) ───────────────────────────────────────\n")
enh <- cls %>%
  dplyr::filter(classification %in% c("Dual","Enhancer"),
                full_ir > 0) %>%
  dplyr::arrange(trigger_type, dplyr::desc(full_ir))
print(enh[, c("ticker","classification","trigger_type","full_ir")])

cat("\n── Sweet spot: Dual  +  full_ir > 0  +  dd_reduction > 1.5pp ──────────\n")
sweet <- cls %>%
  dplyr::filter(classification == "Dual",
                full_ir      > 0,
                dd_reduction > 1.5) %>%
  dplyr::arrange(dplyr::desc(ins_ratio))
print(sweet[, c("ticker","trigger_type","hedge_type","full_ir","dd_reduction","ins_ratio")])

# ==============================================================================
# 5. DEEP DIVE — fingerprint for shortlisted tickers
# ==============================================================================
# Uncomment to run full screen on specific tickers.

if (!isTRUE(getOption("knitr.in.progress"))) {

  # screen_ticker("CVX",  baseline = "BL6040")
  # screen_ticker("CRM",  baseline = "BL6040")
  # screen_ticker("SHW",  baseline = "BL6040")

  # Or loop the sweet-spot shortlist:
  # for (tk in sweet$ticker) screen_ticker(tk, baseline = "BL6040", report = TRUE)

  message("\n✅ DOW30 classify demo complete.")
  message("   Uncomment screen_ticker() calls above to generate fingerprints / HTML reports.")
}
