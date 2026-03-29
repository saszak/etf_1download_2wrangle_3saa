################################################################################
# scripts_saa_execution/saa_drift_analysis.R
# Purpose : Audit Book 3 (total netted exposure) for cluster and category
#           crowding.  Ported from etf_saa_taa/03_drift_analysis.R.
#
# Input   : 02_data_processed/saa_policy_list.rds
#           02_data_processed/saa_selection_matrix.rds
# Output  : drift_report    (tibble, global env)  — one row per ticker
#           crowding_summary (tibble, global env) — one row per cluster
#           02_data_processed/saa_drift_report.rds
################################################################################

library(dplyr)
library(tidyr)
library(here)

# ── Load upstream outputs ────────────────────────────────────────────────────
if (!exists("policy_list")) {
  pl_path <- here::here("02_data_processed/saa_policy_list.rds")
  if (!file.exists(pl_path))
    stop("saa_drift: run saa_policy_weights.R first.")
  policy_list <- readRDS(pl_path)
}

if (!exists("selection_matrix")) {
  sm_path <- here::here("02_data_processed/saa_selection_matrix.rds")
  if (!file.exists(sm_path))
    stop("saa_drift: run saa_alpha_intelligence.R first.")
  selection_matrix <- readRDS(sm_path)
}

# ── 1. TICKER-LEVEL DRIFT REPORT ─────────────────────────────────────────────
# Join Book 3 net weights with cluster metadata.
# Tickers that appear only as SAA anchors (SPY, AGG) won't be in
# selection_matrix — fill their cluster as "Sovereign/BMK".

metadata_slim <- selection_matrix %>%
  select(ticker, cluster, category) %>%
  distinct()

drift_report <- policy_list$TOTAL %>%
  left_join(metadata_slim, by = "ticker") %>%
  mutate(
    cluster  = replace_na(cluster,  "Sovereign/BMK"),
    category = replace_na(category, "Mixed"),
    dollar_exposure = net_weight * 1e6   # reference $1M AUM
  ) %>%
  arrange(desc(net_weight))

# ── 2. CLUSTER CROWDING SUMMARY ───────────────────────────────────────────────
crowding_summary <- drift_report %>%
  group_by(cluster) %>%
  summarise(
    total_net_exposure = sum(net_weight,      na.rm = TRUE),
    total_dollar       = sum(dollar_exposure, na.rm = TRUE),
    ticker_count       = n(),
    tickers            = paste(ticker, collapse = ", "),
    .groups            = "drop"
  ) %>%
  arrange(desc(total_net_exposure))

# ── 3. CATEGORY SUMMARY (Equity / Fixed / Alt / Multi) ───────────────────────
category_summary <- drift_report %>%
  group_by(category) %>%
  summarise(
    total_net_exposure = sum(net_weight, na.rm = TRUE),
    .groups            = "drop"
  ) %>%
  arrange(desc(total_net_exposure))

# ── 4. SAVE ───────────────────────────────────────────────────────────────────
saveRDS(drift_report, here::here("02_data_processed/saa_drift_report.rds"))
message("saa_drift: SAVED → 02_data_processed/saa_drift_report.rds")

# ── Auto-run guard ────────────────────────────────────────────────────────────
if (!isTRUE(getOption("knitr.in.progress"))) {

  cat("\n── Ticker-level drift (Book 3 TOTAL) ───────────────────\n")
  print(drift_report %>%
    select(ticker, cluster, category, net_weight, dollar_exposure))

  cat("\n── Cluster crowding summary ─────────────────────────────\n")
  print(crowding_summary %>%
    select(cluster, total_net_exposure, total_dollar, ticker_count, tickers))

  cat("\n── Category split ───────────────────────────────────────\n")
  print(category_summary)

  cat("\n── Net weight check ─────────────────────────────────────\n")
  cat("  Total net weight:", round(sum(drift_report$net_weight), 4),
      " (should be 1.00)\n")
}
