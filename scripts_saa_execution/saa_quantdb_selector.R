################################################################################
# scripts_saa_execution/saa_quantdb_selector.R
# Purpose : Dynamically select the "Sovereign 5" — 3 Alpha Enhancers +
#           2 Volatility Dampeners — to optimise the SPY core.
#           Ported from etf_saa_taa/07_quantdb_selector.R.
#
# Input   : 02_data_processed/saa_quantdb.rds
# Output  : sovereign_list   (tibble, global env)  — the selected roster
#           enhancers        (tibble, global env)
#           dampeners        (tibble, global env)
#           02_data_processed/saa_sovereign_list.rds
#
# Selection logic
#   Enhancers : ticker != SPY, is_confirmed == YES, alpha_6y > 0
#               scored = (alpha_6y × 0.7) + (mom_d × 0.3)
#               top n_enhancers
#   Dampeners : ticker != SPY, corr_to_bmk < corr_threshold
#               stability_score = –(|kurtosis| + |skew|)  [lower tail risk = better]
#               top n_dampeners
################################################################################

library(dplyr)
library(here)

# ── Parameters ────────────────────────────────────────────────────────────────
n_enhancers    <- 3      # number of alpha enhancers to select
n_dampeners    <- 2      # number of volatility dampeners to select
corr_threshold <- 0.50   # max corr to SPY for dampener eligibility

# ── Load quantdb ──────────────────────────────────────────────────────────────
if (!exists("etf_quantdb")) {
  qdb_path <- here::here("02_data_processed/saa_quantdb.rds")
  if (!file.exists(qdb_path))
    stop("saa_selector: run saa_quantdb_builder.R first.")
  etf_quantdb <- readRDS(qdb_path)
}

# ── 1. ALPHA ENHANCERS ────────────────────────────────────────────────────────
# Goal: maximum cumulative alpha with confirmed cross-timeframe momentum
message("saa_selector: selecting Alpha Enhancers ...")

enhancers <- etf_quantdb %>%
  filter(ticker != "SPY",
         is_confirmed == "YES",
         alpha_6y > 0) %>%
  mutate(score = (alpha_6y * 0.7) + (mom_d * 0.3)) %>%
  slice_max(score, n = n_enhancers, with_ties = FALSE) %>%
  mutate(role = "ENHANCER",
         goal = "Return Expansion") %>%
  select(ticker, cluster, category, role, goal,
         alpha_6y, corr_to_bmk, is_confirmed, mom_d, score)

# ── 2. VOLATILITY DAMPENERS ───────────────────────────────────────────────────
# Goal: low correlation to SPY + near-normal return distribution
message("saa_selector: selecting Volatility Dampeners ...")

dampeners <- etf_quantdb %>%
  filter(ticker != "SPY",
         corr_to_bmk < corr_threshold) %>%
  mutate(stability_score = -(abs(kurtosis_val) + abs(skew_val))) %>%
  slice_max(stability_score, n = n_dampeners, with_ties = FALSE) %>%
  mutate(role = "DAMPENER",
         goal = "Tail-Risk Protection") %>%
  select(ticker, cluster, category, role, goal,
         alpha_6y, corr_to_bmk, is_confirmed, mom_d, stability_score)

# ── 3. SOVEREIGN ROSTER ───────────────────────────────────────────────────────
sovereign_list <- bind_rows(
  enhancers %>% select(-score),
  dampeners %>% select(-stability_score)
)

# ── 4. SAVE ───────────────────────────────────────────────────────────────────
saveRDS(sovereign_list, here::here("02_data_processed/saa_sovereign_list.rds"))
message("saa_selector: SAVED → 02_data_processed/saa_sovereign_list.rds")

# ── Auto-run guard ─────────────────────────────────────────────────────────────
if (!isTRUE(getOption("knitr.in.progress"))) {

  cat("\n── Sovereign Roster ─────────────────────────────────────\n")
  print(sovereign_list %>%
    select(ticker, cluster, role, goal, alpha_6y, corr_to_bmk, is_confirmed))

  cat("\n── Enhancer Detail ──────────────────────────────────────\n")
  print(enhancers %>%
    select(ticker, cluster, alpha_6y, mom_d, score, corr_to_bmk))

  cat("\n── Dampener Detail ──────────────────────────────────────\n")
  print(dampeners %>%
    select(ticker, cluster, corr_to_bmk, alpha_6y, stability_score))

  # Spider DNA for the Sovereign roster (requires saa_visual_dashboard.R)
  if (exists("plot_etf_spider_vertical") && nrow(sovereign_list) > 0) {
    cat("\n── Sovereign DNA (vertical spider) ─────────────────────\n")
    plot_etf_spider_vertical(c(sovereign_list$ticker, "SPY"))
  }
}
