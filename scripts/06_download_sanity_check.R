# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts/06_download_sanity_check.R
# Purpose: Independent Sanity Check (Yahoo vs. Tiingo — both use adjusted close)
# ==============================================================================

library(tidyverse)
library(riingo)
library(here)
library(scales)
library(lubridate)

# API Key — set once in .Renviron: TIINGO_TOKEN=your_key
# Or override here for testing:
# Sys.setenv(TIINGO_TOKEN = "your_key_here")


get_independent_benchmarks <- function() {
  key <- Sys.getenv("RIINGO_TOKEN")
  if (key == "") stop("❌ Tiingo token not found. Add RIINGO_TOKEN=<key> to ~/.Renviron")

  message("📡 Querying Tiingo API for sovereign benchmarks (adjusted close)...")

  tickers    <- c("SPY", "QQQ", "GLD", "TLT")
  today      <- Sys.Date()
  ytd_start  <- as.Date(paste0(year(today), "-01-01"))

  fetch_ytd <- function(tkt) {
    tryCatch({
      px <- riingo_prices(tkt, start_date = ytd_start, end_date = today) %>%
        arrange(date)
      if (nrow(px) < 2) return(NA_real_)
      as.numeric(last(px$adjClose) / first(px$adjClose) - 1)
    }, error = function(e) NA_real_)
  }

  bench_df <- tibble(
    ticker       = tickers,
    expected_ytd = map_dbl(tickers, fetch_ytd)
  )

  return(bench_df)
}

run_global_sanity_check <- function() {
  message("🏁 Starting Independent Sanity Gate...")

  # 1. LOAD INTERNAL RESULTS
  if (!exists("unified_mom_df")) {
    path <- here(project_tree$products$ref_report)
    if (!file.exists(path)) stop("❌ Internal report data missing. Run Stages 01-04 first.")
    unified_mom_df <- read_rds(path)
  }

  # 2. FETCH EXTERNAL TRUTH
  benchmarks <- get_independent_benchmarks()

  # 3. MERGE & VALIDATE
  # Both sources use adjusted close → 1% tolerance covers rounding + 1-day lag only
  comparison <<- unified_mom_df %>%
    filter(ticker %in% benchmarks$ticker) %>%
    left_join(benchmarks, by = "ticker") %>%
    mutate(
      diff   = abs(ytd_nominal - expected_ytd),
      status = case_when(
        is.na(expected_ytd) ~ "API_ERROR",
        diff < 0.01         ~ "PASS",   # 1% — like-for-like adjusted close
        TRUE                ~ "FAIL"
      )
    )

  # 4. OUTPUT REPORT
  message("\n--- INDEPENDENT INTEGRITY REPORT (Yahoo vs Tiingo, adjusted close) ---")
  report_table <- comparison %>%
    select(ticker, status, `Internal (Yahoo)` = ytd_nominal, `External (Tiingo)` = expected_ytd) %>%
    mutate(across(where(is.numeric), ~percent(.x, 0.1)))

  print(as.data.frame(report_table))

  # 5. GATEKEEPING
  status_vector <- comparison$status

  if ("FAIL" %in% status_vector) {
    stop("🚨 PIPELINE HALTED: Data drift detected.")
  } else if (all(status_vector == "API_ERROR")) {
    message("⚠️ SKIPPED: Could not connect to Tiingo. Proceed with caution.")
  } else {
    message("\n✅ SUCCESS: Internal and External data are in sync.")
  }
}

# Execute
run_global_sanity_check()
