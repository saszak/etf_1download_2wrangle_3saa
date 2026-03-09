# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts/06_download_sanity_check.R
# Purpose: Independent Sanity Check (Yahoo vs. MarketWatch Scraper)
# ==============================================================================

library(tidyverse)
library(rvest)
library(here)
library(scales)
#API Key
Sys.setenv(FINNHUB_KEY = "d6mkmapr01qi0ajmkdpgd6mkmapr01qi0ajmkdq0")



get_independent_benchmarks <- function() {
  key <- Sys.getenv("FINNHUB_KEY")
  if (key == "") stop("❌ Finnhub Key not found. Please run: Sys.setenv(FINNHUB_KEY = 'your_key')")
  
  message("📡 Querying Finnhub API for sovereign benchmarks...")
  
  # The 'Sovereign Four' for triangulation
  tickers <- c("SPY", "QQQ", "GLD", "TLT")
  
  fetch_ytd <- function(tkt) {
    url <- paste0("https://finnhub.io/api/v1/stock/metric?symbol=", tkt, "&metric=all&token=", key)
    
    tryCatch({
      res <- GET(url, timeout(5))
      if (status_code(res) != 200) return(NA_real_)
      
      data <- content(res, "parsed")
      # Finnhub metric for YTD price return
      val <- data$metric$yearToDatePriceReturnDaily
      return(as.numeric(val) / 100)
    }, error = function(e) return(NA_real_))
  }
  
  # Map with delay to stay within free tier limits
  bench_df <- tibble(
    ticker = tickers,
    expected_ytd = map_dbl(tickers, ~{Sys.sleep(0.5); fetch_ytd(.x)})
  )
  
  return(bench_df)
}

run_global_sanity_check <- function() {
  message("🏁 Starting Independent Sanity Gate...")
  
  # 1. LOAD INTERNAL RESULTS
  # We check if unified_mom_df exists in memory, otherwise load from RDS
  if(!exists("unified_mom_df")) {
    path <- here(project_tree$products$ref_report)
    if(!file.exists(path)) stop("❌ Internal report data missing. Run Stages 01-04 first.")
    unified_mom_df <- read_rds(path)
  }
  
  # 2. FETCH EXTERNAL TRUTH
  benchmarks <- get_independent_benchmarks()
  
  # 3. MERGE & VALIDATE (Hardened against NAs)
  comparison <<- unified_mom_df %>%
    filter(ticker %in% benchmarks$ticker) %>%
    left_join(benchmarks, by = "ticker") %>%
    mutate(
      diff = abs(ytd_nominal - expected_ytd),
      status = case_when(
        is.na(expected_ytd) ~ "API_ERROR",
        diff < 0.015        ~ "PASS",
        TRUE                ~ "FAIL"
      )
    )
  
  # 4. OUTPUT REPORT
  message("\n--- INDEPENDENT INTEGRITY REPORT ---")
  report_table <- comparison %>%
    select(ticker, status, `Internal (Yahoo)` = ytd_nominal, `External (Finnhub)` = expected_ytd) %>%
    mutate(across(where(is.numeric), ~percent(.x, 0.1)))
  
  print(as.data.frame(report_table))
  
  # 5. GATEKEEPING (Fixed the logical error)
  status_vector <- comparison$status
  
  if ("FAIL" %in% status_vector) {
    warning("🛑 DATA DISCREPANCY: Internal results vary significantly from Finnhub!")
  } else if (all(status_vector == "API_ERROR")) {
    message("⚠️ SKIPPED: Could not connect to Finnhub. Proceed with caution.")
  } else {
    message("\n✅ SUCCESS: Internal and External data are in sync.")
  }
}

# Execute
run_global_sanity_check()