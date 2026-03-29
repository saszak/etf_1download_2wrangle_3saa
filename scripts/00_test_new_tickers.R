################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : scripts/00_test_new_tickers.R
# Purpose : Test-download all NEW tickers (not in original 60) via tq_get().
#           Run before promoting etf_choices.R to live universe.
#           Prints a PASS/FAIL report and saves failures to console.
################################################################################

library(tidyverse)
library(tidyquant)
library(here)

# ------------------------------------------------------------------------------
# 1. Original 60 tickers (reference — used to identify what is NEW)
# ------------------------------------------------------------------------------
original_60 <- c(
  "SPY","IVV","URTH","ACWX","IEFA","QQQ","EWJ","FEZ","DAX","EWQ","EWL",
  "SMMCHA.SW","XLK","CIBR","WCLD","IPO","XLV","VHT","IHI","SMH","QUAL",
  "MTUM","USMV","XLP","XLU","LQD","AGG","BND","SGOV","SHY","IEF","TLT",
  "AOK","XLF","PSP","XLI","XLY","VWO","ITB","XHB","IYT","ITA","IBB","XBI",
  "HYG","EMB","EMLC","EWY","IYR","VIXY","AOR","XLE","PDBC","GLD","SLV",
  "COPX","URA","TIP","PFIX","IGF"
)

# ------------------------------------------------------------------------------
# 2. Load candidate universe (96 tickers after duplicate resolution)
# ------------------------------------------------------------------------------
if (!exists("etf_candidates")) source(here("etf_choices.R"))

new_tickers <- setdiff(etf_candidates$ticker, original_60)
cat(sprintf("\n── Testing %d NEW tickers ────────────────────────────────────────────\n",
            length(new_tickers)))

# ------------------------------------------------------------------------------
# 3. Test-download each ticker: last 30 trading days
# ------------------------------------------------------------------------------
test_one <- function(ticker) {
  tryCatch({
    df <- tq_get(ticker,
                 get = "stock.prices",
                 from = Sys.Date() - 60,
                 to   = Sys.Date())
    n_rows <- nrow(df)
    if (n_rows == 0) return(tibble(ticker = ticker, status = "FAIL", n_rows = 0L, note = "empty response"))
    tibble(ticker = ticker, status = "PASS", n_rows = as.integer(n_rows), note = "")
  }, error = function(e) {
    tibble(ticker = ticker, status = "FAIL", n_rows = 0L, note = conditionMessage(e))
  })
}

cat("Downloading... (may take ~30 seconds)\n\n")
results <- map_dfr(new_tickers, test_one)

# ------------------------------------------------------------------------------
# 4. Report
# ------------------------------------------------------------------------------
pass <- filter(results, status == "PASS")
fail <- filter(results, status == "FAIL")

cat(sprintf("  PASS : %d / %d\n", nrow(pass), length(new_tickers)))
cat(sprintf("  FAIL : %d / %d\n", nrow(fail), length(new_tickers)))

if (nrow(fail) > 0) {
  cat("\n── FAILED TICKERS ───────────────────────────────────────────────────────\n")
  print(fail, n = Inf)
  cat("\n  Action: Remove failed tickers from etf_choices.R before promoting.\n")
} else {
  cat("\n  All new tickers available on Yahoo Finance. Safe to promote.\n")
}

cat("────────────────────────────────────────────────────────────────────────\n\n")

invisible(results)

################################################################################
# END OF FILE
################################################################################
