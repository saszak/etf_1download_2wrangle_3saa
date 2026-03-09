# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./00_etf_data_download/02_data_loaders.R
# Purpose: API Wrangling, Data Warehouse management, and Return Refinery
# ==============================================================================

library(tidyverse)
library(tidyquant)
library(timetk)
library(PerformanceAnalytics)
library(xts)
library(fs)

# --- 1. CONNECT TO INFRASTRUCTURE ---
if(!exists("project_tree")) source("./project_tree.R")

# Only run Init if the metadata doesn't exist in the environment
if(!exists("etf_metadata")) {
  message("🧠 Brain not found. Initializing universe...")
  source(project_tree$scripts$init)
}

#########################
# --- 2.1 API WRAPPERS (Standard TQ) ---
db_get_price <- function(tickers, fD, tD, interval="day"){
  message("📡 Requesting ", length(tickers), " tickers at '", interval, "' frequency...")
  Ra <- tickers %>%
    tq_get(get = "stock.prices", from = fD, to = tD) %>%
    group_by(symbol) %>%
    tq_transmute(select = adjusted, mutate_fun = to.period, 
                 period = interval, col_rename = "adjusted") %>%
    ungroup()
  return(Ra)
}

# --- 2.2 SYNC LOGIC (The Warehouse Creator) ---
sync_etf_prices <- function(ticker_vector, start_date = "2018-01-01") {
  yesterday <- Sys.Date() - 1
  freqs     <- c("day", "week", "month", "quarter")
  suffixes  <- c("d", "w", "m", "q")
  
  # Reference the tree for the raw data directory
  raw_dir   <- project_tree$dirs$data_raw
  
  for(i in seq_along(freqs)) {
    message("📥 Warehouse Fetching: ", freqs[i])
    raw_tibble <- db_get_price(ticker_vector, fD = start_date, tD = yesterday, interval = freqs[i])
    write_rds(raw_tibble, file.path(raw_dir, paste0("raw_p_", suffixes[i], ".rds")))
  }
}

# --- 2.3 RETURN REFINERY (THE SMART CONVERTER) ---
refine_prices_to_returns <- function(suffix = "d") {
  raw_dir <- project_tree$dirs$data_raw
  file_in <- file.path(raw_dir, paste0("raw_p_", suffix, ".rds"))
  
  if (!file.exists(file_in)) stop("❌ Missing raw file: ", file_in)
  
  data_in <- read_rds(file_in)
  
  message("🔄 Converting Warehouse Tibble to Wide XTS...")
  xts_prices <- data_in %>%
    pivot_wider(names_from = symbol, values_from = adjusted) %>%
    tk_xts(date_var = date, silent = TRUE)
  
  storage.mode(xts_prices) <- "numeric"
  message("🧮 Calculating Log Returns: ", suffix)
  
  # Return.calculate handles the xts matrix math efficiently
  xts_returns <- PerformanceAnalytics::Return.calculate(xts_prices, method = "log")
  
  # Clean data: drop first row, carry forward last observation, fill remaining NA with 0
  clean_xts <- xts_returns[-1, ] 
  clean_xts <- zoo::na.locf(clean_xts, na.rm = FALSE)
  clean_xts[is.na(clean_xts)] <- 0
  
  # Clean column names
  colnames(clean_xts) <- gsub("\\..*$", "", colnames(clean_xts))
  
  # Use tree for product pathing for the primary daily return file
  if(suffix == "d") {
    file_out <- project_tree$products$abs_ret_d
  } else {
    file_out <- file.path(raw_dir, paste0("abs_ret_", suffix, ".rds"))
  }
  
  write_rds(clean_xts, file_out)
  message("💾 Warehouse Pillar: ", basename(file_out))
  return(clean_xts)
}

# --- 2.4 RELATIVE ALPHA ENGINE ---
calculate_relative_returns <- function(xts_abs, bmk = "SPY", suffix = "d") {
  if(!(bmk %in% colnames(xts_abs))) stop("❌ Benchmark ", bmk, " missing from data!")
  
  # Matrix subtraction: Each column minus the Benchmark column
  xts_rel <- sweep(xts_abs, 1, xts_abs[, bmk], "-")
  
  # Save to the processed/refined directory defined in the tree
  out_path <- file.path(project_tree$dirs$data_ref, paste0("rel_ret_", suffix, ".rds"))
  write_rds(xts_rel, out_path)
  
  message("💾 Refinery Product: ", basename(out_path))
  return(xts_rel)
}

# --- 2.5 EXECUTION LOGIC ---
if (is_sync_required <- function() {
  # Check existence of target product from tree
  target <- project_tree$products$raw_p_d
  if (!file.exists(target)) return(TRUE)
  
  last_mod <- file.info(target)$mtime
  return((as.numeric(Sys.time()) - as.numeric(last_mod)) > 86400)
}()) {
  sync_etf_prices(all_tickers)
}

# Execute processing
abs_ret_d <- refine_prices_to_returns("d")
rel_ret_d <- calculate_relative_returns(abs_ret_d, bmk = "SPY", suffix = "d")

message("✅ 02_data_loaders.R: Architecture fully stabilized for 61 tickers.")