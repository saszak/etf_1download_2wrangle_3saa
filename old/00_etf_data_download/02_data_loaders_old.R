# ==============================================================================
# MODULE 00: DATA LOADERS (ULTRA-STABLE VERSION)
# ==============================================================================
library(tidyverse)
library(tidyquant)
library(timetk)
library(PerformanceAnalytics)
library(xts)

# --- 2.1 API WRAPPERS (Standard TQ) ---
db_get_price <- function(tickers, fD, tD, interval="day"){
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
  freqs <- c("day", "week", "month", "quarter")
  suffixes <- c("d", "w", "m", "q")
  
  raw_dir <- file.path(base_path, "data_raw")
  if (!dir.exists(raw_dir)) dir.create(raw_dir, recursive = TRUE)
  
  for(i in seq_along(freqs)) {
    message("📥 Warehouse Fetching: ", freqs[i])
    
    # This returns a LONG Tibble
    raw_tibble <- db_get_price(ticker_vector, fD = start_date, tD = yesterday, interval = freqs[i])
    
    # SAVE AS TIBBLE: We keep it as a Tibble in the Warehouse for maximum safety
    # We will convert to XTS inside the Refinery (2.3)
    write_rds(raw_tibble, file.path(raw_dir, paste0("raw_p_", suffixes[i], ".rds")))
  }
}

# --- 2.3 RETURN REFINERY (THE SMART CONVERTER) ---
refine_prices_to_returns <- function(suffix = "d") {
  file_in <- file.path(base_path, "data_raw", paste0("raw_p_", suffix, ".rds"))
  if (!file.exists(file_in)) stop("❌ Missing raw file: ", file_in)
  
  data_in <- read_rds(file_in)
  
  # --- SMART CONVERSION LOGIC ---
  # Check if it's a Tibble (Long Format) and convert to Wide XTS
  if (is.data.frame(data_in)) {
    message("🔄 Converting Warehouse Tibble to Wide XTS...")
    xts_prices <- data_in %>%
      pivot_wider(names_from = symbol, values_from = adjusted) %>%
      tk_xts(date_var = date, silent = TRUE)
  } else {
    xts_prices <- data_in
  }
  
  # --- THE NUMERIC SHIELD ---
  # Ensure all columns are numeric (excluding the index)
  storage.mode(xts_prices) <- "numeric"
  
  # Calculate Returns
  message("🧮 Calculating Log Returns: ", suffix)
  xts_returns <- PerformanceAnalytics::Return.calculate(xts_prices, method = "log")
  
  # Clean the first row and any gaps
  clean_xts <- xts_returns[-1, ] 
  clean_xts <- zoo::na.locf(clean_xts, na.rm = FALSE)
  clean_xts[is.na(clean_xts)] <- 0
  
  # Save Absolute Returns back to Warehouse
  file_out <- file.path(base_path, "data_raw", paste0("abs_ret_", suffix, ".rds"))
  write_rds(clean_xts, file_out)
  
  message("💾 Warehouse Pillar: abs_ret_", suffix)
  return(clean_xts)
}

# --- 2.4 RELATIVE ALPHA ENGINE ---
calculate_relative_returns <- function(xts_abs, bmk = "SPY", suffix = "d") {
  # Clean names (strip .Adjusted)
  colnames(xts_abs) <- gsub("\\..*$", "", colnames(xts_abs))
  
  if(!(bmk %in% colnames(xts_abs))) stop("❌ Benchmark ", bmk, " missing!")
  
  # Alpha = Asset - BMK
  xts_rel <- sweep(xts_abs, 1, xts_abs[, bmk], "-")
  
  out_dir <- file.path(base_path, "01_etf_wrangle", "data_processed")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  write_rds(xts_rel, file.path(out_dir, paste0("rel_ret_", suffix, ".rds")))
  message("💾 Refinery Product: rel_ret_", suffix)
  return(xts_rel)
}

# --- 2.5 CACHE MONITOR ---
is_sync_required <- function() {
  target <- file.path(base_path, "data_raw", "raw_p_d.rds")
  if (!file.exists(target)) return(TRUE)
  last_mod <- file.info(target)$mtime
  return((as.numeric(Sys.time()) - as.numeric(last_mod)) > 86400)
}

message("✅ 02_data_loaders.R: Architecture fully stabilized.")