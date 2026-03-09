# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts/01_etf_wrangle.R
# Purpose: Stage 01 - API Download, Winsorization, & Sigma Refinery
# Architecture: DBE with Cache-Gating & Force-Refresh Flag
# ==============================================================================

library(tidyverse)
library(tidyquant)
library(timetk)
library(PerformanceAnalytics)
library(xts)
library(here)

# --- 1. CONNECT TO INFRASTRUCTURE ---
if(!exists("project_tree")) source(here("project_tree.R"))
if(!exists("etf_metadata")) source(here(project_tree$scripts$init))

all_tickers <- etf_metadata$ticker

# Resolve Force-Refresh Flag from global OPTIONS (default to FALSE if not found)
force_refresh <- if(exists("OPTIONS") && !is.null(OPTIONS$force_fresh_sync)) OPTIONS$force_fresh_sync else FALSE

message("📥 Stage 01: Wrangling ", length(all_tickers), " Tickers...")

# --- 2. DOWNLOAD/SYNC LOGIC (Smart Cache with Force Flag) ---
sync_needed <- (function() {
  target <- here(project_tree$products$raw_p_d)
  
  if (force_refresh) {
    message("🔄 [FORCE REFRESH] Global flag active. Bypassing cache...")
    return(TRUE)
  }
  
  if (!file.exists(target)) {
    message("❓ Cache file missing. Initiating first-time sync...")
    return(TRUE)
  }
  
  file_age <- as.numeric(difftime(Sys.time(), file.info(target)$mtime, units = "secs"))
  if (file_age > 86400) {
    message("⏰ Cache age (", round(file_age/3600, 1), " hrs) exceeds limit. Syncing...")
    return(TRUE)
  }
  
  return(FALSE)
})()

if (sync_needed) {
  message("📡 Syncing FULL price history from API...")
  raw_data <- all_tickers %>%
    tq_get(get = "stock.prices", from = "2018-01-01", to = Sys.Date())
  
  if(nrow(raw_data) == 0) stop("❌ API Return Empty. Check connectivity or ticker list.")
  
  write_rds(raw_data, here(project_tree$products$raw_p_d))
  message("💾 Data cached to: ", project_tree$products$raw_p_d)
} else {
  message("📦 ✨ Local cache is valid. Loading raw data...")
  raw_data <- read_rds(here(project_tree$products$raw_p_d))
}

# --- 3. REFINERY: LOG RETURNS & CLEANING ---
message("🔄 Refining Returns (Using Adjusted Prices)...")

abs_ret_d <- raw_data %>%
  select(date, symbol, adjusted) %>% 
  pivot_wider(names_from = symbol, values_from = adjusted) %>%
  tk_xts(date_var = date, silent = TRUE) %>%
  Return.calculate(method = "log") %>%
  .[-1, ] %>%                                 
  zoo::na.locf(na.rm = FALSE) %>%         
  { .[is.na(.)] <- 0; . }                 

# --- 4. OUTLIER MANAGEMENT (WINSORIZATION) ---
message("✂️ Applying Functional Winsorization...")
xts_ret <- abs_ret_d 
for (tkt in colnames(abs_ret_d)) {
  clip_val <- etf_metadata %>% filter(ticker == tkt) %>% pull(winsor_pct)
  if (length(clip_val) == 0 || is.na(clip_val)) clip_val <- 0.02
  
  q_limits <- quantile(abs_ret_d[, tkt], probs = c(clip_val, 1 - clip_val), na.rm = TRUE)
  xts_ret[, tkt] <- pmax(pmin(abs_ret_d[, tkt], q_limits[2]), q_limits[1])
}

# --- 5. RELATIVE SPREAD CALCULATION (NEW) ---
# We calculate relative strength using the Winsorized returns
# Logic: r_relative = r_ticker - r_spy
message("📊 Generating Relative Return Spreads (xts_rel vs SPY)...")
if("SPY" %in% colnames(xts_ret)) {
  spy_ret <- xts_ret[, "SPY"]
  xts_rel <- sweep(xts_ret, 1, spy_ret, "-")
} else {
  warning("⚠️ SPY not found in refined returns. xts_rel cannot be calculated.")
  xts_rel <- NULL
}

# --- 6. ANALYTICAL SIGMA ---
message("🧮 Calculating Analytical Sigma for Outlier Detection...")
roll_m <- rollapply(xts_ret, width = 252, FUN = mean, fill = NA, align = "right")
roll_s <- rollapply(xts_ret, width = 252, FUN = sd,   fill = NA, align = "right")
xts_sigma <- (xts_ret - roll_m) / roll_s

# --- 7. REGIME LABELING ---
outlier_report <- xts_sigma[nrow(xts_sigma), ] %>%
  as.data.frame() %>%
  pivot_longer(everything(), names_to = "ticker", values_to = "current_sigma") %>%
  left_join(etf_metadata %>% select(ticker, sigma_limit), by = "ticker") %>%
  mutate(label = case_when(
    current_sigma > sigma_limit  ~ "[EXHAUSTION-HIGH]",
    current_sigma < -sigma_limit ~ "[EXHAUSTION-LOW]",
    TRUE                         ~ "[STABLE]"
  ))

# --- 8. PERSISTENCE ---
message("💾 Persisting refined products...")

write_rds(xts_ret, here(project_tree$products$refined_ret)) 
write_rds(xts_sigma, here(project_tree$products$sigma_mat))   
write_rds(outlier_report, here(project_tree$products$ref_report))  

# Persist the Relative Spread matrix
if (!is.null(xts_rel)) {
  # Note: Ensure project_tree$products$xts_rel is defined in project_tree.R
  # Or use a direct path if it's not yet in the tree
  write_rds(xts_rel, here("02_data_processed/xts_rel.rds"))
}

if(!is.null(project_tree$products$abs_ret_d)) {
  write_rds(abs_ret_d, here(project_tree$products$abs_ret_d))
}

# Export globals
raw_data <<- raw_data
xts_rel  <<- xts_rel 

message("✅ Stage 01 Complete: Full flexibility preserved.")