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
##################################################################################
if(!exists("project_tree")) source(here("project_tree.R"))
if(!exists("etf_metadata")) source(here(project_tree$scripts$init))

if(file.exists(here("utility/util_functions.R"))) {
  source(here("utility/util_functions.R"))
} else {
  stop("❌ Critical Error: utility/util_functions.R not found.")
}

all_tickers <- etf_metadata$ticker
force_refresh <- if(exists("OPTIONS") && !is.null(OPTIONS$force_fresh_sync)) OPTIONS$force_fresh_sync else FALSE

message("📥 Stage 01: Wrangling ", length(all_tickers), " Tickers...")

# --- 2. DOWNLOAD/SYNC LOGIC (Smart Cache with Force Flag) ---
##################################################################################
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
  
  # CLEANING STEP: Remove exact duplicates and handle overlapping dates from API
  raw_data <- raw_data %>%
    group_by(symbol, date) %>%
    slice_tail(n = 1) %>% 
    ungroup()
  
  write_rds(raw_data, here(project_tree$products$raw_p_d))
  message("💾 Data cached to: ", project_tree$products$raw_p_d)
} else {
  message("📦 ✨ Local cache is valid. Loading raw data...")
  raw_data <- read_rds(here(project_tree$products$raw_p_d))
}

# --- 3. REFINERY: LOG RETURNS & CLEANING ---
##################################################################################
message("🔄 Refining Returns (Using Adjusted Prices)...")

spy_trading_dates <- raw_data %>% filter(symbol == "SPY") %>% pull(date)

abs_ret_d <- raw_data %>%
  select(date, symbol, adjusted) %>%
  # Safety: values_fn = last ensures we don't create list-cols if duplicates exist
  pivot_wider(names_from = symbol, values_from = adjusted, values_fn = last) %>%
  arrange(date) %>%
  filter(date %in% spy_trading_dates) %>%           # keep only US trading dates BEFORE computing returns
  tk_xts(date_var = date, silent = TRUE) %>%
  { .[!duplicated(index(.)), ] } %>%               # drop any duplicate dates (e.g. IBIT weekend rows)
  Return.calculate(method = "log") %>%
  .[-1, ] %>%
  zoo::na.locf(na.rm = FALSE) %>%
  { .[is.na(.)] <- 0; . }                 

# --- 4. OUTLIER MANAGEMENT (WINSORIZATION) ---
##################################################################################
message("✂️ Applying Functional Winsorization...")
xts_ret       <- abs_ret_d          # clean returns — comparable to Bloomberg
xts_ret_winsor <- abs_ret_d         # winsorized copy — for sigma / regime calculations only
for (tkt in colnames(abs_ret_d)) {
  clip_val <- etf_metadata %>% filter(ticker == tkt) %>% pull(winsor_pct)
  if (length(clip_val) == 0 || is.na(clip_val)) clip_val <- 0.02

  q_limits <- quantile(abs_ret_d[, tkt], probs = c(clip_val, 1 - clip_val), na.rm = TRUE)
  xts_ret_winsor[, tkt] <- pmax(pmin(abs_ret_d[, tkt], q_limits[2]), q_limits[1])
}

# --- 5. RELATIVE SPREAD & TRANSFORMATIONS ---
##################################################################################
xts_rel <- generate_relative_returns(xts_ret, bmk = "SPY")

if (!is.null(xts_rel)) {
  message("📈 Calculating Cumulative Alpha and Wealth Index...")
  xts_rel_cum <- cumprod(1 + xts_rel) - 1
  xts_rel_wlth <- cumprod(1 + xts_rel)
}

# --- 6. ANALYTICAL SIGMA ---
##################################################################################
message("🧮 Calculating Analytical Sigma for Outlier Detection...")
# Sigma uses winsorized returns — prevents single crash days from dominating rolling vol
xts_ret_winsor <- xts_ret_winsor[!duplicated(index(xts_ret_winsor)), ]
roll_m <- suppressWarnings(rollapply(xts_ret_winsor, width = 252, FUN = mean, fill = NA, align = "right"))
roll_s <- suppressWarnings(rollapply(xts_ret_winsor, width = 252, FUN = sd,   fill = NA, align = "right"))
xts_sigma <- (xts_ret_winsor - roll_m) / roll_s

# --- 7. REGIME LABELING ---
##################################################################################
outlier_report <- suppressWarnings(xts_sigma[nrow(xts_sigma), ]) %>%
  as.data.frame() %>%
  pivot_longer(everything(), names_to = "ticker", values_to = "current_sigma") %>%
  left_join(etf_metadata %>% select(ticker, sigma_limit), by = "ticker") %>%
  mutate(label = case_when(
    current_sigma > sigma_limit  ~ "[EXHAUSTION-HIGH]",
    current_sigma < -sigma_limit ~ "[EXHAUSTION-LOW]",
    TRUE                         ~ "[STABLE]"
  ))

# --- 8. PERSISTENCE ---
##################################################################################
message("💾 Persisting refined products...")

write_rds(xts_ret,        here(project_tree$products$refined_ret))
write_rds(xts_ret_winsor, here("02_data_processed/xts_ret_winsor.rds"))
write_rds(xts_sigma,      here(project_tree$products$sigma_mat))
write_rds(outlier_report, here(project_tree$products$ref_report))

# ── Derived portfolio returns (DERIVED_UNIVERSE → xts columns) ────────────────
# Built here so global.R just does read_rds() — no runtime computation needed.
if (!exists("DERIVED_UNIVERSE")) source(here("scripts/00_derived_universe.R"))
xts_derived_ret <- xts_ret[, character(0)]   # empty xts, same index
for (.i in seq_len(nrow(DERIVED_UNIVERSE))) {
  .du  <- DERIVED_UNIVERSE[.i, ]
  .ret <- tryCatch(build_derived_returns(.du, xts_ret), error = function(e) {
    message("⚠️  Skipping ", .du$id, ": ", conditionMessage(e)); NULL
  })
  if (!is.null(.ret)) xts_derived_ret <- merge(xts_derived_ret, .ret, join = "left")
}
write_rds(xts_derived_ret, here(project_tree$products$derived_ret))
message("💾 Derived portfolio returns saved: ", paste(colnames(xts_derived_ret), collapse = " | "))
rm(.i, .du, .ret, xts_derived_ret)

if (!is.null(xts_rel)) {
  write_rds(xts_rel, here("02_data_processed/xts_rel.rds"))
  write_rds(xts_rel_cum, here("02_data_processed/xts_rel_cum.rds"))
  write_rds(xts_rel_wlth, here("02_data_processed/xts_rel_wealth.rds"))
  message("💾 Relative products saved.")
}

if(!is.null(project_tree$products$abs_ret_d)) {
  write_rds(abs_ret_d, here(project_tree$products$abs_ret_d))
}

# Global Exports
raw_data      <<- raw_data
xts_ret_winsor <<- xts_ret_winsor
xts_rel       <<- xts_rel
xts_rel_cum  <<- xts_rel_cum
xts_wlth <<- xts_rel_wlth
signal_table <<- xts_rel 

message("✅ Stage 01 Complete: Artifacts persisted.")

# --- 9. DATA AUDIT ---
##################################################################################
source(here("utility/data_audit.R"))
audit <- run_data_audit(xts_ret, etf_metadata)
if (!audit$passed) warning("⚠️  Data audit: one or more FAIL checks — review output above.")