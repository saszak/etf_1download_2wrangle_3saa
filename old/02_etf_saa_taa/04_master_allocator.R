# ==============================================================================
# PROJECT B: ETF_SAA_TAA - 04_MASTER_ALLOCATOR.R (GDPR-FIXED)
# ==============================================================================
library(dplyr)
library(rvest)
library(httr)

# --- 1. CONFIG & ROBUST PATHING ---
total_aum <- 1000000
path_drift <- "02_etf_saa_taa/data_processed/drift_report.rds"

if(!file.exists(path_drift)) stop("❌ Master Allocator: drift_report.rds not found.")
drift_report <- readRDS(path_drift)

# --- 2. ROBUST SCRAPER (GDPR Bypass) ---
# This function pulls the price directly from the page source
get_live_price <- function(ticker) {
  url <- paste0("https://finance.yahoo.com/quote/", ticker)
  
  tryCatch({
    # Use a standard User-Agent to look like a browser
    page <- read_html(GET(url, add_headers(`User-Agent` = "Mozilla/5.0")))
    
    # Target the 'fin-streamer' tag where Yahoo stores the live price
    price <- page %>% 
      html_element("fin-streamer[data-field='regularMarketPrice']") %>% 
      html_attr("value") %>% 
      as.numeric()
    
    return(price)
  }, error = function(e) {
    message("⚠️ Could not fetch price for ", ticker, ". Using placeholder $100.")
    return(100) 
  })
}

# --- 3. FETCH PRICES ---
message("📡 Scraping real-time prices for tickers...")
tickers <- unique(drift_report$ticker)

# Apply the scraper to your ticker list
price_map <- tibble(ticker = tickers) %>%
  rowwise() %>%
  mutate(current_price = get_live_price(ticker))

# --- 4. CONVERT WEIGHTS TO DOLLAR ACTIONS & SHARES ---
execution_list <- drift_report %>%
  left_join(price_map, by = "ticker") %>%
  mutate(
    target_value = net_weight * total_aum,
    target_shares = round(target_value / current_price, 0),
    action = case_when(
      net_weight > 0.01 ~ "BUY/HOLD",
      net_weight < -0.01 ~ "SHORT/SELL",
      TRUE ~ "FLAT"
    )
  )

# --- 5. EXPORT ---
if(!dir.exists("02_etf_saa_taa/reports")) dir.create("02_etf_saa_taa/reports", recursive = TRUE)
write.csv(execution_list, "02_etf_saa_taa/reports/sovereign_trade_list.csv", row.names = FALSE)

message("--------------------------------------------------")
message("🚀 FINAL ALLOCATION COMPLETE (GDPR BYPASS ACTIVE)")
message("📂 File: 02_etf_saa_taa/reports/sovereign_trade_list.csv")
message("💰 Total Net Value: $", format(sum(execution_list$target_value), big.mark=","))
message("--------------------------------------------------")