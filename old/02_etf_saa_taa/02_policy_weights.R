# ==============================================================================
# PROJECT B: ETF_SAA_TAA - 02_POLICY_WEIGHTS.R
# Purpose: Generate Book 1 (SAA), Book 2 (TAA), and Book 3 (Unified)
# ==============================================================================
library(dplyr)

# --- 1. SETTINGS & PATHS ---
total_aum <- 1000000
taa_slot_size <- 0.05  # 5% allocation per Alpha signal

# Anchoring paths to your module folder
path_matrix <- "02_etf_saa_taa/data_processed/selection_matrix.rds"
path_output <- "02_etf_saa_taa/data_processed/policy_list.rds"

if(!file.exists(path_matrix)) stop("❌ Selection Matrix not found. Run 01_alpha_intelligence first.")
selection_matrix <- readRDS(path_matrix)

# --- BOOK 1: THE SOVEREIGN SAA (60/40 Anchor) ---
saa_book <- tibble(
  ticker = c("SPY", "AGG"),
  weight = c(0.60, 0.40),
  book   = "BOOK_1_SAA"
)

# --- BOOK 2: THE TAA LS (Zero-Net Synthetics) ---
message("⚖️ Harvesting Spring-Loads for Book 2...")

# Logic: Identify signals that aren't already part of the core anchor
spring_loads <- selection_matrix %>%
  filter(status == "Spring-Load" & !ticker %in% c("SPY", "AGG")) %>%
  slice_max(alpha_velocity_z, n = 3) 

if(nrow(spring_loads) > 0) {
  # Long Legs: The 'Alpha' capture
  taa_longs <- spring_loads %>%
    mutate(weight = taa_slot_size, book = "BOOK_2_TAA_LONG") %>%
    select(ticker, weight, book)
  
  # Short Legs (Funding): Underweight SPY for Equities, AGG for Bonds
  # We use 'category' from your original signal_table
  taa_shorts <- spring_loads %>%
    mutate(
      ticker = if_else(category == "Equity", "SPY", "AGG"),
      weight = -taa_slot_size,
      book   = "BOOK_2_TAA_SHORT"
    ) %>%
    select(ticker, weight, book)
  
  ls_book <- bind_rows(taa_longs, taa_shorts)
} else {
  message("ℹ️ No Spring-Loads detected. Book 2 is empty.")
  ls_book <- tibble(ticker = character(), weight = numeric(), book = character())
}

# --- BOOK 3: THE TOTAL PORTFOLIO (Sum of 1 & 2) ---
# This merges the strategic baseline with the tactical tilts
total_combined <- bind_rows(saa_book, ls_book) %>%
  group_by(ticker) %>%
  summarise(net_weight = sum(weight), .groups = "drop") %>%
  mutate(
    book = "BOOK_3_TOTAL",
    target_value = net_weight * total_aum
  )

# --- SAVE FOR DOWNSTREAM ---
policy_list <- list(
  SAA = saa_book,
  TAA = ls_book,
  TOTAL = total_combined
)

saveRDS(policy_list, path_output)
message("✅ Sovereign Books Unified. Total Target Value: $", format(total_aum, big.mark=","))




