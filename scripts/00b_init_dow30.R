################################################################################
# FILE    : scripts/00b_init_dow30.R
# Purpose : Extend etf_metadata with current DOW 30 constituents fetched live
#           from tidyquant::tq_index("DOW"). Tickers already in the core
#           universe are skipped automatically.
#
# DESIGN  : Source AFTER 00_init_universe.R and BEFORE 01_etf_wrangle.R.
#           01_etf_wrangle.R checks if(!exists("etf_metadata")) before sourcing
#           init — so if this script has already extended etf_metadata, the
#           wrangle will pick up the full list automatically.
#
# USAGE
#   # With DOW30 extension:
#   source(here("scripts/00_init_universe.R"))
#   source(here("scripts/00b_init_dow30.R"))
#   source(here("scripts/01_etf_wrangle.R"))
#
#   # Core only (no change to any file):
#   source(here("scripts/00_init_universe.R"))
#   source(here("scripts/01_etf_wrangle.R"))
#
# REMOVING : restart R and source core only — etf_metadata reverts to base.
################################################################################

library(tidyverse)
library(tidyquant)
library(here)

if (!exists("etf_metadata")) source(here("scripts/00_init_universe.R"))

# ── Fetch current DOW 30 constituents ─────────────────────────────────────────
message("📥 Fetching DOW 30 index constituents from tidyquant...")

dow_raw <- tryCatch(
  tq_index("DOW"),
  error = function(e) stop("Failed to fetch DOW index: ", conditionMessage(e))
)

# tq_index returns: symbol, company, weight, ...
dow_raw <- dow_raw %>%
  rename(ticker = symbol, name = company) %>%
  select(ticker, name, weight) %>%
  filter(!is.na(ticker),
         grepl("^[A-Z]{1,5}$", ticker),          # valid stock ticker only
         !grepl("dollar|cash|currency", name,
                ignore.case = TRUE))

# ── Skip tickers already in core universe ─────────────────────────────────────
already_in <- dow_raw$ticker[dow_raw$ticker %in% etf_metadata$ticker]
if (length(already_in) > 0)
  message(sprintf("  ℹ Already in universe — skipped: %s",
                  paste(already_in, collapse = ", ")))

dow_new <- dow_raw %>% filter(!ticker %in% etf_metadata$ticker)

if (nrow(dow_new) == 0) {
  message("✅ All DOW 30 tickers already in universe — nothing added.")
} else {
  # ── Build metadata rows ──────────────────────────────────────────────────────
  # Individual stocks use tree_level = "Stock" and sub_block = "DOW30".
  # sigma_limit / winsor_pct are wider than ETFs to accommodate single-stock vol.
  dow_meta <- dow_new %>%
    transmute(
      seq_id      = NA_integer_,          # recalculated below
      id          = "EQ.L3.US.DOW",
      ticker      = ticker,
      name        = name,
      asset_class = "Equity",
      tree_level  = "Stock",
      geo         = "US",
      sub_block   = "DOW30",
      pf_function = "Satellite",
      inception   = NA_character_,        # unknown; wrangle handles missing data
      sigma_limit = 3.0,
      winsor_pct  = 0.03
    )

  # ── Append and recalculate seq_id ───────────────────────────────────────────
  etf_metadata <- bind_rows(etf_metadata, dow_meta) %>%
    mutate(seq_id = row_number())

  message(sprintf("✅ DOW30 extension: added %d tickers → etf_metadata now has %d rows.",
                  nrow(dow_meta), nrow(etf_metadata)))
  message(sprintf("   New tickers: %s",
                  paste(sort(dow_meta$ticker), collapse = ", ")))
}
