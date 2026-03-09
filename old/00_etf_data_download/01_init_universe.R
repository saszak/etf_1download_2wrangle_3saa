# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./00_etf_data_download/01_init_universe.R
# Purpose: Define Sovereign ETF Universe, Meta-Logic, and HAA Vectors
# ==============================================================================

library(tidyverse)

# 0 Connect to the Navigation Map
source("./project_tree.R")


message("🌌 Initializing Sovereign Universe Foundation (61 Tickers)...")

################################################################
# 1. MASTER METADATA (The Sovereign Source of Truth)
################################################################
# Layers: Strategy Function | Market Perception | Factual Trait
# Controls: Sigma Limit (Alert Threshold) | Winsor Pct (Tail Clipping)
################################################################
etf_metadata <- tribble(
  ~ticker,     ~sub_block,    ~strat_func,       ~mkt_perception,    ~factual_trait,        ~sigma_limit, ~winsor_pct, ~purpose,
  # --- ANCHORS & CORES (Strategy-Core) ---
  "SPY",       "Anchor",      "Strategy-Core",   "Broad-Benchmark",  "Mkt-Dependence",      2.0,          0.01,        "S&P 500 Trust",
  "IVV",       "Core",        "Strategy-Core",   "Broad-Benchmark",  "Mkt-Dependence",      2.0,          0.01,        "S&P 500 Core",
  "URTH",      "Global",      "Strategy-Core",   "Broad-Benchmark",  "Mkt-Dependence",      2.2,          0.01,        "MSCI World Index",
  "ACWX",      "Global",      "Strategy-Core",   "Broad-Benchmark",  "Mkt-Dependence",      2.2,          0.01,        "MSCI World ex-US",
  "IEFA",      "Global",      "Strategy-Core",   "Broad-Benchmark",  "Mkt-Dependence",      2.2,          0.01,        "MSCI EAFE",
  "EWJ",       "Global",      "Strategy-Core",   "Intl-Developed",   "Idiosyncratic",      2.5,          0.02,        "MSCI Japan (Nikkei)",
  "FEZ",       "Europe",      "Strategy-Core",   "Intl-Developed",   "Currency-Linked",    2.5,          0.02,        "Euro Stoxx 50",
  "DAX",       "Europe",      "Strategy-Core",   "Intl-Developed",   "Idiosyncratic",      2.5,          0.02,        "DAX 40 (Germany)",
  "EWQ",       "Europe",      "Strategy-Core",   "Intl-Developed",   "Idiosyncratic",      2.5,          0.02,        "CAC 40 (France)",
  "EWL",       "Europe",      "Strategy-Core",   "Intl-Developed",   "Idiosyncratic",      2.5,          0.02,        "MSCI Switzerland",
  "SMMCHA.SW", "Europe",      "Strategy-Core",   "Intl-Developed",   "Idiosyncratic",      2.5,          0.02,        "Swiss Mid Cap",
  
  # --- GROWTH ENGINES (Core-Growth) ---
  "XLK",       "Sector",      "Core-Growth",     "Cyclical-Growth",  "Relative-Strength",  2.5,          0.02,        "Technology",
  "XLV",       "Sector",      "Core-Growth",     "Defensive",        "Mkt-Dependence",      2.2,          0.01,        "Health Care",
  "VHT",       "Health_C",    "Core-Growth",     "Broad-Benchmark",  "Mkt-Dependence",      2.2,          0.01,        "Vanguard Health",
  "IHI",       "Health_C",    "Core-Growth",     "Thematic-Niche",   "Relative-Strength",  2.8,          0.03,        "Medical Devices",
  "SMH",       "Compute",     "Core-Growth",     "Cyclical-Growth",  "High-Sigma",         3.5,          0.05,        "Semiconductors",
  "QUAL",      "Factor",      "Core-Growth",     "Value-Yield",      "Relative-Strength",  2.2,          0.01,        "Quality Factor",
  "VLUE",      "Factor",      "Core-Growth",     "Value-Yield",      "Relative-Strength",  2.2,          0.01,        "Value Factor",
  "MTUM",      "Factor",      "Core-Growth",     "Cyclical-Growth",  "Relative-Strength",  2.5,          0.02,        "Momentum Factor",
  "USMV",      "Factor",      "Core-Growth",     "Defensive",        "Low-Vol",            2.0,          0.01,        "Min Volatility",
  
  # --- INCOME & CARRY (Core-Income) ---
  "XLP",       "Sector",      "Core-Income",     "Defensive",        "Idiosyncratic",      2.0,          0.01,        "Consumer Staples",
  "XLU",       "Sector",      "Core-Income",     "Defensive",        "Idiosyncratic",      2.0,          0.01,        "Utilities",
  "LQD",       "IG",          "Core-Income",     "Cyclical",         "Mkt-Dependence",      2.2,          0.01,        "Corp Invest Grade",
  "IHF",       "Health_C",    "Core-Income",     "Defensive",        "Idiosyncratic",      2.0,          0.01,        "Healthcare Providers",
  
  # --- STABILIZERS & BALLAST (Core-Stabilizer) ---
  "AGG",       "Agg",         "Core-Stabilizer", "Defensive",        "Inverse-Corr",       2.0,          0.01,        "US Aggregate Bond",
  "BND",       "Agg",         "Core-Stabilizer", "Defensive",        "Inverse-Corr",       2.0,          0.01,        "Total Bond Market",
  "SGOV",      "Gov",         "Core-Stabilizer", "Safe-Haven",       "Low-Vol",            1.5,          0.005,       "0-3m Treasury",
  "SHY",       "Curve",       "Core-Stabilizer", "Defensive",        "Inverse-Corr",       2.0,          0.01,        "1-3y Treasury",
  "IEF",       "Curve",       "Core-Stabilizer", "Defensive",        "Inverse-Equities",   2.0,          0.01,        "7-10y Treasury",
  "TLT",       "Curve",       "Core-Stabilizer", "Defensive",        "Duration-Sensitive", 2.5,          0.02,        "20y+ Treasury",
  "AOK",       "Balanced",    "Core-Stabilizer", "Defensive",        "Low-Vol",            1.8,          0.01,        "Conservative Alloc",
  
  # --- TACTICAL SATELLITES (Tactical-Alpha) ---
  "XLF",       "Sector",      "Tactical-Alpha",  "Cyclical",         "Mkt-Dependence",      2.5,          0.02,        "Financials",
  "XLI",       "Sector",      "Tactical-Alpha",  "Cyclical",         "Mkt-Dependence",      2.5,          0.02,        "Industrials",
  "XLY",       "Sector",      "Tactical-Alpha",  "Cyclical",         "Mkt-Dependence",      2.5,          0.02,        "Consumer Disc",
  "ITB",       "Housing",     "Tactical-Alpha",  "Cyclical",         "Mkt-Dependence",      3.0,          0.03,        "US Home Construction",
  "XHB",       "Housing",     "Tactical-Alpha",  "Cyclical",         "Mkt-Dependence",      3.0,          0.03,        "S&P Homebuilders",
  "IYT",       "Logistics",   "Tactical-Alpha",  "Cyclical",         "Mkt-Dependence",      2.8,          0.02,        "Dow Transports",
  "SEA",       "Logistics",   "Tactical-Alpha",  "Thematic-Niche",   "High-Idiosyncratic", 3.5,          0.04,        "Global Shipping",
  "ITA",       "Defense",     "Tactical-Alpha",  "Cyclical",         "Idiosyncratic",      3.0,          0.03,        "US Aero & Defense",
  "SHLD",      "Defense",     "Tactical-Alpha",  "Thematic-Niche",   "High-Idiosyncratic", 3.5,          0.04,        "Defense Tech",
  "IBB",       "Health_C",    "Tactical-Alpha",  "Thematic-Niche",   "High-Sigma",         3.5,          0.04,        "Nasdaq Biotech",
  "XBI",       "Health_C",    "Tactical-Alpha",  "Thematic-Niche",   "High-Sigma",         4.0,          0.05,        "S&P Biotech (EW)",
  "HYG",       "HY",          "Tactical-Alpha",  "Value-Yield",      "High-Sigma",         2.5,          0.02,        "High Yield Corp",
  "EMB",       "EM_Hard",     "Tactical-Alpha",  "Value-Yield",      "Idiosyncratic",      2.5,          0.02,        "EM Sovereign (USD)",
  "EMLC",      "EM_LCL",      "Tactical-Alpha",  "Value-Yield",      "Idiosyncratic",      2.5,          0.02,        "EM Local Currency",
  "EWY",       "EM_Hard",     "Tactical-Alpha",  "Intl-Developed",   "Idiosyncratic",      2.8,          0.02,        "iShares South Korea",
  "AOM",       "Balanced",    "Tactical-Alpha",  "Broad-Benchmark",  "Mkt-Dependence",      2.2,          0.01,        "Moderate Alloc",
  "AOR",       "Balanced",    "Tactical-Alpha",  "Broad-Benchmark",  "Mkt-Dependence",      2.2,          0.01,        "Growth Alloc",
  "IYR",       "REIT",        "Tactical-Alpha",  "Value-Yield",      "Idiosyncratic",      2.8,          0.03,        "US Real Estate",
  
  # --- REAL ASSET SHIELDS (Real-Shield) ---
  "XLE",       "Sector",      "Real-Shield",     "Commodity",        "Inflation-Lead",     3.0,          0.03,        "Energy",
  "GLD",       "Cmdty",       "Real-Shield",     "Safe-Haven",       "Non-Financial",      2.2,          0.02,        "Gold Bullion",
  "SLV",       "Cmdty",       "Real-Shield",     "Safe-Haven",       "Non-Financial",      3.0,          0.03,        "Silver Bullion",
  "COPX",      "Resource",    "Real-Shield",     "Commodity",        "Inflation-Lead",     3.0,          0.03,        "Copper Miners",
  "URA",       "Resource",    "Real-Shield",     "Thematic-Niche",   "High-Sigma",         4.0,          0.05,        "Uranium Miners",
  "TIP",       "TIPS",        "Real-Shield",     "Safe-Haven",       "Inflation-Lead",     2.0,          0.01,        "TIPS Bond",
  "IGF",       "Infra",       "Real-Shield",     "Infrastructure",   "Non-Financial",      2.2,          0.01,        "Global Infra"
)

################################################################
# 2. DYNAMIC VECTOR GENERATION (HAA Infrastructure)
################################################################

# Hierarchical Category Vectors (cat_tickers)
cat_tickers <- etf_metadata %>%
  group_split(strat_func) %>%
  set_names(map_chr(., ~unique(.x$strat_func))) %>%
  map(~.x$ticker)

# Physical Block Vectors (sub_block_tickers)
block_tickers <- etf_metadata %>%
  group_split(sub_block) %>%
  set_names(map_chr(., ~unique(.x$sub_block))) %>%
  map(~.x$ticker)

# Global Export
all_tickers <- etf_metadata$ticker

################################################################
# 3. CONSOLE REPORT
################################################################
message("--- SOVEREIGN FOUNDATION FINALIZED ---")
message("✅ Ticker Inventory: ", length(all_tickers), " unique ETFs.")
iwalk(cat_tickers, ~message("   - [", .y, "]: ", length(.x), " tickers"))
message("--- Ready for Download & Sigma Analysis ---")