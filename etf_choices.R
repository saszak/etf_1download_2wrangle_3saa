# =============================================================================
# PROJECT:   ETF SAA/TAA Universe
# FILE:      etf_choices.R  (TEMP — candidate universe, not yet promoted to prod)
# PATH:      etf_1download_2wrangle_3saa/etf_choices.R
# PURPOSE:   Hierarchically structured ~100-ticker candidate ETF universe for
#            SAA/TAA research. Replaces the flat 60-ticker list with explicit
#            asset_class, tree_level, geo, sub_block, pf_function, inception,
#            sigma_limit, and winsor_pct metadata.
# STATUS:    TEMP — review duplicates and signal-only tickers before promoting.
# =============================================================================

library(tidyverse)

etf_candidates <- tribble(
  ~id, ~ticker, ~name,                              ~asset_class, ~tree_level,  ~geo,    ~sub_block,    ~pf_function,      ~inception,    ~sigma_limit, ~winsor_pct,

  # ---------------------------------------------------------------------------
  # EQUITY — L1 World (Anchor)
  # ---------------------------------------------------------------------------
  1L,  "URTH",  "iShares MSCI World",               "Equity",     "L1_World",   "Global","World",        "Anchor",          "2010-06-10",  2.2,          0.01,
  2L,  "ACWI",  "iShares MSCI ACWI",                "Equity",     "L1_World",   "Global","World",        "Anchor",          "2008-03-26",  2.2,          0.01,
  3L,  "ACWX",  "iShares MSCI ACWI ex-US",          "Equity",     "L1_World",   "Global","World_exUS",   "Core-Growth",     "2008-03-26",  2.2,          0.01,

  # ---------------------------------------------------------------------------
  # EQUITY — L2 Region (US)
  # ---------------------------------------------------------------------------
  4L,  "SPY",   "SPDR S&P 500",                     "Equity",     "L2_Region",  "US",    "US_Broad",     "Anchor",          "1993-01-22",  2.0,          0.01,
  5L,  "VTI",   "Vanguard Total US Market",          "Equity",     "L2_Region",  "US",    "US_Broad",     "Core-Growth",     "2001-05-24",  2.0,          0.01,
  6L,  "QQQ",   "Invesco Nasdaq 100",                "Equity",     "L2_Region",  "US",    "US_Growth",    "Core-Growth",     "1999-03-10",  2.5,          0.02,
  7L,  "IJH",   "iShares S&P 400 Mid-Cap",           "Equity",     "L2_Region",  "US",    "US_MidCap",    "Core-Growth",     "2000-05-22",  2.5,          0.02,
  8L,  "IWM",   "iShares Russell 2000 Small-Cap",    "Equity",     "L2_Region",  "US",    "US_SmallCap",  "Core-Growth",     "2000-05-22",  2.8,          0.02,

  # ---------------------------------------------------------------------------
  # EQUITY — L2 Region (DM / EM)
  # ---------------------------------------------------------------------------
  9L,  "IEFA",  "iShares Core MSCI EAFE",            "Equity",     "L2_Region",  "DM",    "DM_Broad",     "Core-Growth",     "2012-10-18",  2.2,          0.01,
  # VEA dropped — near-duplicate of IEFA; IEFA retained
  # EEM dropped — near-duplicate of VWO; VWO retained (lower cost, Korea covered by EWY)
  11L, "VWO",   "Vanguard FTSE EM",                  "Equity",     "L2_Region",  "EM",    "EM_Broad",     "Tactical",        "2005-03-04",  2.8,          0.02,

  # ---------------------------------------------------------------------------
  # EQUITY — L3 Country (DM)
  # ---------------------------------------------------------------------------
  13L, "EWJ",   "iShares MSCI Japan",                "Equity",     "L3_Country", "DM",    "Japan",        "Core-Growth",     "1996-03-12",  2.5,          0.02,
  14L, "DXJ",   "WisdomTree Japan Hedged Equity",    "Equity",     "L3_Country", "DM",    "Japan_Hdg",    "Tactical",        "2006-06-16",  2.5,          0.02,
  15L, "FEZ",   "SPDR Euro Stoxx 50",                "Equity",     "L3_Country", "DM",    "Europe",       "Core-Growth",     "2002-10-15",  2.5,          0.02,
  16L, "DAX",   "Global X DAX Germany",              "Equity",     "L3_Country", "DM",    "Germany",      "Tactical",        "2007-10-23",  2.5,          0.02,
  17L, "EWQ",   "iShares MSCI France",               "Equity",     "L3_Country", "DM",    "France",       "Tactical",        "1996-03-12",  2.5,          0.02,
  18L, "EWL",   "iShares MSCI Switzerland",          "Equity",     "L3_Country", "DM",    "Switzerland",  "Tactical",        "1996-03-12",  2.5,          0.02,
  19L, "EWU",   "iShares MSCI United Kingdom",       "Equity",     "L3_Country", "DM",    "UK",           "Tactical",        "1996-03-12",  2.5,          0.02,
  20L, "EWC",   "iShares MSCI Canada",               "Equity",     "L3_Country", "DM",    "Canada",       "Tactical",        "1996-03-12",  2.5,          0.02,
  21L, "EWA",   "iShares MSCI Australia",            "Equity",     "L3_Country", "DM",    "Australia",    "Tactical",        "1996-03-12",  2.5,          0.02,
  22L, "EWI",   "iShares MSCI Italy",                "Equity",     "L3_Country", "DM",    "Italy",        "Tactical",        "1996-03-12",  2.8,          0.02,
  23L, "EWP",   "iShares MSCI Spain",                "Equity",     "L3_Country", "DM",    "Spain",        "Tactical",        "1996-03-12",  2.8,          0.02,

  # ---------------------------------------------------------------------------
  # EQUITY — L3 Country (EM)
  # ---------------------------------------------------------------------------
  24L, "INDA",  "iShares MSCI India",                "Equity",     "L3_Country", "EM",    "India",        "Tactical",        "2012-02-02",  2.8,          0.02,
  25L, "FXI",   "iShares China Large-Cap",           "Equity",     "L3_Country", "EM",    "China",        "Tactical",        "2004-10-05",  3.0,          0.03,
  26L, "EWT",   "iShares MSCI Taiwan",               "Equity",     "L3_Country", "EM",    "Taiwan",       "Tactical",        "2000-06-20",  2.8,          0.02,
  27L, "EWY",   "iShares MSCI South Korea",          "Equity",     "L3_Country", "EM",    "Korea",        "Tactical",        "2000-05-09",  2.8,          0.02,
  28L, "EWZ",   "iShares MSCI Brazil",               "Equity",     "L3_Country", "EM",    "Brazil",       "Tactical",        "2000-07-10",  3.5,          0.04,

  # ---------------------------------------------------------------------------
  # EQUITY — L3 Sector (US)
  # ---------------------------------------------------------------------------
  29L, "XLK",   "Technology Select Sector",          "Equity",     "L3_Sector",  "US",    "Tech",         "Core-Growth",     "1998-12-22",  2.5,          0.02,
  30L, "XLC",   "Communication Services Select",     "Equity",     "L3_Sector",  "US",    "Comms",        "Core-Growth",     "2018-06-18",  2.5,          0.02,
  31L, "XLV",   "Health Care Select Sector",         "Equity",     "L3_Sector",  "US",    "HealthCare",   "Core-Growth",     "1998-12-22",  2.2,          0.01,
  32L, "XLF",   "Financial Select Sector",           "Equity",     "L3_Sector",  "US",    "Financials",   "Tactical",        "1998-12-22",  2.5,          0.02,
  33L, "XLI",   "Industrial Select Sector",          "Equity",     "L3_Sector",  "US",    "Industrials",  "Tactical",        "1998-12-22",  2.5,          0.02,
  34L, "XLP",   "Consumer Staples Select",           "Equity",     "L3_Sector",  "US",    "Staples",      "Core-Stabilizer", "1998-12-22",  2.0,          0.01,
  35L, "XLY",   "Consumer Disc Select Sector",       "Equity",     "L3_Sector",  "US",    "Discretionary","Tactical",        "1998-12-22",  2.5,          0.02,
  36L, "XLE",   "Energy Select Sector",              "Equity",     "L3_Sector",  "US",    "Energy",       "Real-Shield",     "1998-12-22",  3.0,          0.03,
  37L, "XLB",   "Materials Select Sector",           "Equity",     "L3_Sector",  "US",    "Materials",    "Real-Shield",     "1998-12-22",  2.5,          0.02,
  38L, "XLRE",  "Real Estate Select Sector",         "Equity",     "L3_Sector",  "US",    "REIT_Sector",  "Core-Income",     "2015-10-07",  2.5,          0.02,
  39L, "XLU",   "Utilities Select Sector",           "Equity",     "L3_Sector",  "US",    "Utilities",    "Core-Stabilizer", "1998-12-22",  2.0,          0.01,

  # ---------------------------------------------------------------------------
  # EQUITY — Factor
  # ---------------------------------------------------------------------------
  40L, "QUAL",  "iShares MSCI USA Quality",          "Equity",     "Factor",     "US",    "Quality",      "Core-Factor",     "2013-07-16",  2.2,          0.01,
  41L, "MTUM",  "iShares MSCI USA Momentum",         "Equity",     "Factor",     "US",    "Momentum",     "Core-Factor",     "2013-04-16",  2.5,          0.02,
  42L, "USMV",  "iShares MSCI USA Min Vol",          "Equity",     "Factor",     "US",    "MinVol",       "Core-Factor",     "2011-10-18",  2.0,          0.01,
  43L, "VTV",   "Vanguard Value",                    "Equity",     "Factor",     "US",    "Value",        "Core-Factor",     "2004-01-26",  2.2,          0.01,
  # VLUE dropped — near-duplicate of VTV; VTV retained (history from 2004 vs 2013, lower cost)
  45L, "DGRW",  "WisdomTree US Dividend Growth",     "Equity",     "Factor",     "US",    "DivGrowth",    "Core-Factor",     "2013-05-22",  2.2,          0.01,
  46L, "COWZ",  "Pacer US Cash Cows 100",            "Equity",     "Factor",     "US",    "CashFlow",     "Core-Factor",     "2016-12-16",  2.2,          0.01,
  47L, "ACWV",  "iShares MSCI ACWI Min Vol",         "Equity",     "Factor",     "Global","MinVol_Glbl",  "Core-Factor",     "2011-10-18",  2.0,          0.01,

  # ---------------------------------------------------------------------------
  # FIXED INCOME — Government (Curve)
  # ---------------------------------------------------------------------------
  48L, "SGOV",  "iShares 0-3 Month Treasury",        "FixedIncome","FI_Gov",     "US",    "Cash",         "Core-Stabilizer", "2020-08-04",  1.5,          0.005,
  49L, "SHY",   "iShares 1-3 Year Treasury",         "FixedIncome","FI_Gov",     "US",    "Curve_S",      "Core-Stabilizer", "2002-07-22",  2.0,          0.01,
  50L, "IEI",   "iShares 3-7 Year Treasury",         "FixedIncome","FI_Gov",     "US",    "Curve_M",      "Core-Stabilizer", "2007-01-05",  2.0,          0.01,
  51L, "IEF",   "iShares 7-10 Year Treasury",        "FixedIncome","FI_Gov",     "US",    "Curve_L",      "Core-Stabilizer", "2002-07-22",  2.0,          0.01,
  52L, "TLT",   "iShares 20+ Year Treasury",         "FixedIncome","FI_Gov",     "US",    "Curve_XL",     "Core-Stabilizer", "2002-07-22",  2.5,          0.02,

  # ---------------------------------------------------------------------------
  # FIXED INCOME — TIPS (Inflation-Linked)
  # ---------------------------------------------------------------------------
  53L, "TIP",   "iShares TIPS Bond",                 "FixedIncome","FI_TIPS",    "US",    "TIPS_Broad",   "Real-Shield",     "2003-12-04",  2.0,          0.01,
  54L, "LTPZ",  "PIMCO 15+ Year TIPS",               "FixedIncome","FI_TIPS",    "US",    "TIPS_Long",    "Real-Shield",     "2009-09-03",  2.5,          0.02,

  # ---------------------------------------------------------------------------
  # FIXED INCOME — Investment Grade
  # ---------------------------------------------------------------------------
  55L, "AGG",   "iShares Core US Agg Bond",          "FixedIncome","FI_IG",      "US",    "Agg",          "Core-Stabilizer", "2003-09-22",  2.0,          0.01,
  56L, "LQD",   "iShares iBoxx IG Corp Bond",        "FixedIncome","FI_IG",      "US",    "IG_Corp",      "Core-Income",     "2002-07-22",  2.2,          0.01,
  57L, "VCIT",  "Vanguard Intermediate IG Corp",     "FixedIncome","FI_IG",      "US",    "IG_Corp_M",    "Core-Income",     "2009-11-19",  2.2,          0.01,
  58L, "MUB",   "iShares National Muni Bond",        "FixedIncome","FI_IG",      "US",    "Muni",         "Core-Income",     "2007-09-07",  1.8,          0.01,

  # ---------------------------------------------------------------------------
  # FIXED INCOME — High Yield
  # ---------------------------------------------------------------------------
  59L, "HYG",   "iShares iBoxx HY Corp Bond",        "FixedIncome","FI_HY",      "US",    "HY_Corp",      "Tactical",        "2007-04-04",  2.5,          0.02,
  # JNK dropped — near-duplicate of HYG; HYG retained (longer history, higher AUM)

  # ---------------------------------------------------------------------------
  # FIXED INCOME — International / EM / Hedge
  # ---------------------------------------------------------------------------
  61L, "BNDX",  "Vanguard Total Intl Bond Hedged",   "FixedIncome","FI_Intl",    "DM",    "Intl_Agg",     "Core-Stabilizer", "2013-05-31",  2.0,          0.01,
  62L, "EMB",   "iShares JP Morgan USD EM Bond",     "FixedIncome","FI_EM",      "EM",    "EM_Hard",      "Tactical",        "2007-12-17",  2.5,          0.02,
  63L, "EMLC",  "VanEck Local Currency EM Bond",     "FixedIncome","FI_EM",      "EM",    "EM_Local",     "Tactical",        "2010-07-22",  2.5,          0.02,
  64L, "PFIX",  "Simplify Interest Rate Hedge",      "FixedIncome","FI_Hedge",   "US",    "Rate_Hedge",   "Real-Shield",     "2021-05-11",  3.0,          0.03,

  # ---------------------------------------------------------------------------
  # COMMODITY — Broad
  # ---------------------------------------------------------------------------
  65L, "PDBC",  "Invesco Opt Yield Commodity",       "Commodity",  "Cmdty_Broad","Multi", "Cmdty_Broad",  "Real-Shield",     "2014-11-07",  2.5,          0.02,
  66L, "DJP",   "iPath Bloomberg Commodity",         "Commodity",  "Cmdty_Broad","Multi", "Cmdty_Broad",  "Real-Shield",     "2006-10-23",  2.5,          0.02,

  # ---------------------------------------------------------------------------
  # COMMODITY — Precious Metals
  # ---------------------------------------------------------------------------
  67L, "GLD",   "SPDR Gold Shares",                  "Commodity",  "Cmdty_PM",   "Multi", "Gold",         "Real-Shield",     "2004-11-18",  2.2,          0.02,
  68L, "SLV",   "iShares Silver Trust",              "Commodity",  "Cmdty_PM",   "Multi", "Silver",       "Real-Shield",     "2006-04-21",  3.0,          0.03,

  # ---------------------------------------------------------------------------
  # COMMODITY — Agriculture
  # ---------------------------------------------------------------------------
  69L, "MOO",   "VanEck Agribusiness",               "Commodity",  "Cmdty_Agri", "Multi", "Agri_Equity",  "Real-Shield",     "2007-08-31",  2.5,          0.02,
  70L, "DBA",   "Invesco DB Agriculture",            "Commodity",  "Cmdty_Agri", "Multi", "Agri_Futures", "Real-Shield",     "2007-01-05",  2.5,          0.02,

  # ---------------------------------------------------------------------------
  # COMMODITY — Metals / Materials
  # ---------------------------------------------------------------------------
  71L, "COPX",  "Global X Copper Miners",            "Commodity",  "Cmdty_Metal","Multi", "Copper",       "Real-Shield",     "2010-09-16",  3.0,          0.03,
  72L, "URA",   "Global X Uranium ETF",              "Commodity",  "Cmdty_Metal","Multi", "Uranium",      "Satellite",       "2010-11-04",  4.0,          0.05,
  73L, "LIT",   "Global X Lithium and Battery Tech", "Commodity",  "Cmdty_Metal","Multi", "Lithium",      "Satellite",       "2010-07-22",  3.5,          0.04,

  # ---------------------------------------------------------------------------
  # FX — Currency
  # ---------------------------------------------------------------------------
  74L, "UUP",   "Invesco DB USD Index Bullish",      "FX",         "FX",         "US",    "USD",          "Signal",          "2007-02-20",  1.5,          0.01,
  75L, "FXE",   "Invesco CurrencyShares EUR/USD",    "FX",         "FX",         "DM",    "EUR",          "Signal",          "2005-12-09",  1.5,          0.01,
  76L, "FXY",   "Invesco CurrencyShares JPY",        "FX",         "FX",         "DM",    "JPY",          "Signal",          "2007-02-12",  1.5,          0.01,
  77L, "FXB",   "Invesco CurrencyShares GBP",        "FX",         "FX",         "DM",    "GBP",          "Signal",          "2006-06-26",  1.5,          0.01,
  78L, "FXF",   "Invesco CurrencyShares CHF",        "FX",         "FX",         "DM",    "CHF",          "Signal",          "2006-06-26",  1.5,          0.01,

  # ---------------------------------------------------------------------------
  # REAL ASSET — Infrastructure & REIT
  # ---------------------------------------------------------------------------
  79L, "IGF",   "iShares Global Infrastructure",     "RealAsset",  "Infra",      "Global","Infra",        "Core-Income",     "2007-12-10",  2.2,          0.01,
  80L, "IYR",   "iShares US Real Estate",            "RealAsset",  "REIT",       "US",    "REIT_US",      "Core-Income",     "2000-06-12",  2.8,          0.03,
  81L, "VNQI",  "Vanguard Intl Real Estate",         "RealAsset",  "REIT",       "DM",    "REIT_Intl",    "Tactical",        "2010-11-01",  2.5,          0.02,

  # ---------------------------------------------------------------------------
  # EQUITY — Satellite (Thematic / Sub-sector)
  # ---------------------------------------------------------------------------
  82L, "VHT",   "Vanguard Health Care",              "Equity",     "Satellite",  "US",    "HC_Broad",     "Satellite",       "2004-01-26",  2.2,          0.01,
  83L, "IHI",   "iShares Medical Devices",           "Equity",     "Satellite",  "US",    "MedDevices",   "Satellite",       "2006-05-01",  2.8,          0.03,
  84L, "IBB",   "iShares Nasdaq Biotech",            "Equity",     "Satellite",  "US",    "Biotech",      "Satellite",       "2001-02-05",  3.5,          0.04,
  85L, "XBI",   "SPDR S&P Biotech",                  "Equity",     "Satellite",  "US",    "Biotech",      "Satellite",       "2006-01-31",  4.0,          0.05,
  86L, "SMH",   "VanEck Semiconductor",              "Equity",     "Satellite",  "US",    "Semis",        "Satellite",       "2011-12-20",  3.5,          0.05,
  87L, "CIBR",  "First Trust Cybersecurity",         "Equity",     "Satellite",  "US",    "Cyber",        "Satellite",       "2015-07-07",  2.8,          0.02,
  88L, "WCLD",  "WisdomTree Cloud Computing",        "Equity",     "Satellite",  "US",    "Cloud",        "Satellite",       "2019-09-16",  3.5,          0.04,
  89L, "AIQ",   "Global X AI and Big Data",          "Equity",     "Satellite",  "Global","AI",           "Satellite",       "2018-09-12",  3.0,          0.03,
  90L, "ITA",   "iShares US Aerospace and Defense",  "Equity",     "Satellite",  "US",    "Defense",      "Satellite",       "2006-05-01",  3.0,          0.03,
  91L, "ITB",   "iShares US Home Construction",      "Equity",     "Satellite",  "US",    "Housing",      "Satellite",       "2006-05-01",  3.0,          0.03,
  92L, "KRE",   "SPDR S&P Regional Banking",         "Equity",     "Satellite",  "US",    "Reg_Banks",    "Satellite",       "2006-06-19",  3.0,          0.03,
  93L, "IYT",   "iShares US Transportation",         "Equity",     "Satellite",  "US",    "Transport",    "Satellite",       "2003-10-06",  2.8,          0.02,
  94L, "PSP",   "Invesco Listed Private Equity",     "Equity",     "Satellite",  "Global","Private_Eq",   "Satellite",       "2006-10-24",  3.5,          0.04,
  95L, "IPO",   "Renaissance IPO ETF",               "Equity",     "Satellite",  "US",    "IPO",          "Satellite",       "2013-10-14",  3.5,          0.04,
  96L, "JEPI",  "JPMorgan Equity Premium Income",    "Equity",     "Satellite",  "US",    "Eq_Income",    "Core-Income",     "2020-05-20",  2.0,          0.01,

  # ---------------------------------------------------------------------------
  # ALTERNATIVE — Crypto
  # ---------------------------------------------------------------------------
  97L, "IBIT",  "iShares Bitcoin Trust",             "Alternative","Satellite",  "Global","Crypto",       "Satellite",       "2024-01-11",  5.0,          0.08,

  # ---------------------------------------------------------------------------
  # MULTI-ASSET — Balanced Allocation
  # ---------------------------------------------------------------------------
  98L, "AOR",   "iShares Growth Allocation",         "MultiAsset", "Balanced",   "Global","Balanced_G",   "Signal",          "2008-11-04",  2.2,          0.01,
  99L, "AOK",   "iShares Conservative Allocation",   "MultiAsset", "Balanced",   "Global","Balanced_C",   "Signal",          "2008-11-04",  1.8,          0.01,

  # ---------------------------------------------------------------------------
  # SIGNAL — Volatility / Macro Indicator
  # ---------------------------------------------------------------------------
  100L,"VIXY",  "ProShares VIX Short-Term Futures",  "Signal",     "Signal",     "US",    "VIX",          "Signal",          "2011-01-03",  5.0,          0.10
)

# =============================================================================
# DERIVED LOOKUP VECTORS
# =============================================================================

# Flat vector of all tickers (for download / wrangle pipelines)
all_tickers <- etf_candidates$ticker

# Grouped by tree_level
tree_tickers <- etf_candidates |>
  group_by(tree_level) |>
  summarise(tickers = list(ticker), .groups = "drop")

# Grouped by asset_class
class_tickers <- etf_candidates |>
  group_by(asset_class) |>
  summarise(tickers = list(ticker), .groups = "drop")

# Grouped by pf_function
func_tickers <- etf_candidates |>
  group_by(pf_function) |>
  summarise(tickers = list(ticker), .groups = "drop")

# =============================================================================
# CONSOLE SUMMARY
# =============================================================================

cat("\n--- ETF Candidate Universe Summary ---\n")
cat(sprintf("Total tickers: %d\n\n", nrow(etf_candidates)))

cat("Count by asset_class:\n")
print(
  etf_candidates |>
    count(asset_class, sort = TRUE) |>
    rename(`n` = n)
)

cat("\nCount by pf_function:\n")
print(
  etf_candidates |>
    count(pf_function, sort = TRUE) |>
    rename(`n` = n)
)

cat("--------------------------------------\n\n")

# =============================================================================
# NOTES — Universe Changes Relative to Original 60-Ticker Flat List
# =============================================================================

# TICKERS ADDED (new vs original 60):
#   L1 World:        URTH, ACWI, ACWX
#   DM Countries:    DXJ, FEZ, DAX, EWQ, EWL, EWU, EWC, EWA, EWI, EWP
#   EM Countries:    INDA, FXI, EWT, EWY, EWZ
#   Sectors:         XLC, XLRE
#   Factor:          QUAL, MTUM, USMV, VTV, VLUE, DGRW, COWZ, ACWV
#   FI Gov:          SGOV, IEI
#   FI TIPS:         TIP, LTPZ
#   FI IG:           VCIT, MUB
#   FI HY:           JNK
#   FI Intl/EM/Hdg:  BNDX, EMLC, PFIX
#   Commodity:       PDBC, DJP, SLV, MOO, DBA, COPX, URA, LIT
#   FX:              UUP, FXE, FXY, FXB, FXF
#   RealAsset:       VNQI
#   Satellite:       VHT, IHI, IBB, XBI, SMH, CIBR, WCLD, AIQ, ITA, ITB,
#                    KRE, IYT, PSP, IPO, JEPI
#   Alternative:     IBIT
#   MultiAsset:      AOR, AOK
#   Signal:          VIXY

# TICKERS DROPPED (were in original 60, excluded here):
#   IVV   — redundant with SPY (both S&P 500); keep SPY as Anchor
#   BND   — redundant with AGG (both US Agg); keep AGG
#   SMMCHA.SW — Swiss-listed, non-US ticker; data sourcing complexity

# SIGNAL-ONLY TICKERS (informational / macro context; not held as positions):
#   UUP, FXE, FXY, FXB, FXF  — FX direction signals
#   AOR, AOK                  — blended allocation benchmarks
#   VIXY                      — volatility regime signal

# DUPLICATES TO RESOLVE BEFORE PROMOTING TO PRODUCTION:
#   HYG / JNK     — both cover US HY corps; pick one or average; suggest HYG
#                   (higher AUM, longer history) and drop JNK
#   VTV / VLUE    — both US Value factor; VTV (Vanguard, lower cost) vs VLUE
#                   (MSCI methodology); evaluate factor loading overlap
#   IBB / XBI     — both US Biotech; IBB is large-cap weighted, XBI equal-
#                   weight; retain both only if explicit style split desired
#   VWO / EEM     — both EM broad; VWO excludes South Korea, EEM includes it;
#                   resolve based on Korea exposure preference
#   IEFA / VEA    — both DM broad; near-identical exposure; one is redundant
