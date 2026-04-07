################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : scripts/00_init_universe.R
# Purpose : Sovereign Universe — 138 tickers (108 ETFs + 30 DOW stocks), hierarchical asset class tree.
#           Promoted from etf_choices.R after duplicate resolution.
#
# SCHEMA
#   seq_id        Sequential integer 1..N — housekeeping row reference (added programmatically)
#   id            Dot-path classification code (e.g. "EQ.L2.US.GRW") — non-unique; ticker is the key
#   ticker        Yahoo Finance symbol
#   name          Full ETF name
#   asset_class   Equity | FixedIncome | Commodity | FX | RealAsset |
#                 MultiAsset | Alternative | Signal
#   tree_level    L1_World | L2_Region | L3_Country | L3_Sector | Factor |
#                 Satellite | FI_Gov | FI_TIPS | FI_IG | FI_HY | FI_Intl |
#                 FI_EM | FI_Hedge | Cmdty_Broad | Cmdty_PM | Cmdty_Agri |
#                 Cmdty_Metal | FX | REIT | Infra | Balanced | Signal |
#                 MgdFutures | MktNeutral | TailRisk | Hedge | RiskParity
#   geo           US | Global | DM | EM | Multi
#   sub_block     Granular sub-group label
#   pf_function   Anchor | Core-Growth | Core-Stabilizer | Core-Income |
#                 Core-Factor | Tactical | Real-Shield | Satellite | Signal |
#                 DAA
#   inception     Character date "YYYY-MM-DD"
#   sigma_limit   Z-score winsorization cap
#   winsor_pct    Return winsorization percentage
#
# DUPLICATE RESOLUTIONS (vs etf_choices.R candidate list)
#   Dropped JNK  → HYG retained  (higher AUM, longer history)
#   Dropped VLUE → VTV retained  (history from 2004 vs 2013, lower cost)
#   Dropped EEM  → VWO retained  (lower cost; Korea covered by EWY)
#   Dropped VEA  → IEFA retained (more widely tracked benchmark)
################################################################################

library(tidyverse)

if (!exists("project_tree")) source(here::here("project_tree.R"))

message("Initializing Sovereign Universe (138 tickers: 108 ETFs + 30 DOW stocks)...")

etf_metadata <- tribble(
  ~seq_id, ~id,              ~ticker,    ~name,                               ~asset_class,  ~tree_level,   ~geo,    ~sub_block,     ~pf_function,      ~inception,    ~sigma_limit, ~winsor_pct,

  # ── EQUITY: L1 World ────────────────────────────────────────────────────────
  1L,  "EQ.L1.GL",       "URTH",    "iShares MSCI World",                 "Equity",      "L1_World",    "Global","World",         "Anchor",          "2010-06-10",  2.2,          0.01,
  2L,  "EQ.L1.GL",       "ACWI",    "iShares MSCI ACWI",                  "Equity",      "L1_World",    "Global","World",         "Anchor",          "2008-03-26",  2.2,          0.01,
  3L,  "EQ.L1.GL.EXUS",  "ACWX",    "iShares MSCI ACWI ex-US",            "Equity",      "L1_World",    "Global","World_exUS",    "Core-Growth",     "2008-03-26",  2.2,          0.01,

  # ── EQUITY: L2 US Broad ─────────────────────────────────────────────────────
  4L,  "EQ.L2.US",       "SPY",     "SPDR S&P 500",                       "Equity",      "L2_Region",   "US",    "US_Broad",      "Anchor",          "1993-01-22",  2.0,          0.01,
  5L,  "EQ.L2.US",       "VTI",     "Vanguard Total US Market",           "Equity",      "L2_Region",   "US",    "US_Broad",      "Core-Growth",     "2001-05-24",  2.0,          0.01,
  6L,  "EQ.L2.US.GRW",   "QQQ",     "Invesco Nasdaq 100",                 "Equity",      "L2_Region",   "US",    "US_Growth",     "Core-Growth",     "1999-03-10",  2.5,          0.02,
  7L,  "EQ.L2.US.MID",   "IJH",     "iShares S&P 400 Mid-Cap",            "Equity",      "L2_Region",   "US",    "US_MidCap",     "Core-Growth",     "2000-05-22",  2.5,          0.02,
  8L,  "EQ.L2.US.SML",   "IWM",     "iShares Russell 2000 Small-Cap",     "Equity",      "L2_Region",   "US",    "US_SmallCap",   "Core-Growth",     "2000-05-22",  2.8,          0.02,

  # ── EQUITY: L2 DM & EM Broad ────────────────────────────────────────────────
  9L,  "EQ.L2.DM",       "IEFA",    "iShares Core MSCI EAFE",             "Equity",      "L2_Region",   "DM",    "DM_Broad",      "Core-Growth",     "2012-10-18",  2.2,          0.01,
  10L, "EQ.L2.EM",       "VWO",     "Vanguard FTSE EM",                   "Equity",      "L2_Region",   "EM",    "EM_Broad",      "Tactical",        "2005-03-04",  2.8,          0.02,
  11L, "EQ.L2.EM.ASIA",  "AAXJ",    "iShares MSCI AC Asia ex Japan",      "Equity",      "L2_Region",   "EM",    "Asia_exJP",     "Tactical",        "2008-08-13",  2.8,          0.02,

  # ── EQUITY: L2 European Broad (regional, not single-country) ────────────────
  12L, "EQ.L2.DM.EUR50", "FEZ",     "SPDR Euro Stoxx 50",                 "Equity",      "L2_Region",   "DM",    "Europe_EZ50",   "Core-Growth",     "2002-10-15",  2.5,          0.02,
  13L, "EQ.L2.DM.EUREZ", "EZU",     "iShares MSCI Eurozone",              "Equity",      "L2_Region",   "DM",    "Europe_EZ",     "Core-Growth",     "2000-07-25",  2.5,          0.02,
  14L, "EQ.L2.DM.EUR",   "IEV",     "iShares Europe ETF",                 "Equity",      "L2_Region",   "DM",    "Europe_Broad",  "Core-Growth",     "2000-07-25",  2.5,          0.02,

  # ── EQUITY: L3 DM Countries ─────────────────────────────────────────────────
  15L, "EQ.L3.DM.JPN",   "EWJ",     "iShares MSCI Japan",                 "Equity",      "L3_Country",  "DM",    "Japan",         "Core-Growth",     "1996-03-12",  2.5,          0.02,
  16L, "EQ.L3.DM.JPN.H", "DXJ",     "WisdomTree Japan Hedged Equity",     "Equity",      "L3_Country",  "DM",    "Japan_Hdg",     "Tactical",        "2006-06-16",  2.5,          0.02,
  17L, "EQ.L3.DM.DEU",   "DAX",     "Global X DAX Germany",               "Equity",      "L3_Country",  "DM",    "Germany",       "Tactical",        "2007-10-23",  2.5,          0.02,
  18L, "EQ.L3.DM.FRA",   "EWQ",     "iShares MSCI France",                "Equity",      "L3_Country",  "DM",    "France",        "Tactical",        "1996-03-12",  2.5,          0.02,
  19L, "EQ.L3.DM.CHE",   "EWL",     "iShares MSCI Switzerland",           "Equity",      "L3_Country",  "DM",    "Switzerland",   "Tactical",        "1996-03-12",  2.5,          0.02,
  20L, "EQ.L3.DM.GBR",   "EWU",     "iShares MSCI United Kingdom",        "Equity",      "L3_Country",  "DM",    "UK",            "Tactical",        "1996-03-12",  2.5,          0.02,
  21L, "EQ.L3.DM.CAN",   "EWC",     "iShares MSCI Canada",                "Equity",      "L3_Country",  "DM",    "Canada",        "Tactical",        "1996-03-12",  2.5,          0.02,
  22L, "EQ.L3.DM.AUS",   "EWA",     "iShares MSCI Australia",             "Equity",      "L3_Country",  "DM",    "Australia",     "Tactical",        "1996-03-12",  2.5,          0.02,
  23L, "EQ.L3.DM.ITA",   "EWI",     "iShares MSCI Italy",                 "Equity",      "L3_Country",  "DM",    "Italy",         "Tactical",        "1996-03-12",  2.8,          0.02,
  24L, "EQ.L3.DM.ESP",   "EWP",     "iShares MSCI Spain",                 "Equity",      "L3_Country",  "DM",    "Spain",         "Tactical",        "1996-03-12",  2.8,          0.02,

  # ── EQUITY: L3 EM Countries ─────────────────────────────────────────────────
  25L, "EQ.L3.EM.IND",   "INDA",    "iShares MSCI India",                 "Equity",      "L3_Country",  "EM",    "India",         "Tactical",        "2012-02-02",  2.8,          0.02,
  26L, "EQ.L3.EM.CHN",   "FXI",     "iShares China Large-Cap",            "Equity",      "L3_Country",  "EM",    "China",         "Tactical",        "2004-10-05",  3.0,          0.03,
  27L, "EQ.L3.EM.TWN",   "EWT",     "iShares MSCI Taiwan",                "Equity",      "L3_Country",  "EM",    "Taiwan",        "Tactical",        "2000-06-20",  2.8,          0.02,
  28L, "EQ.L3.EM.KOR",   "EWY",     "iShares MSCI South Korea",           "Equity",      "L3_Country",  "EM",    "Korea",         "Tactical",        "2000-05-09",  2.8,          0.02,
  29L, "EQ.L3.EM.BRA",   "EWZ",     "iShares MSCI Brazil",                "Equity",      "L3_Country",  "EM",    "Brazil",        "Tactical",        "2000-07-10",  3.5,          0.04,

  # ── EQUITY: L3 US Sectors (full GICS 11) ────────────────────────────────────
  30L, "EQ.L3.US.SECT",  "XLK",     "Technology Select Sector",           "Equity",      "L3_Sector",   "US",    "Tech",          "Core-Growth",     "1998-12-22",  2.5,          0.02,
  31L, "EQ.L3.US.SECT",  "XLC",     "Communication Services Select",      "Equity",      "L3_Sector",   "US",    "Comms",         "Core-Growth",     "2018-06-18",  2.5,          0.02,
  32L, "EQ.L3.US.SECT",  "XLV",     "Health Care Select Sector",          "Equity",      "L3_Sector",   "US",    "HealthCare",    "Core-Growth",     "1998-12-22",  2.2,          0.01,
  33L, "EQ.L3.US.SECT",  "XLF",     "Financial Select Sector",            "Equity",      "L3_Sector",   "US",    "Financials",    "Tactical",        "1998-12-22",  2.5,          0.02,
  34L, "EQ.L3.US.SECT",  "XLI",     "Industrial Select Sector",           "Equity",      "L3_Sector",   "US",    "Industrials",   "Tactical",        "1998-12-22",  2.5,          0.02,
  35L, "EQ.L3.US.SECT",  "XLP",     "Consumer Staples Select",            "Equity",      "L3_Sector",   "US",    "Staples",       "Core-Stabilizer", "1998-12-22",  2.0,          0.01,
  36L, "EQ.L3.US.SECT",  "XLY",     "Consumer Disc Select Sector",        "Equity",      "L3_Sector",   "US",    "Discretionary", "Tactical",        "1998-12-22",  2.5,          0.02,
  37L, "EQ.L3.US.SECT",  "XLE",     "Energy Select Sector",               "Equity",      "L3_Sector",   "US",    "Energy",        "Real-Shield",     "1998-12-22",  3.0,          0.03,
  38L, "EQ.L3.US.SECT",  "XLB",     "Materials Select Sector",            "Equity",      "L3_Sector",   "US",    "Materials",     "Real-Shield",     "1998-12-22",  2.5,          0.02,
  39L, "EQ.L3.US.SECT",  "XLRE",    "Real Estate Select Sector",          "Equity",      "L3_Sector",   "US",    "REIT_Sector",   "Core-Income",     "2015-10-07",  2.5,          0.02,
  40L, "EQ.L3.US.SECT",  "XLU",     "Utilities Select Sector",            "Equity",      "L3_Sector",   "US",    "Utilities",     "Core-Stabilizer", "1998-12-22",  2.0,          0.01,

  # ── EQUITY: Core Factors ─────────────────────────────────────────────────────
  41L, "EQ.FAC.US",      "QUAL",    "iShares MSCI USA Quality",           "Equity",      "Factor",      "US",    "Quality",       "Core-Factor",     "2013-07-16",  2.2,          0.01,
  42L, "EQ.FAC.US",      "MTUM",    "iShares MSCI USA Momentum",          "Equity",      "Factor",      "US",    "Momentum",      "Core-Factor",     "2013-04-16",  2.5,          0.02,
  43L, "EQ.FAC.US",      "USMV",    "iShares MSCI USA Min Vol",           "Equity",      "Factor",      "US",    "MinVol",        "Core-Factor",     "2011-10-18",  2.0,          0.01,
  44L, "EQ.FAC.US",      "VTV",     "Vanguard Value",                     "Equity",      "Factor",      "US",    "Value",         "Core-Factor",     "2004-01-26",  2.2,          0.01,
  45L, "EQ.FAC.US",      "DGRW",    "WisdomTree US Dividend Growth",      "Equity",      "Factor",      "US",    "DivGrowth",     "Core-Factor",     "2013-05-22",  2.2,          0.01,
  46L, "EQ.FAC.US",      "COWZ",    "Pacer US Cash Cows 100",             "Equity",      "Factor",      "US",    "CashFlow",      "Core-Factor",     "2016-12-16",  2.2,          0.01,
  47L, "EQ.FAC.GL",      "ACWV",    "iShares MSCI ACWI Min Vol",          "Equity",      "Factor",      "Global","MinVol_Glbl",   "Core-Factor",     "2011-10-18",  2.0,          0.01,

  # ── FIXED INCOME: US Government Curve ───────────────────────────────────────
  48L, "FI.GOV.US.CASH", "SGOV",    "iShares 0-3 Month Treasury",         "FixedIncome", "FI_Gov",      "US",    "Cash",          "Core-Stabilizer", "2020-08-04",  1.5,          0.005,
  49L, "FI.GOV.US.S",    "SHY",     "iShares 1-3 Year Treasury",          "FixedIncome", "FI_Gov",      "US",    "Curve_S",       "Core-Stabilizer", "2002-07-22",  2.0,          0.01,
  50L, "FI.GOV.US.M",    "IEI",     "iShares 3-7 Year Treasury",          "FixedIncome", "FI_Gov",      "US",    "Curve_M",       "Core-Stabilizer", "2007-01-05",  2.0,          0.01,
  51L, "FI.GOV.US.L",    "IEF",     "iShares 7-10 Year Treasury",         "FixedIncome", "FI_Gov",      "US",    "Curve_L",       "Core-Stabilizer", "2002-07-22",  2.0,          0.01,
  52L, "FI.GOV.US.XL",   "TLT",     "iShares 20+ Year Treasury",          "FixedIncome", "FI_Gov",      "US",    "Curve_XL",      "Core-Stabilizer", "2002-07-22",  2.5,          0.02,

  # ── FIXED INCOME: TIPS ───────────────────────────────────────────────────────
  53L, "FI.TIPS.US",     "TIP",     "iShares TIPS Bond",                  "FixedIncome", "FI_TIPS",     "US",    "TIPS_Broad",    "Real-Shield",     "2003-12-04",  2.0,          0.01,
  54L, "FI.TIPS.US.L",   "LTPZ",    "PIMCO 15+ Year TIPS",                "FixedIncome", "FI_TIPS",     "US",    "TIPS_Long",     "Real-Shield",     "2009-09-03",  2.5,          0.02,

  # ── FIXED INCOME: US Investment Grade ───────────────────────────────────────
  55L, "FI.IG.US.AGG",   "AGG",     "iShares Core US Agg Bond",           "FixedIncome", "FI_IG",       "US",    "Agg",           "Core-Stabilizer", "2003-09-22",  2.0,          0.01,
  56L, "FI.IG.US.CORP",  "LQD",     "iShares iBoxx IG Corp Bond",         "FixedIncome", "FI_IG",       "US",    "IG_Corp",       "Core-Income",     "2002-07-22",  2.2,          0.01,
  57L, "FI.IG.US.CORP",  "VCIT",    "Vanguard Intermediate IG Corp",      "FixedIncome", "FI_IG",       "US",    "IG_Corp_M",     "Core-Income",     "2009-11-19",  2.2,          0.01,
  58L, "FI.IG.US.MUNI",  "MUB",     "iShares National Muni Bond",         "FixedIncome", "FI_IG",       "US",    "Muni",          "Core-Income",     "2007-09-07",  1.8,          0.01,

  # ── FIXED INCOME: High Yield ─────────────────────────────────────────────────
  59L, "FI.HY.US",       "HYG",     "iShares iBoxx HY Corp Bond",         "FixedIncome", "FI_HY",       "US",    "HY_Corp",       "Tactical",        "2007-04-04",  2.5,          0.02,

  # ── FIXED INCOME: International / EM / Hedge ────────────────────────────────
  60L, "FI.INTL.DM",     "BNDX",    "Vanguard Total Intl Bond Hedged",    "FixedIncome", "FI_Intl",     "DM",    "Intl_Agg",      "Core-Stabilizer", "2013-05-31",  2.0,          0.01,
  61L, "FI.EM.HARD",     "EMB",     "iShares JP Morgan USD EM Bond",      "FixedIncome", "FI_EM",       "EM",    "EM_Hard",       "Tactical",        "2007-12-17",  2.5,          0.02,
  62L, "FI.EM.LOC",      "EMLC",    "VanEck Local Currency EM Bond",      "FixedIncome", "FI_EM",       "EM",    "EM_Local",      "Tactical",        "2010-07-22",  2.5,          0.02,
  63L, "FI.HDG.US.RATE", "PFIX",    "Simplify Interest Rate Hedge",       "FixedIncome", "FI_Hedge",    "US",    "Rate_Hedge",    "Real-Shield",     "2021-05-11",  3.0,          0.03,

  # ── COMMODITY: Broad ─────────────────────────────────────────────────────────
  64L, "CM.BROAD.MU",    "PDBC",    "Invesco Opt Yield Commodity",        "Commodity",   "Cmdty_Broad", "Multi", "Cmdty_Broad",   "Real-Shield",     "2014-11-07",  2.5,          0.02,
  65L, "CM.BROAD.MU",    "DJP",     "iPath Bloomberg Commodity",          "Commodity",   "Cmdty_Broad", "Multi", "Cmdty_Broad",   "Real-Shield",     "2006-10-23",  2.5,          0.02,

  # ── COMMODITY: Precious Metals ───────────────────────────────────────────────
  66L, "CM.PM.MU.GOLD",  "GLD",     "SPDR Gold Shares",                   "Commodity",   "Cmdty_PM",    "Multi", "Gold",          "Real-Shield",     "2004-11-18",  2.2,          0.02,
  67L, "CM.PM.MU.SLVR",  "SLV",     "iShares Silver Trust",               "Commodity",   "Cmdty_PM",    "Multi", "Silver",        "Real-Shield",     "2006-04-21",  3.0,          0.03,

  # ── COMMODITY: Agriculture ───────────────────────────────────────────────────
  68L, "CM.AGRI.MU",     "MOO",     "VanEck Agribusiness",                "Commodity",   "Cmdty_Agri",  "Multi", "Agri_Equity",   "Real-Shield",     "2007-08-31",  2.5,          0.02,
  69L, "CM.AGRI.MU",     "DBA",     "Invesco DB Agriculture",             "Commodity",   "Cmdty_Agri",  "Multi", "Agri_Futures",  "Real-Shield",     "2007-01-05",  2.5,          0.02,

  # ── COMMODITY: Metals & Resources ────────────────────────────────────────────
  70L, "CM.MET.MU.COP",  "COPX",    "Global X Copper Miners",             "Commodity",   "Cmdty_Metal", "Multi", "Copper",        "Real-Shield",     "2010-09-16",  3.0,          0.03,
  71L, "CM.MET.MU.URA",  "URA",     "Global X Uranium ETF",               "Commodity",   "Cmdty_Metal", "Multi", "Uranium",       "Satellite",       "2010-11-04",  4.0,          0.05,
  72L, "CM.MET.MU.LIT",  "LIT",     "Global X Lithium and Battery Tech",  "Commodity",   "Cmdty_Metal", "Multi", "Lithium",       "Satellite",       "2010-07-22",  3.5,          0.04,

  # ── FX ───────────────────────────────────────────────────────────────────────
  73L, "FX.DM.USD",      "UUP",     "Invesco DB USD Index Bullish",       "FX",          "FX",          "US",    "USD",           "Signal",          "2007-02-20",  1.5,          0.01,
  74L, "FX.DM.EUR",      "FXE",     "Invesco CurrencyShares EUR/USD",     "FX",          "FX",          "DM",    "EUR",           "Signal",          "2005-12-09",  1.5,          0.01,
  75L, "FX.DM.JPY",      "FXY",     "Invesco CurrencyShares JPY",         "FX",          "FX",          "DM",    "JPY",           "Signal",          "2007-02-12",  1.5,          0.01,
  76L, "FX.DM.GBP",      "FXB",     "Invesco CurrencyShares GBP",         "FX",          "FX",          "DM",    "GBP",           "Signal",          "2006-06-26",  1.5,          0.01,
  77L, "FX.DM.CHF",      "FXF",     "Invesco CurrencyShares CHF",         "FX",          "FX",          "DM",    "CHF",           "Signal",          "2006-06-26",  1.5,          0.01,

  # ── REAL ASSETS ──────────────────────────────────────────────────────────────
  78L, "RA.INFRA.GL",    "IGF",     "iShares Global Infrastructure",      "RealAsset",   "Infra",       "Global","Infra",         "Core-Income",     "2007-12-10",  2.2,          0.01,
  79L, "RA.REIT.US",     "IYR",     "iShares US Real Estate",             "RealAsset",   "REIT",        "US",    "REIT_US",       "Core-Income",     "2000-06-12",  2.8,          0.03,
  80L, "RA.REIT.DM",     "VNQI",    "Vanguard Intl Real Estate",          "RealAsset",   "REIT",        "DM",    "REIT_Intl",     "Tactical",        "2010-11-01",  2.5,          0.02,

  # ── EQUITY: Satellites — Healthcare ─────────────────────────────────────────
  81L, "EQ.SAT.US.HC",   "VHT",     "Vanguard Health Care",               "Equity",      "Satellite",   "US",    "HC_Broad",      "Satellite",       "2004-01-26",  2.2,          0.01,
  82L, "EQ.SAT.US.HC",   "IHI",     "iShares Medical Devices",            "Equity",      "Satellite",   "US",    "MedDevices",    "Satellite",       "2006-05-01",  2.8,          0.03,
  83L, "EQ.SAT.US.HC",   "IBB",     "iShares Nasdaq Biotech",             "Equity",      "Satellite",   "US",    "Biotech",       "Satellite",       "2001-02-05",  3.5,          0.04,
  84L, "EQ.SAT.US.HC",   "XBI",     "SPDR S&P Biotech",                   "Equity",      "Satellite",   "US",    "Biotech",       "Satellite",       "2006-01-31",  4.0,          0.05,

  # ── EQUITY: Satellites — Technology ─────────────────────────────────────────
  85L, "EQ.SAT.US.TECH", "SMH",     "VanEck Semiconductor",               "Equity",      "Satellite",   "US",    "Semis",         "Satellite",       "2011-12-20",  3.5,          0.05,
  86L, "EQ.SAT.US.TECH", "CIBR",    "First Trust Cybersecurity",          "Equity",      "Satellite",   "US",    "Cyber",         "Satellite",       "2015-07-07",  2.8,          0.02,
  87L, "EQ.SAT.US.TECH", "WCLD",    "WisdomTree Cloud Computing",         "Equity",      "Satellite",   "US",    "Cloud",         "Satellite",       "2019-09-16",  3.5,          0.04,
  88L, "EQ.SAT.GL.TECH", "AIQ",     "Global X AI and Big Data",           "Equity",      "Satellite",   "Global","AI",            "Satellite",       "2018-09-12",  3.0,          0.03,

  # ── EQUITY: Satellites — Defense / Housing / Transport / Financials ─────────
  89L, "EQ.SAT.US.DEF",  "ITA",     "iShares US Aerospace and Defense",   "Equity",      "Satellite",   "US",    "Defense",       "Satellite",       "2006-05-01",  3.0,          0.03,
  90L, "EQ.SAT.US.HSG",  "ITB",     "iShares US Home Construction",       "Equity",      "Satellite",   "US",    "Housing",       "Satellite",       "2006-05-01",  3.0,          0.03,
  91L, "EQ.SAT.US.FIN",  "KRE",     "SPDR S&P Regional Banking",          "Equity",      "Satellite",   "US",    "Reg_Banks",     "Satellite",       "2006-06-19",  3.0,          0.03,
  92L, "EQ.SAT.US.TRP",  "IYT",     "iShares US Transportation",          "Equity",      "Satellite",   "US",    "Transport",     "Satellite",       "2003-10-06",  2.8,          0.02,

  # ── EQUITY: Satellites — Thematic ────────────────────────────────────────────
  93L, "EQ.SAT.GL.PE",   "PSP",     "Invesco Listed Private Equity",      "Equity",      "Satellite",   "Global","Private_Eq",    "Satellite",       "2006-10-24",  3.5,          0.04,
  94L, "EQ.SAT.US.IPO",  "IPO",     "Renaissance IPO ETF",                "Equity",      "Satellite",   "US",    "IPO",           "Satellite",       "2013-10-14",  3.5,          0.04,
  95L, "EQ.SAT.US.INC",  "JEPI",    "JPMorgan Equity Premium Income",     "Equity",      "Satellite",   "US",    "Eq_Income",     "Core-Income",     "2020-05-20",  2.0,          0.01,

  # ── ALTERNATIVE ──────────────────────────────────────────────────────────────
  96L, "AL.SAT.GL.CRY",  "IBIT",    "iShares Bitcoin Trust",              "Alternative", "Satellite",   "Global","Crypto",        "Satellite",       "2024-01-11",  5.0,          0.08,

  # ── MULTI-ASSET: Balanced benchmarks ─────────────────────────────────────────
  97L,  "MA.BAL.GL.GRW",  "AOR",     "iShares Growth Allocation",          "MultiAsset",  "Balanced",    "Global","Balanced_G",    "Signal",          "2008-11-04",  2.2,          0.01,
  98L,  "MA.BAL.GL.CON",  "AOK",     "iShares Conservative Allocation",    "MultiAsset",  "Balanced",    "Global","Balanced_C",    "Signal",          "2008-11-04",  1.8,          0.01,

  # ── DAA / TAA OVERLAY TOOLS ──────────────────────────────────────────────────
  99L,  "AL.MFT.MU",      "DBMF",    "iM DBi Managed Futures Strategy",    "Alternative", "MgdFutures",  "Multi", "MgdFut",        "DAA",             "2019-05-14",  3.0,          0.03,
  100L, "AL.MFT.MU",      "CTA",     "Simplify Managed Futures Strategy",  "Alternative", "MgdFutures",  "Multi", "MgdFut_Alt",    "DAA",             "2021-03-25",  3.0,          0.03,
  101L, "AL.MKN.US",      "BTAL",    "AGFiQ US Market Neutral Anti-Beta",  "Alternative", "MktNeutral",  "US",    "Mkt_Neutral",   "DAA",             "2011-09-15",  2.5,          0.02,
  102L, "AL.TAIL.US",     "TAIL",    "Cambria Tail Risk ETF",              "Alternative", "TailRisk",    "US",    "Tail_Hedge",    "DAA",             "2017-04-05",  3.0,          0.03,
  103L, "AL.HED.US",      "SH",      "ProShares Short S&P 500",            "Alternative", "Hedge",       "US",    "Short_SPY",     "DAA",             "2006-06-19",  3.5,          0.04,
  104L, "MA.RP.GL",       "RPAR",    "RPAR Risk Parity ETF",               "MultiAsset",  "RiskParity",  "Global","RiskParity",    "DAA",             "2019-12-12",  2.5,          0.02,

  # ── EUR-HEDGED UCITS ──────────────────────────────────────────────────────────
  105L, "EQ.L1.GL.EURH",  "IWDE.AS", "iShares MSCI World EUR Hedged",      "Equity",      "L1_World",    "Global","World_EURHdg",  "Tactical",        "2010-06-16",  2.2,          0.01,
  106L, "EQ.L2.US.EURH",  "IUES.AS", "iShares S&P 500 EUR Hedged",         "Equity",      "L2_Region",   "US",    "SP500_EURHdg", "Tactical",        "2010-06-16",  2.0,          0.01,
  107L, "FI.HY.US.EURH",  "IHYE.L",  "iShares USD HY Corp Bond EUR Hedged","FixedIncome", "FI_HY",       "US",    "HY_EURHdg",    "Tactical",        "2017-11-06",  2.5,          0.02,
  108L, "CM.PM.MU.EURH",  "IGLD.DE", "iShares Physical Gold EUR Hedged ETC","Commodity",  "Cmdty_PM",    "Multi", "Gold_EURHdg",  "Real-Shield",     "2022-06-16",  2.2,          0.02,

  # ── DOW 30 Individual Stocks ───────────────────���─────────────────────────────
  109L, "EQ.L3.US.DOW",   "AAPL",   "Apple Inc",                          "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            3.0,          0.03,
  110L, "EQ.L3.US.DOW",   "AMGN",   "Amgen Inc",                          "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            3.0,          0.03,
  111L, "EQ.L3.US.DOW",   "AMZN",   "Amazon.com Inc",                     "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            3.0,          0.03,
  112L, "EQ.L3.US.DOW",   "AXP",    "American Express Co",                "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            3.0,          0.03,
  113L, "EQ.L3.US.DOW",   "BA",     "Boeing Co",                          "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            3.5,          0.04,
  114L, "EQ.L3.US.DOW",   "CAT",    "Caterpillar Inc",                    "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            3.0,          0.03,
  115L, "EQ.L3.US.DOW",   "CRM",    "Salesforce Inc",                     "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            3.0,          0.03,
  116L, "EQ.L3.US.DOW",   "CSCO",   "Cisco Systems Inc",                  "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            2.5,          0.02,
  117L, "EQ.L3.US.DOW",   "CVX",    "Chevron Corp",                       "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            3.0,          0.03,
  118L, "EQ.L3.US.DOW",   "DIS",    "Walt Disney Co",                     "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            3.0,          0.03,
  119L, "EQ.L3.US.DOW",   "GS",     "Goldman Sachs Group",                "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            3.0,          0.03,
  120L, "EQ.L3.US.DOW",   "HD",     "Home Depot Inc",                     "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            2.5,          0.02,
  121L, "EQ.L3.US.DOW",   "HON",    "Honeywell Intl",                     "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            2.5,          0.02,
  122L, "EQ.L3.US.DOW",   "IBM",    "IBM Corp",                           "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            2.5,          0.02,
  123L, "EQ.L3.US.DOW",   "JNJ",    "Johnson & Johnson",                  "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            2.5,          0.02,
  124L, "EQ.L3.US.DOW",   "JPM",    "JPMorgan Chase",                     "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            3.0,          0.03,
  125L, "EQ.L3.US.DOW",   "KO",     "Coca-Cola Co",                       "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            2.0,          0.01,
  126L, "EQ.L3.US.DOW",   "MCD",    "McDonald's Corp",                    "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            2.5,          0.02,
  127L, "EQ.L3.US.DOW",   "MMM",    "3M Co",                              "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            2.5,          0.02,
  128L, "EQ.L3.US.DOW",   "MRK",    "Merck & Co",                         "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            2.5,          0.02,
  129L, "EQ.L3.US.DOW",   "MSFT",   "Microsoft Corp",                     "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            2.5,          0.02,
  130L, "EQ.L3.US.DOW",   "NKE",    "Nike Inc",                           "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            3.0,          0.03,
  131L, "EQ.L3.US.DOW",   "NVDA",   "NVIDIA Corp",                        "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            4.0,          0.05,
  132L, "EQ.L3.US.DOW",   "PG",     "Procter & Gamble",                   "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            2.0,          0.01,
  133L, "EQ.L3.US.DOW",   "SHW",    "Sherwin-Williams",                   "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            2.5,          0.02,
  134L, "EQ.L3.US.DOW",   "TRV",    "Travelers Cos",                      "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            2.5,          0.02,
  135L, "EQ.L3.US.DOW",   "UNH",    "UnitedHealth Group",                 "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            3.0,          0.03,
  136L, "EQ.L3.US.DOW",   "V",      "Visa Inc",                           "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            2.5,          0.02,
  137L, "EQ.L3.US.DOW",   "VZ",     "Verizon Communications",             "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            2.5,          0.02,
  138L, "EQ.L3.US.DOW",   "WMT",    "Walmart Inc",                        "Equity",      "Stock",       "US",    "DOW30",        "Satellite",       NA,            2.0,          0.01
)


################################################################################
# SHORT NAMES — curated labels ≤ 18 chars for display in charts / treemaps.
# Joined into etf_metadata after DOW30 extension so DOW30 stocks automatically
# fall back to their company name (already short enough from tidyquant).
################################################################################

.short_names <- tribble(
  ~ticker,    ~short_name,
  # ── Equity: L1 World ─────────────────────────────────────────────────────────
  "URTH",     "MSCI World",
  "ACWI",     "MSCI ACWI",
  "ACWX",     "ACWI ex-US",
  # ── Equity: L2 US Broad ──────────────────────────────────────────────────────
  "SPY",      "S&P 500",
  "VTI",      "Total US Mkt",
  "QQQ",      "Nasdaq 100",
  "IJH",      "S&P 400 Mid",
  "IWM",      "Russell 2000",
  # ── Equity: L2 DM & EM ───────────────────────────────────────────────────────
  "IEFA",     "MSCI EAFE",
  "VWO",      "EM Equity",
  "AAXJ",     "Asia ex-Japan",
  # ── Equity: L2 Europe ────────────────────────────────────────────────────────
  "FEZ",      "Euro Stoxx 50",
  "EZU",      "MSCI Eurozone",
  "IEV",      "Europe Broad",
  # ── Equity: L3 DM Countries ──────────────────────────────────────────────────
  "EWJ",      "Japan",
  "DXJ",      "Japan Hedged",
  "DAX",      "Germany DAX",
  "EWQ",      "France",
  "EWL",      "Switzerland",
  "EWU",      "UK Equity",
  "EWC",      "Canada",
  "EWA",      "Australia",
  "EWI",      "Italy",
  "EWP",      "Spain",
  # ── Equity: L3 EM Countries ──────────────────────────────────────────────────
  "INDA",     "India",
  "FXI",      "China Large-Cap",
  "EWT",      "Taiwan",
  "EWY",      "South Korea",
  "EWZ",      "Brazil",
  # ── Equity: L3 US Sectors ────────────────────────────────────────────────────
  "XLK",      "Tech Sector",
  "XLC",      "Comm Services",
  "XLV",      "Health Care",
  "XLF",      "Financials",
  "XLI",      "Industrials",
  "XLP",      "Cons Staples",
  "XLY",      "Cons Disc",
  "XLE",      "Energy",
  "XLB",      "Materials",
  "XLRE",     "Real Estate",
  "XLU",      "Utilities",
  # ── Equity: Factors ──────────────────────────────────────────────────────────
  "QUAL",     "Quality",
  "MTUM",     "Momentum",
  "USMV",     "Min Vol",
  "VTV",      "Value",
  "DGRW",     "Div Growth",
  "COWZ",     "Cash Flow",
  "ACWV",     "Glbl Min Vol",
  # ── Fixed Income: US Govt ────────────────────────────────────────────────────
  "SGOV",     "Cash / T-Bills",
  "SHY",      "Tsy 1-3Y",
  "IEI",      "Tsy 3-7Y",
  "IEF",      "Tsy 7-10Y",
  "TLT",      "Tsy 20Y+",
  # ── Fixed Income: TIPS ───────────────────────────────────────────────────────
  "TIP",      "TIPS Broad",
  "LTPZ",     "TIPS Long",
  # ── Fixed Income: IG ─────────────────────────────────────────────────────────
  "AGG",      "US Agg Bond",
  "LQD",      "IG Corp Bond",
  "VCIT",     "IG Corp Mid",
  "MUB",      "Muni Bond",
  # ── Fixed Income: HY / Intl / EM / Hedge ─────────────────────────────────────
  "HYG",      "High Yield",
  "BNDX",     "Intl Bond Hdg",
  "EMB",      "EM Bond Hard",
  "EMLC",     "EM Bond Local",
  "PFIX",     "Rate Hedge",
  # ── Commodity ────────────────────────────────────────────────────────────────
  "PDBC",     "Cmdty Broad",
  "DJP",      "Cmdty Index",
  "GLD",      "Gold",
  "SLV",      "Silver",
  "MOO",      "Agribusiness",
  "DBA",      "Agriculture",
  "COPX",     "Copper Miners",
  "URA",      "Uranium",
  "LIT",      "Lithium",
  # ── FX ───────────────────────────────────────────────────────────────────────
  "UUP",      "USD Index",
  "FXE",      "EUR/USD",
  "FXY",      "JPY",
  "FXB",      "GBP",
  "FXF",      "CHF",
  # ── Real Assets ──────────────────────────────────────────────────────────────
  "IGF",      "Glbl Infra",
  "IYR",      "US REIT",
  "VNQI",     "Intl REIT",
  # ── Satellites: Healthcare ───────────────────────────────────────────────────
  "VHT",      "Health Care Bd",
  "IHI",      "Med Devices",
  "IBB",      "Biotech",
  "XBI",      "Biotech Small",
  # ── Satellites: Technology ───────────────────────────────────────────────────
  "SMH",      "Semiconductors",
  "CIBR",     "Cybersecurity",
  "WCLD",     "Cloud",
  "AIQ",      "AI & Big Data",
  # ── Satellites: Other ────────────────────────────────────────────────────────
  "ITA",      "Aerospace/Def",
  "ITB",      "Homebuilders",
  "KRE",      "Reg Banks",
  "IYT",      "Transport",
  "PSP",      "Private Equity",
  "IPO",      "IPO",
  "JEPI",     "Eq Premium Inc",
  # ── Alternative ──────────────────────────────────────────────────────────────
  "IBIT",     "Bitcoin",
  # ── Multi-Asset ──────────────────────────────────────────────────────────────
  "AOR",      "Growth Alloc",
  "AOK",      "Consv Alloc",
  # ── DAA / Overlay ────────────────────────────────────────────────────────────
  "DBMF",     "Mgd Futures",
  "CTA",      "Mgd Futures Alt",
  "BTAL",     "Mkt Neutral",
  "TAIL",     "Tail Risk",
  "SH",       "Short S&P 500",
  "RPAR",     "Risk Parity",
  # ── EUR-Hedged UCITS ─────────────────────────────────────────────────────────
  "IWDE.AS",  "MSCI World EUR",
  "IUES.AS",  "S&P 500 EUR",
  "IHYE.L",   "HY Bond EUR",
  "IGLD.DE",  "Gold EUR Hdg",
  # ── DOW 30 Stocks ─────────────────────────────────────────────────────────────
  "AAPL",     "Apple",
  "AMGN",     "Amgen",
  "AMZN",     "Amazon",
  "AXP",      "Amex",
  "BA",       "Boeing",
  "CAT",      "Caterpillar",
  "CRM",      "Salesforce",
  "CSCO",     "Cisco",
  "CVX",      "Chevron",
  "DIS",      "Disney",
  "GS",       "Goldman Sachs",
  "HD",       "Home Depot",
  "HON",      "Honeywell",
  "IBM",      "IBM",
  "JNJ",      "J&J",
  "JPM",      "JPMorgan",
  "KO",       "Coca-Cola",
  "MCD",      "McDonald's",
  "MMM",      "3M",
  "MRK",      "Merck",
  "MSFT",     "Microsoft",
  "NKE",      "Nike",
  "NVDA",     "NVIDIA",
  "PG",       "P&G",
  "SHW",      "Sherwin-Williams",
  "TRV",      "Travelers",
  "UNH",      "UnitedHealth",
  "V",        "Visa",
  "VZ",       "Verizon",
  "WMT",      "Walmart"
)

# Join short_name; DOW30 stocks fall back to their company name
etf_metadata <- etf_metadata %>%
  left_join(.short_names, by = "ticker") %>%
  mutate(short_name = coalesce(short_name, name))
rm(.short_names)

################################################################################
# DYNAMIC LOOKUP VECTORS
################################################################################

all_tickers   <- etf_metadata$ticker

cat_tickers   <- etf_metadata %>%
  group_split(pf_function) %>%
  set_names(map_chr(., ~unique(.x$pf_function))) %>%
  map(~.x$ticker)

class_tickers <- etf_metadata %>%
  group_split(asset_class) %>%
  set_names(map_chr(., ~unique(.x$asset_class))) %>%
  map(~.x$ticker)

tree_tickers  <- etf_metadata %>%
  group_split(tree_level) %>%
  set_names(map_chr(., ~unique(.x$tree_level))) %>%
  map(~.x$ticker)

block_tickers <- etf_metadata %>%
  group_split(sub_block) %>%
  set_names(map_chr(., ~unique(.x$sub_block))) %>%
  map(~.x$ticker)

################################################################################
# GLOBAL TICKER VECTORS  (g_* prefix — available in every sourced context)
#
# Derived from etf_metadata — no hard-coding.  Add new groups here as needed.
# Convention: g_<scope>_tickers
################################################################################

# ── By tree level ─────────────────────────────────────────────────────────────
g_sector_tickers  <- tree_tickers[["L3_Sector"]]      # US GICS sectors (XLK, XLV, …)
g_region_tickers  <- tree_tickers[["L2_Region"]]      # Regional equity ETFs
g_factor_tickers  <- tree_tickers[["Factor"]]         # Factor/smart-beta ETFs

# ── By asset class ────────────────────────────────────────────────────────────
g_eq_tickers      <- class_tickers[["Equity"]]        # All equity (incl. sectors)
g_fi_tickers      <- class_tickers[["FixedIncome"]]   # All fixed income
g_cmdty_tickers   <- class_tickers[["Commodity"]]     # Commodities (GLD, SLV, USO, …)
g_alt_tickers     <- class_tickers[["Alternative"]]   # Alternatives / hedge-fund-like
g_multi_tickers   <- class_tickers[["MultiAsset"]]    # Multi-asset / balanced ETFs

# ── By portfolio function ─────────────────────────────────────────────────────
g_anchor_tickers  <- cat_tickers[["Anchor"]]          # Core anchors (SPY, URTH, AGG)
g_growth_tickers  <- cat_tickers[["Core-Growth"]]     # Core growth (QQQ, XLK, …)
g_stabilizer_tickers <- cat_tickers[["Core-Stabilizer"]]  # Volatility dampeners
g_satellite_tickers  <- cat_tickers[["Satellite"]]    # High-conviction satellites
g_tactical_tickers   <- cat_tickers[["Tactical"]]     # TAA / momentum tools

# ── Composite convenience groups ─────────────────────────────────────────────
g_saa_tickers <- etf_metadata %>%
  filter(pf_function %in% c("Anchor", "Core-Growth", "Core-Stabilizer",
                             "Core-Income", "Core-Factor", "Satellite")) %>%
  pull(ticker)                                         # SAA universe (no Signal/DAA)

g_eq_nonsector_tickers <- setdiff(g_eq_tickers, g_sector_tickers)
                                                       # Equity minus GICS sectors

################################################################################
# CONSOLE REPORT
################################################################################

message("--- SOVEREIGN UNIVERSE FINALIZED (", nrow(etf_metadata), " TICKERS | incl. 6 DAA tools) ---")
message("  seq_id: 1 to ", max(etf_metadata$seq_id))
iwalk(cat_tickers, ~message("  [", .y, "]: ", length(.x), " tickers"))
message("--- Universe Loaded & Ready ---")

################################################################################
# END OF FILE
################################################################################
