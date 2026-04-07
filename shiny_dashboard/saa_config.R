# ==============================================================================
# shiny_dashboard/saa_config.R
# PURPOSE : SAA tree definition for Shiny app — Core hierarchical universe.
#           Separate from etf_metadata (analytical universe); this is the
#           app-layer config: what shows on launch and at each depth level.
#
# COLUMNS
#   ticker     Yahoo Finance symbol (joins to perf_data)
#   bucket     Cash | FI | EQ | Alt
#   depth      0 = minimal essential (4 anchors — launch default)
#              1 = first expansion
#              2 = US Sectors
#              3 = Country detail
#
# USAGE (in filtered reactive)
#   filter(saa_depth <= input$depth)          # progressive drill-down
#   filter(saa_bucket %in% input$bucket)      # branch focus
# ==============================================================================

# ==============================================================================
# DISPLAY_RANK : Institutional display order for Table + Treemap tiles.
#   Ordered by the market cap / index breadth each instrument represents.
#   Groups of 100 — gaps allow future insertions without renumbering.
#   Tickers not listed get rank 9999 (sorted last).
# ==============================================================================
DISPLAY_RANK <- tribble(
  ~ticker,  ~display_rank,

  # ── Core World Equity (by index breadth / AUM) ────────────────────────────
  "URTH",   101L,   # MSCI World  (~$70T)
  "ACWI",   102L,   # MSCI ACWI   (~$80T incl EM)
  "SPY",    103L,   # S&P 500     (~$45T)
  "QQQ",    104L,   # Nasdaq 100  (~$20T)
  "IEFA",   105L,   # Dev ex-US   (~$25T)
  "ACWX",   106L,   # ACWI ex-US
  "EFA",    107L,   # MSCI EAFE
  "IWM",    108L,   # Russell 2000
  "IJH",    109L,   # S&P 400 Mid
  "VWO",    110L,   # EM broad
  "AAXJ",   111L,   # Asia ex-Japan
  "FEZ",    112L,   # Euro Stoxx 50
  "EZU",    113L,   # MSCI EMU

  # ── US Sectors (by S&P 500 weight, 2025) ──────────────────────────────────
  "XLK",    201L,   # Technology       ~31%
  "XLF",    202L,   # Financials       ~13%
  "XLV",    203L,   # Health Care      ~12%
  "XLY",    204L,   # Consumer Disc    ~10%
  "XLI",    205L,   # Industrials       ~9%
  "XLC",    206L,   # Comm Services     ~9%
  "XLP",    207L,   # Consumer Staples  ~6%
  "XLE",    208L,   # Energy            ~4%
  "XLRE",   209L,   # Real Estate      ~2.5%
  "XLB",    210L,   # Materials        ~2.3%
  "XLU",    211L,   # Utilities        ~2.3%

  # ── Country ETFs (by GDP / mkt cap) ───────────────────────────────────────
  "DAX",    301L,   # Germany
  "EWQ",    302L,   # France
  "EWU",    303L,   # UK
  "EWL",    304L,   # Switzerland
  "EWI",    305L,   # Italy
  "EWP",    306L,   # Spain
  "EWA",    307L,   # Australia
  "FXI",    308L,   # China
  "INDA",   309L,   # India
  "EWZ",    310L,   # Brazil
  "EWY",    311L,   # South Korea

  # ── Fixed Income (by market size / duration) ──────────────────────────────
  "AGG",    401L,   # US Agg (broadest)
  "BND",    402L,   # Total Bond
  "TLT",    403L,   # 20Y+ Treasury
  "IEF",    404L,   # 7-10Y Treasury
  "IEI",    405L,   # 3-7Y Treasury
  "SHY",    406L,   # 1-3Y Treasury
  "SGOV",   407L,   # 0-3M T-Bills
  "LQD",    408L,   # IG Corporate
  "HYG",    409L,   # High Yield
  "EMB",    410L,   # EM Sovereign USD
  "TIP",    411L,   # TIPS
  "MBB",    412L,   # Mortgage-Backed

  # ── Commodities (by global market size) ───────────────────────────────────
  "GLD",    501L,   # Gold
  "SLV",    502L,   # Silver
  "GSG",    503L,   # Broad Commodities
  "USO",    504L,   # Crude Oil
  "DJP",    505L,   # Bloomberg Commodity

  # ── Alternatives / Thematics ──────────────────────────────────────────────
  "SMH",    601L,   # Semiconductors
  "AIQ",    602L,   # AI & Big Data
  "SKYY",   603L,   # Cloud
  "URA",    604L,   # Uranium
  "ARKG",   605L,   # Genomics
  "ICLN",   606L,   # Clean Energy
  "DXJ",    607L,   # Japan Hedged

  # ── DOW 30 (by approximate market cap, 2025) ──────────────────────────────
  "AAPL",  1001L,
  "MSFT",  1002L,
  "NVDA",  1003L,
  "AMZN",  1004L,
  "V",     1005L,
  "JPM",   1006L,
  "UNH",   1007L,
  "WMT",   1008L,
  "PG",    1009L,
  "JNJ",   1010L,
  "GS",    1011L,
  "HD",    1012L,
  "CAT",   1013L,
  "CSCO",  1014L,
  "IBM",   1015L,
  "AXP",   1016L,
  "CVX",   1017L,
  "MCD",   1018L,
  "HON",   1019L,
  "CRM",   1020L,
  "DIS",   1021L,
  "AMGN",  1022L,
  "SHW",   1023L,
  "TRV",   1024L,
  "NKE",   1025L,
  "VZ",    1026L,
  "MMM",   1027L,
  "MRK",   1028L,
  "KO",    1029L,
  "BA",    1030L
)

SAA_CONFIG <- tribble(
  ~ticker,  ~saa_bucket, ~saa_depth,

  # ── Depth 0: Minimal essential (launch default) ───────────────────────────
  "SGOV",   "Cash",      0L,
  "AGG",    "FI",        0L,
  "URTH",   "EQ",        0L,
  "GLD",    "Alt",       0L,

  # ── Depth 1: First expansion ──────────────────────────────────────────────
  "SHY",    "Cash",      1L,
  "IEI",    "FI",        1L,
  "IEF",    "FI",        1L,
  "TLT",    "FI",        1L,
  "LQD",    "FI",        1L,
  "HYG",    "FI",        1L,
  "SPY",    "EQ",        1L,
  "QQQ",    "EQ",        1L,
  "IWM",    "EQ",        1L,
  "IJH",    "EQ",        1L,
  "IEFA",   "EQ",        1L,
  "VWO",    "EQ",        1L,
  "AAXJ",   "EQ",        1L,
  "FEZ",    "EQ",        1L,
  "EZU",    "EQ",        1L,

  # ── Depth 2: US Sectors ───────────────────────────────────────────────────
  "XLK",    "EQ",        2L,
  "XLC",    "EQ",        2L,
  "XLV",    "EQ",        2L,
  "XLF",    "EQ",        2L,
  "XLI",    "EQ",        2L,
  "XLP",    "EQ",        2L,
  "XLY",    "EQ",        2L,
  "XLE",    "EQ",        2L,
  "XLB",    "EQ",        2L,
  "XLRE",   "EQ",        2L,
  "XLU",    "EQ",        2L,

  # ── Depth 3: Country detail ───────────────────────────────────────────────
  "DAX",    "EQ",        3L,
  "EWQ",    "EQ",        3L,
  "EWL",    "EQ",        3L,
  "EWU",    "EQ",        3L,
  "EWA",    "EQ",        3L,
  "EWI",    "EQ",        3L,
  "EWP",    "EQ",        3L,
  "FXI",    "EQ",        3L,
  "INDA",   "EQ",        3L,
  "EWZ",    "EQ",        3L,
  "EWY",    "EQ",        3L
)
