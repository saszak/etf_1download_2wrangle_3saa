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
