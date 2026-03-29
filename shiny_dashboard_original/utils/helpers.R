# ==============================================================================
# shiny_dashboard/utils/helpers.R
# PURPOSE : Shared UI helpers, theme constants, and column definitions.
# ==============================================================================

# ── Theme constants ────────────────────────────────────────────────────────────
DARK_BG     <- "#111318"
DARK_PANEL  <- "#16181d"
DARK_HDR    <- "#0d0f12"
DARK_ROW    <- "#1a1c22"
DARK_STRIPE <- "#1e2028"
DARK_BORDER <- "#2a2c35"
TEXT_MAIN   <- "#d1d5db"
TEXT_DIM    <- "#6b7280"
BLUE_TICK   <- "#60a5fa"

# ── Inline SVG sparkline (last 60 trading days, normalised) ───────────────────
.svg_spark <- function(prices, w = 80, h = 28) {
  prices <- as.numeric(prices[!is.na(prices)])
  prices <- tail(prices, 60)
  if (length(prices) < 3) return("")
  mn <- min(prices); mx <- max(prices)
  if (mx == mn) return(sprintf(
    '<svg width="%d" height="%d"><line x1="0" y1="%d" x2="%d" y2="%d"
      stroke="#555" stroke-width="1"/></svg>', w, h, h / 2L, w, h / 2L))
  norm  <- (prices - mn) / (mx - mn)
  n     <- length(norm)
  xs    <- round(seq(0, w, length.out = n), 1)
  ys    <- round(h - norm * h, 1)
  pts   <- paste(sprintf("%.1f,%.1f", xs, ys), collapse = " ")
  color <- if (tail(prices, 1) >= prices[1]) "#22c55e" else "#ef4444"
  sprintf(
    '<svg width="%d" height="%d" style="overflow:visible;display:block">
       <polyline points="%s" fill="none" stroke="%s" stroke-width="1.5"
                 stroke-linejoin="round" stroke-linecap="round"/>
     </svg>', w, h, pts, color)
}

# ── Return cell renderer — solid Bloomberg-style badge ────────────────────────
.ret_cell <- function(value) {
  if (is.na(value)) return(div(style = "color:#555", "\u2014"))
  bg  <- if (value >= 0) "#14532d" else "#7f1d1d"
  fg  <- if (value >= 0) "#86efac" else "#fca5a5"
  div(style = paste0(
    "background:", bg, "; color:", fg, "; font-weight:700;",
    "padding:1px 7px; border-radius:3px; font-size:12px;",
    "text-align:right; display:inline-block; min-width:62px;"),
    sprintf("%+.2f%%", value * 100))
}

# ── reactable theme ───────────────────────────────────────────────────────────
tbl_theme <- reactableTheme(
  backgroundColor  = DARK_BG,
  color            = TEXT_MAIN,
  borderColor      = DARK_BORDER,
  stripedColor     = DARK_STRIPE,
  highlightColor   = "#252830",
  headerStyle      = list(
    backgroundColor = DARK_HDR,
    color           = "#9ca3af",
    fontWeight      = "600",
    fontSize        = "11px",
    textTransform   = "uppercase",
    letterSpacing   = "0.05em",
    borderBottom    = paste0("2px solid ", DARK_BORDER)
  ),
  groupHeaderStyle = list(
    backgroundColor = "#1c1f28",
    color           = "#e2e8f0",
    fontWeight      = "700",
    fontSize        = "12px",
    borderBottom    = "1px solid #3a3d48"
  ),
  rowStyle  = list(borderBottom = paste0("1px solid ", DARK_BORDER)),
  cellStyle = list(display = "flex", alignItems = "center")
)

# ── Column definitions ────────────────────────────────────────────────────────
col_defs <- list(

  symbol = colDef(
    name  = "Ticker", width = 72,
    style = list(fontWeight = "700", color = BLUE_TICK, fontSize = "13px")
  ),

  name = colDef(
    name = "Name", minWidth = 170, maxWidth = 260,
    style = list(color = TEXT_DIM, fontSize = "11px"),
    cell = function(v) div(
      title = v,
      style = "overflow:hidden; text-overflow:ellipsis; white-space:nowrap;",
      coalesce(v, ""))
  ),

  spark = colDef(name = "", width = 90, sortable = FALSE, html = TRUE,
                 cell = function(v) v),

  latest_price = colDef(
    name = "Last Px", width = 72,
    format = colFormat(digits = 2),
    style  = list(fontSize = "12px", fontWeight = "600")
  ),

  ret_1d  = colDef(name = "%1D",  width = 78, html = TRUE, cell = function(v) .ret_cell(v)),
  ret_5d  = colDef(name = "%5D",  width = 78, html = TRUE, cell = function(v) .ret_cell(v)),
  ret_1m  = colDef(name = "%1M",  width = 78, html = TRUE, cell = function(v) .ret_cell(v)),
  ret_ytd = colDef(name = "%YTD", width = 82, html = TRUE, cell = function(v) .ret_cell(v)),

  range_pct = colDef(
    name = "52W Range", width = 145,
    cell = function(value) {
      if (is.na(value)) return("")
      pct   <- round(value * 100)
      color <- if (value >= 0.70) "#22c55e" else
                if (value <= 0.30) "#ef4444" else "#3b82f6"
      div(style = "display:flex; align-items:center; gap:6px; width:100%;",
        div(style = "flex:1; background:#2a2c35; border-radius:3px; height:8px;",
          div(style = paste0("background:", color, "; width:", pct,
                             "%; height:8px; border-radius:3px;"))),
        span(style = "font-size:11px; color:#6b7280; min-width:30px;",
             paste0(pct, "%")))
    }
  ),

  trend_regime = colDef(
    name = "Regime", width = 135,
    style = function(value) {
      list(
        color = switch(coalesce(value, ""),
          "Bullish"            = "#22c55e",
          "Bearish"            = "#ef4444",
          "Bullish-Correction" = "#f59e0b",
          "Bearish-Relief"     = "#f97316",
          TEXT_DIM),
        fontWeight = "600", fontSize = "12px")
    }
  ),

  sub_block = colDef(name = "Block", width = 130,
                     style = list(color = TEXT_DIM, fontSize = "11px")),

  pf_function = colDef(
    name = "PF Function", width = 130,
    style = function(value) {
      color <- switch(coalesce(value, ""),
        "Anchor"          = "#60a5fa", "Core-Growth"     = "#34d399",
        "Core-Stabilizer" = "#a78bfa", "Core-Factor"     = "#818cf8",
        "Core-Income"     = "#fbbf24", "Real-Shield"     = "#f87171",
        "Satellite"       = "#fb923c", "Signal"          = "#94a3b8",
        "Tactical"        = "#e879f9", TEXT_DIM)
      list(color = color, fontWeight = "600", fontSize = "12px")
    }
  ),

  # Hidden (used for groupBy / joins only)
  asset_class     = colDef(show = FALSE),
  latest_date     = colDef(show = FALSE),
  low_52w         = colDef(show = FALSE),
  high_52w        = colDef(show = FALSE),
  tree_level      = colDef(show = FALSE),
  momentum_status = colDef(show = FALSE),
  label           = colDef(show = FALSE)
)
