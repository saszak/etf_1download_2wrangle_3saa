# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts/dashboard_etf_performance.R
# Purpose: Bloomberg-style ETF Performance Dashboard
#          Sparklines · %1D/%5D/%1M/%YTD · Sector grouping · Dark theme
# Run with: shiny::runApp("scripts/dashboard_etf_performance.R")
# ==============================================================================

library(shiny)
library(bslib)
library(reactable)
library(plotly)
library(tidyverse)
library(htmltools)
library(here)
library(scales)
library(lubridate)

# ── Data ───────────────────────────────────────────────────────────────────────
if (!exists("project_tree")) source(here("project_tree.R"))
if (!exists("etf_metadata")) source(here(project_tree$scripts$init))

raw_data       <- read_rds(here(project_tree$products$raw_p_d))
tech_summary   <- read_rds(here(project_tree$products$tech_summary))
outlier_report <- read_rds(here(project_tree$products$ref_report))

today     <- Sys.Date()
ytd_start <- as.Date(paste0(year(today), "-01-01"))
mtd_start <- floor_date(today, "month")
w52_start <- today - 365

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
  if (is.na(value)) return(div(style = "color:#555", "—"))
  bg  <- if (value >= 0) "#14532d" else "#7f1d1d"
  fg  <- if (value >= 0) "#86efac" else "#fca5a5"
  div(style = paste0(
    "background:", bg, "; color:", fg, "; font-weight:700;",
    "padding:1px 7px; border-radius:3px; font-size:12px;",
    "text-align:right; display:inline-block; min-width:62px;"),
    sprintf("%+.2f%%", value * 100))
}

# ── Build performance table ────────────────────────────────────────────────────
perf_data <- raw_data %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(adjusted = as.numeric(adjusted)) %>%
  summarise(
    latest_price = last(adjusted),
    latest_date  = last(date),

    # Period returns (arithmetic)
    ret_1d  = (last(adjusted) / nth(adjusted, -2L))  - 1,
    ret_5d  = (last(adjusted) / nth(adjusted, -6L))  - 1,
    ret_1m  = (last(adjusted) / nth(adjusted, -22L)) - 1,
    ret_ytd = (last(adjusted) / first(adjusted[date >= ytd_start])) - 1,

    # 52-week range
    low_52w   = min(adjusted[date >= w52_start],  na.rm = TRUE),
    high_52w  = max(adjusted[date >= w52_start],  na.rm = TRUE),
    range_pct = pmin(pmax(
      (last(adjusted) - min(adjusted[date >= w52_start], na.rm = TRUE)) /
      (max(adjusted[date >= w52_start], na.rm = TRUE) -
       min(adjusted[date >= w52_start], na.rm = TRUE)), 0), 1),

    # Sparkline: last 60 prices as SVG string
    spark = .svg_spark(adjusted),

    .groups = "drop"
  ) %>%
  left_join(
    etf_metadata %>% select(ticker, name, asset_class, pf_function,
                            tree_level, sub_block),
    by = c("symbol" = "ticker")
  ) %>%
  left_join(
    tech_summary %>% select(ticker, trend_regime, momentum_status),
    by = c("symbol" = "ticker")
  ) %>%
  left_join(
    outlier_report %>% select(ticker, label),
    by = c("symbol" = "ticker")
  ) %>%
  filter(!is.na(ret_ytd)) %>%
  mutate(
    asset_class = coalesce(asset_class, "Other"),
    sub_block   = coalesce(sub_block,   "Other")
  )

# ── Custom cluster map ─────────────────────────────────────────────────────────
CLUSTER_MAP <- tibble::tribble(
  ~ticker,  ~cluster,
  # Core Assets — world benchmarks + top regions
  "URTH",   "Core Assets",
  "ACWI",   "Core Assets",
  "SPY",    "Core Assets",
  "QQQ",    "Core Assets",
  "IEFA",   "Core Assets",
  "ACWX",   "Core Assets",
  # SPY Sectors
  "XLK",    "SPY Sectors",
  "XLC",    "SPY Sectors",
  "XLV",    "SPY Sectors",
  "XLF",    "SPY Sectors",
  "XLI",    "SPY Sectors",
  "XLP",    "SPY Sectors",
  "XLY",    "SPY Sectors",
  "XLE",    "SPY Sectors",
  "XLB",    "SPY Sectors",
  "XLRE",   "SPY Sectors",
  "XLU",    "SPY Sectors"
)

perf_data <- perf_data %>%
  left_join(CLUSTER_MAP, by = c("symbol" = "ticker")) %>%
  mutate(cluster = coalesce(cluster, "Rest of Universe"))

# Benchmark reference row
spy_row <- perf_data %>% filter(symbol == "SPY")

# ── Theme ──────────────────────────────────────────────────────────────────────
DARK_BG    <- "#111318"
DARK_PANEL <- "#16181d"
DARK_HDR   <- "#0d0f12"
DARK_ROW   <- "#1a1c22"
DARK_STRIPE<- "#1e2028"
DARK_BORDER<- "#2a2c35"
TEXT_MAIN  <- "#d1d5db"
TEXT_DIM   <- "#6b7280"
BLUE_TICK  <- "#60a5fa"

tbl_theme <- reactableTheme(
  backgroundColor = DARK_BG,
  color           = TEXT_MAIN,
  borderColor     = DARK_BORDER,
  stripedColor    = DARK_STRIPE,
  highlightColor  = "#252830",
  headerStyle     = list(
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
    borderBottom    = paste0("1px solid #3a3d48")
  ),
  rowStyle         = list(borderBottom = paste0("1px solid ", DARK_BORDER)),
  cellStyle        = list(display = "flex", alignItems = "center")
)

# ── Column definitions ─────────────────────────────────────────────────────────
col_defs <- list(

  symbol = colDef(
    name  = "Ticker", width = 72,
    style = list(fontWeight = "700", color = BLUE_TICK, fontSize = "13px")
  ),

  name = colDef(
    name = "Name", minWidth = 170, maxWidth = 260,
    style = list(color = TEXT_DIM, fontSize = "11px"),
    cell  = function(v) div(title = v, style = "overflow:hidden;
                            text-overflow:ellipsis; white-space:nowrap;",
                            coalesce(v, ""))
  ),

  spark = colDef(
    name = "", width = 90, sortable = FALSE,
    html = TRUE,
    cell = function(v) v
  ),

  latest_price = colDef(
    name   = "Last Px", width = 72,
    format = colFormat(digits = 2),
    style  = list(fontSize = "12px", fontWeight = "600")
  ),

  ret_1d = colDef(
    name = "%1D", width = 78, html = TRUE,
    cell = function(v) .ret_cell(v)
  ),

  ret_5d = colDef(
    name = "%5D", width = 78, html = TRUE,
    cell = function(v) .ret_cell(v)
  ),

  ret_1m = colDef(
    name = "%1M", width = 78, html = TRUE,
    cell = function(v) .ret_cell(v)
  ),

  ret_ytd = colDef(
    name = "%YTD", width = 82, html = TRUE,
    cell = function(v) .ret_cell(v)
  ),

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
             paste0(pct, "%"))
      )
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
          TEXT_DIM
        ),
        fontWeight = "600", fontSize = "12px"
      )
    }
  ),

  sub_block = colDef(
    name = "Block", width = 130,
    style = list(color = TEXT_DIM, fontSize = "11px")
  ),

  # Hidden columns (used for groupBy)
  asset_class   = colDef(show = FALSE),
  latest_date   = colDef(show = FALSE),
  low_52w       = colDef(show = FALSE),
  high_52w      = colDef(show = FALSE),
  pf_function = colDef(
    name  = "PF Function", width = 130,
    style = function(value) {
      color <- switch(coalesce(value, ""),
        "Anchor"         = "#60a5fa",
        "Core-Growth"    = "#34d399",
        "Core-Stabilizer"= "#a78bfa",
        "Core-Factor"    = "#818cf8",
        "Core-Income"    = "#fbbf24",
        "Real-Shield"    = "#f87171",
        "Satellite"      = "#fb923c",
        "Signal"         = "#94a3b8",
        "Tactical"       = "#e879f9",
        TEXT_DIM
      )
      list(color = color, fontWeight = "600", fontSize = "12px")
    }
  ),
  tree_level    = colDef(show = FALSE),
  momentum_status = colDef(show = FALSE),
  label         = colDef(show = FALSE)
)

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = tags$span(
    style = "font-weight:700; letter-spacing:0.02em;",
    "ETF Universe — Performance Screen"
  ),
  tags$head(tags$style(HTML("
    /* ── Value boxes ── */
    .bslib-value-box,
    .value-box                         { min-height: 72px !important; max-height: 88px !important; padding: 6px 10px !important; }
    .bslib-value-box .value-box-title,
    .value-box .value-box-title        { font-size: 9px  !important; line-height: 1.2 !important; text-transform: uppercase; letter-spacing: .04em;
                                         overflow: hidden !important; text-overflow: ellipsis !important; white-space: nowrap !important; }
    .bslib-value-box .value-box-value,
    .value-box .value-box-value        { font-size: 14px !important; font-weight: 700 !important; line-height: 1.2 !important;
                                         overflow: hidden !important; text-overflow: ellipsis !important; white-space: nowrap !important; }
    .bslib-value-box .value-box-area,
    .value-box .value-box-area         { overflow: hidden !important; min-width: 0 !important;
                                         display: flex !important; flex-direction: column !important;
                                         justify-content: center !important; }
    .bslib-value-box .value-box-grid,
    .value-box .value-box-grid         { align-items: center !important; }
    /* icon in showcase: halve default ~32px → 16px */
    .bslib-value-box .value-box-showcase,
    .value-box .value-box-showcase     { font-size: 14px !important; min-width: 28px !important; }
    .bslib-value-box .value-box-showcase svg,
    .value-box .value-box-showcase svg { width: 14px !important; height: 14px !important; }
    .bslib-value-box .value-box-showcase i,
    .value-box .value-box-showcase i   { font-size: 14px !important; }

    /* ── Main card fills remaining viewport ── */
    .main-card {
      height: calc(100vh - 218px) !important;
      min-height: 480px !important;
    }
    .main-card .tab-content          { height: calc(100% - 42px) !important; }
    .main-card .tab-pane.active      { height: 100% !important; display: flex !important; flex-direction: column; }
    .main-card .reactable            { flex: 1; overflow: auto; }
  "))),
  theme = bs_theme(
    bootswatch   = "darkly",
    base_font    = font_google("Inter"),
    heading_font = font_google("Inter"),
    bg           = DARK_BG,
    fg           = TEXT_MAIN
  ),

  sidebar = sidebar(
    width = 210,
    bg    = DARK_PANEL,

    h6("Filters", style = paste0("color:", TEXT_DIM, "; margin-bottom:8px;")),

    selectInput("asset_class_filter", "Asset Class",
      choices  = c("All", sort(unique(perf_data$asset_class))),
      selected = "All"),

    selectInput("pf_function_filter", "PF Function",
      choices  = c("All",
                   "Core-Assets",
                   "──────────────",
                   sort(unique(perf_data$pf_function))),
      selected = "All"),

    selectInput("sort_by", "Sort By",
      choices  = c("YTD" = "ret_ytd", "1M" = "ret_1m", "5D" = "ret_5d",
                   "1D"  = "ret_1d",  "52W Pos" = "range_pct",
                   "Ticker" = "symbol"),
      selected = "ret_ytd"),

    hr(style = paste0("border-color:", DARK_BORDER, ";")),
    selectInput("treemap_group", "Treemap grouping",
      choices  = c("Clusters" = "cluster", "Asset Class" = "asset_class"),
      selected = "cluster"),

    hr(style = paste0("border-color:", DARK_BORDER, ";")),
    checkboxInput("relative_mode", "Relative to BMK", value = FALSE),
    conditionalPanel(
      condition = "input.relative_mode",
      selectInput("bmk", "Benchmark",
        choices  = c("SPY", "URTH"),
        selected = "SPY")
    ),

    hr(style = paste0("border-color:", DARK_BORDER, ";")),
    checkboxInput("hide_signal",    "Hide Signal tickers",  value = FALSE),
    checkboxInput("only_bullish",   "Bullish regime only",  value = FALSE),
    checkboxInput("group_by_class", "Group by Asset Class", value = FALSE),

    hr(style = paste0("border-color:", DARK_BORDER, ";")),
    p(style = paste0("font-size:11px; color:", TEXT_DIM, ";"),
      paste0("Data as of: ", format(today, "%b %d, %Y")))
  ),

  # KPI strip
  layout_columns(
    col_widths = c(3, 3, 3, 3), fill = FALSE,
    value_box(
      title    = "SPY YTD",
      value    = sprintf("%+.1f%%", (spy_row$ret_ytd %||% 0) * 100),
      theme    = if ((spy_row$ret_ytd %||% 0) >= 0) "success" else "danger",
      showcase = if ((spy_row$ret_ytd %||% 0) >= 0) icon("arrow-up") else icon("arrow-down")
    ),
    value_box(
      title    = "SPY 1M",
      value    = sprintf("%+.1f%%", (spy_row$ret_1m %||% 0) * 100),
      theme    = if ((spy_row$ret_1m %||% 0) >= 0) "success" else "danger",
      showcase = icon("calendar")
    ),
    value_box(
      title    = "Bullish Breadth",
      value    = textOutput("bullish_pct", inline = TRUE),
      theme    = "secondary",
      showcase = icon("shield")
    ),
    value_box(
      title    = "Universe",
      value    = textOutput("n_shown", inline = TRUE),
      theme    = "secondary",
      showcase = icon("th")
    )
  ),

  card(
    class       = "main-card",
    full_screen = TRUE,
    tabsetPanel(
      id   = "main_tabs",
      type = "tabs",

      # ── Tab 1: Performance Table ───────────────────────────────────────────
      tabPanel(
        title = "Table",
        div(style = paste0("background:", DARK_HDR, "; color:", TEXT_DIM,
                           "; font-size:11px; padding:5px 12px; margin-bottom:4px;"),
            textOutput("table_caption", inline = TRUE)),
        reactableOutput("perf_table",
                        height = "calc(100vh - 290px)")
      ),

      # ── Tab 2: Finviz Treemap ──────────────────────────────────────────────
      tabPanel(
        title = "Treemap",
        div(style = paste0("background:", DARK_HDR, "; color:", TEXT_DIM,
                           "; font-size:11px; padding:5px 12px;",
                           " display:flex; gap:18px; margin-bottom:4px;"),
            span("Equal tiles  \u2502  Colour = YTD return  \u2502  Corner label = 1M return  ",
                 span(style = "color:#ef4444; font-weight:700;", "\u25a0 negative"),
                 "  ",
                 span(style = "color:#22c55e; font-weight:700;", "\u25a0 positive"))),
        plotlyOutput("treemap",
                     height = "calc(100vh - 290px)")
      )
    )
  )
)

# ── Server ──────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  filtered <- reactive({
    df <- perf_data
    if (input$hide_signal)                   df <- df %>% filter(pf_function != "Signal")
    if (input$asset_class_filter != "All")   df <- df %>% filter(asset_class == input$asset_class_filter)
    if (input$pf_function_filter == "Core-Assets") {
      df <- df %>% filter(pf_function %in% c("Anchor", "Core-Growth", "Core-Stabilizer"))
    } else if (!input$pf_function_filter %in% c("All", "── Core-Assets ──", "──────────────")) {
      df <- df %>% filter(pf_function == input$pf_function_filter)
    }
    if (input$only_bullish)                  df <- df %>% filter(trend_regime == "Bullish")

    # Relative mode: subtract benchmark returns from all return columns
    if (isTRUE(input$relative_mode)) {
      bmk_row <- perf_data %>% filter(symbol == input$bmk) %>% slice(1)
      if (nrow(bmk_row) == 1) {
        df <- df %>% mutate(
          ret_1d  = ret_1d  - bmk_row$ret_1d,
          ret_5d  = ret_5d  - bmk_row$ret_5d,
          ret_1m  = ret_1m  - bmk_row$ret_1m,
          ret_ytd = ret_ytd - bmk_row$ret_ytd
        )
      }
    }

    df %>% arrange(desc(.data[[input$sort_by]]))
  })

  output$bullish_pct <- renderText({
    df <- filtered()
    sprintf("%d / %d", sum(df$trend_regime == "Bullish", na.rm = TRUE), nrow(df))
  })

  output$n_shown <- renderText({
    paste0(nrow(filtered()), " ETFs")
  })

  output$table_caption <- renderText({
    paste0("Sorted by: ", input$sort_by,
           " | Data as of ", format(today, "%d %b %Y"))
  })

  # ── Treemap ──────────────────────────────────────────────────────────────────
  output$treemap <- renderPlotly({
    df        <- filtered()
    grp_col   <- input$treemap_group   # "asset_class" or "cluster"

    # Finviz diverging colorscale:
    #   negative side: light pink (near zero) → deep red (large loss)
    #   positive side: light green (near zero) → deep green (large gain)
    finviz_scale <- list(
      c(0.00, "#7f0000"),   # large negative  → deep red
      c(0.30, "#cc2222"),   # moderate negative → medium red
      c(0.46, "#f4aaaa"),   # small negative   → light pink
      c(0.50, "#555555"),   # zero             → mid-grey
      c(0.54, "#a3f0bc"),   # small positive   → light green
      c(0.70, "#1a7a2e"),   # moderate positive → medium green
      c(1.00, "#00AA44")    # large positive   → deep green
    )

    # Build two-level hierarchy: group → ticker
    # (no root node — avoids the empty tile plotly renders for values = 0)

    # Helper: emoji circle based on return sign
    dot <- function(x) dplyr::case_when(
      x >  0.01 ~ "\U1F7E2",   # 🟢 green
      x < -0.01 ~ "\U1F534",   # 🔴 red
      TRUE      ~ "\u26AA"     # ⚪ grey (near-zero)
    )

    # Group nodes — equal weight = count of tickers, colour = median YTD
    cluster_order <- c("Core Assets", "SPY Sectors", "Rest of Universe")

    sectors <- df %>%
      group_by(grp = .data[[grp_col]]) %>%
      summarise(
        n_tickers = n(),
        color     = median(ret_ytd * 100, na.rm = TRUE),
        ret1m_med = median(ret_1m  * 100, na.rm = TRUE),
        .groups   = "drop"
      ) %>%
      mutate(
        ids       = grp,
        labels    = grp,
        parents   = "",
        values    = n_tickers,
        tile_text = sprintf("<b>%s</b><br>YTD med: %+.1f%%<br>%s 1M med: %+.1f%%",
                            grp, color, dot(ret1m_med / 100), ret1m_med),
        hover     = sprintf("%s<br>YTD median: %+.1f%%<br>1M median: %+.1f%%",
                            grp, color, ret1m_med),
        ord       = match(grp, cluster_order, nomatch = 99L)
      ) %>%
      arrange(ord) %>%
      select(-ord)

    # Ticker leaf nodes — all values = 1 → equal tile sizes
    tickers <- df %>%
      mutate(
        ids       = symbol,
        labels    = symbol,
        parents   = .data[[grp_col]],
        values    = 1,
        color     = ret_ytd * 100,
        tile_text = sprintf("<b>%s</b><br>YTD: %+.1f%%<br>%s %+.1f%%",
                            symbol, ret_ytd * 100, dot(ret_1m), ret_1m * 100),
        hover     = sprintf(
          "<b>%s</b><br>%s<br>YTD: %+.1f%%<br>1M: %+.1f%%<br>1D: %+.1f%%",
          symbol, coalesce(name, ""), ret_ytd * 100, ret_1m * 100, ret_1d * 100
        )
      ) %>%
      select(ids, labels, parents, values, color, tile_text, hover)

    nodes <- bind_rows(
      sectors %>% select(ids, labels, parents, values, color, tile_text, hover),
      tickers
    )

    plotly::plot_ly() %>%
      plotly::add_trace(
        type          = "treemap",
        ids           = nodes$ids,
        labels        = nodes$labels,
        parents       = nodes$parents,
        values        = nodes$values,
        text          = nodes$tile_text,
        customdata    = nodes$hover,
        hovertemplate = "%{customdata}<extra></extra>",
        texttemplate  = "%{text}",
        textposition  = "top left",
        textfont      = list(color = "#ffffff", size = 11, family = "Inter"),
        marker = list(
          colors     = nodes$color,
          colorscale = finviz_scale,
          cmid       = 0,
          showscale  = TRUE,
          colorbar   = list(
            title       = list(text = "YTD %", font = list(color = "#aaa", size = 11)),
            tickfont    = list(color = "#aaa", size = 10),
            tickformat  = "+.1f",
            ticksuffix  = "%",
            len         = 0.6,
            thickness   = 14,
            bgcolor     = "#111318",
            bordercolor = "#2a2c35"
          ),
          line = list(width = 1.2, color = "#111318")
        ),
        sort    = FALSE,
        tiling  = list(packing = "squarify"),
        pathbar = list(
          visible   = TRUE,
          thickness = 22,
          textfont  = list(color = "#d1d5db", size = 11)
        )
      ) %>%
      plotly::layout(
        paper_bgcolor = DARK_BG,
        plot_bgcolor  = DARK_BG,
        margin        = list(t = 10, b = 10, l = 10, r = 10),
        font          = list(color = TEXT_MAIN, family = "Inter")
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })

  # ── Performance table ─────────────────────────────────────────────────────────
  output$perf_table <- renderReactable({
    df         <- filtered()
    group_cols <- if (input$group_by_class) "asset_class" else NULL

    # Show asset_class column only when not grouping
    col_defs_live <- col_defs

    # Relative mode: update return column headers
    if (isTRUE(input$relative_mode)) {
      sfx <- paste0(" vs ", input$bmk)
      col_defs_live[["ret_1d"]]$name  <- paste0("%1D",  sfx)
      col_defs_live[["ret_5d"]]$name  <- paste0("%5D",  sfx)
      col_defs_live[["ret_1m"]]$name  <- paste0("%1M",  sfx)
      col_defs_live[["ret_ytd"]]$name <- paste0("%YTD", sfx)
    }
    col_defs_live[["asset_class"]] <- colDef(
      name  = "Asset Class", width = 120,
      show  = !input$group_by_class,
      style = list(color = TEXT_DIM, fontSize = "11px")
    )

    reactable(
      df,
      groupBy         = group_cols,
      searchable      = TRUE,
      sortable        = TRUE,
      striped         = TRUE,
      highlight       = TRUE,
      defaultPageSize = 100,
      theme           = tbl_theme,
      columns         = col_defs_live,
      columnGroups    = list(
        colGroup(name = "Returns", columns = c("ret_1d","ret_5d","ret_1m","ret_ytd")),
        colGroup(name = "Range",   columns = c("range_pct"))
      ),
      defaultColDef = colDef(vAlign = "center", headerVAlign = "bottom")
    )
  })
}

shinyApp(ui, server)
