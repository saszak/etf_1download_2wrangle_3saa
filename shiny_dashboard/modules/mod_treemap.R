# ==============================================================================
# shiny_dashboard/modules/mod_treemap.R
# PURPOSE : Tab holder — two treemap views:
#   ETF      : Finviz-style ETF universe treemap (filtered by sidebar)
#   Equities : DOW30 stocks nested inside GICS sector ETFs (fixed view)
# ==============================================================================

# ── DOW30 → GICS sector ETF lookup (hardcoded; stable) ────────────────────────
.DOW30_SECTORS <- tibble::tribble(
  ~ticker, ~sector_etf, ~sector_name,
  "AAPL",  "XLK",  "Technology",
  "MSFT",  "XLK",  "Technology",
  "NVDA",  "XLK",  "Technology",
  "IBM",   "XLK",  "Technology",
  "CRM",   "XLK",  "Technology",
  "CSCO",  "XLK",  "Technology",
  "UNH",   "XLV",  "Health Care",
  "JNJ",   "XLV",  "Health Care",
  "MRK",   "XLV",  "Health Care",
  "AMGN",  "XLV",  "Health Care",
  "JPM",   "XLF",  "Financials",
  "GS",    "XLF",  "Financials",
  "AXP",   "XLF",  "Financials",
  "V",     "XLF",  "Financials",
  "TRV",   "XLF",  "Financials",
  "HD",    "XLY",  "Consumer Disc",
  "MCD",   "XLY",  "Consumer Disc",
  "NKE",   "XLY",  "Consumer Disc",
  "AMZN",  "XLY",  "Consumer Disc",
  "DIS",   "XLC",  "Comm Services",
  "VZ",    "XLC",  "Comm Services",
  "BA",    "XLI",  "Industrials",
  "CAT",   "XLI",  "Industrials",
  "HON",   "XLI",  "Industrials",
  "MMM",   "XLI",  "Industrials",
  "CVX",   "XLE",  "Energy",
  "KO",    "XLP",  "Consumer Staples",
  "PG",    "XLP",  "Consumer Staples",
  "WMT",   "XLP",  "Consumer Staples",
  "SHW",   "XLB",  "Materials"
)

# ── Colour scale — B2 Finviz Institutional ────────────────────────────────────
.finviz_scale <- list(
  c(0.00, "#922b21"),   # deep institutional red
  c(0.25, "#e74c3c"),   # medium red
  c(0.45, "#fde8e6"),   # pale blush
  c(0.50, "#f5f5f5"),   # neutral cream
  c(0.55, "#e8f8f0"),   # pale mint
  c(0.75, "#27ae60"),   # medium green
  c(1.00, "#1a5e35")    # deep forest green
)

.dot <- function(x) dplyr::case_when(
  x >  0.01 ~ "\U1F7E2",
  x < -0.01 ~ "\U1F534",
  TRUE       ~ "\u26AA"
)

# ── UI ─────────────────────────────────────────────────────────────────────────
mod_treemap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = paste0("background:", DARK_HDR, "; color:", TEXT_DIM,
                       "; font-size:11px; padding:5px 12px; margin-bottom:4px;",
                       " display:flex; gap:18px;"),
        span("Equal tiles  \u2502  Colour = YTD return  \u2502  Corner label = 1M return  ",
             span(style = "color:#ef4444; font-weight:700;", "\u25a0 negative"),
             "  ",
             span(style = "color:#22c55e; font-weight:700;", "\u25a0 positive"))),
    tabsetPanel(
      type = "tabs",
      tabPanel("ETF",
        plotlyOutput(ns("treemap"), height = "calc(100vh - 330px)")
      ),
      tabPanel("Equities",
        plotlyOutput(ns("eq_treemap"), height = "calc(100vh - 330px)")
      ),
      tabPanel("Top Picks",
        tagList(
          div(
            style = paste0(
              "background:#e8eaed; font-size:11px; padding:4px 12px; margin-bottom:2px;",
              " display:flex; gap:18px; align-items:center; color:#2c3e50;"
            ),
            span("\u2605 = Top Pick  \u2502  Colour = Expected Return  \u2502  Hover for analyst detail"),
            div(
              style = "margin-left:auto; display:flex; gap:10px; align-items:center;",
              span("Group by:"),
              radioButtons(ns("tp_group"), NULL,
                choices  = c("Sector" = "sector", "Region" = "region"),
                selected = "sector", inline = TRUE
              ),
              span(style = "margin-left:8px;", "Region:"),
              checkboxGroupInput(ns("tp_region"), NULL,
                choices  = c("US", "EU", "CH", "ROW"),
                selected = c("US", "EU", "CH", "ROW"),
                inline   = TRUE
              ),
              span(style = "margin-left:8px;", "Filter:"),
              checkboxInput(ns("tp_only_picks"), "Top Picks only", value = FALSE),
              checkboxInput(ns("tp_only_buy"),   "Buy rated only",  value = FALSE)
            )
          ),
          plotlyOutput(ns("tp_treemap"), height = "calc(100vh - 360px)")
        )
      )
    )
  )
}

# ── Server ─────────────────────────────────────────────────────────────────────
mod_treemap_server <- function(id, filtered, treemap_group,
                               relative_mode = reactive(FALSE),
                               bmk           = reactive("SPY")) {
  moduleServer(id, function(input, output, session) {

    # ── ETF treemap (existing logic, unchanged) ────────────────────────────────
    output$treemap <- renderPlotly({
      df      <- filtered()
      grp_col <- treemap_group()

      cluster_order <- c("Core Assets", "SPY Sectors", "Rest of Universe")

      sectors <- df %>%
        group_by(grp = .data[[grp_col]]) %>%
        summarise(
          n_tickers = n(),
          color     = median(ret_ytd * 100, na.rm = TRUE),
          ret_mtd   = median(ret_mtd  * 100, na.rm = TRUE),
          .groups   = "drop"
        ) %>%
        mutate(
          ids       = grp,
          labels    = grp,
          parents   = "",
          values    = n_tickers,
          tile_text = sprintf(
            "<b>%s</b><br>YTD <b>%+.1f%%</b><br><span style='font-size:9px'>MTD %+.1f%%</span>",
            grp, color, ret_mtd),
          hover     = sprintf(
            "<b>%s</b><br>YTD median: %+.1f%%<br>MTD median: %+.1f%%<br>%d tickers",
            grp, color, ret_mtd, n_tickers),
          ord       = match(grp, cluster_order, nomatch = 99L)
        ) %>%
        arrange(ord) %>%
        select(-ord)

      tickers <- df %>%
        arrange(display_rank) %>%
        mutate(
          ids       = symbol,
          labels    = symbol,
          parents   = .data[[grp_col]],
          values    = 1,
          color     = ret_ytd * 100,
          tile_text = sprintf(
            "<b>%s</b><br><span style='font-size:9px'>%s</span><br>%+.1f%%<br><span style='font-size:9px'>MTD %+.1f%%</span>",
            symbol, coalesce(short_name, ""),
            ret_ytd * 100, ret_mtd * 100),
          hover     = sprintf(
            "<b>%s</b> \u2014 %s<br>YTD: %+.1f%%<br>MTD: %+.1f%%<br>1D: %+.1f%%",
            symbol, coalesce(name, ""),
            ret_ytd * 100, ret_mtd * 100, ret_1d * 100)
        ) %>%
        select(ids, labels, parents, values, color, tile_text, hover)

      nodes <- bind_rows(
        sectors %>% select(ids, labels, parents, values, color, tile_text, hover),
        tickers
      )

      .build_treemap_plotly(nodes)
    })

    # ── Equities treemap (DOW30 inside sector ETFs) ────────────────────────────
    output$eq_treemap <- renderPlotly({

      rel  <- isTRUE(relative_mode())
      b    <- bmk()
      pfx  <- if (rel) paste0("vs ", b, " ") else ""

      # Benchmark row for relative adjustment
      bmk_row <- perf_data %>% filter(symbol == b) %>% slice(1)
      bmk_ytd <- if (rel && nrow(bmk_row) == 1) bmk_row$ret_ytd else 0
      bmk_mtd <- if (rel && nrow(bmk_row) == 1) bmk_row$ret_mtd else 0
      bmk_1d  <- if (rel && nrow(bmk_row) == 1) bmk_row$ret_1d  else 0

      # Sector ETF performance (parents)
      sect_perf <- perf_data %>%
        filter(symbol %in% .DOW30_SECTORS$sector_etf) %>%
        select(symbol, ret_ytd, ret_mtd, ret_1m, ret_1d) %>%
        mutate(
          ret_ytd = ret_ytd - bmk_ytd,
          ret_mtd = ret_mtd - bmk_mtd,
          ret_1d  = ret_1d  - bmk_1d
        )

      # DOW30 stock performance (children) — only those in perf_data
      stock_perf <- perf_data %>%
        filter(symbol %in% .DOW30_SECTORS$ticker) %>%
        select(symbol, name, ret_ytd, ret_mtd, ret_1m, ret_1d) %>%
        mutate(
          ret_ytd = ret_ytd - bmk_ytd,
          ret_mtd = ret_mtd - bmk_mtd,
          ret_1d  = ret_1d  - bmk_1d
        )

      if (nrow(stock_perf) == 0) {
        return(plotly::plot_ly() %>%
          plotly::layout(
            paper_bgcolor = DARK_BG,
            annotations   = list(list(
              text      = "DOW30 data not yet downloaded \u2014 run 01_etf_wrangle.R",
              x = 0.5, y = 0.5, xref = "paper", yref = "paper",
              showarrow = FALSE, font = list(color = TEXT_DIM, size = 14)
            ))
          ))
      }

      # Map stocks to sectors; keep only stocks with a known sector
      stocks_mapped <- .DOW30_SECTORS %>%
        inner_join(stock_perf, by = c("ticker" = "symbol"))

      # Sector parent nodes
      sector_nodes <- stocks_mapped %>%
        group_by(sector_etf, sector_name) %>%
        summarise(n = n(), .groups = "drop") %>%
        left_join(sect_perf, by = c("sector_etf" = "symbol")) %>%
        mutate(
          ytd = coalesce(ret_ytd, 0) * 100,
          mtd = coalesce(ret_mtd, 0) * 100,
          d1  = coalesce(ret_1d,  0) * 100,
          lbl = paste0(sector_etf, " \u2014 ", sector_name),
          ids       = sector_etf,
          labels    = lbl,
          parents   = "",
          values    = n,
          color     = ytd,
          tile_text = sprintf(
            "<b>%s</b><br>%sYTD <b>%+.1f%%</b><br><span style='font-size:9px'>%sMTD %+.1f%%</span>",
            lbl, pfx, ytd, pfx, mtd),
          hover = sprintf(
            "<b>%s</b><br>%sYTD: %+.1f%%<br>%sMTD: %+.1f%%<br>%s1D: %+.1f%%<br>%d stocks",
            lbl, pfx, ytd, pfx, mtd, pfx, d1, n)
        ) %>%
        select(ids, labels, parents, values, color, tile_text, hover)

      # Stock child nodes
      stock_nodes <- stocks_mapped %>%
        mutate(
          ytd = ret_ytd * 100,
          mtd = ret_mtd * 100,
          d1  = ret_1d  * 100,
          ids       = ticker,
          labels    = ticker,
          parents   = sector_etf,
          values    = 1,
          color     = ytd,
          tile_text = sprintf(
            "<b>%s</b><br>%+.1f%%<br><span style='font-size:9px'>%sMTD %+.1f%%</span>",
            ticker, ytd, pfx, mtd),
          hover = sprintf(
            "<b>%s</b> \u2014 %s<br>%sYTD: %+.1f%%<br>%sMTD: %+.1f%%<br>%s1D: %+.1f%%",
            ticker, coalesce(name, ""), pfx, ytd, pfx, mtd, pfx, d1)
        ) %>%
        select(ids, labels, parents, values, color, tile_text, hover)

      nodes <- bind_rows(sector_nodes, stock_nodes)

      .build_treemap_plotly(nodes)
    })

    # ── Top Picks treemap (EquityCoreList) ────────────────────────────────────
    output$tp_treemap <- renderPlotly({
      grp_raw <- if (is.null(input$tp_group)) "sector" else input$tp_group
      grp     <- if (grp_raw == "region") "region_grp" else grp_raw

      sel_regions <- if (is.null(input$tp_region) || length(input$tp_region) == 0)
        c("US", "EU", "CH", "ROW") else input$tp_region

      df <- eq_coverage %>%
        filter(!is.na(exp_ret), !is.na(Company)) %>%
        mutate(region_grp = dplyr::case_when(
          region == "USA/Canada"              ~ "US",
          region == "Europe"                  ~ "EU",
          region == "Switzerland"             ~ "CH",
          TRUE                                ~ "ROW"
        )) %>%
        filter(region_grp %in% sel_regions)

      if (isTRUE(input$tp_only_picks)) df <- df %>% filter(top_pick)
      if (isTRUE(input$tp_only_buy))   df <- df %>% filter(rating == "Buy")

      if (nrow(df) == 0) {
        return(plotly::plot_ly() %>%
          plotly::layout(
            paper_bgcolor = "#f8f9fa",
            annotations = list(list(
              text = "No stocks match current filters.",
              x = 0.5, y = 0.5, xref = "paper", yref = "paper",
              showarrow = FALSE, font = list(color = "#7f8c8d", size = 14)
            ))
          ))
      }

      grp_col  <- df[[grp]]
      exp_pct  <- round(df$exp_ret * 100, 1)

      # Parent nodes: group summaries
      parents <- df %>%
        mutate(.grp = .data[[grp]]) %>%
        group_by(.grp) %>%
        summarise(
          n          = n(),
          n_picks    = sum(top_pick, na.rm = TRUE),
          med_exp    = median(exp_ret, na.rm = TRUE) * 100,
          med_upside = median(upside_pct, na.rm = TRUE),
          .groups    = "drop"
        ) %>%
        mutate(
          ids       = .grp,
          labels    = .grp,
          parents   = "",
          values    = n,
          color     = med_exp,
          tile_text = sprintf(
            "<b>%s</b><br>Exp Ret <b>%+.1f%%</b><br><span style='font-size:9px'>%d stocks \u2502 %d \u2605</span>",
            .grp, med_exp, n, n_picks
          ),
          hover = sprintf(
            "<b>%s</b><br>Median exp return: %+.1f%%<br>Median upside: %+.1f%%<br>%d stocks (%d Top Picks)",
            .grp, med_exp, med_upside, n, n_picks
          )
        ) %>%
        select(ids, labels, parents, values, color, tile_text, hover)

      # Child nodes: individual stocks
      children <- df %>%
        mutate(
          .grp      = .data[[grp]],
          star      = if_else(top_pick, "\u2605 ", ""),
          rat_col   = if_else(rating == "Buy", "#1a5e35", "#7f8c8d"),
          ids       = ticker,
          labels    = ticker,
          parents   = .grp,
          values    = 1,
          color     = exp_ret * 100,
          tile_text = sprintf(
            "<b>%s%s</b><br><span style='font-size:9px'>%s</span><br>%+.1f%%<br><span style='font-size:9px'>%s</span>",
            star, ticker,
            substr(Company, 1, 20),
            exp_ret * 100,
            rating
          ),
          hover = sprintf(
            "<b>%s\u2605</b> %s<br>%s<br>Rating: <b>%s</b> \u2502 Risk: %s<br>Exp Return: <b>%+.1f%%</b><br>Target: %s \u2502 Price: %s \u2502 Upside: %+.1f%%<br>P/E: %s \u2502 Div Yield: %s<br>EPS 25E: %s \u2502 EPS 26E: %s<br>Analyst: %s",
            if_else(top_pick, "", ""),
            ticker, Company,
            rating, coalesce(risk, ""),
            exp_ret * 100,
            ifelse(!is.na(tgt_price), sprintf("%.1f", tgt_price), "n/a"),
            ifelse(!is.na(price),     sprintf("%.1f", price),     "n/a"),
            coalesce(upside_pct, 0),
            ifelse(!is.na(pe),      sprintf("%.1fx", pe),         "n/a"),
            ifelse(!is.na(div_yield), sprintf("%.1f%%", div_yield * 100), "n/a"),
            ifelse(!is.na(eps25),   sprintf("%.2f", eps25),       "n/a"),
            ifelse(!is.na(eps26),   sprintf("%.2f", eps26),       "n/a"),
            coalesce(analyst, "")
          )
        ) %>%
        select(ids, labels, parents, values, color, tile_text, hover)

      nodes <- bind_rows(parents, children)

      .build_treemap_plotly(nodes) %>%
        plotly::layout(
          coloraxis = list(
            colorbar = list(title = list(text = "Exp Ret %"))
          )
        )
    })
  })
}

# ── Shared plotly builder — B2 Finviz Institutional ───────────────────────────
.build_treemap_plotly <- function(nodes) {
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
      textfont      = list(color = "#1c2833", size = 12, family = "Arial"),
      marker = list(
        colors     = nodes$color,
        colorscale = .finviz_scale,
        cmid       = 0,
        showscale  = TRUE,
        colorbar   = list(
          title      = list(text = "YTD %",
                            font = list(color = "#2c3e50", size = 10, family = "Arial")),
          tickfont   = list(color = "#2c3e50", size = 9, family = "Arial"),
          tickformat = "+.1f", ticksuffix = "%",
          len = 0.55, thickness = 12,
          bgcolor = "#f8f9fa", bordercolor = "#e8eaed"
        ),
        line = list(width = 1.5, color = "#ffffff")
      ),
      sort    = FALSE,
      tiling  = list(packing = "squarify"),
      pathbar = list(
        visible  = TRUE, thickness = 22,
        textfont = list(color = "#2c3e50", size = 11, family = "Arial"),
        bgcolor  = "#e8eaed"
      )
    ) %>%
    plotly::layout(
      paper_bgcolor = "#f8f9fa",
      plot_bgcolor  = "#f8f9fa",
      margin        = list(t = 6, b = 24, l = 6, r = 6),
      font          = list(color = "#1c2833", family = "Arial"),
      annotations   = list(list(
        text      = "\u25a0 Red = negative YTD   \u25a0 Green = positive YTD   |   Intensity = magnitude   |   Hover for details",
        x = 0, y = -0.02, xref = "paper", yref = "paper",
        xanchor = "left", yanchor = "top",
        showarrow = FALSE,
        font = list(color = "#7f8c8d", size = 9, family = "Arial")
      ))
    ) %>%
    plotly::config(displayModeBar = FALSE)
}
