# ==============================================================================
# shiny_dashboard/modules/mod_treemap.R
# PURPOSE : Tab holder — two treemap views:
#   ETF      : Finviz-style ETF universe treemap (filtered by sidebar)
#   Equities : DOW30 stocks nested inside GICS sector ETFs (fixed view)
# ==============================================================================

# ── SAA portfolio definition (Option A: role-based, weight-proportional) ───────
.SAA_TREE <- tibble::tribble(
  ~ticker, ~group,                    ~weight, ~display_label,
  # ── Allocation benchmarks (FI proxy ~35%) ──────────────────────────────────
  "AOM",   "Allocation Benchmarks",   0.117,   "AOM",
  "AOR",   "Allocation Benchmarks",   0.117,   "AOR",
  "AOK",   "Allocation Benchmarks",   0.116,   "AOK",
  # ── Fixed income anchors (~20%) ────────────────────────────────────────────
  "AGG",   "Fixed Income Anchors",    0.040,   "AGG",
  "IEF",   "Fixed Income Anchors",    0.040,   "IEF",
  "TLT",   "Fixed Income Anchors",    0.040,   "TLT",
  "HYG",   "Fixed Income Anchors",    0.040,   "HYG",
  "TIP",   "Fixed Income Anchors",    0.040,   "TIP",
  # ── Equity anchors (50%) ───────────────────────────────────────────────────
  "URTH",  "Equity Anchors",          0.250,   "URTH",
  "SPY",   "Equity Anchors",          0.250,   "SPY",
  # ── Growth satellites (15%) ────────────────────────────────────────────────
  "QQQ",   "Growth Satellites",       0.050,   "QQQ",
  "XLK",   "Growth Satellites",       0.050,   "XLK",
  "SMH",   "Growth Satellites",       0.050,   "SMH",
  # ── Alt & CCY (~15%) ───────────────────────────────────────────────────────
  "GLD",   "Alt & CCY",               0.040,   "Gold",
  "UUP",   "Alt & CCY",               0.025,   "USD",
  "FXE",   "Alt & CCY",               0.025,   "EUR",
  "FXY",   "Alt & CCY",               0.025,   "JPY",
  "FXB",   "Alt & CCY",               0.025,   "GBP",
  "FXF",   "Alt & CCY",               0.025,   "CHF"
)

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
    # ── Compact pill-button CSS ────────────────────────────────────────────────
    tags$style(HTML(paste0(
      "#", ns("float_period"), " .shiny-options-group {",
      "  display:flex; gap:2px; margin:0; padding:0; }",
      "#", ns("float_period"), " label {",
      "  margin:0; padding:2px 6px;",
      "  font-size:10px; font-weight:600; cursor:pointer;",
      "  background:#1f2937; color:#9ca3af;",
      "  border:1px solid #374151; border-radius:3px;",
      "  line-height:1.4; }",
      "#", ns("float_period"), " input[type=radio] { display:none; }",
      "#", ns("float_period"), " input[type=radio]:checked + span {",
      "  background:#3b82f6 !important; color:#fff !important;",
      "  border-color:#3b82f6 !important; }"
    ))),
    # ── Global header ─────────────────────────────────────────────────────────
    div(
      style = paste0("background:", DARK_HDR, "; color:", TEXT_DIM,
                     "; font-size:11px; padding:5px 12px; margin-bottom:4px;",
                     " display:flex; align-items:center; gap:18px;"),
      span("Equal tiles  \u2502  Colour = YTD return  \u2502  Corner label = 1M return  ",
           span(style = "color:#ef4444; font-weight:700;", "\u25a0 negative"),
           "  ",
           span(style = "color:#22c55e; font-weight:700;", "\u25a0 positive")),
      div(
        style = "margin-left:auto; display:flex; align-items:center; gap:12px;",
        div(
          style = paste0(
            "display:flex; align-items:center; gap:2px;",
            " background:#1a2332; border:1px solid #374151;",
            " border-radius:4px; padding:2px 5px;",
            " font-size:9.5px;"
          ),
          tags$style(HTML(paste0(
            "#", ns("float_plot"),  ".form-group,",
            "#", ns("float_rel"),   ".form-group,",
            "#", ns("float_large"), ".form-group { margin-bottom:0; margin-right:0; }",
            "#", ns("float_plot"),  " label,",
            "#", ns("float_rel"),   " label,",
            "#", ns("float_large"), " label { font-size:9.5px !important; }"
          ))),
          checkboxInput(ns("float_plot"),  "Float",  value = FALSE),
          checkboxInput(ns("float_rel"),   "vs BMK", value = FALSE),
          checkboxInput(ns("float_large"), "Large",  value = TRUE)
        ),
        div(
          style = "display:flex; align-items:center; gap:4px;",
          span("BMK:", style = "font-size:10px; color:#9ca3af; white-space:nowrap;"),
          tags$div(
            style = "font-size:10.5px;",
            tags$style(HTML(paste0(
              "#", ns("float_bmk"), " { height:27px !important;",
              "  padding-top:2px !important; padding-bottom:2px !important;",
              "  font-size:10.5px !important; }"
            ))),
            selectInput(ns("float_bmk"), NULL,
              choices  = c("SPY","URTH","ACWI"),
              selected = "SPY",
              width    = "72px"
            )
          )
        ),
        radioButtons(ns("float_period"), NULL,
          choices  = c("1M","3M","6M","YTD","1Y","3Y","Max"),
          selected = "1Y", inline = TRUE)
      )
    ),
    # ── Tabs — wrapped in position:relative for absolutePanel overlay ─────────
    div(
      style = "position:relative;",
      tabsetPanel(
        id   = ns("tm_tabs"),
        type = "tabs",
        tabPanel("ETF",
          plotlyOutput(ns("treemap"),     height = "calc(100vh - 345px)")
        ),
        tabPanel("SAA",
          div(
            style = paste0("background:", DARK_HDR, "; color:", TEXT_DIM, ";",
                           " font-size:11px; padding:3px 12px; margin-bottom:2px;"),
            "Tile size = SAA weight  \u2502  Colour = YTD return  \u2502  Hover for details"
          ),
          plotlyOutput(ns("saa_treemap"), height = "calc(100vh - 365px)")
        ),
        tabPanel("Equities",
          plotlyOutput(ns("eq_treemap"),  height = "calc(100vh - 345px)")
        ),
        tabPanel("6040",
          tagList(
            div(
              style = paste0(
                "background:", DARK_HDR, "; color:", TEXT_DIM, ";",
                " font-size:11px; padding:4px 12px; margin-bottom:2px;",
                " display:flex; align-items:center; gap:18px;"
              ),
              span("60/40 Portfolio  \u2502  Cumulative return vs benchmark"),
              div(
                style = "margin-left:auto; display:flex; align-items:center; gap:8px;",
                span("Portfolio:", style = "font-size:10px; color:#9ca3af;"),
                radioButtons(ns("pf6040_ccy"), NULL,
                  choices  = c("USD 60/40" = "usd", "EUR 60/40" = "eur", "CHF 60/40" = "chf"),
                  selected = "usd", inline = TRUE
                ),
                span("Period:", style = "font-size:10px; color:#9ca3af; margin-left:8px;"),
                radioButtons(ns("pf6040_period"), NULL,
                  choices  = c("YTD","1Y","3Y","Max"),
                  selected = "1Y", inline = TRUE
                )
              )
            ),
            plotOutput(ns("pf6040_chart"), height = "calc(100vh - 365px)")
          )
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
            plotlyOutput(ns("tp_treemap"), height = "calc(100vh - 375px)")
          )
        )
      ),
      uiOutput(ns("float_panel"))   # overlays all tabs
    )
  )
}

# ── Server ─────────────────────────────────────────────────────────────────────
mod_treemap_server <- function(id, filtered, treemap_group,
                               relative_mode = reactive(FALSE),
                               bmk           = reactive("SPY")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

      .build_treemap_plotly(nodes, source = "etf_tm") %>%
        plotly::event_register("plotly_hover")
    })

    # ── SAA treemap (role-based, weight-proportional) ─────────────────────────
    output$saa_treemap <- renderPlotly({

      rel     <- isTRUE(relative_mode())
      b       <- bmk()
      bmk_row <- perf_data %>% filter(symbol == b) %>% slice(1)
      bmk_ytd <- if (rel && nrow(bmk_row) == 1) bmk_row$ret_ytd else 0
      bmk_mtd <- if (rel && nrow(bmk_row) == 1) bmk_row$ret_mtd else 0
      bmk_1d  <- if (rel && nrow(bmk_row) == 1) bmk_row$ret_1d  else 0
      pfx     <- if (rel) paste0("vs ", b, " ") else ""

      saa_perf <- .SAA_TREE %>%
        left_join(
          perf_data %>% select(symbol, name, short_name, ret_ytd, ret_mtd, ret_1d),
          by = c("ticker" = "symbol")
        ) %>%
        mutate(
          ret_ytd = (coalesce(ret_ytd, 0) - bmk_ytd) * 100,
          ret_mtd = (coalesce(ret_mtd, 0) - bmk_mtd) * 100,
          ret_1d  = (coalesce(ret_1d,  0) - bmk_1d)  * 100
        )

      # Group order
      grp_order <- c("Allocation Benchmarks", "Fixed Income Anchors",
                     "Equity Anchors", "Growth Satellites", "Alt & CCY")

      # Parent nodes (groups)
      group_nodes <- saa_perf %>%
        group_by(group) %>%
        summarise(
          total_w  = sum(weight),
          med_ytd  = weighted.mean(ret_ytd, weight),
          med_mtd  = weighted.mean(ret_mtd, weight),
          n        = n(),
          .groups  = "drop"
        ) %>%
        mutate(
          ord       = match(group, grp_order, nomatch = 99L),
          ids       = group,
          labels    = group,
          parents   = "",
          values    = total_w,
          color     = med_ytd,
          tile_text = sprintf(
            "<b>%s</b><br>%sYTD <b>%+.1f%%</b><br><span style='font-size:9px'>Weight %.0f%%</span>",
            group, pfx, med_ytd, total_w * 100),
          hover = sprintf(
            "<b>%s</b><br>%sYTD (wtd avg): %+.1f%%<br>%sMTD: %+.1f%%<br>Weight: %.0f%%<br>%d tickers",
            group, pfx, med_ytd, pfx, med_mtd, total_w * 100, n)
        ) %>%
        arrange(ord) %>%
        select(ids, labels, parents, values, color, tile_text, hover)

      # Child nodes (tickers) — use display_label for tile (shows CCY codes)
      ticker_nodes <- saa_perf %>%
        mutate(
          ids       = ticker,
          labels    = display_label,
          parents   = group,
          values    = weight,
          color     = ret_ytd,
          tile_text = sprintf(
            "<b>%s</b><br><span style='font-size:9px'>%s</span><br>%sYTD <b>%+.1f%%</b><br><span style='font-size:9px'>Wt %.0f%%</span>",
            display_label, coalesce(short_name, ticker),
            pfx, ret_ytd, weight * 100),
          hover = sprintf(
            "<b>%s</b> (%s) \u2014 %s<br>%sYTD: %+.1f%%<br>%sMTD: %+.1f%%<br>%s1D: %+.1f%%<br>SAA Weight: %.0f%%",
            display_label, ticker, coalesce(name, ""),
            pfx, ret_ytd, pfx, ret_mtd, pfx, ret_1d, weight * 100)
        ) %>%
        select(ids, labels, parents, values, color, tile_text, hover)

      nodes <- bind_rows(group_nodes, ticker_nodes)

      .build_treemap_plotly(nodes, source = "saa_tm") %>%
        plotly::layout(
          annotations = list(list(
            text = paste0(
              "\u25a0 Red = negative ",   pfx, "YTD   ",
              "\u25a0 Green = positive ", pfx, "YTD   |   ",
              "Tile size = SAA weight   |   Hover for details"
            ),
            x = 0, y = -0.02, xref = "paper", yref = "paper",
            xanchor = "left", yanchor = "top",
            showarrow = FALSE,
            font = list(color = "#7f8c8d", size = 9, family = "Arial")
          ))
        ) %>%
        plotly::event_register("plotly_hover")
    })

    # ── Float plot helpers ─────────────────────────────────────────────────────
    .white_card_theme <- function() {
      theme_minimal(base_size = 10) +
        theme(
          plot.background  = element_rect(fill = "white", colour = "#e5e7eb"),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.grid.major = element_line(colour = "#f3f4f6", linewidth = 0.4),
          panel.grid.minor = element_blank(),
          plot.title       = element_text(colour = "#111827", size = 10, face = "bold"),
          plot.subtitle    = element_text(colour = "#4b5563", size = 7.5, lineheight = 1.4),
          axis.text        = element_text(colour = "#374151", size = 9.4, face = "bold"),
          axis.ticks       = element_line(colour = "#d1d5db"),
          plot.margin      = margin(8, 10, 6, 6)
        )
    }

    .no_data_plot <- function(msg) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = msg,
                 colour = "#6b7280", size = 3.2, hjust = 0.5, lineheight = 1.4) +
        theme_void() +
        theme(plot.background = element_rect(fill = "white", colour = "#e5e7eb"))
    }

    .cum_ret_pct <- function(r) as.numeric(cumprod(1 + r) - 1) * 100
    .max_dd_pct  <- function(v) {
      peak <- cummax(1 + v / 100)
      min((1 + v / 100) / peak - 1) * 100
    }

    .float_x_scales <- function(period) {
      list(
        breaks = scales::breaks_width(switch(period,
          "1M" = "1 week", "3M" = "1 month", "6M" = "1 month",
          "YTD" = "1 month", "1Y" = "3 months", "3Y" = "6 months",
          "Max" = "1 year", "3 months")),
        labels = switch(period,
          "1M" = "%d%b", "3M" = "%b%y", "6M" = "%b%y", "YTD" = "%b%y",
          "1Y" = "%b%y", "3Y" = "%b%y", "Max" = "%Y", "%b%y")
      )
    }

    .kpi_subtitle <- function(cum_tk, cum_bmk, ytd_tk, ytd_bmk, d1_tk, lbl, b) {
      rel_perf <- tail(cum_tk, 1) - tail(cum_bmk, 1)
      max_dd   <- .max_dd_pct(cum_tk)
      rel_dd   <- .max_dd_pct(cum_tk - cum_bmk)
      sprintf(
        "YTD %+.1f%%  \u2502  BMK(%s) %+.1f%%  \u2502  1D %+.1f%%  \u2502  RelPerf %+.1f%%\nMaxDD %.1f%%  \u2502  MaxRelDD %.1f%%",
        ytd_tk, b, ytd_bmk, d1_tk, rel_perf, max_dd, rel_dd
      )
    }

    # ── Universal ticker parser ───────────────────────────────────────────────
    # Handles two customdata formats:
    #   SAA    : "<b>CHF</b> (FXF) — ..."  → extracts "FXF" from parentheses
    #   ETF/EQ : "<b>SPY</b> — ..."        → extracts "SPY" from bold tag
    .parse_hover_ticker <- function(cd) {
      if (is.null(cd) || !nzchar(cd)) return(NULL)
      m <- regmatches(cd, regexpr("\\([A-Z][A-Z0-9]{0,5}\\)", cd))
      if (length(m) > 0) return(gsub("[()]", "", m))
      m <- regmatches(cd, regexpr("<b>[A-Z][A-Z0-9]{1,5}</b>", cd))
      if (length(m) > 0) return(gsub("<b>|</b>", "", m))
      NULL
    }

    # ── Multi-source hover reactive ────────────────────────────────────────────
    # Active tab's source is checked FIRST to prevent stale event data from
    # other tabs winning. All sources then fall through in order.
    hovered_ticker <- reactive({
      active_tab <- if (is.null(input$tm_tabs)) "ETF" else input$tm_tabs
      active_src <- switch(active_tab,
        "ETF"       = "etf_tm",
        "SAA"       = "saa_tm",
        "Equities"  = "eq_tm",
        NULL
      )
      sources <- c(active_src,
                   setdiff(c("saa_tm", "etf_tm", "eq_tm"), active_src))

      for (src in sources) {
        hov <- suppressWarnings(event_data("plotly_hover", source = src))
        if (is.null(hov)) next

        # Primary: parse customdata hover text
        if (!is.null(hov$customdata) && length(hov$customdata) > 0) {
          tk <- .parse_hover_ticker(as.character(hov$customdata[[1]]))
          if (!is.null(tk)) return(tk)
        }

        # Fallback: node id — works for 6040 child tiles and portfolio root.
        # Both individual tickers and derived portfolios live in xts_ret_shiny.
        if (!is.null(hov$id) && length(hov$id) > 0) {
          candidate <- as.character(hov$id[[1]])
          if (nzchar(candidate) && candidate %in% colnames(xts_ret_shiny)) return(candidate)
        }
      }
      NULL
    })

    output$float_panel <- renderUI({
      req(isTRUE(input$float_plot))
      large <- isTRUE(input$float_large)
      w  <- if (large) "660px" else "340px"
      h  <- if (large) "420px" else "210px"
      hw <- if (large) 330 else 170   # half-width in px
      hh <- if (large) 210 else 105   # half-height in px
      absolutePanel(
        id        = ns("hover_panel"),
        top  = paste0("calc(50% - ", hh, "px)"),
        left = paste0("calc(50% - ", hw, "px)"),
        width     = w,
        draggable = TRUE,
        style     = paste0(
          "background:white;",
          " border:1px solid #e5e7eb; border-radius:8px;",
          " padding:8px; z-index:1000;",
          " box-shadow:0 4px 20px rgba(0,0,0,0.12);"
        ),
        div(
          style = "color:#9ca3af; font-size:9px; margin-bottom:4px;",
          "\u2195 Drag to reposition"
        ),
        plotOutput(ns("hover_plot"), height = h)
      )
    })

    output$hover_plot <- renderPlot({
      req(isTRUE(input$float_plot))
      tk <- hovered_ticker()
      if (is.null(tk)) {
        return(.no_data_plot("Hover a tile\n(e.g. SPY, AGG, or USD 60/40)\nfor the return chart"))
      }

      period  <- if (is.null(input$float_period)) "1Y" else input$float_period
      use_rel <- isTRUE(input$float_rel)
      b       <- if (is.null(input$float_bmk)) "SPY" else input$float_bmk
      if (tk == b) b <- setdiff(c("SPY", "URTH", "ACWI"), tk)[1]

      # Display label: derived portfolio name > SAA display_label > ticker
      if (tk %in% DERIVED_UNIVERSE$id) {
        lbl <- DERIVED_UNIVERSE$name[DERIVED_UNIVERSE$id == tk][1]
      } else {
        lbl <- .SAA_TREE$display_label[.SAA_TREE$ticker == tk]
        lbl <- if (length(lbl) == 0 || is.na(lbl[1])) tk else lbl[1]
      }
      if (lbl == b) lbl <- paste0(lbl, " (", tk, ")")

      win_start <- switch(period,
        "1M"  = Sys.Date() -  30,  "3M"  = Sys.Date() -  91,
        "6M"  = Sys.Date() - 182,  "YTD" = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
        "1Y"  = Sys.Date() - 365,  "3Y"  = Sys.Date() - 1095,
        "Max" = as.Date("2000-01-01"), Sys.Date() - 365
      )
      ytd_start <- as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01"))

      # ── Build return series ─────────────────────────────────────────────────
      cols_needed <- c(tk, b)
      use_xts <- all(cols_needed %in% colnames(xts_ret_shiny))

      if (use_xts) {
        win  <- paste0(format(win_start,  "%Y-%m-%d"), "/")
        ytdw <- paste0(format(ytd_start,  "%Y-%m-%d"), "/")
        r      <- xts_ret_shiny[win,  cols_needed]
        r_ytd  <- xts_ret_shiny[ytdw, cols_needed]
        if (nrow(r) < 10) return(.no_data_plot(paste0("Insufficient data for ", lbl)))

        cum_tk  <- .cum_ret_pct(r[, tk])
        cum_bmk <- .cum_ret_pct(r[, b])
        ytd_tk  <- tail(.cum_ret_pct(r_ytd[, tk]),  1)
        ytd_bmk <- tail(.cum_ret_pct(r_ytd[, b]),   1)
        d1_tk   <- as.numeric(tail(xts_ret_shiny[, tk], 1)) * 100
        dates   <- as.Date(zoo::index(r))

      } else {
        # Fallback: raw_data price (rebased)
        if (!tk %in% unique(raw_data$symbol)) {
          return(.no_data_plot(paste0(
            tk, " not in universe\nAdd to 00_init_universe.R\nand re-run 01_etf_wrangle.R")))
        }
        price_wide <- raw_data %>%
          dplyr::filter(symbol %in% cols_needed, date >= win_start) %>%
          dplyr::select(date, symbol, adjusted) %>%
          dplyr::mutate(adjusted = as.numeric(adjusted)) %>%
          tidyr::pivot_wider(names_from = symbol, values_from = adjusted) %>%
          dplyr::arrange(date)
        if (nrow(price_wide) < 10 || !tk %in% names(price_wide)) {
          return(.no_data_plot(paste0("Insufficient data for ", lbl)))
        }
        price_wide <- price_wide %>%
          dplyr::mutate(
            cum_tk  = (.data[[tk]] / dplyr::first(.data[[tk]])  - 1) * 100,
            cum_bmk = (if (b %in% names(.)) {
              .data[[b]] / dplyr::first(.data[[b]]) - 1
            } else { 0 }) * 100
          )
        cum_tk  <- price_wide$cum_tk
        cum_bmk <- price_wide$cum_bmk
        dates   <- price_wide$date
        ytd_tk  <- tail(cum_tk, 1)
        ytd_bmk <- tail(cum_bmk, 1)
        d1_tk   <- tail(diff(cum_tk), 1)
      }

      subtitle <- .kpi_subtitle(cum_tk, cum_bmk, ytd_tk, ytd_bmk, d1_tk, lbl, b)
      xsc      <- .float_x_scales(period)

      # ── Absolute plot ───────────────────────────────────────────────────────
      if (!use_rel) {
        df <- tibble::tibble(date = dates,
                             !!lbl := cum_tk,
                             !!b   := cum_bmk) %>%
          tidyr::pivot_longer(-date, names_to = "series", values_to = "val")
        end_lbl <- df %>% dplyr::group_by(series) %>%
          dplyr::filter(date == max(date)) %>% dplyr::ungroup()
        col_vals  <- c("#2563eb", "#16a34a")
        names(col_vals)  <- c(lbl, b)
        size_vals <- c(1.1, 0.7)
        names(size_vals) <- c(lbl, b)

        ggplot(df, aes(date, val, colour = series, linewidth = series)) +
          geom_hline(yintercept = 0, colour = "#9ca3af", linewidth = 0.4) +
          geom_line() +
          geom_text(data = end_lbl,
                    aes(label = sprintf("%s\n%+.1f%%", series, val)),
                    hjust = -0.05, size = 3.25, fontface = "bold",
                    show.legend = FALSE) +
          scale_colour_manual(values = col_vals) +
          scale_linewidth_manual(values = size_vals) +
          scale_x_date(breaks = xsc$breaks, date_labels = xsc$labels,
                       expand = expansion(mult = c(0, 0.14))) +
          scale_y_continuous(
            labels = function(x) paste0(ifelse(x >= 0, "+", ""), round(x, 1), "%")) +
          guides(colour = "none", linewidth = "none") +
          labs(title    = paste0(lbl, " vs ", b, "  \u2022  ", period),
               subtitle = subtitle,
               x = NULL, y = NULL) +
          .white_card_theme()

      # ── Relative (spread) plot ──────────────────────────────────────────────
      } else {
        spread <- cum_tk - cum_bmk
        df <- tibble::tibble(date = dates, val = spread,
                             series = paste0(lbl, " \u2212 ", b))
        end_lbl <- df %>% dplyr::filter(date == max(date))

        ggplot(df, aes(date, val)) +
          geom_hline(yintercept = 0, colour = "#9ca3af", linewidth = 0.5,
                     linetype = "dashed") +
          geom_area(fill = "#2563eb", alpha = 0.10) +
          geom_line(colour = "#2563eb", linewidth = 1) +
          geom_text(data = end_lbl,
                    aes(label = sprintf("%+.1f%%", val)),
                    hjust = -0.1, size = 3.5, colour = "#2563eb", fontface = "bold") +
          scale_x_date(breaks = xsc$breaks, date_labels = xsc$labels,
                       expand = expansion(mult = c(0, 0.10))) +
          scale_y_continuous(
            labels = function(x) paste0(ifelse(x >= 0, "+", ""), round(x, 1), "%")) +
          labs(title    = paste0(lbl, " \u2212 ", b, "  \u2022  ", period),
               subtitle = subtitle,
               x = NULL, y = NULL) +
          .white_card_theme()
      }
    }, bg = "white")

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

      .build_treemap_plotly(nodes, source = "eq_tm") %>%
        plotly::event_register("plotly_hover")
    })

    # ── 6040 portfolio chart (static — no hover complexity) ───────────────────
    output$pf6040_chart <- renderPlot({
      ccy      <- if (is.null(input$pf6040_ccy))    "usd" else input$pf6040_ccy
      period   <- if (is.null(input$pf6040_period)) "1Y"  else input$pf6040_period
      du_id    <- switch(ccy, "usd" = "usd6040", "eur" = "eur6040_v3", "chf" = "chf6040_v1")
      pf_label <- DERIVED_UNIVERSE$name[DERIVED_UNIVERSE$id == du_id][1]

      if (!du_id %in% colnames(xts_ret_shiny))
        return(.no_data_plot(paste0(pf_label, "\nnot yet built — re-run 01_etf_wrangle.R")))

      win_start <- switch(period,
        "YTD" = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
        "1Y"  = Sys.Date() - 365,
        "3Y"  = Sys.Date() - 1095,
        "Max" = as.Date("2000-01-01")
      )
      ytd_start <- as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01"))
      win  <- paste0(format(win_start,  "%Y-%m-%d"), "/")
      ytdw <- paste0(format(ytd_start,  "%Y-%m-%d"), "/")

      # ── Series definition per currency ─────────────────────────────────────
      # USD: pf + EQ (SPY) + FI (AGG)
      # EUR/CHF: pf + EQ part + FI part + USD 6040 as reference
      series_map <- switch(ccy,
        "usd" = list(eq = "SPY",     fi = "AGG",     ref = NULL),
        "eur" = list(eq = "IWDE.AS", fi = "EUNA.DE", ref = NULL),
        "chf" = list(eq = "IWDE.AS", fi = "EUNA.DE", ref = NULL)
      )
      eq_tk  <- series_map$eq
      fi_tk  <- series_map$fi
      ref_id <- series_map$ref

      fi_missing <- !fi_tk %in% colnames(xts_ret_shiny)

      all_tks <- c(du_id, eq_tk, if (!fi_missing) fi_tk, ref_id)
      all_tks <- all_tks[all_tks %in% colnames(xts_ret_shiny)]

      r <- na.omit(xts_ret_shiny[win, all_tks])
      if (nrow(r) < 10) return(.no_data_plot("Insufficient data"))
      dates <- as.Date(zoo::index(r))

      # Labels: use ticker for components, portfolio name for blends
      ref_label <- if (!is.null(ref_id) && ref_id %in% colnames(r))
        DERIVED_UNIVERSE$name[DERIVED_UNIVERSE$id == ref_id][1] else NULL

      series_labels <- setNames(colnames(r), colnames(r))
      series_labels[du_id] <- pf_label
      if (!is.null(ref_id) && ref_id %in% names(series_labels))
        series_labels[ref_id] <- ref_label

      # ── Cumulative returns ──────────────────────────────────────────────────
      cum_list <- lapply(colnames(r), function(tk) .cum_ret_pct(r[, tk]))
      names(cum_list) <- colnames(r)

      r_ytd  <- na.omit(xts_ret_shiny[ytdw, c(du_id, eq_tk)])
      ytd_pf  <- if (nrow(r_ytd) > 0) tail(.cum_ret_pct(r_ytd[, du_id]), 1) else NA
      ytd_bmk <- if (nrow(r_ytd) > 0) tail(.cum_ret_pct(r_ytd[, eq_tk]), 1) else NA
      d1_pf   <- as.numeric(tail(xts_ret_shiny[, du_id], 1)) * 100

      subtitle <- .kpi_subtitle(cum_list[[du_id]], cum_list[[eq_tk]],
                                ytd_pf, ytd_bmk, d1_pf, pf_label, eq_tk)
      xsc <- .float_x_scales(period)

      # ── Ann return + vol (PerformanceAnalytics — canonical source) ──────────
      stats_tbl <- purrr::map_dfr(colnames(r), function(tk) {
        tibble::tibble(
          id      = tk,
          series  = series_labels[tk],
          ann_ret = as.numeric(Return.annualized(r[, tk], scale = 252)) * 100,
          ann_vol = as.numeric(StdDev.annualized(r[, tk], scale = 252)) * 100
        )
      })

      # ── Build long df with display labels ──────────────────────────────────
      df_wide <- as.data.frame(cum_list)
      df_wide$date <- dates
      colnames(df_wide)[seq_along(colnames(r))] <- unname(series_labels[colnames(r)])

      df <- df_wide %>%
        tidyr::pivot_longer(-date, names_to = "series", values_to = "val")

      end_lbl <- df %>% dplyr::group_by(series) %>%
        dplyr::filter(date == max(date)) %>% dplyr::ungroup() %>%
        dplyr::left_join(dplyr::select(stats_tbl, series, ann_ret, ann_vol),
                         by = "series")

      # ── Colours + linewidths ────────────────────────────────────────────────
      # slot 1 = portfolio (purple), slot 2 = EQ (green), slot 3 = FI (blue),
      # slot 4 = USD ref (grey, dashed)
      display_series <- unname(series_labels[colnames(r)])
      base_cols  <- c("#7c3aed", "#16a34a", "#3b82f6", "#9ca3af")
      base_sizes <- c(1.3, 0.8, 0.8, 0.6)
      col_vals   <- setNames(base_cols[seq_along(display_series)],  display_series)
      size_vals  <- setNames(base_sizes[seq_along(display_series)], display_series)

      # USD ref gets a dashed line
      ltype_vals <- setNames(rep("solid", length(display_series)), display_series)
      if (!is.null(ref_label) && ref_label %in% display_series)
        ltype_vals[ref_label] <- "dashed"

      # ── Extensive legend: "TICKER — Full Name  |  +X.X% ann  σ Y.Y%" ────────
      .full_name <- function(tk) {
        nm <- etf_metadata$name[etf_metadata$ticker == tk]
        if (length(nm) == 0 || is.na(nm[1])) tk else nm[1]
      }
      legend_labels <- sapply(colnames(r), function(tk) {
        st  <- stats_tbl[stats_tbl$id == tk, ]
        ann <- if (nrow(st) > 0) sprintf("%+.1f%% ann  \u03c3 %.1f%%", st$ann_ret, st$ann_vol) else ""
        if (tk == du_id)
          sprintf("%s  |  %s", series_labels[tk], ann)
        else
          sprintf("%s \u2014 %s  |  %s", tk, .full_name(tk), ann)
      })
      names(legend_labels) <- unname(series_labels[colnames(r)])

      # ── Quarterly grid breaks ───────────────────────────────────────────────
      q_breaks <- seq.Date(
        from = lubridate::floor_date(min(dates), "quarter"),
        to   = lubridate::ceiling_date(max(dates), "quarter"),
        by   = "quarter"
      )

      ggplot(df, aes(date, val, colour = series, linewidth = series,
                     linetype = series)) +
        geom_vline(xintercept = as.numeric(q_breaks),
                   colour = "#e5e7eb", linewidth = 0.3) +
        geom_hline(yintercept = 0, colour = "#9ca3af", linewidth = 0.4) +
        geom_line() +
        ggrepel::geom_text_repel(
          data        = end_lbl,
          aes(label   = sprintf("%+.1f%% total\n%+.1f%% ann  \u03c3 %.1f%%",
                                val, ann_ret, ann_vol)),
          direction   = "y",
          nudge_x     = 14,
          hjust       = 0,
          size        = 3.2,
          fontface    = "bold",
          lineheight  = 1.3,
          segment.size  = 0.3,
          segment.color = "#9ca3af",
          show.legend = FALSE
        ) +
        scale_colour_manual(values = col_vals, labels = legend_labels) +
        scale_linewidth_manual(values = size_vals) +
        scale_linetype_manual(values = ltype_vals) +
        scale_x_date(breaks = xsc$breaks, date_labels = xsc$labels,
                     expand = expansion(mult = c(0, 0.28))) +
        scale_y_continuous(
          labels = function(x) paste0(ifelse(x >= 0, "+", ""), round(x, 1), "%")) +
        guides(
          colour    = guide_legend(title = NULL, override.aes = list(linewidth = 1.5),
                                   ncol = 1),
          linewidth = "none",
          linetype  = "none"
        ) +
        labs(title    = paste0(pf_label, "  \u2022  ", period),
             subtitle = subtitle, x = NULL, y = NULL,
             caption  = if (fi_missing)
               paste0("\u26a0 FI leg (", fi_tk, ") not in data — re-run 01_etf_wrangle.R")
             else NULL) +
        .white_card_theme() +
        theme(legend.position  = c(0.01, 0.99),
              legend.justification = c("left", "top"),
              legend.background = element_rect(fill = alpha("white", 0.85), colour = NA),
              legend.text      = element_text(size = 8.5, family = "mono",
                                              colour = "#1B2A4A", face = "bold"),
              legend.key.width = unit(1.5, "cm"),
              panel.grid.major.x = element_blank())
    }, bg = "white")

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

    # ── Force pre-render for non-default tabs so plotly JS hover listeners bind ──
    outputOptions(output, "saa_treemap", suspendWhenHidden = FALSE)
    outputOptions(output, "eq_treemap",  suspendWhenHidden = FALSE)
  })
}

# ── Shared plotly builder — B2 Finviz Institutional ───────────────────────────
.build_treemap_plotly <- function(nodes, source = NULL, branchvalues = "remainder",
                                  level = NULL) {
  plotly::plot_ly(source = source) %>%
    plotly::add_trace(
      type          = "treemap",
      ids           = nodes$ids,
      labels        = nodes$labels,
      parents       = nodes$parents,
      values        = nodes$values,
      branchvalues  = branchvalues,
      level         = level,
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
    plotly::config(displayModeBar = FALSE) %>%
    plotly::event_register("plotly_hover")
}
