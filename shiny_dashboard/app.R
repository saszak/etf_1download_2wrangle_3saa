# ==============================================================================
# shiny_dashboard/app.R
# PURPOSE : ETF Universe — Performance Screen
# RUN     : shiny::runApp("shiny_dashboard")
# ==============================================================================

# Explicitly source global.R — guards inside prevent double-loading
source("global.R")

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = tags$span(
    style = "font-weight:700; letter-spacing:0.02em;",
    "ETF Universe \u2014 Performance Screen"
  ),

  tags$head(tags$style(HTML("
    .bslib-value-box, .value-box {
      min-height:72px !important; max-height:88px !important; padding:6px 10px !important; }
    .bslib-value-box .value-box-title, .value-box .value-box-title {
      font-size:9px !important; line-height:1.2 !important;
      text-transform:uppercase; letter-spacing:.04em;
      overflow:hidden !important; text-overflow:ellipsis !important; white-space:nowrap !important; }
    .bslib-value-box .value-box-value, .value-box .value-box-value {
      font-size:14px !important; font-weight:700 !important; line-height:1.2 !important;
      overflow:hidden !important; text-overflow:ellipsis !important; white-space:nowrap !important; }
    .bslib-value-box .value-box-area, .value-box .value-box-area {
      overflow:hidden !important; min-width:0 !important;
      display:flex !important; flex-direction:column !important; justify-content:center !important; }
    .bslib-value-box .value-box-grid, .value-box .value-box-grid { align-items:center !important; }
    .bslib-value-box .value-box-showcase, .value-box .value-box-showcase {
      font-size:14px !important; min-width:28px !important; }
    .bslib-value-box .value-box-showcase svg, .value-box .value-box-showcase svg {
      width:14px !important; height:14px !important; }
    .main-card { height:calc(100vh - 218px) !important; min-height:480px !important; }
    .main-card .tab-content { height:calc(100% - 42px) !important; }
    .main-card .tab-pane.active { height:100% !important; display:flex !important; flex-direction:column; }
    .main-card .reactable { flex:1; overflow:auto; }
  "))),

  theme = bs_theme(
    bootswatch   = "darkly",
    base_font    = font_google("Inter"),
    heading_font = font_google("Inter"),
    bg           = DARK_BG,
    fg           = TEXT_MAIN
  ),

  # ── Sidebar ─────────────────────────────────────────────────────────────────
  sidebar = sidebar(
    width = 220, bg = DARK_PANEL,

    # ── Mode selector ──────────────────────────────────────────────────────────
    radioButtons("mode", NULL,
      choices  = c("Core SAA", "Monitor", "Explore"),
      selected = "Core SAA"),

    hr(style = paste0("border-color:", DARK_BORDER, "; margin:6px 0;")),

    # ── Core SAA controls ──────────────────────────────────────────────────────
    conditionalPanel(
      condition = "input.mode == 'Core SAA'",
      sliderInput("saa_depth", "Detail level",
        min = 0, max = 3, value = 1, step = 1, ticks = TRUE),
      checkboxGroupInput("saa_bucket", "Bucket",
        choices  = c("Cash", "FI", "EQ", "Alt"),
        selected = "EQ",
        inline   = TRUE)
    ),

    # ── Monitor controls ───────────────────────────────────────────────────────
    conditionalPanel(
      condition = "input.mode == 'Monitor'",
      radioButtons("period", "Period",
        choices  = c("MTD" = "ret_mtd", "YTD" = "ret_ytd",
                     "3M"  = "ret_3m",  "1M"  = "ret_1m"),
        selected = "ret_ytd",
        inline   = FALSE)
    ),

    # ── Explore controls ───────────────────────────────────────────────────────
    conditionalPanel(
      condition = "input.mode == 'Explore'",
      selectInput("explore_class", "Asset Class",
        choices  = c("All", sort(unique(perf_data$asset_class))),
        selected = "All"),
      checkboxInput("hide_signal",  "Hide Signal tickers", value = FALSE),
      checkboxInput("only_bullish", "Bullish regime only", value = FALSE)
    ),

    hr(style = paste0("border-color:", DARK_BORDER, "; margin:6px 0;")),

    # ── Always visible ─────────────────────────────────────────────────────────
    selectInput("sort_by", "Sort By",
      choices  = c("YTD" = "ret_ytd", "MTD" = "ret_mtd",
                   "3M"  = "ret_3m",  "1M"  = "ret_1m",
                   "5D"  = "ret_5d",  "1D"  = "ret_1d",
                   "52W Pos" = "range_pct", "Ticker" = "symbol"),
      selected = "ret_ytd"),

    selectInput("treemap_group", "Treemap grouping",
      choices  = c("Clusters" = "cluster", "Asset Class" = "asset_class"),
      selected = "cluster"),

    checkboxInput("group_by_class", "Group by Asset Class", value = FALSE),

    hr(style = paste0("border-color:", DARK_BORDER, "; margin:6px 0;")),

    checkboxInput("relative_mode", "Relative to BMK", value = FALSE),
    conditionalPanel(
      condition = "input.relative_mode",
      selectInput("bmk", "Benchmark", choices = c("SPY", "URTH", "AGG"), selected = "SPY")
    ),

    hr(style = paste0("border-color:", DARK_BORDER, "; margin:6px 0;")),
    p(style = paste0("font-size:11px; color:", TEXT_DIM, ";"),
      paste0("Data as of: ", format(today, "%b %d, %Y")))
  ),

  # ── KPI strip ───────────────────────────────────────────────────────────────
  layout_columns(
    col_widths = c(2, 2, 2, 3, 3), fill = FALSE,
    value_box(
      title    = "SPY YTD",
      value    = sprintf("%+.1f%%", (spy_row$ret_ytd %||% 0) * 100),
      theme    = if ((spy_row$ret_ytd %||% 0) >= 0) "success" else "danger",
      showcase = if ((spy_row$ret_ytd %||% 0) >= 0) icon("arrow-up") else icon("arrow-down")
    ),
    value_box(
      title    = "SPY MTD",
      value    = sprintf("%+.1f%%", (spy_row$ret_mtd %||% 0) * 100),
      theme    = if ((spy_row$ret_mtd %||% 0) >= 0) "success" else "danger",
      showcase = icon("calendar-days")
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

  # ── Main card ───────────────────────────────────────────────────────────────
  card(
    class = "main-card", full_screen = TRUE,
    tabsetPanel(
      id = "main_tabs", type = "tabs",
      tabPanel("Table",   mod_perf_table_ui("perf_table")),
      tabPanel("Treemap", mod_treemap_ui("treemap")),
      tabPanel("Plots",    mod_plots_ui("plots")),
      tabPanel("AbsRel",   mod_absrel_ui("absrel")),
      tabPanel("Cal Year", mod_calyear_ui("calyear")),
      tabPanel("Comp",      mod_comp_ui("comp")),
      tabPanel("Technical", mod_technical_ui("technical")),
      tabPanel("Regime",    mod_regime_ui("regime")),
      tabPanel("RiskRet",   mod_riskret_ui("riskret"))
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── sort_by: Monitor mode auto-follows period selection ──────────────────────
  effective_sort <- reactive({
    if (input$mode == "Monitor") input$period else input$sort_by
  })

  filtered <- reactive({
    df <- switch(input$mode,

      "Core SAA" = {
        d <- perf_data %>%
          filter(!is.na(saa_depth), saa_depth <= input$saa_depth)
        if (!setequal(input$saa_bucket, c("Cash", "FI", "EQ", "Alt")))
          d <- d %>% filter(saa_bucket %in% input$saa_bucket)
        d
      },

      "Monitor" = {
        perf_data %>% filter(!is.na(saa_depth))   # full SAA tree
      },

      "Explore" = {
        d <- perf_data
        if (!is.null(input$explore_class) && input$explore_class != "All")
          d <- d %>% filter(asset_class == input$explore_class)
        if (isTRUE(input$hide_signal))  d <- d %>% filter(pf_function != "Signal")
        if (isTRUE(input$only_bullish)) d <- d %>% filter(trend_regime == "Bullish")
        d
      }
    )

    if (isTRUE(input$relative_mode)) {
      bmk_row <- perf_data %>% filter(symbol == input$bmk) %>% slice(1)
      if (nrow(bmk_row) == 1) {
        df <- df %>% mutate(
          ret_1d  = ret_1d  - bmk_row$ret_1d,
          ret_5d  = ret_5d  - bmk_row$ret_5d,
          ret_1m  = ret_1m  - bmk_row$ret_1m,
          ret_3m  = ret_3m  - bmk_row$ret_3m,
          ret_mtd = ret_mtd - bmk_row$ret_mtd,
          ret_ytd = ret_ytd - bmk_row$ret_ytd
        )
      }
    }

    df %>% arrange(desc(.data[[effective_sort()]]))
  })

  output$bullish_pct <- renderText({
    df <- filtered()
    sprintf("%d / %d", sum(df$trend_regime == "Bullish", na.rm = TRUE), nrow(df))
  })

  output$n_shown <- renderText({
    paste0(nrow(filtered()), " ETFs")
  })

  mod_perf_table_server("perf_table", filtered,
    relative_mode  = reactive(input$relative_mode),
    bmk            = reactive(input$bmk),
    group_by_class = reactive(input$group_by_class)
  )

  mod_treemap_server("treemap", filtered,
    treemap_group = reactive(input$treemap_group)
  )

  mod_plots_server("plots", filtered)

  mod_absrel_server("absrel", filtered,
    relative_mode = reactive(input$relative_mode),
    bmk           = reactive(input$bmk)
  )

  mod_calyear_server("calyear", filtered,
    relative_mode = reactive(input$relative_mode),
    bmk           = reactive(input$bmk)
  )

  mod_comp_server("comp", filtered,
    relative_mode = reactive(input$relative_mode),
    bmk           = reactive(input$bmk)
  )

  mod_technical_server("technical", filtered)

  mod_regime_server("regime", filtered,
    relative_mode = reactive(input$relative_mode),
    bmk           = reactive(input$bmk)
  )

  mod_riskret_server("riskret", filtered,
    bmk = reactive(input$bmk)
  )
}

shinyApp(ui, server)
