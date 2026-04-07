# ==============================================================================
# shiny_dashboard/modules/mod_riskret.R
# PURPOSE : Risk-Return scatter — Absolute | Relative (vs BMK)
# ==============================================================================

mod_riskret_ui <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    id   = ns("tabs"),
    type = "pills",
    tabPanel("Custom",
      layout_columns(
        col_widths = c(6, 6),
        fill = TRUE,
        card(
          full_screen = TRUE,
          card_header("Absolute  —  Ann. Vol vs Ann. Return (Full History)",
                      style = paste0("background:", DARK_HDR, "; color:", TEXT_DIM,
                                     "; font-size:11px; padding:5px 12px;")),
          plotlyOutput(ns("abs_scatter"), height = "calc(100vh - 360px)")
        ),
        card(
          full_screen = TRUE,
          card_header(textOutput(ns("rel_title"), inline = TRUE),
                      style = paste0("background:", DARK_HDR, "; color:", TEXT_DIM,
                                     "; font-size:11px; padding:5px 12px;")),
          plotlyOutput(ns("rel_scatter"), height = "calc(100vh - 360px)")
        )
      )
    ),
    tabPanel("PA Check",
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          card_header("Absolute — chart.RiskReturnScatter (Full History)",
                      style = paste0("background:", DARK_HDR, "; color:", TEXT_DIM,
                                     "; font-size:11px; padding:5px 12px;")),
          plotOutput(ns("pa_abs"), height = "calc(100vh - 360px)")
        ),
        card(
          full_screen = TRUE,
          card_header(textOutput(ns("pa_rel_title"), inline = TRUE),
                      style = paste0("background:", DARK_HDR, "; color:", TEXT_DIM,
                                     "; font-size:11px; padding:5px 12px;")),
          plotOutput(ns("pa_rel"), height = "calc(100vh - 360px)")
        )
      ),
    ),
    tabPanel("Return Matrix",
      tagList(
        div(
          style = paste0(
            "background:", DARK_HDR, "; color:", TEXT_DIM, ";",
            " font-size:11px; padding:5px 12px; margin-bottom:4px;",
            " display:flex; gap:18px; align-items:center;"
          ),
          span("\u25a0 Red < 0%  \u2502  Pink 0\u20133%  \u2502  Blue 3\u20137%  \u2502  Light Green 7\u201310%  \u2502  Dark Green > 10%  \u2502  White numbers = P/E headwind"),
          div(
            style = "display:flex; gap:14px; margin-left:auto; align-items:center;",
            span("From year:"),
            sliderInput(
              ns("from_yr"), label = NULL,
              min = 1900, max = as.integer(format(Sys.Date(), "%Y")) - 5,
              value = 1950, step = 5, width = "220px", ticks = FALSE
            ),
            checkboxInput(ns("show_25yr"), "25-yr diagonal", value = TRUE),
            checkboxInput(ns("show_text"),  "Show numbers",  value = TRUE)
          )
        ),
        div(
          style = paste0("overflow-y:auto; height:calc(100vh - 302px); background:white;"),
          plotOutput(ns("ret_matrix"), height = "calc(100vh - 302px)")
        )
      )
    ),
    tabPanel("Ann. Stats",
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(
            "Absolute — Ann. Return / Vol / Sharpe (Full History)",
            style = paste0("background:", DARK_HDR, "; color:", TEXT_DIM,
                           "; font-size:11px; padding:5px 12px;")
          ),
          reactableOutput(ns("abs_stats"), height = "calc(100vh - 360px)")
        ),
        card(
          card_header(
            textOutput(ns("rel_stats_title"), inline = TRUE),
            style = paste0("background:", DARK_HDR, "; color:", TEXT_DIM,
                           "; font-size:11px; padding:5px 12px;")
          ),
          reactableOutput(ns("rel_stats"), height = "calc(100vh - 360px)")
        )
      )
    )
  )
}

mod_riskret_server <- function(id, filtered, bmk) {
  moduleServer(id, function(input, output, session) {

    AC_COLORS <- c(
      "Equity"       = "#10b981",
      "Fixed Income" = "#8b5cf6",
      "Commodity"    = "#f59e0b",
      "Real Assets"  = "#f97316",
      "Currency"     = "#60a5fa",
      "Other"        = "#6b7280"
    )

    # ── Risk metrics from full history xts_ret_shiny ─────────────────────────
    # ann_ret: geometric annualised return (full history)
    # ann_vol: annualised vol (full history)
    # te:      tracking error vs BMK (full history)
    # alpha:   annualised return of (ticker - BMK) series
    risk_df <- reactive({
      bmk_sym <- bmk()
      R       <- xts_ret_shiny
      syms    <- colnames(R)

      bmk_col <- if (bmk_sym %in% syms) as.numeric(R[, bmk_sym]) else rep(0, nrow(R))

      ann_ret <- apply(as.matrix(R), 2, function(x) {
        x <- x[!is.na(x)]
        if (length(x) < 2) return(NA_real_)
        prod(1 + x) ^ (252 / length(x)) - 1
      })

      ann_vol <- apply(as.matrix(R), 2, function(x)
        sd(x, na.rm = TRUE) * sqrt(252))

      te <- apply(as.matrix(R), 2, function(x)
        sd(x - bmk_col, na.rm = TRUE) * sqrt(252))

      alpha <- apply(as.matrix(R), 2, function(x) {
        rel <- x - bmk_col
        rel <- rel[!is.na(rel)]
        if (length(rel) < 2) return(NA_real_)
        prod(1 + rel) ^ (252 / length(rel)) - 1
      })

      tibble(symbol = syms, ann_ret = ann_ret, ann_vol = ann_vol,
             te = te, alpha = alpha)
    })

    # ── Shared dark layout ────────────────────────────────────────────────────
    .dark_layout <- function(p, xtitle, ytitle, shapes = NULL) {
      p %>%
        layout(
          paper_bgcolor = DARK_BG,
          plot_bgcolor  = DARK_BG,
          font   = list(color = TEXT_MAIN, family = "Inter", size = 11),
          xaxis  = list(title = xtitle, ticksuffix = "%", nticks = 20,
                        gridcolor = DARK_BORDER, zerolinecolor = "#3a3d48"),
          yaxis  = list(title = ytitle, ticksuffix = "%", nticks = 20,
                        gridcolor = DARK_BORDER, zerolinecolor = "#3a3d48"),
          legend = list(font = list(size = 10), bgcolor = DARK_PANEL,
                        bordercolor = DARK_BORDER, borderwidth = 1),
          margin = list(t = 20, b = 50, l = 60, r = 20),
          shapes = shapes
        ) %>%
        plotly::config(displayModeBar = FALSE)
    }

    # ── Absolute scatter ──────────────────────────────────────────────────────
    output$abs_scatter <- renderPlotly({
      df <- filtered() %>%
        left_join(risk_df(), by = "symbol") %>%
        filter(!is.na(ann_vol), !is.na(ann_ret)) %>%
        mutate(
          vol_pct   = round(ann_vol * 100, 2),
          ret_pct   = round(ann_ret * 100, 2),
          ac        = coalesce(asset_class, "Other"),
          hover_txt = paste0(
            "<b>", symbol, "</b><br>",
            coalesce(name, ""), "<br>",
            "Vol: ", vol_pct, "%<br>",
            "Ann. Ret: ", sprintf("%+.2f%%", ret_pct)
          )
        )

      validate(need(nrow(df) > 0, "No data to display"))

      plot_ly(
        data         = df,
        x            = ~vol_pct,
        y            = ~ret_pct,
        text         = ~symbol,
        color        = ~ac,
        colors       = AC_COLORS,
        type         = "scatter",
        mode         = "markers+text",
        textposition = "top center",
        textfont     = list(size = 9, color = "#9ca3af"),
        marker       = list(size = 12, line = list(width = 0.5, color = "#ffffff30")),
        hovertext    = ~hover_txt,
        hoverinfo    = "text"
      ) %>%
        .dark_layout("Annualised Volatility (%)", "Annualised Return (%)")
    })

    # ── Relative scatter ──────────────────────────────────────────────────────
    output$rel_title <- renderText({
      paste0("Relative  —  Tracking Error vs Alpha vs ", bmk())
    })

    output$rel_scatter <- renderPlotly({
      bmk_sym <- bmk()

      df <- filtered() %>%
        filter(symbol != bmk_sym) %>%
        left_join(risk_df(), by = "symbol") %>%
        filter(!is.na(te), !is.na(alpha)) %>%
        mutate(
          te_pct    = round(te * 100, 2),
          alpha_pct = round(alpha * 100, 2),
          ac        = coalesce(asset_class, "Other"),
          hover_txt = paste0(
            "<b>", symbol, "</b><br>",
            coalesce(name, ""), "<br>",
            "TE: ", te_pct, "%<br>",
            "Alpha: ", sprintf("%+.2f%%", alpha_pct)
          )
        )

      validate(need(nrow(df) > 0, "No data to display"))

      zero_line <- list(list(
        type = "line", xref = "paper", yref = "y",
        x0 = 0, x1 = 1, y0 = 0, y1 = 0,
        line = list(color = "#6b7280", dash = "dot", width = 1)
      ))

      plot_ly(
        data         = df,
        x            = ~te_pct,
        y            = ~alpha_pct,
        text         = ~symbol,
        color        = ~ac,
        colors       = AC_COLORS,
        type         = "scatter",
        mode         = "markers+text",
        textposition = "top center",
        textfont     = list(size = 9, color = "#9ca3af"),
        marker       = list(size = 12, line = list(width = 0.5, color = "#ffffff30")),
        hovertext    = ~hover_txt,
        hoverinfo    = "text"
      ) %>%
        .dark_layout(
          paste0("Tracking Error vs ", bmk_sym, " (%)"),
          paste0("Annualised Alpha vs ", bmk_sym, " (%)"),
          shapes = zero_line
        )
    })

    # ── Ann. Stats: table.AnnualizedReturns ──────────────────────────────────
    .pa_stats_tbl <- function(R, syms_meta) {
      tbl <- PerformanceAnalytics::table.AnnualizedReturns(R, Rf = 0, scale = 252)
      # tbl rows = metrics, cols = tickers — transpose to tidy
      as.data.frame(t(tbl)) %>%
        rownames_to_column("symbol") %>%
        as_tibble() %>%
        rename(
          ann_ret    = `Annualized Return`,
          ann_vol    = `Annualized Std Dev`,
          ann_sharpe = `Annualized Sharpe (Rf=0%)`
        ) %>%
        left_join(syms_meta, by = "symbol") %>%
        arrange(desc(ann_sharpe))
    }

    .stats_reactable <- function(df, ret_label = "Ann. Return", sharpe_label = "Sharpe") {
      reactable(
        df,
        searchable      = TRUE,
        striped         = TRUE,
        highlight       = TRUE,
        defaultPageSize = 100,
        theme           = tbl_theme,
        defaultColDef   = colDef(vAlign = "center", headerVAlign = "bottom"),
        columns = list(
          symbol     = colDef(name = "Ticker", width = 72,
                              style = list(fontWeight = "700", color = BLUE_TICK,
                                           fontSize = "13px")),
          name       = colDef(name = "Name", minWidth = 160,
                              style = list(color = TEXT_DIM, fontSize = "11px")),
          asset_class = colDef(name = "Class", width = 110,
                               style = list(color = TEXT_DIM, fontSize = "11px")),
          ann_ret    = colDef(
            name = ret_label, width = 105, html = TRUE,
            cell = function(v) .ret_cell(v)
          ),
          ann_vol    = colDef(
            name = "Ann. Vol", width = 90,
            format = colFormat(percent = TRUE, digits = 1),
            style  = list(fontSize = "12px")
          ),
          ann_sharpe = colDef(
            name = sharpe_label, width = 90,
            format = colFormat(digits = 2),
            style  = function(v) list(
              color      = if (!is.na(v) && v >= 0) "#86efac" else "#fca5a5",
              fontWeight = "700", fontSize = "12px"
            )
          )
        )
      )
    }

    syms_meta <- reactive({
      perf_data %>% select(symbol, name, asset_class) %>% distinct()
    })

    output$abs_stats <- renderReactable({
      syms <- filtered()$symbol
      syms <- syms[syms %in% colnames(xts_ret_shiny)]
      validate(need(length(syms) >= 1, "No tickers in xts_ret_shiny"))

      R  <- xts_ret_shiny[, syms]
      df <- .pa_stats_tbl(R, syms_meta())
      .stats_reactable(df, ret_label = "Ann. Return", sharpe_label = "Sharpe (Rf=0)")
    })

    output$rel_stats_title <- renderText({
      paste0("Relative vs ", bmk(), " — Ann. Alpha / TE / IR (full history)")
    })

    output$rel_stats <- renderReactable({
      bmk_sym <- bmk()
      syms    <- filtered()$symbol
      syms    <- syms[syms %in% colnames(xts_ret_shiny)]
      syms    <- syms[syms != bmk_sym]
      validate(need(length(syms) >= 1, "No tickers to compare"))
      validate(need(bmk_sym %in% colnames(xts_ret_shiny), "BMK not in xts_ret_shiny"))

      bmk_col <- xts_ret_shiny[, bmk_sym]
      R_rel   <- xts_ret_shiny[, syms] - as.numeric(bmk_col)

      df <- .pa_stats_tbl(R_rel, syms_meta())
      .stats_reactable(df, ret_label = "Ann. Alpha", sharpe_label = "Info Ratio")
    })

    # ── PA Check helpers ──────────────────────────────────────────────────────
    .pa_syms <- reactive({
      s <- filtered()$symbol
      s[s %in% colnames(xts_ret_shiny)]
    })

    .pa_rel_xts <- reactive({
      bmk_sym <- bmk()
      syms    <- .pa_syms()
      syms    <- syms[syms != bmk_sym]
      validate(need(bmk_sym %in% colnames(xts_ret_shiny), "BMK not in xts_ret_shiny"))
      xts_ret_shiny[, syms] - as.numeric(xts_ret_shiny[, bmk_sym])
    })

    output$pa_abs <- renderPlot({
      syms <- .pa_syms()
      validate(need(length(syms) >= 2, "Need at least 2 tickers"))
      op <- par(bg = "#111318", col.axis = "#d1d5db", col.lab = "#d1d5db",
                col.main = "#d1d5db", fg = "#d1d5db", cex = 1.5)
      on.exit(par(op))
      PerformanceAnalytics::chart.RiskReturnScatter(
        xts_ret_shiny[, syms],
        Rf       = 0,
        main     = paste0("Absolute  —  ", length(syms), " tickers (full history)"),
        colorset = "#3b82f6"
      )
      grid(nx = 20, ny = 20, col = "#cccccc55", lty = "solid", lwd = 0.4)
    }, bg = "#111318")

    output$pa_rel_title <- renderText({
      paste0("Relative vs ", bmk(), " — chart.RiskReturnScatter (Full History)")
    })

    output$pa_rel <- renderPlot({
      R_rel <- .pa_rel_xts()
      validate(need(ncol(R_rel) >= 1, "No tickers to compare"))
      op <- par(bg = "#111318", col.axis = "#d1d5db", col.lab = "#d1d5db",
                col.main = "#d1d5db", fg = "#d1d5db", cex = 1.5)
      on.exit(par(op))
      PerformanceAnalytics::chart.RiskReturnScatter(
        R_rel,
        Rf       = 0,
        main     = paste0("Relative vs ", bmk(), "  —  ", ncol(R_rel), " tickers (full history)"),
        colorset = "#f59e0b"
      )
      grid(nx = 20, ny = 20, col = "#cccccc55", lty = "solid", lwd = 0.4)
    }, bg = "#111318")

    # ── Return Matrix ─────────────────────────────────────────────────────────
    output$ret_matrix <- renderPlot({
      plot_stock_matrix(
        from_yr   = input$from_yr,
        show_25yr = isTRUE(input$show_25yr),
        show_text = isTRUE(input$show_text)
      )
    }, bg = "white")

    output$pa_grid <- renderReactable({
      syms <- .pa_syms()
      validate(need(length(syms) >= 1, "No tickers"))

      R   <- xts_ret_shiny[, syms]
      tbl <- PerformanceAnalytics::table.Stats(R) %>%
        as.data.frame() %>%
        rownames_to_column("metric")

      reactable(
        tbl,
        striped         = TRUE,
        highlight       = TRUE,
        compact         = TRUE,
        defaultPageSize = 20,
        theme           = tbl_theme,
        defaultColDef   = colDef(
          vAlign      = "center",
          format      = colFormat(digits = 4),
          style       = list(fontSize = "11px"),
          minWidth    = 70
        ),
        columns = list(
          metric = colDef(
            name     = "Metric", width = 200, format = NULL,
            style    = list(fontWeight = "600", color = TEXT_DIM, fontSize = "11px")
          )
        )
      )
    })
  })
}
