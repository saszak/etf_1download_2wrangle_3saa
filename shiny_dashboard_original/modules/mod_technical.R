# ==============================================================================
# shiny_dashboard/modules/mod_technical.R
# PURPOSE : 200DMA State Machine — Plot 2 style, one chart per filtered ETF.
#           3-panel per ticker: price + 200DMA (top) | oscillator (mid) | DD (bottom)
#           Bull = teal · Neutral = amber · Bear = orange-red · Thresholds +4% / -2%
# ==============================================================================

mod_technical_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = paste0(
        "background:", DARK_HDR, "; color:", TEXT_DIM, ";",
        " font-size:11px; padding:5px 12px; margin-bottom:4px;",
        " display:flex; justify-content:space-between; align-items:center;"
      ),
      textOutput(ns("caption"), inline = TRUE),
      span(
        style = paste0("color:", TEXT_DIM, "; font-size:10px;"),
        "Teal = Bull  \u2502  Amber = Neutral  \u2502  Red-Orange = Bear  \u2502",
        "  Thresholds: +4% / \u22122%  \u2502  Bottom panel = drawdown"
      )
    ),
    div(
      style = paste0(
        "overflow-y:auto; height:calc(100vh - 302px); background:", DARK_BG, ";"
      ),
      uiOutput(ns("panels_ui"))
    )
  )
}

mod_technical_server <- function(id, filtered) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    avail_tickers <- reactive({
      syms <- filtered()$symbol
      syms[syms %in% colnames(xts_ret_shiny)]
    })

    output$caption <- renderText({
      n_avail <- length(avail_tickers())
      n_total <- nrow(filtered())
      if (n_avail == n_total)
        sprintf("%d ticker%s  \u2502  200DMA hysteresis: +4%% / \u22122%%",
                n_avail, if (n_avail != 1) "s" else "")
      else
        sprintf("%d / %d tickers (%d skipped — not in returns data)  \u2502  +4%% / \u22122%%",
                n_avail, n_total, n_total - n_avail)
    })

    output$panels_ui <- renderUI({
      tks <- avail_tickers()
      if (length(tks) == 0)
        return(div(style = paste0("color:", TEXT_DIM, "; padding:20px;"),
                   "No tickers match current filters or returns data not loaded."))

      plot_list <- lapply(seq_along(tks), function(i) {
        div(
          style = "border-bottom:1px solid #2a2c35; padding-bottom:4px;",
          plotOutput(ns(paste0("plot_", i)), height = "550px")
        )
      })
      do.call(tagList, plot_list)
    })

    observe({
      tks <- avail_tickers()

      for (i in seq_along(tks)) {
        local({
          tk      <- tks[[i]]
          plot_id <- paste0("plot_", i)

          output[[plot_id]] <- renderPlot({
            tryCatch({
              # Build base-1 wealth index (required by get_enriched_xts_data)
              wealth <- cumprod(1 + xts_ret_shiny[, tk])
              colnames(wealth) <- tk

              # plot_200DMA prints internally — renderPlot captures the device output
              plot_200DMA(
                ticker_symbol = tk,
                xts_wealth    = wealth,
                xts_price     = xts_ret_shiny[, tk, drop = FALSE],
                upper         = 0.04,
                lower         = -0.02
              )
            }, error = function(e) {
              ggplot() +
                annotate("text", x = 0.5, y = 0.5,
                         label = paste0(tk, ": ", conditionMessage(e)),
                         colour = "grey50", size = 3.5) +
                theme_void()
            })
          }, bg = "white")
        })
      }
    })
  })
}
