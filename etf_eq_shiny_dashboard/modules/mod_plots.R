# ==============================================================================
# shiny_dashboard/modules/mod_plots.R
# PURPOSE : Per-ticker 200DMA signal panel — one chart per filtered ETF,
#           assembled into a 2-column patchwork grid (style: exec summary §13c).
# ==============================================================================

mod_plots_ui <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    type = "tabs",
    tabPanel("200DMA",
      tagList(
        div(
          style = paste0(
            "background:", DARK_HDR, "; color:", TEXT_DIM, ";",
            " font-size:11px; padding:5px 12px; margin-bottom:4px;",
            " display:flex; justify-content:space-between; align-items:center;"
          ),
          textOutput(ns("caption"), inline = TRUE),
          span(style = paste0("color:", TEXT_DIM, "; font-size:10px;"),
               "Blue = price (rebased 100)  \u2502  Amber dashed = 200DMA  \u2502  Red shading = trend OFF")
        ),
        div(
          style = paste0(
            "overflow-y:auto; height:calc(100vh - 302px); background:", DARK_BG, ";"
          ),
          uiOutput(ns("panels_ui"))
        )
      )
    ),
    tabPanel("AbsRel",   mod_absrel_ui(ns("absrel"))),
    tabPanel("Cal Year", mod_calyear_ui(ns("calyear")))
  )
}

mod_plots_server <- function(id, filtered, relative_mode = reactive(FALSE), bmk = reactive("SPY")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$caption <- renderText({
      df <- filtered()
      n  <- nrow(df)
      sprintf("%d ticker%s selected", n, if (n != 1) "s" else "")
    })

    # Dynamic plotOutput height — 220px per row, min 300px
    output$panels_ui <- renderUI({
      df    <- filtered()
      n     <- nrow(df)
      if (n == 0) return(div(style = paste0("color:", TEXT_DIM, "; padding:20px;"),
                             "No tickers match current filters."))
      n_rows <- ceiling(n / 2)
      height <- paste0(max(300L, n_rows * 240L), "px")
      plotOutput(ns("panels"), height = height)
    })

    output$panels <- renderPlot({
      df      <- filtered() %>% arrange(display_rank)
      symbols <- df$symbol
      if (length(symbols) == 0) return(NULL)

      # Short-name lookup: ticker → curated short_name from perf_data
      name_lut <- setNames(perf_data$short_name, perf_data$symbol)

      # Build per-ticker panels (filter from pre-computed trend_signals_shiny)
      plots <- purrr::map(symbols, function(tk) {
        lbl <- name_lut[[tk]] %||% ""
        .ma200_single_panel(trend_signals_shiny, tk, label = lbl)
      }) %>% purrr::compact()

      if (length(plots) == 0) return(NULL)

      n_tickers <- length(plots)
      n_cols    <- if (n_tickers == 1L) 1L else 2L

      patchwork::wrap_plots(plots, ncol = n_cols) +
        patchwork::plot_annotation(
          title    = sprintf("Price vs 200DMA \u2014 %d Selected ETFs", n_tickers),
          subtitle = paste0(
            "Blue = price (rebased to 100 at first observation)  \u2502  ",
            "Amber dashed = 200DMA  \u2502  Red shading = trend OFF  \u2502  ",
            "% above MA shown top-right"
          ),
          theme = theme(
            plot.title    = element_text(face = "bold", size = 13),
            plot.subtitle = element_text(colour = "grey50",  size = 9),
            plot.margin   = margin(8, 8, 4, 8)
          )
        )
    }, bg = "white")

    mod_absrel_server("absrel",   filtered, relative_mode, bmk)
    mod_calyear_server("calyear", filtered, relative_mode, bmk)
  })
}
