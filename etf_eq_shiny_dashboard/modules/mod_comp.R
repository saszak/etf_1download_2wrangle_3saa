# ==============================================================================
# shiny_dashboard/modules/mod_comp.R
# PURPOSE : Cumulative wealth comparison — 13d style, one chart per filtered ETF.
#           Each chart: ETF (purple) vs benchmark (grey), regime-shaded, end labels.
#           Benchmark = SPY by default; follows sidebar BMK choice when ticked.
# ==============================================================================

mod_comp_ui <- function(id) {
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
        "Purple = ETF  \u2502  Grey = benchmark  \u2502",
        "  Pink = Fall  \u2502  Green = Recovery  \u2502  Ann return at line end"
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

mod_comp_server <- function(id, filtered, relative_mode, bmk) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Effective benchmark
    eff_bmk <- reactive({
      if (isTRUE(relative_mode())) bmk() else "SPY"
    })

    # Tickers available in xts_ret_shiny (and benchmark must also be available)
    avail_tickers <- reactive({
      b    <- eff_bmk()
      syms <- filtered() %>% arrange(display_rank) %>% pull(symbol)
      syms <- syms[syms %in% colnames(xts_ret_shiny)]
      syms <- syms[syms != b]          # exclude benchmark row itself
      syms
    })

    output$caption <- renderText({
      n_avail <- length(avail_tickers())
      n_total <- nrow(filtered())
      b       <- eff_bmk()
      bmk_str <- paste0("vs ", b)
      if (n_avail >= n_total - 1L)     # -1 for the bmk row itself
        sprintf("%d ticker%s  \u2502  %s", n_avail, if (n_avail != 1) "s" else "", bmk_str)
      else
        sprintf("%d / %d tickers (%d skipped)  \u2502  %s",
                n_avail, n_total, n_total - n_avail, bmk_str)
    })

    output$panels_ui <- renderUI({
      tks <- avail_tickers()
      if (length(tks) == 0)
        return(div(style = paste0("color:", TEXT_DIM, "; padding:20px;"),
                   "No tickers match current filters or returns data not loaded."))

      plot_list <- lapply(seq_along(tks), function(i) {
        div(
          style = "border-bottom:1px solid #2a2c35; padding-bottom:4px;",
          plotOutput(ns(paste0("plot_", i)), height = "380px")
        )
      })
      do.call(tagList, plot_list)
    })

    observe({
      tks <- avail_tickers()
      b   <- eff_bmk()

      for (i in seq_along(tks)) {
        local({
          tk      <- tks[[i]]
          bmk_sym <- b
          plot_id <- paste0("plot_", i)

          output[[plot_id]] <- renderPlot({
            # Both series must exist
            if (!tk      %in% colnames(xts_ret_shiny) ||
                !bmk_sym %in% colnames(xts_ret_shiny)) return(NULL)

            tk_name  <- perf_data$short_name[perf_data$symbol == tk][1] %||% ""
            tk_lbl   <- .tk_label(tk, tk_name)

            ret_list <- list(
              tk      = xts_ret_shiny[, tk],
              bmk_sym = xts_ret_shiny[, bmk_sym]
            )
            names(ret_list) <- c(tk, bmk_sym)

            tryCatch(
              plot_multi_wealth_endlabel(
                ret_list = ret_list,
                rt       = rt_shiny,
                colours  = c("#7c3aed", "#9ca3af"),
                labels   = c(tk_lbl, bmk_sym)
              ) +
                labs(title = paste0(tk_lbl, "  \u2014  Cumulative Wealth vs ", bmk_sym)),
              error = function(e) {
                ggplot() +
                  annotate("text", x = 0.5, y = 0.5,
                           label = paste0(tk, ": ", conditionMessage(e)),
                           colour = "grey50", size = 3.5) +
                  theme_void()
              }
            )
          }, bg = "white")
        })
      }
    })
  })
}
