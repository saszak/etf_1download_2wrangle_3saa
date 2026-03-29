# ==============================================================================
# shiny_dashboard/modules/mod_absrel.R
# PURPOSE : Absolute vs Relative calendar-year performance — one P11c-style
#           dual-panel chart per filtered ticker, stacked vertically.
#           Each chart: Left = absolute cumulative return | Right = vs SPY (α)
#           Shared Y axis · Each year as a coloured line · Current year in red.
# ==============================================================================

mod_absrel_ui <- function(id) {
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
        "Left = absolute return  \u2502  Right = vs SPY (\u03b1)  \u2502",
        "  Shared Y axis  \u2502  Current year in red"
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

mod_absrel_server <- function(id, filtered, relative_mode, bmk) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Effective benchmark: sidebar selection when Relative mode is on, else SPY
    eff_bmk <- reactive({
      if (isTRUE(relative_mode())) bmk() else "SPY"
    })

    # xts_rel to use: pre-computed vs SPY, or computed on the fly vs chosen bmk
    xts_rel_live <- reactive({
      b <- eff_bmk()
      if (b == "SPY") return(xts_rel_shiny)
      # Compute relative returns vs chosen bmk (log returns → subtraction)
      if (!b %in% colnames(xts_ret_shiny)) return(xts_rel_shiny)
      xts_ret_shiny - as.numeric(xts_ret_shiny[, b])
    })

    # Tickers available in both xts objects (intersection with filtered set)
    avail_tickers <- reactive({
      syms   <- filtered()$symbol
      in_ret <- syms[syms %in% colnames(xts_ret_shiny)]
      in_rel <- in_ret[in_ret %in% colnames(xts_rel_live())]
      in_rel
    })

    output$caption <- renderText({
      n_avail <- length(avail_tickers())
      n_total <- nrow(filtered())
      bmk_str <- paste0("vs ", eff_bmk())
      if (n_avail == n_total)
        sprintf("%d ticker%s  \u2502  %s", n_avail, if (n_avail != 1) "s" else "", bmk_str)
      else
        sprintf("%d / %d tickers (%d skipped)  \u2502  %s",
                n_avail, n_total, n_total - n_avail, bmk_str)
    })

    # One plotOutput per ticker, dynamically generated
    output$panels_ui <- renderUI({
      tks <- avail_tickers()
      if (length(tks) == 0)
        return(div(style = paste0("color:", TEXT_DIM, "; padding:20px;"),
                   "No tickers match current filters or returns data not loaded."))

      plot_output_list <- lapply(seq_along(tks), function(i) {
        plot_id <- ns(paste0("plot_", i))
        div(
          style = "border-bottom:1px solid #2a2c35; padding-bottom:4px;",
          plotOutput(plot_id, height = "430px")
        )
      })

      do.call(tagList, plot_output_list)
    })

    # Render each plot independently — re-fires when eff_bmk() changes
    observe({
      tks      <- avail_tickers()
      rel_xts  <- xts_rel_live()
      bmk_lbl  <- eff_bmk()
      for (i in seq_along(tks)) {
        local({
          tk      <- tks[[i]]
          rx      <- rel_xts
          bl      <- bmk_lbl
          plot_id <- paste0("plot_", i)
          output[[plot_id]] <- renderPlot({
            tryCatch(
              plot_rel_calendar_perf(tk, xts_ret_shiny, rx, bmk_label = bl),
              error = function(e) {
                ggplot() +
                  annotate("text", x = 0.5, y = 0.5,
                           label = paste0(tk, ": ", conditionMessage(e)),
                           colour = "grey50", size = 4) +
                  theme_void()
              }
            )
          }, bg = "white")
        })
      }
    })
  })
}
