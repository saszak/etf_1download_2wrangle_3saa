# ==============================================================================
# shiny_dashboard/modules/mod_calyear.R
# PURPOSE : Calendar analog projection — P12b style, one ticker per row.
#           Left panel = absolute return analog  |  Right = vs benchmark (α)
#           Grey = all years · Blue = analogs · Cone = 10–90th pct · Red = now
# ==============================================================================

mod_calyear_ui <- function(id) {
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
        "Left = absolute  \u2502  Right = vs benchmark (\u03b1)  \u2502",
        "  Blue = analogs  \u2502  Cone = 10\u201390th pct  \u2502  Red = current year"
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

mod_calyear_server <- function(id, filtered, relative_mode, bmk) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Effective benchmark
    eff_bmk <- reactive({
      if (isTRUE(relative_mode())) bmk() else "SPY"
    })

    # xts_rel live (same logic as AbsRel tab)
    xts_rel_live <- reactive({
      b <- eff_bmk()
      if (b == "SPY") return(xts_rel_shiny)
      if (!b %in% colnames(xts_ret_shiny)) return(xts_rel_shiny)
      xts_ret_shiny - as.numeric(xts_ret_shiny[, b])
    })

    # Tickers available in returns data
    avail_tickers <- reactive({
      syms   <- filtered()$symbol
      in_ret <- syms[syms %in% colnames(xts_ret_shiny)]
      in_ret[in_ret %in% colnames(xts_rel_live())]
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

    output$panels_ui <- renderUI({
      tks <- avail_tickers()
      if (length(tks) == 0)
        return(div(style = paste0("color:", TEXT_DIM, "; padding:20px;"),
                   "No tickers match current filters or returns data not loaded."))

      plot_list <- lapply(seq_along(tks), function(i) {
        div(
          style = "border-bottom:1px solid #2a2c35; padding-bottom:4px;",
          plotOutput(ns(paste0("plot_", i)), height = "430px")
        )
      })
      do.call(tagList, plot_list)
    })

    observe({
      tks     <- avail_tickers()
      rel_xts <- xts_rel_live()
      bmk_lbl <- eff_bmk()

      for (i in seq_along(tks)) {
        local({
          tk      <- tks[[i]]
          rx      <- rel_xts
          bl      <- bmk_lbl
          plot_id <- paste0("plot_", i)

          output[[plot_id]] <- renderPlot({
            tk_name <- perf_data$short_name[perf_data$symbol == tk][1] %||% ""
            tk_lbl  <- .tk_label(tk, tk_name)
            p_abs <- tryCatch(
              plot_calendar_analog(xts_ret_shiny[, tk], rt_shiny) +
                labs(title = paste0(tk_lbl, "  \u2014  Absolute")),
              error = function(e) {
                ggplot() + annotate("text", x=0.5, y=0.5,
                  label = paste0(tk, " abs: ", conditionMessage(e)),
                  colour = "grey50", size = 3.5) + theme_void()
              }
            )

            has_rel <- tk %in% colnames(rx)
            p_rel <- if (has_rel) {
              tryCatch(
                plot_calendar_analog(rx[, tk], rt_shiny) +
                  labs(title = paste0(tk_lbl, "  \u2014  vs ", bl, " (\u03b1)")),
                error = function(e) {
                  ggplot() + annotate("text", x=0.5, y=0.5,
                    label = paste0(tk, " rel: ", conditionMessage(e)),
                    colour = "grey50", size = 3.5) + theme_void()
                }
              )
            } else {
              ggplot() + annotate("text", x=0.5, y=0.5,
                label = paste0(tk, " not in relative returns"),
                colour = "grey50", size = 3.5) + theme_void()
            }

            (p_abs | p_rel) +
              patchwork::plot_annotation(
                theme = theme(plot.margin = margin(4, 4, 2, 4))
              )
          }, bg = "white")
        })
      }
    })
  })
}
