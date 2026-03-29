# ==============================================================================
# shiny_dashboard/modules/mod_regime.R
# PURPOSE : SPY DD Regime + Regime Statistics — two plots per filtered ticker.
#
#   Plot 1 (regime overlay) — plot_regime_overlay():
#     Benchmark cumulative return line with Fall/Recovery/Consolidation regime
#     bars, plus the filtered ticker overlaid as a second line.
#     → Benchmark defines the regime; ticker shows how it moved within those regimes.
#
#   Plot 2 (regime stats) — plot_regime_stats():
#     Each ticker's own regime statistics: avg return, % time, avg duration per
#     state (using the ticker's own drawdown to define its regime cycle).
#
#   BMK: SPY by default; follows sidebar "Relative to BMK" + benchmark selector.
# ==============================================================================

mod_regime_ui <- function(id) {
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
        "Top = BMK regime + rel cum return  \u2502",
        "  Bottom = ticker\u2019s own regime statistics  \u2502  T_fall = 10%"
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

mod_regime_server <- function(id, filtered, relative_mode, bmk) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    eff_bmk <- reactive({
      if (isTRUE(relative_mode())) bmk() else "SPY"
    })

    avail_tickers <- reactive({
      b    <- eff_bmk()
      syms <- filtered()$symbol
      syms <- syms[syms %in% colnames(xts_ret_shiny)]
      syms <- syms[syms != b]          # exclude benchmark itself (no self-overlay)
      syms
    })

    output$caption <- renderText({
      n_avail <- length(avail_tickers())
      n_total <- nrow(filtered())
      b       <- eff_bmk()
      if (n_avail >= n_total - 1L)
        sprintf("%d ticker%s  \u2502  Regime BMK: %s  \u2502  T_fall = 10%%",
                n_avail, if (n_avail != 1) "s" else "", b)
      else
        sprintf("%d / %d tickers (%d skipped)  \u2502  Regime BMK: %s",
                n_avail, n_total, n_total - n_avail, b)
    })

    output$panels_ui <- renderUI({
      tks <- avail_tickers()
      if (length(tks) == 0)
        return(div(style = paste0("color:", TEXT_DIM, "; padding:20px;"),
                   "No tickers match current filters or returns data not loaded."))

      plot_list <- lapply(seq_along(tks), function(i) {
        div(
          style = "border-bottom:1px solid #2a2c35; padding-bottom:8px; margin-bottom:4px;",
          # Plot 1: regime overlay (with Rel strip inside) + relative area panel
          plotOutput(ns(paste0("plot_ov_", i)),   height = "620px"),
          # Plot 2: regime statistics (shorter)
          plotOutput(ns(paste0("plot_stat_", i)), height = "320px")
        )
      })
      do.call(tagList, plot_list)
    })

    observe({
      tks     <- avail_tickers()
      bmk_sym <- eff_bmk()

      if (!bmk_sym %in% colnames(xts_ret_shiny)) return()

      bmk_ret <- xts_ret_shiny[, bmk_sym]

      for (i in seq_along(tks)) {
        local({
          tk      <- tks[[i]]
          br      <- bmk_ret
          b       <- bmk_sym
          ov_id   <- paste0("plot_ov_",   i)
          stat_id <- paste0("plot_stat_", i)

          # ── Plot 1: regime overlay + relative cumulative return ───────────
          output[[ov_id]] <- renderPlot({
            tryCatch(
              plot_regime_overlay_ext(
                ticker  = tk,
                bmk     = b,
                xts_ret = xts_ret_shiny
              ),
              error = function(e) {
                ggplot() +
                  annotate("text", x = 0.5, y = 0.5,
                           label = paste0("overlay ", tk, ": ", conditionMessage(e)),
                           colour = "grey50", size = 3.5) +
                  theme_void()
              }
            )
          }, bg = "white")

          # ── Plot 2: ticker regime statistics ──────────────────────────────
          output[[stat_id]] <- renderPlot({
            if (!tk %in% colnames(xts_ret_shiny)) return(NULL)
            tryCatch(
              plot_regime_stats(
                xts_ret_col = xts_ret_shiny[, tk],
                asset_name  = tk
              ),
              error = function(e) {
                ggplot() +
                  annotate("text", x = 0.5, y = 0.5,
                           label = paste0("stats ", tk, ": ", conditionMessage(e)),
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
