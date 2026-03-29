# ==============================================================================
# shiny_dashboard/modules/mod_treemap.R
# PURPOSE : Finviz-style treemap module (plotly, YTD colour, equal tile sizes)
# ==============================================================================

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
    plotlyOutput(ns("treemap"), height = "calc(100vh - 290px)")
  )
}

mod_treemap_server <- function(id, filtered, treemap_group) {
  moduleServer(id, function(input, output, session) {

    output$treemap <- renderPlotly({
      df      <- filtered()
      grp_col <- treemap_group()

      finviz_scale <- list(
        c(0.00, "#7f0000"), c(0.30, "#cc2222"), c(0.46, "#f4aaaa"),
        c(0.50, "#555555"),
        c(0.54, "#a3f0bc"), c(0.70, "#1a7a2e"), c(1.00, "#00AA44")
      )

      dot <- function(x) dplyr::case_when(
        x >  0.01 ~ "\U1F7E2",
        x < -0.01 ~ "\U1F534",
        TRUE      ~ "\u26AA"
      )

      cluster_order <- c("Core Assets", "SPY Sectors", "Rest of Universe")

      sectors <- df %>%
        group_by(grp = .data[[grp_col]]) %>%
        summarise(
          n_tickers = n(),
          color     = median(ret_ytd * 100, na.rm = TRUE),
          ret1m_med = median(ret_1m  * 100, na.rm = TRUE),
          .groups   = "drop"
        ) %>%
        mutate(
          ids       = grp,
          labels    = grp,
          parents   = "",
          values    = n_tickers,
          tile_text = sprintf("<b>%s</b><br>YTD med: %+.1f%%<br>%s 1M med: %+.1f%%",
                              grp, color, dot(ret1m_med / 100), ret1m_med),
          hover     = sprintf("%s<br>YTD median: %+.1f%%<br>1M median: %+.1f%%",
                              grp, color, ret1m_med),
          ord       = match(grp, cluster_order, nomatch = 99L)
        ) %>%
        arrange(ord) %>%
        select(-ord)

      tickers <- df %>%
        mutate(
          ids       = symbol,
          labels    = symbol,
          parents   = .data[[grp_col]],
          values    = 1,
          color     = ret_ytd * 100,
          tile_text = sprintf("<b>%s</b><br>YTD: %+.1f%%<br>%s %+.1f%%",
                              symbol, ret_ytd * 100, dot(ret_1m), ret_1m * 100),
          hover     = sprintf(
            "<b>%s</b><br>%s<br>YTD: %+.1f%%<br>1M: %+.1f%%<br>1D: %+.1f%%",
            symbol, coalesce(name, ""),
            ret_ytd * 100, ret_1m * 100, ret_1d * 100)
        ) %>%
        select(ids, labels, parents, values, color, tile_text, hover)

      nodes <- bind_rows(
        sectors %>% select(ids, labels, parents, values, color, tile_text, hover),
        tickers
      )

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
          textfont      = list(color = "#ffffff", size = 11, family = "Inter"),
          marker = list(
            colors     = nodes$color,
            colorscale = finviz_scale,
            cmid       = 0,
            showscale  = TRUE,
            colorbar   = list(
              title       = list(text = "YTD %", font = list(color = "#aaa", size = 11)),
              tickfont    = list(color = "#aaa", size = 10),
              tickformat  = "+.1f", ticksuffix = "%",
              len = 0.6, thickness = 14,
              bgcolor = "#111318", bordercolor = "#2a2c35"
            ),
            line = list(width = 1.2, color = "#111318")
          ),
          sort    = FALSE,
          tiling  = list(packing = "squarify"),
          pathbar = list(visible = TRUE, thickness = 22,
                         textfont = list(color = "#d1d5db", size = 11))
        ) %>%
        plotly::layout(
          paper_bgcolor = DARK_BG,
          plot_bgcolor  = DARK_BG,
          margin        = list(t = 10, b = 10, l = 10, r = 10),
          font          = list(color = TEXT_MAIN, family = "Inter")
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
  })
}
