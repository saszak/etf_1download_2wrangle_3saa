# ==============================================================================
# shiny_dashboard/modules/mod_perf_table.R
# PURPOSE : Performance table module (reactable with grouping + relative mode)
# ==============================================================================

mod_perf_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = paste0("background:", DARK_HDR, "; color:", TEXT_DIM,
                       "; font-size:11px; padding:5px 12px; margin-bottom:4px;"),
        textOutput(ns("caption"), inline = TRUE)),
    reactableOutput(ns("table"), height = "calc(100vh - 290px)")
  )
}

mod_perf_table_server <- function(id, filtered, relative_mode, bmk, group_by_class) {
  moduleServer(id, function(input, output, session) {

    output$caption <- renderText({
      paste0("Data as of ", format(today, "%d %b %Y"))
    })

    output$table <- renderReactable({
      df         <- filtered() %>% arrange(display_rank)
      group_cols <- if (isTRUE(group_by_class())) "asset_class" else NULL

      col_defs_live <- col_defs

      # Relative mode: relabel return columns
      if (isTRUE(relative_mode())) {
        sfx <- paste0(" vs ", bmk())
        col_defs_live[["ret_1d"]]$name  <- paste0("%1D",  sfx)
        col_defs_live[["ret_5d"]]$name  <- paste0("%5D",  sfx)
        col_defs_live[["ret_1m"]]$name  <- paste0("%1M",  sfx)
        col_defs_live[["ret_mtd"]]$name <- paste0("%MTD", sfx)
        col_defs_live[["ret_ytd"]]$name <- paste0("%YTD", sfx)
      }

      # asset_class column: show only when NOT grouping
      col_defs_live[["asset_class"]] <- colDef(
        name  = "Asset Class", width = 120,
        show  = !isTRUE(group_by_class()),
        style = list(color = TEXT_DIM, fontSize = "11px")
      )

      reactable(
        df,
        groupBy         = group_cols,
        searchable      = TRUE,
        sortable        = TRUE,
        striped         = TRUE,
        highlight       = TRUE,
        defaultPageSize = 100,
        theme           = tbl_theme,
        columns         = col_defs_live,
        columnGroups    = list(
          colGroup(name = "Returns", columns = c("ret_1d","ret_5d","ret_1m","ret_mtd","ret_ytd")),
          colGroup(name = "Range",   columns = c("range_pct"))
        ),
        defaultColDef = colDef(vAlign = "center", headerVAlign = "bottom")
      )
    })
  })
}
