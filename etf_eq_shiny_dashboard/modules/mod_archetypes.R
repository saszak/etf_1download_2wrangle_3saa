# ==============================================================================
# shiny_dashboard/modules/mod_archetypes.R
# PURPOSE : Regime Archetypes sub-panel (4th pill in Regime tab).
#           Bar chart: count per archetype across equity universe vs SPY.
#           Click a bar to filter the detail table below.
#
# DATA    : archetype_data — built in global.R from equity_spread_screen() +
#           archetype rules (Cycle Amplifier / Recovery Sprinter /
#           Defensive Burden / All-Regime Drag / Corr-Fail / Other)
# ==============================================================================

# ── Palette ────────────────────────────────────────────────────────────────────
ARCH_COLOURS <- c(
  "Cycle Amplifier"  = "#22c55e",
  "Recovery Sprinter"= "#f59e0b",
  "Defensive Burden" = "#3b82f6",
  "All-Regime Drag"  = "#ef4444",
  "Corr-Fail"        = "#9ca3af",
  "Other"            = "#6b7280"
)

ARCH_ORDER <- c("Cycle Amplifier", "Recovery Sprinter",
                "Defensive Burden", "All-Regime Drag",
                "Corr-Fail", "Other")

# ── Plot function ──────────────────────────────────────────────────────────────
.plot_archetype_bar <- function(arch_df, date_range_label = "") {
  counts <- arch_df %>%
    count(archetype) %>%
    mutate(
      archetype = factor(archetype, levels = ARCH_ORDER),
      fill_col  = ARCH_COLOURS[as.character(archetype)]
    ) %>%
    filter(!is.na(archetype))

  n_total <- nrow(arch_df)
  n_eq    <- arch_df %>% filter(archetype != "Corr-Fail") %>% nrow()

  subtitle <- paste0(
    "Fall \u226510%  |  Corr-Fail = \u03c1 < 0.75",
    if (nzchar(date_range_label)) paste0("  |  ", date_range_label) else ""
  )

  ggplot(counts, aes(x = archetype, y = n, fill = archetype)) +
    geom_col(width = 0.65, show.legend = FALSE) +
    geom_text(
      aes(label = n),
      vjust = -0.4,
      size  = 4.5,
      fontface = "bold",
      colour = "white"
    ) +
    scale_fill_manual(values = ARCH_COLOURS) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.15)),
      breaks = scales::pretty_breaks(5)
    ) +
    labs(
      title    = paste0("Equity Universe Regime Archetypes \u2014 ",
                        n_total, " tickers vs SPY"),
      subtitle = subtitle,
      x = NULL,
      y = "Ticker Count"
    ) +
    theme_minimal(base_family = "Inter", base_size = 13) +
    theme(
      plot.background  = element_rect(fill = DARK_BG,  colour = NA),
      panel.background = element_rect(fill = DARK_BG,  colour = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(colour = "#2d3748", linewidth = 0.4),
      plot.title    = element_text(colour = TEXT_MAIN, face = "bold", size = 14),
      plot.subtitle = element_text(colour = TEXT_DIM,  size = 11),
      axis.text.x   = element_text(colour = TEXT_MAIN, size = 11),
      axis.text.y   = element_text(colour = TEXT_DIM,  size = 10),
      axis.title.y  = element_text(colour = TEXT_DIM,  size = 10)
    )
}

# ── UI ─────────────────────────────────────────────────────────────────────────
mod_archetypes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(7, 5),
      # Left: bar chart
      card(
        full_screen = TRUE,
        card_header(
          "Regime Archetypes — Equity Universe vs SPY",
          style = paste0("background:", DARK_HDR, "; color:", TEXT_DIM,
                         "; font-size:11px; padding:5px 12px;")
        ),
        div(
          style = paste0("color:", TEXT_DIM,
                         "; font-size:10px; padding:3px 12px 0;"),
          "Click a bar to filter the ticker table"
        ),
        plotOutput(ns("bar"), height = "calc(100vh - 400px)",
                   click = ns("bar_click"))
      ),
      # Right: detail table
      card(
        full_screen = TRUE,
        card_header(
          textOutput(ns("tbl_header"), inline = TRUE),
          style = paste0("background:", DARK_HDR, "; color:", TEXT_DIM,
                         "; font-size:11px; padding:5px 12px;")
        ),
        div(
          style = "overflow-y:auto; height:calc(100vh - 400px);",
          reactableOutput(ns("tbl"))
        )
      )
    )
  )
}

# ── Server ─────────────────────────────────────────────────────────────────────
mod_archetypes_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Date range label from archetype_data
    date_label <- tryCatch({
      dr <- range(index(xts_ret_shiny), na.rm = TRUE)
      paste0(format(dr[1], "%b %Y"), "\u2013", format(dr[2], "%b %Y"))
    }, error = function(e) "")

    # Clicked archetype (NULL = show all)
    selected_arch <- reactiveVal(NULL)

    observeEvent(input$bar_click, {
      counts <- archetype_data %>%
        count(archetype) %>%
        mutate(archetype = factor(archetype, levels = ARCH_ORDER))

      clicked <- nearPoints(
        counts %>% mutate(x_num = as.numeric(archetype)),
        input$bar_click,
        xvar = "x_num", yvar = "n",
        threshold = 30, maxpoints = 1
      )

      if (nrow(clicked) == 1) {
        arch <- as.character(clicked$archetype[1])
        if (identical(selected_arch(), arch)) {
          selected_arch(NULL)   # toggle off
        } else {
          selected_arch(arch)
        }
      }
    })

    # Bar chart
    output$bar <- renderPlot({
      p <- .plot_archetype_bar(archetype_data, date_label)

      # Highlight selected bar
      sel <- selected_arch()
      if (!is.null(sel)) {
        counts <- archetype_data %>%
          count(archetype) %>%
          mutate(archetype = factor(archetype, levels = ARCH_ORDER))
        p <- p +
          geom_col(
            data = counts %>% filter(archetype != sel),
            aes(x = archetype, y = n),
            fill = "#1f2937", width = 0.65, inherit.aes = FALSE
          )
      }
      p
    }, bg = DARK_BG)

    # Table header
    output$tbl_header <- renderText({
      sel <- selected_arch()
      if (is.null(sel)) "All Archetypes" else paste0(sel, " — tickers")
    })

    # Detail table
    filtered_arch <- reactive({
      sel <- selected_arch()
      df  <- archetype_data %>%
        select(
          Ticker    = ticker,
          Archetype = archetype,
          IR        = ir,
          `ρ (full)`= rho_full,
          `ρ (1y)`  = rho_1y,
          `Fall type`        = Fall_modal_type,
          `Recovery type`    = Recovery_modal_type,
          `Consol. type`     = Consolidation_modal_type,
          `Fall α`           = Fall_mean_alpha,
          `Rec. α`           = Recovery_mean_alpha
        )
      if (!is.null(sel)) df <- df %>% filter(Archetype == sel)
      df %>% arrange(Archetype, desc(IR))
    })

    output$tbl <- renderReactable({
      df <- filtered_arch()

      reactable(
        df,
        theme       = reactableTheme(
          backgroundColor = DARK_BG,
          color           = TEXT_MAIN,
          borderColor     = "#2d3748",
          stripedColor    = "#1a2035",
          headerStyle     = list(background = DARK_HDR, color = TEXT_DIM,
                                 fontSize = "11px")
        ),
        striped       = TRUE,
        highlight     = TRUE,
        compact       = TRUE,
        defaultPageSize = 30,
        columns = list(
          IR        = colDef(format = colFormat(digits = 2)),
          `ρ (full)`= colDef(format = colFormat(digits = 3)),
          `ρ (1y)`  = colDef(format = colFormat(digits = 3)),
          `Fall α`  = colDef(format = colFormat(digits = 3)),
          `Rec. α`  = colDef(format = colFormat(digits = 3)),
          Archetype = colDef(
            cell = function(value) {
              col <- ARCH_COLOURS[value]
              if (is.na(col)) col <- "#6b7280"
              div(style = paste0("color:", col, "; font-weight:600;"), value)
            }
          ),
          `Fall type` = colDef(
            cell = function(value) {
              if (is.na(value)) return("")
              col <- switch(value,
                HEDGE = "#22c55e", STABLE = "#86efac",
                ALPHA = "#3b82f6", LAG    = "#f59e0b", LOSS = "#ef4444",
                "#9ca3af")
              div(style = paste0("color:", col, "; font-weight:600;"), value)
            }
          ),
          `Recovery type` = colDef(
            cell = function(value) {
              if (is.na(value)) return("")
              col <- switch(value,
                HEDGE = "#22c55e", STABLE = "#86efac",
                ALPHA = "#3b82f6", LAG    = "#f59e0b", LOSS = "#ef4444",
                "#9ca3af")
              div(style = paste0("color:", col, "; font-weight:600;"), value)
            }
          ),
          `Consol. type` = colDef(
            cell = function(value) {
              if (is.na(value)) return("")
              col <- switch(value,
                HEDGE = "#22c55e", STABLE = "#86efac",
                ALPHA = "#3b82f6", LAG    = "#f59e0b", LOSS = "#ef4444",
                "#9ca3af")
              div(style = paste0("color:", col, "; font-weight:600;"), value)
            }
          )
        )
      )
    })
  })
}
