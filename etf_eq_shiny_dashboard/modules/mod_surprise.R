# ==============================================================================
# shiny_dashboard/modules/mod_surprise.R
# PURPOSE : "Surprise Moves" — identify ETFs with statistically unusual
#           relative performance vs SPY over a chosen lookback window.
#
# METHOD  : For each ticker in xts_rel_shiny (already ticker − SPY daily ret):
#   1. Compute cumulative relative return over the current window (sum of log-rets)
#   2. Roll the same-length window over the full history → distribution
#   3. Z-score = (current − mean_hist) / sd_hist
#   4. |Z| ≥ 2 = historically unusual move
#
# WINDOWS : 1W (5d) | 1M (22d) | 1Q (63d) | YTD | 1Y (252d)
# ==============================================================================

.SURP_WINDOWS <- c("1W" = 5L, "1M" = 22L, "1Q" = 63L, "1Y" = 252L)

# ── UI ─────────────────────────────────────────────────────────────────────────
mod_surprise_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = paste0(
        "background:", DARK_HDR, "; color:", TEXT_DIM, ";",
        " font-size:11px; padding:5px 12px; margin-bottom:4px;",
        " display:flex; gap:18px; align-items:center;"
      ),
      span(
        "Z-score of cumulative return vs SPY  \u2502  ",
        "Scored against own rolling history of same-length windows  \u2502  ",
        "\u00b12\u03c3 = historically unusual"
      ),
      div(
        style = "margin-left:auto; display:flex; align-items:center; gap:8px;",
        span("Window:"),
        radioButtons(ns("window"), NULL,
          choices  = c("1W", "1M", "1Q", "YTD", "1Y"),
          selected = "1Q",
          inline   = TRUE
        )
      )
    ),
    layout_columns(
      col_widths = c(7, 5),
      div(
        style = "overflow-y:auto; height:calc(100vh - 302px); background:#f8f9fa;",
        plotOutput(ns("zplot"), height = "calc(100vh - 302px)")
      ),
      div(
        style = paste0("overflow-y:auto; height:calc(100vh - 302px); background:", DARK_PANEL, ";"),
        reactableOutput(ns("ztable"))
      )
    )
  )
}

# ── Server ─────────────────────────────────────────────────────────────────────
mod_surprise_server <- function(id, filtered) {
  moduleServer(id, function(input, output, session) {

    # ── Core z-score computation ───────────────────────────────────────────────
    surprise_df <- reactive({
      win_label <- input$window

      n_days <- if (win_label == "YTD") {
        sum(zoo::index(xts_rel_shiny) >= ytd_start)
      } else {
        .SURP_WINDOWS[[win_label]]
      }
      n_days <- max(n_days, 1L)

      syms <- filtered()$symbol
      syms <- syms[syms %in% colnames(xts_rel_shiny)]
      syms <- syms[syms != "SPY"]

      if (length(syms) == 0L || nrow(xts_rel_shiny) < n_days * 2L) {
        return(tibble())
      }

      rel_mat <- as.matrix(xts_rel_shiny[, syms, drop = FALSE])
      n_total <- nrow(rel_mat)

      result <- lapply(syms, function(tk) {
        x        <- rel_mat[, tk]
        x[is.na(x)] <- 0

        # Rolling cumulative sum over n_days (log-ret approximation)
        roll_cum <- vapply(n_days:n_total, function(i)
          sum(x[(i - n_days + 1L):i]),
          numeric(1L)
        )

        cur_val  <- roll_cum[length(roll_cum)]
        hist_val <- roll_cum[-length(roll_cum)]

        mu  <- mean(hist_val, na.rm = TRUE)
        sig <- sd(hist_val,   na.rm = TRUE)
        z   <- if (!is.na(sig) && sig > 1e-8) (cur_val - mu) / sig else NA_real_

        tibble(
          ticker    = tk,
          cum_rel   = cur_val  * 100,
          hist_mean = mu       * 100,
          hist_sd   = sig      * 100,
          z_score   = z
        )
      })

      bind_rows(result) %>%
        left_join(
          perf_data %>% select(symbol, short_name, asset_class),
          by = c("ticker" = "symbol")
        ) %>%
        filter(!is.na(z_score)) %>%
        arrange(z_score)
    })

    # ── Bar chart ─────────────────────────────────────────────────────────────
    output$zplot <- renderPlot({
      df <- surprise_df()
      if (nrow(df) == 0) {
        return(
          ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "No data for current filters.",
                     colour = "#9ca3af", size = 4) +
            theme_void(base_size = 11) +
            theme(plot.background = element_rect(fill = "#f8f9fa", colour = NA))
        )
      }

      win_label <- input$window

      df <- df %>%
        mutate(
          lbl      = .tk_label(ticker, short_name),
          lbl      = factor(lbl, levels = lbl),
          fill_col = dplyr::case_when(
            z_score >=  2  ~ "#1a5e35",   # deep forest green  (treemap top)
            z_score <= -2  ~ "#922b21",   # deep institutional red (treemap bottom)
            z_score >=  0  ~ "#27ae60",   # medium green
            TRUE           ~ "#e74c3c"    # medium red
          ),
          txt_hjust = if_else(z_score >= 0, 0, 1),
          txt_x     = z_score + if_else(z_score >= 0, 0.12, -0.12)
        )

      x_range   <- max(abs(df$z_score), 2.5, na.rm = TRUE) * 1.22
      n_sig     <- sum(abs(df$z_score) >= 2)
      bar_width <- if (nrow(df) > 40) 0.55 else 0.70

      ggplot(df, aes(x = z_score, y = lbl)) +
        # ±2σ bands (shaded) — treemap palette
        annotate("rect",
          xmin = -Inf, xmax = -2, ymin = -Inf, ymax = Inf,
          fill = "#fde8e6", alpha = 0.70) +
        annotate("rect",
          xmin =  2, xmax =  Inf, ymin = -Inf, ymax = Inf,
          fill = "#e8f8f0", alpha = 0.70) +
        # zero line
        geom_vline(xintercept = 0,
                   colour = "#9ca3af", linewidth = 0.4) +
        # ±2σ dashed lines
        geom_vline(xintercept = c(-2, 2),
                   linetype = "dashed", colour = "#7f8c8d", linewidth = 0.5) +
        # bars
        geom_col(aes(fill = fill_col), width = bar_width) +
        scale_fill_identity() +
        # labels: show % and z-score
        geom_text(
          aes(x     = txt_x,
              label = sprintf("%+.1f%% (z=%.2f)", cum_rel, z_score),
              hjust = txt_hjust),
          colour = "#2c3e50", size = 2.7
        ) +
        # ±2σ axis annotations
        annotate("text", x = -2, y = nrow(df) + 0.6, label = "\u22122\u03c3",
                 colour = "#7f8c8d", size = 3, vjust = 0) +
        annotate("text", x =  2, y = nrow(df) + 0.6, label = "+2\u03c3",
                 colour = "#7f8c8d", size = 3, vjust = 0) +
        scale_x_continuous(
          limits   = c(-x_range, x_range),
          expand   = expansion(0)
        ) +
        labs(
          title    = sprintf(
            "Surprise Moves \u2014 %s window  \u2502  %d of %d outside \u00b12\u03c3",
            win_label, n_sig, nrow(df)
          ),
          subtitle = paste0(
            "Cumulative return vs SPY, z-scored against own rolling history  \u2502  ",
            "Green = outperform  |  Red = underperform"
          ),
          x = "Z-score",
          y = NULL
        ) +
        theme_minimal(base_size = 11) +
        theme(
          plot.background    = element_rect(fill = "#f8f9fa", colour = NA),
          panel.background   = element_rect(fill = "#f8f9fa", colour = NA),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = "#e8eaed", linewidth = 0.4),
          panel.grid.minor   = element_blank(),
          axis.text.x        = element_text(colour = "#1B3A6B", face = "bold", size = 9),
          axis.text.y        = element_text(
            colour    = "#1B3A6B",
            face      = "bold",
            size      = 9
          ),
          axis.title.x       = element_text(colour = "#1B3A6B", face = "bold", size = 9),
          plot.title         = element_text(colour = "#1c2833", face = "bold", size = 12),
          plot.subtitle      = element_text(colour = "#7f8c8d", size = 9),
          plot.margin        = margin(8, 16, 8, 8)
        )
    }, bg = "#f8f9fa")

    # ── Table (sorted by |z|) ─────────────────────────────────────────────────
    output$ztable <- renderReactable({
      df <- surprise_df()
      if (nrow(df) == 0) return(NULL)

      df_tbl <- df %>%
        arrange(desc(abs(z_score))) %>%
        mutate(
          label = .tk_label(ticker, short_name),
          flag  = dplyr::case_when(
            z_score >=  2  ~ "\U1F7E2 Surge",
            z_score <= -2  ~ "\U1F534 Drop",
            z_score >=  1  ~ "\u26AA Mild +",
            z_score <= -1  ~ "\u26AA Mild \u2212",
            TRUE           ~ "\u2013 Neutral"
          )
        ) %>%
        select(label, asset_class, cum_rel, hist_mean, hist_sd, z_score, flag)

      reactable(
        df_tbl,
        searchable      = TRUE,
        striped         = TRUE,
        highlight       = TRUE,
        defaultPageSize = 60,
        theme           = tbl_theme,
        defaultColDef   = colDef(vAlign = "center", headerVAlign = "bottom"),
        columns = list(
          label       = colDef(
            name     = "Ticker",
            minWidth = 150,
            style    = list(fontWeight = "700", color = BLUE_TICK, fontSize = "12px")
          ),
          asset_class = colDef(
            name  = "Class",
            width = 90,
            style = list(color = TEXT_DIM, fontSize = "11px")
          ),
          cum_rel     = colDef(
            name   = "Cum Rel%",
            width  = 85,
            format = colFormat(digits = 1, suffix = "%"),
            style  = function(v) list(
              color      = if (!is.na(v) && v >= 0) "#27ae60" else "#e74c3c",
              fontWeight = "700",
              fontSize   = "12px"
            )
          ),
          hist_mean   = colDef(
            name   = "Hist \u03bc%",
            width  = 80,
            format = colFormat(digits = 1, suffix = "%"),
            style  = list(fontSize = "11px", color = TEXT_DIM)
          ),
          hist_sd     = colDef(
            name   = "Hist \u03c3%",
            width  = 75,
            format = colFormat(digits = 1, suffix = "%"),
            style  = list(fontSize = "11px", color = TEXT_DIM)
          ),
          z_score     = colDef(
            name   = "Z",
            width  = 65,
            format = colFormat(digits = 2),
            style  = function(v) list(
              color      = if (!is.na(v) && abs(v) >= 2) "#fbbf24" else TEXT_DIM,
              fontWeight = if (!is.na(v) && abs(v) >= 2) "700" else "400",
              fontSize   = "12px"
            )
          ),
          flag        = colDef(
            name  = "Signal",
            width = 90,
            style = list(fontSize = "11px")
          )
        )
      )
    })
  })
}
