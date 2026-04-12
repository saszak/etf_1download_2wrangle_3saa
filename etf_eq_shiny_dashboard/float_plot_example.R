# ==============================================================================
# float_plot_example.R
# Standalone Shiny example: white-card float plot with BMK dropdown
# Run:  shiny::runApp("etf_eq_shiny_dashboard/float_plot_example.R")
# ==============================================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(here)

# ── Load data (same as global.R does) ─────────────────────────────────────────
if (!exists("xts_ret_shiny")) {
  xts_ret_shiny <- readRDS(here("02_data_processed/xts_ret_returns.rds"))
}

# ── Helpers ────────────────────────────────────────────────────────────────────
.cum_ret <- function(r) as.numeric(cumprod(1 + r) - 1) * 100

.max_dd <- function(cum_pct) {
  r <- cum_pct / 100
  peak <- cummax(1 + r)
  min((1 + r) / peak - 1) * 100
}

.kpi_subtitle <- function(tk, bmk, xts_ret, period = "1Y") {

  # window
  win_start <- switch(period,
    "1M"  = Sys.Date() -  30,
    "3M"  = Sys.Date() -  91,
    "6M"  = Sys.Date() - 182,
    "YTD" = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
    "1Y"  = Sys.Date() - 365,
    "3Y"  = Sys.Date() - 1095,
    "Max" = as.Date("2000-01-01"),
    Sys.Date() - 365
  )
  ytd_start <- as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01"))

  cols <- c(tk, bmk)
  cols <- cols[cols %in% colnames(xts_ret)]
  if (length(cols) < 2) return("— insufficient data —")

  win  <- paste0(format(win_start,  "%Y-%m-%d"), "/")
  ytdw <- paste0(format(ytd_start,  "%Y-%m-%d"), "/")

  r     <- xts_ret[win,  cols]
  r_ytd <- xts_ret[ytdw, cols]
  if (nrow(r) < 5) return("— insufficient data —")

  cum_tk  <- .cum_ret(r[, tk])
  cum_bmk <- .cum_ret(r[, bmk])
  ytd_tk  <- tail(.cum_ret(r_ytd[, tk]),  1)
  ytd_bmk <- tail(.cum_ret(r_ytd[, bmk]), 1)

  # 1-day
  r_1d    <- tail(xts_ret[, tk],  1)
  d1_tk   <- as.numeric(r_1d) * 100
  d1_bmk  <- as.numeric(tail(xts_ret[, bmk], 1)) * 100

  # Metrics
  rel_perf  <- tail(cum_tk, 1) - tail(cum_bmk, 1)
  max_dd    <- .max_dd(cum_tk)
  rel_dd    <- .max_dd(cum_tk - cum_bmk)

  line1 <- sprintf(
    "YTD %s%+.1f%%   1D %+.1f%%   RelPerf %+.1f%%",
    if (tk == bmk) "" else paste0("(", bmk, " ", sprintf("%+.1f%%", ytd_bmk), ")  "),
    ytd_tk, d1_tk, rel_perf
  )
  line2 <- sprintf(
    "MaxDD %.1f%%   MaxRelDD %.1f%%   vs %s",
    max_dd, rel_dd, bmk
  )

  paste0(line1, "\n", line2)
}

.win_x_scales <- function(period) {
  brk <- switch(period,
    "1M"  = "1 week",  "3M"  = "1 month", "6M"  = "1 month",
    "YTD" = "1 month", "1Y"  = "3 months","3Y"  = "6 months",
    "Max" = "1 year",  "3 months"
  )
  lbl <- switch(period,
    "1M" = "%d%b", "3M" = "%b%y", "6M" = "%b%y", "YTD" = "%b%y",
    "1Y" = "%b%y", "3Y" = "%b%y", "Max" = "%Y", "%b%y"
  )
  list(breaks = scales::breaks_width(brk), labels = lbl)
}

.white_theme <- function() {
  theme_minimal(base_size = 10) +
    theme(
      plot.background  = element_rect(fill = "white", colour = "#e5e7eb"),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_line(colour = "#f3f4f6", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(colour = "#111827", size = 11, face = "bold"),
      plot.subtitle    = element_text(colour = "#4b5563", size = 7.5, lineheight = 1.4),
      axis.text        = element_text(colour = "#374151", size = 7.5, face = "bold"),
      axis.ticks       = element_line(colour = "#d1d5db"),
      plot.margin      = margin(8, 10, 6, 6)
    )
}

.float_plot <- function(tk, bmk, xts_ret, period = "1Y", use_rel = FALSE) {

  lbl <- tk  # in full app, swap for display_label

  win_start <- switch(period,
    "1M"  = Sys.Date() -  30,  "3M"  = Sys.Date() -  91,
    "6M"  = Sys.Date() - 182,  "YTD" = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
    "1Y"  = Sys.Date() - 365,  "3Y"  = Sys.Date() - 1095,
    "Max" = as.Date("2000-01-01"), Sys.Date() - 365
  )

  cols <- c(tk, bmk)
  if (!all(cols %in% colnames(xts_ret))) {
    return(ggplot() + annotate("text", 0.5, 0.5, label = paste(tk, "or", bmk, "not found"),
                               colour = "#6b7280") + theme_void())
  }

  win <- paste0(format(win_start, "%Y-%m-%d"), "/")
  r   <- xts_ret[win, cols]
  if (nrow(r) < 10) {
    return(ggplot() + annotate("text", 0.5, 0.5, label = "Insufficient data",
                               colour = "#6b7280") + theme_void())
  }

  cum_tk  <- .cum_ret(r[, tk])
  cum_bmk <- .cum_ret(r[, bmk])
  dates   <- as.Date(zoo::index(r))
  xsc     <- .win_x_scales(period)

  if (use_rel) {
    df <- tibble(date = dates, val = cum_tk - cum_bmk,
                 series = paste0(lbl, " \u2212 ", bmk))
    end_lbl <- df %>% filter(date == max(date))
    p <- ggplot(df, aes(date, val)) +
      geom_hline(yintercept = 0, colour = "#9ca3af", linewidth = 0.5, linetype = "dashed") +
      geom_area(fill = "#3b82f6", alpha = 0.10) +
      geom_line(colour = "#3b82f6", linewidth = 1) +
      geom_text(data = end_lbl, aes(label = sprintf("%+.1f%%", val)),
                hjust = -0.1, size = 2.8, colour = "#3b82f6", fontface = "bold") +
      scale_x_date(breaks = xsc$breaks, date_labels = xsc$labels,
                   expand = expansion(mult = c(0, 0.08))) +
      scale_y_continuous(labels = function(x) paste0(ifelse(x >= 0, "+", ""), round(x, 1), "%")) +
      labs(title = paste0(lbl, " \u2212 ", bmk, "  \u2022  ", period),
           subtitle = .kpi_subtitle(tk, bmk, xts_ret, period),
           x = NULL, y = NULL) +
      .white_theme()

  } else {
    df <- tibble(date = dates,
                 !!lbl := cum_tk,
                 !!bmk  := cum_bmk) %>%
      pivot_longer(-date, names_to = "series", values_to = "val")

    end_lbl <- df %>% group_by(series) %>% filter(date == max(date)) %>% ungroup()

    col_vals  <- c("#2563eb", "#16a34a")               # blue = ticker, green = BMK
    names(col_vals) <- c(lbl, bmk)
    size_vals <- c(1.1, 0.7)
    names(size_vals) <- c(lbl, bmk)

    p <- ggplot(df, aes(date, val, colour = series, linewidth = series)) +
      geom_hline(yintercept = 0, colour = "#9ca3af", linewidth = 0.4) +
      geom_line() +
      geom_text(data = end_lbl, aes(label = sprintf("%s\n%+.1f%%", series, val)),
                hjust = -0.05, size = 2.6, fontface = "bold",
                show.legend = FALSE) +
      scale_colour_manual(values = col_vals) +
      scale_linewidth_manual(values = size_vals) +
      scale_x_date(breaks = xsc$breaks, date_labels = xsc$labels,
                   expand = expansion(mult = c(0, 0.12))) +
      scale_y_continuous(labels = function(x) paste0(ifelse(x >= 0, "+", ""), round(x, 1), "%")) +
      labs(title = paste0(lbl, " vs ", bmk, "  \u2022  ", period),
           subtitle = .kpi_subtitle(tk, bmk, xts_ret, period),
           x = NULL, y = NULL, colour = NULL, linewidth = NULL) +
      guides(colour = "none", linewidth = "none") +
      .white_theme()
  }

  p
}

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(tags$style(HTML("body { background: #f9fafb; }"))),
  div(
    style = paste0(
      "max-width:580px; margin:40px auto;",
      "background:white; border-radius:10px;",
      "box-shadow:0 2px 16px rgba(0,0,0,0.10); padding:16px;"
    ),
    # ── Controls row ──────────────────────────────────────────────────────────
    div(
      style = "display:flex; align-items:center; gap:12px; margin-bottom:10px; flex-wrap:wrap;",
      div(
        style = "display:flex; align-items:center; gap:6px;",
        tags$label("Ticker:", style = "font-size:12px; color:#374151; font-weight:600;"),
        selectInput("tk", NULL,
          choices  = c("QQQ","SMH","XLK","GLD","URTH","IEF","HYG","TLT"),
          selected = "QQQ",
          width    = "90px"
        )
      ),
      div(
        style = "display:flex; align-items:center; gap:6px;",
        tags$label("BMK:", style = "font-size:12px; color:#374151; font-weight:600;"),
        selectInput("bmk", NULL,
          choices  = c("SPY","URTH","ACWI"),
          selected = "SPY",
          width    = "90px"
        )
      ),
      div(
        style = "display:flex; align-items:center; gap:6px;",
        tags$label("Period:", style = "font-size:12px; color:#374151; font-weight:600;"),
        selectInput("period", NULL,
          choices  = c("1M","3M","6M","YTD","1Y","3Y","Max"),
          selected = "1Y",
          width    = "80px"
        )
      ),
      div(
        style = "margin-top:4px;",
        checkboxInput("use_rel", "Relative mode", value = FALSE)
      )
    ),
    # ── Plot ──────────────────────────────────────────────────────────────────
    plotOutput("float_plot", height = "340px")
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # If ticker == BMK, fall back to SPY as BMK
  safe_bmk <- reactive({
    if (input$tk == input$bmk) "SPY" else input$bmk
  })

  output$float_plot <- renderPlot({
    req(input$tk, input$bmk, input$period)
    .float_plot(
      tk      = input$tk,
      bmk     = safe_bmk(),
      xts_ret = xts_ret_shiny,
      period  = input$period,
      use_rel = isTRUE(input$use_rel)
    )
  }, bg = "white")
}

shinyApp(ui, server)
