# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts/util_functions.R
# Purpose: Global Utility Library for Signal-Project & Technical Logic
# ==============================================================================

# ── html_to_pdf ────────────────────────────────────────────────────────────────
# Convert a rendered HTML file to PDF via Chrome headless (webshot2).
# Requires: webshot2, Chrome installed at the path below.
#
# USAGE
#   html_to_pdf("Rmd/my_report.html")
#   html_to_pdf("Rmd/my_report.html", "03_reports/my_report.pdf")
#   html_to_pdf("Rmd/my_report.html", delay = 3)   # extra wait for JS rendering
#
# PARAMETERS
#   html_path  chr — path to the .html file (relative or absolute)
#   pdf_path   chr — output PDF path; default = same name/location as html, .pdf extension
#   delay      num — seconds to wait for JS to render before screenshotting (default 1)
#   vwidth     num — viewport width in px (default 1400)
#   vheight    num — viewport height in px (default 900)
# ------------------------------------------------------------------------------
html_to_pdf <- function(html_path,
                        pdf_path = NULL,
                        delay    = 1,
                        vwidth   = 1400,
                        vheight  = 900) {

  if (!requireNamespace("webshot2", quietly = TRUE))
    stop("Install webshot2: install.packages('webshot2')")

  # Set Chrome path if not already in environment
  chrome <- Sys.getenv("CHROMOTE_CHROME")
  if (nchar(chrome) == 0) {
    default_chrome <- "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
    if (file.exists(default_chrome)) {
      Sys.setenv(CHROMOTE_CHROME = default_chrome)
      message(sprintf("🌐 Chrome found: %s", default_chrome))
    } else {
      stop("Chrome not found. Set CHROMOTE_CHROME in ~/.Renviron or install Chrome.")
    }
  }

  if (!file.exists(html_path))
    stop(sprintf("HTML file not found: %s", html_path))

  # Default output path: same dir/name as input, .pdf extension
  if (is.null(pdf_path))
    pdf_path <- sub("\\.html?$", ".pdf", html_path, ignore.case = TRUE)

  message(sprintf("📄 Converting: %s → %s", html_path, pdf_path))

  webshot2::webshot(
    url     = html_path,
    file    = pdf_path,
    delay   = delay,
    vwidth  = vwidth,
    vheight = vheight
  )

  if (file.exists(pdf_path))
    message(sprintf("✅ PDF saved: %s  (%.1f KB)", pdf_path, file.size(pdf_path) / 1024))
  else
    warning("⚠️  PDF not created — check webshot2 output above.")

  invisible(pdf_path)
}

# ── screen_52w ─────────────────────────────────────────────────────────────────
# Screen any list of tickers for proximity to their 52-week high / low.
# Downloads 1Y + 1M of daily OHLCV from Yahoo, computes metrics, returns a
# sorted tibble and optionally prints a range bar chart.
#
# USAGE
#   screen_52w(dow30)
#   screen_52w(dow30, near_low_pct = 0.05)       # flag within 5% of 52W low
#   screen_52w(c("AAPL","GLD","TLT"), plot = FALSE)
#
# PARAMETERS
#   tickers       chr vector — ticker symbols
#   near_low_pct  num — flag as near_low if pct_from_low <= this (default 0.10)
#   near_high_pct num — flag as near_high if abs(pct_from_high) <= this (default 0.10)
#   src           chr — data source passed to quantmod::getSymbols (default "yahoo")
#   plot          logical — print range bar chart (default TRUE)
#
# RETURNS (invisibly)
#   tibble: ticker | current | low_52w | high_52w | pct_from_low | pct_from_high |
#           range_pct | near_low | near_high
#   Sorted by pct_from_low ascending (closest to 52W low first)
# ------------------------------------------------------------------------------
screen_52w <- function(tickers,
                       near_low_pct  = 0.10,
                       near_high_pct = 0.10,
                       src           = "yahoo",
                       plot          = TRUE) {

  if (!requireNamespace("quantmod", quietly = TRUE))
    stop("Install quantmod: install.packages('quantmod')")

  from_date <- Sys.Date() - 395   # ~13 months to ensure 252 trading days

  message(sprintf("📥 Downloading %d tickers from %s ...", length(tickers), src))

  rows <- lapply(tickers, function(tk) {
    tryCatch({
      raw <- suppressMessages(
        quantmod::getSymbols(tk, src = src, from = from_date,
                             auto.assign = FALSE, warnings = FALSE)
      )
      ad  <- as.numeric(quantmod::Ad(raw))
      hi  <- as.numeric(quantmod::Hi(raw))
      lo  <- as.numeric(quantmod::Lo(raw))

      # Last 252 trading days = 1 year
      n       <- min(252L, length(ad))
      current <- last(ad)
      low_52w  <- min(lo[  (length(lo)  - n + 1):length(lo)],  na.rm = TRUE)
      high_52w <- max(hi[  (length(hi)  - n + 1):length(hi)],  na.rm = TRUE)

      tibble::tibble(
        ticker        = tk,
        current       = round(current, 2),
        low_52w       = round(low_52w,  2),
        high_52w      = round(high_52w, 2),
        pct_from_low  = (current / low_52w  - 1),
        pct_from_high = (current / high_52w - 1),   # always <= 0
        range_pct     = (high_52w / low_52w - 1)
      )
    }, error = function(e) {
      message(sprintf("  ✗ %s: %s", tk, conditionMessage(e)))
      NULL
    })
  })

  result <- dplyr::bind_rows(rows) %>%
    dplyr::mutate(
      near_low  = pct_from_low  <= near_low_pct,
      near_high = abs(pct_from_high) <= near_high_pct
    ) %>%
    dplyr::arrange(pct_from_low)

  # ── Console summary ───────────────────────────────────────────────────────────
  message(sprintf("\n── 52-Week Screen  (%s) ─────────────────────────────────────",
                  format(Sys.Date(), "%Y-%m-%d")))
  message(sprintf("%-6s  %8s  %8s  %8s  %+9s  %+9s",
                  "Ticker", "Current", "52W Low", "52W High", "% fr Low", "% fr High"))
  message(strrep("─", 60))
  for (i in seq_len(nrow(result))) {
    r   <- result[i, ]
    tag <- dplyr::case_when(r$near_low  ~ " ← NEAR LOW",
                            r$near_high ~ " ← NEAR HIGH",
                            TRUE        ~ "")
    message(sprintf("%-6s  %8.2f  %8.2f  %8.2f  %+8.1f%%  %+8.1f%%%s",
                    r$ticker, r$current, r$low_52w, r$high_52w,
                    r$pct_from_low * 100, r$pct_from_high * 100, tag))
  }
  n_low  <- sum(result$near_low,  na.rm = TRUE)
  n_high <- sum(result$near_high, na.rm = TRUE)
  message(strrep("─", 60))
  message(sprintf("Near 52W low  (≤%.0f%%): %d tickers", near_low_pct * 100, n_low))
  message(sprintf("Near 52W high (≤%.0f%%): %d tickers\n", near_high_pct * 100, n_high))

  # ── Range bar chart ───────────────────────────────────────────────────────────
  if (plot && nrow(result) > 0) {
    plot_df <- result %>%
      dplyr::mutate(
        ticker     = factor(ticker, levels = rev(ticker)),   # sorted: low first = bottom
        pos_in_range = pct_from_low / range_pct,            # 0=at low, 1=at high
        fill_col   = dplyr::case_when(
          near_low  ~ "#ef4444",
          near_high ~ "#22c55e",
          TRUE      ~ "#3b82f6"
        )
      )

    p <- ggplot2::ggplot(plot_df, ggplot2::aes(y = ticker)) +

      # Full range bar (grey background)
      ggplot2::geom_segment(
        ggplot2::aes(x = 0, xend = range_pct * 100, yend = ticker),
        colour = "#d1d5db", linewidth = 4, lineend = "round"
      ) +

      # Current position dot
      ggplot2::geom_point(
        ggplot2::aes(x = pct_from_low * 100, colour = fill_col),
        size = 3.5
      ) +

      # 52W low label
      ggplot2::geom_text(
        ggplot2::aes(x = 0, label = sprintf("$%.0f", low_52w)),
        hjust = 1.15, size = 2.6, colour = "#6b7280"
      ) +

      # 52W high label
      ggplot2::geom_text(
        ggplot2::aes(x = range_pct * 100, label = sprintf("$%.0f", high_52w)),
        hjust = -0.15, size = 2.6, colour = "#6b7280"
      ) +

      # Current price label
      ggplot2::geom_text(
        ggplot2::aes(x = pct_from_low * 100,
                     label = sprintf("$%.0f", current),
                     colour = fill_col),
        vjust = -0.9, size = 2.5, fontface = "bold"
      ) +

      ggplot2::scale_colour_identity() +
      ggplot2::scale_x_continuous(
        labels = function(x) sprintf("+%.0f%%", x),
        expand = ggplot2::expansion(mult = c(0.12, 0.12))
      ) +
      ggplot2::labs(
        title    = sprintf("52-Week Range Screen  |  %d tickers  |  %s",
                           nrow(result), format(Sys.Date(), "%Y-%m-%d")),
        subtitle = sprintf(
          "Red = near 52W low (≤%.0f%%)   Green = near 52W high (≤%.0f%%)   Dot = current price",
          near_low_pct * 100, near_high_pct * 100),
        x = "% above 52-week low", y = NULL
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor   = ggplot2::element_blank(),
        plot.title         = ggplot2::element_text(face = "bold", size = 13),
        plot.subtitle      = ggplot2::element_text(colour = "grey50", size = 9),
        axis.text.y        = ggplot2::element_text(size = 9, face = "bold"),
        plot.margin        = ggplot2::margin(10, 40, 10, 10)
      )

    print(p)
  }

  invisible(result)
}

#' Generate Relative Return Spreads (Arithmetic Alpha)
#' 
#' Functionalized logic to compare tickers against a benchmark.
#' Logic: r_relative = r_ticker - r_bmk
#'
#' @param xts_ret An xts object containing return data.
#' @param bmk Character string of the benchmark ticker (default "SPY").
#' @return An xts object of relative returns (Arithmetic Alpha).
#' 
generate_relative_returns <- function(xts_ret, bmk = "SPY") {
  
  # 1. Validation: Ensure benchmark exists (Ref: prof_data_script_db check)
  if (!(bmk %in% colnames(xts_ret))) {
    warning(paste0("⚠️ Benchmark '", bmk, "' not found in returns. xts_rel cannot be calculated."))
    return(NULL)
  }
  
  message(paste0("📊 Generating Relative Return Spreads (xts_rel vs ", bmk, ")..."))
  
  # 2. Extract Benchmark
  spy_ret <- xts_ret[, bmk]
  
  # 3. Arithmetic Alpha Calculation
  # sweep() efficiently subtracts the benchmark column from all other columns
  rel_ret <- sweep(xts_ret, 1, spy_ret, "-")
  
  # 4. Return full file/object
  return(rel_ret)
}

#' Check for Missing Functions
#' 
#' Compares current environment against required functions for the pipeline.
#' 
check_required_functions <- function() {
  required <- c("generate_relative_returns")
  existing <- ls(envir = .GlobalEnv)
  missing <- setdiff(required, existing)
  
  if(length(missing) > 0) {
    message(paste("⚠️ Missing functions in environment:", paste(missing, collapse = ", ")))
  } else {
    message("✅ Utility functions validated.")
  }
}

# ==============================================================================
# END OF FILE
# ==============================================================================