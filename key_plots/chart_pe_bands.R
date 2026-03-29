################################################################################
# CHART TEMPLATE : pe_bands
# NAME           : P/E Price Bands (Bloomberg-style)
# FILE           : key_plots/chart_pe_bands.R
#
# WHAT IT SHOWS
#   Replicates Bloomberg "Price Bands Based on LTM P/E" for the S&P 500.
#   Five coloured bands show the implied index price if it traded at each
#   historical P/E level (mean ± 1sd, mean ± 2sd).  Bands move over time
#   because EPS changes — the step-function shape is the EPS history.
#   Actual price is overlaid as a bold dark line.
#
# DESIGN
#   • Bands: Avg+2sd (blue) / Avg+1sd (red) / Avg (purple) /
#            Avg-1sd (yellow) / Avg-2sd (cyan)
#   • Actual price: white (dark bg) or dark blue (light bg)
#   • Right-hand annotation table: current vs mean P/E + implied prices
#   • Optional dark Bloomberg-style background
#
# DATA SOURCES (tried in order, first success wins)
#   1. S&P Global  spglobal.com/spdji  sp-500-eps-est.xlsx  — operating + as-reported EPS
#   2. Shiller Yale HTTPS  ie_data.xls  — as-reported EPS since 1871
#   3. Shiller Yale HTTP   ie_data.xls  — same, alternate URL
#   4. multpl.com scraping + Yahoo Finance  — as-reported EPS
#   Cached locally after first download.
#
# INVOKE
#   source(here("key_plots/chart_pe_bands.R"))
#   plot_pe_bands()                                    # operating EPS, 10Y, dark
#   plot_pe_bands(eps_type = "as_reported")            # Shiller-style
#   plot_pe_bands(n_years = 20, dark = FALSE)
#   plot_pe_bands(start_date = "2008-01-01")
#
# PARAMETERS
#   n_years      numeric — lookback window for P/E band statistics (default 10)
#   eps_type     chr     — "operating" (Bloomberg-style) or "as_reported" (Shiller)
#   band_pes     numeric vector — explicit P/E levels (overrides n_years stats)
#   start_date   chr/Date — chart display start; NULL = full history
#   dark         logical — Bloomberg dark background (default TRUE)
#   cache_path   path — where to save/load data (default input/sp500_eps.rds)
#   force_refresh logical — ignore cache and re-download (default FALSE)
#   title        chr — chart title override
#
# OUTPUTS (returned invisibly)
#   list: p (ggplot), pe_stats (tibble of band levels), band_df (long tibble)
################################################################################

library(tidyverse)
library(readxl)
library(httr)
library(lubridate)
library(scales)
library(here)

# ── Data download + parse ──────────────────────────────────────────────────────
# Source priority:
#   1. S&P Global xlsx  — operating EPS (matches Bloomberg) + as-reported
#   2. Shiller HTTPS    — as-reported EPS since 1950
#   3. Shiller HTTP     — same, alternate URL
#   4. multpl.com       — as-reported EPS (scraped)

.load_eps <- function(cache_path    = here("input/sp500_eps.rds"),
                      eps_type      = "operating",   # "operating" or "as_reported"
                      force_refresh = FALSE) {

  if (file.exists(cache_path) && !force_refresh) {
    cached <- read_rds(cache_path)
    # Return correct EPS column based on eps_type
    if (eps_type == "operating" && "earnings_op" %in% colnames(cached)) {
      message(sprintf("📥 Cache hit — using operating EPS (%s → %s)",
                      min(cached$date), max(cached$date)))
      out <- cached %>% mutate(earnings = earnings_op) %>%
        select(date, price, earnings, pe_ltm)
      attr(out, "eps_source") <- attr(cached, "eps_source") %||% "cache"
      return(out)
    }
    message(sprintf("📥 Cache hit — using as-reported EPS (%s → %s)",
                    min(cached$date), max(cached$date)))
    out <- cached %>% mutate(earnings = earnings_ar) %>%
      select(date, price, earnings, pe_ltm)
    attr(out, "eps_source") <- attr(cached, "eps_source") %||% "cache"
    return(out)
  }

  result <- NULL   # will hold list(date, price, earnings_op, earnings_ar)

  # ── Source 1: S&P Global sp-500-eps-est.xlsx ─────────────────────────────────
  spg_url <- "https://www.spglobal.com/spdji/en/documents/additional-material/sp-500-eps-est.xlsx"
  message(sprintf("🌐 [1/4] Trying S&P Global: %s", spg_url))
  tmp_xlsx <- tempfile(fileext = ".xlsx")

  spg_ok <- tryCatch({
    r <- httr::GET(spg_url, httr::write_disk(tmp_xlsx, overwrite = TRUE),
                   httr::timeout(45),
                   httr::add_headers(`User-Agent` = "Mozilla/5.0"))
    !httr::http_error(r)
  }, error = function(e) { message("  ✗ ", conditionMessage(e)); FALSE })

  if (spg_ok) {
    result <- tryCatch({
      # Inspect sheets — S&P Global layout changes occasionally
      sheets <- readxl::excel_sheets(tmp_xlsx)
      message(sprintf("  Sheets found: %s", paste(sheets, collapse = ", ")))

      # Target sheet usually contains "ESTIMATES" or is sheet 1
      target_sheet <- sheets[grepl("ESTIMATE|DATA|EPS|P.E", sheets,
                                    ignore.case = TRUE)][1]
      if (is.na(target_sheet)) target_sheet <- sheets[1]
      message(sprintf("  Parsing sheet: '%s'", target_sheet))

      raw <- readxl::read_xlsx(tmp_xlsx, sheet = target_sheet,
                                col_names = TRUE) %>%
        # Keep only rows where first column looks like a date or year
        mutate(across(everything(), as.character))

      # Find date column and EPS columns by name
      nms <- tolower(colnames(raw))
      date_col <- which(grepl("date|quarter|period|year", nms))[1]
      op_col   <- which(grepl("operat", nms) & grepl("actual|report|trailing|ltm|12", nms))[1]
      ar_col   <- which(grepl("report|gaap|as.rep", nms) & grepl("actual|trailing|ltm|12", nms))[1]

      # Fallback: if can't distinguish, use first two numeric-looking columns after date
      if (is.na(op_col) || is.na(ar_col)) {
        num_cols <- which(sapply(raw, function(x)
          mean(!is.na(suppressWarnings(as.numeric(x)))) > 0.5))
        num_cols <- setdiff(num_cols, date_col)
        op_col   <- if (length(num_cols) >= 1) num_cols[1] else NA
        ar_col   <- if (length(num_cols) >= 2) num_cols[2] else op_col
      }

      if (is.na(date_col)) stop("Date column not found in S&P Global file.")

      parsed <- raw %>%
        select(date_raw = all_of(date_col),
               eps_op   = all_of(op_col),
               eps_ar   = all_of(ar_col)) %>%
        mutate(
          date    = suppressWarnings(
            coalesce(
              lubridate::mdy(date_raw),
              lubridate::ymd(date_raw),
              lubridate::dmy(date_raw),
              # Excel numeric date
              as.Date(as.numeric(date_raw), origin = "1899-12-30")
            )
          ),
          eps_op  = as.numeric(eps_op),
          eps_ar  = as.numeric(eps_ar)
        ) %>%
        filter(!is.na(date), !is.na(eps_op) | !is.na(eps_ar),
               eps_op > 0 | is.na(eps_op)) %>%
        # Quarterly → expand to monthly (forward-fill within quarter)
        arrange(date) %>%
        mutate(date = as.Date(format(date, "%Y-%m-01"))) %>%
        select(date, eps_op, eps_ar) %>%
        mutate(cape = NA_real_)   # S&P Global has no CAPE

      message(sprintf("  ✓ S&P Global parsed: %d rows (%s → %s)",
                      nrow(parsed), min(parsed$date), max(parsed$date)))
      list(eps = parsed, source = "S&P Global")

    }, error = function(e) {
      message("  ✗ S&P Global parse failed: ", conditionMessage(e)); NULL
    })
  }

  # ── Sources 2 & 3: Shiller XLS ───────────────────────────────────────────────
  if (is.null(result)) {
    xls_urls <- c(
      "https://shiller.econ.yale.edu/data/ie_data.xls",
      "http://www.econ.yale.edu/~shiller/data/ie_data.xls"
    )
    for (i in seq_along(xls_urls)) {
      if (!is.null(result)) break
      message(sprintf("🌐 [%d/4] Trying Shiller: %s", i + 1, xls_urls[i]))
      tmp_xls <- tempfile(fileext = ".xls")
      ok <- tryCatch({
        r <- httr::GET(xls_urls[i], httr::write_disk(tmp_xls, overwrite = TRUE),
                       httr::timeout(30))
        !httr::http_error(r)
      }, error = function(e) { message("  ✗ ", conditionMessage(e)); FALSE })
      if (!ok) next

      result <- tryCatch({
        raw_full <- readxl::read_xls(tmp_xls, sheet = "Data", skip = 7,
                                      col_types = "text")
        # Find CAPE column (usually col 13, labelled "CAPE" or "P/E10")
        nms_lower <- tolower(colnames(raw_full))
        cape_col  <- which(grepl("cape|p.e10|pe10|cyclically", nms_lower))[1]
        if (is.na(cape_col)) cape_col <- 13L

        raw <- raw_full %>%
          select(date_raw = 1, price = 2, earnings = 4,
                 cape_raw = all_of(cape_col)) %>%
          filter(!is.na(date_raw)) %>%
          mutate(across(c(price, earnings, cape_raw), as.numeric)) %>%
          filter(!is.na(earnings), earnings > 0) %>%
          mutate(
            yr   = as.integer(floor(as.numeric(date_raw))),
            mo   = pmin(pmax(as.integer(round((as.numeric(date_raw) %% 1) * 100)), 1L), 12L),
            date = as.Date(sprintf("%04d-%02d-01", yr, mo))
          ) %>%
          filter(!is.na(date), yr >= 1950) %>%
          arrange(date) %>%
          select(date, eps_ar = earnings, cape = cape_raw) %>%
          mutate(eps_op = eps_ar)   # Shiller has no operating EPS — use same
        message(sprintf("  ✓ Shiller parsed: %d months (CAPE col: %d)", nrow(raw), cape_col))
        list(eps = raw, source = "Shiller")
      }, error = function(e) { message("  ✗ Parse failed: ", conditionMessage(e)); NULL })
    }
  }

  # ── Source 4: multpl.com ──────────────────────────────────────────────────────
  if (is.null(result)) {
    message("🌐 [4/4] Trying multpl.com...")
    if (!requireNamespace("rvest", quietly = TRUE))
      install.packages("rvest", repos = "https://cloud.r-project.org")

    result <- tryCatch({
      page <- rvest::read_html(
        "https://www.multpl.com/s-p-500-earnings-per-share/table/by-month")
      tbl  <- rvest::html_table(page)[[1]]
      colnames(tbl) <- c("date_chr", "eps_chr")

      parsed <- tbl %>%
        mutate(
          date   = lubridate::mdy(date_chr),
          eps_ar = as.numeric(str_remove_all(eps_chr, "[^0-9\\.]"))
        ) %>%
        filter(!is.na(date), !is.na(eps_ar), eps_ar > 0) %>%
        mutate(date = as.Date(format(date, "%Y-%m-01")),
               eps_op = eps_ar) %>%
        select(date, eps_op, eps_ar) %>%
        arrange(date) %>%
        mutate(cape = NA_real_)   # multpl.com has no CAPE

      message(sprintf("  ✓ multpl.com: %d months", nrow(parsed)))
      list(eps = parsed, source = "multpl.com")
    }, error = function(e) { message("  ✗ ", conditionMessage(e)); NULL })
  }

  if (is.null(result))
    stop("All 4 data sources failed. Check internet connection.")

  # ── Fetch monthly S&P 500 price from Yahoo ────────────────────────────────────
  message("📈 Fetching monthly ^GSPC from Yahoo...")
  if (!requireNamespace("quantmod", quietly = TRUE))
    install.packages("quantmod", repos = "https://cloud.r-project.org")

  px <- tryCatch({
    gspc <- suppressMessages(
      quantmod::getSymbols("^GSPC", src = "yahoo",
                            from = min(result$eps$date),
                            auto.assign = FALSE)
    )
    # Last adjusted close of each calendar month — no to.monthly needed
    data.frame(
      date_raw = as.Date(zoo::index(quantmod::Ad(gspc))),
      price    = as.numeric(quantmod::Ad(gspc))
    ) %>%
      mutate(ym = format(date_raw, "%Y-%m")) %>%
      group_by(ym) %>%
      slice_tail(n = 1) %>%
      ungroup() %>%
      transmute(date  = as.Date(paste0(ym, "-01")),
                price = price)
  }, error = function(e) {
    message("  ✗ Yahoo price fetch failed: ", conditionMessage(e)); NULL
  })

  if (is.null(px)) stop("Yahoo price fetch failed — cannot build EPS dataset.")
  message(sprintf("  ✓ Yahoo price: %d months", nrow(px)))

  # ── Join price + EPS ──────────────────────────────────────────────────────────
  combined <- result$eps %>%
    group_by(date) %>% slice_tail(n = 1) %>% ungroup() %>%   # deduplicate months
    inner_join(px, by = "date") %>%
    arrange(date) %>%
    mutate(
      pe_ltm = price / eps_ar,
      pe_ltm = if_else(pe_ltm <= 0 | pe_ltm > 200, NA_real_, pe_ltm)
    ) %>%
    filter(!is.na(price)) %>%
    rename(earnings_op = eps_op, earnings_ar = eps_ar)

  message(sprintf("✅ EPS data ready [%s]: %d months (%s → %s)",
                  result$source,
                  nrow(combined), min(combined$date), max(combined$date)))

  dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
  write_rds(combined, cache_path)
  message(sprintf("💾 Cached to: %s", cache_path))

  # Return with chosen EPS column as 'earnings'; attach source as attribute
  out <- combined %>%
    mutate(earnings = if (eps_type == "operating") earnings_op else earnings_ar) %>%
    select(date, price, earnings, pe_ltm)
  attr(out, "eps_source") <- result$source
  out
}

# ── Main plot function ─────────────────────────────────────────────────────────
plot_pe_bands <- function(
    n_years      = 10,
    eps_type     = "operating",   # "operating" (Bloomberg) or "as_reported" (Shiller)
    n_sd         = 3,             # number of sd bands above/below mean (default 3)
    band_pes     = NULL,
    start_date   = NULL,
    dark         = TRUE,
    cache_path   = here("input/sp500_eps.rds"),
    force_refresh= FALSE,
    title        = NULL
) {

  shiller   <- .load_eps(cache_path, eps_type, force_refresh)
  # Also read full cached data to get both EPS columns for title annotation
  both_eps  <- tryCatch(read_rds(cache_path), error = function(e) NULL)
  latest_op <- if (!is.null(both_eps) && "earnings_op" %in% colnames(both_eps))
                 last(both_eps$earnings_op) else NA_real_
  latest_ar <- if (!is.null(both_eps) && "earnings_ar" %in% colnames(both_eps))
                 last(both_eps$earnings_ar) else NA_real_

  # ── Compute band P/E levels from trailing n_years of history ─────────────────
  cutoff  <- max(shiller$date) %m-% months(round(n_years * 12))
  pe_hist <- shiller %>% filter(date >= cutoff) %>% pull(pe_ltm)

  pe_mean <- mean(pe_hist, na.rm = TRUE)
  pe_sd   <- sd(pe_hist,   na.rm = TRUE)

  if (is.null(band_pes)) {
    # Build symmetric bands: +n_sd, ..., +1, avg, -1, ..., -n_sd
    sd_steps <- seq(n_sd, -n_sd, by = -1)
    band_pes <- pe_mean + sd_steps * pe_sd
    # Floor bottom band at 5th percentile (never negative)
    band_pes[length(band_pes)] <- max(last(band_pes),
                                       quantile(pe_hist, 0.05, na.rm = TRUE))
  }

  # ── Auto-extend upward if current price is above top band ────────────────────
  current_eps_raw  <- last(shiller$earnings)
  current_px_raw   <- last(shiller$price)
  current_pe_raw   <- current_px_raw / current_eps_raw

  extra_above <- 0L
  while (max(band_pes) < current_pe_raw * 1.05) {
    extra_above  <- extra_above + 1L
    band_pes     <- c(pe_mean + (n_sd + extra_above) * pe_sd, band_pes)
  }
  if (extra_above > 0)
    message(sprintf("  ℹ️  Added %d extra band(s) above — current P/E %.1fx exceeds Avg+%dsd",
                    extra_above, current_pe_raw, n_sd))

  # ── Band labels + colours ─────────────────────────────────────────────────────
  n_bands    <- length(band_pes)
  mid_idx    <- which.min(abs(band_pes - pe_mean))   # index of the mean band

  band_labels <- sapply(seq_len(n_bands), function(i) {
    diff_sd <- round((band_pes[i] - pe_mean) / pe_sd, 1)
    if (abs(diff_sd) < 0.1)
      sprintf("Avg        (%.1fx)", band_pes[i])
    else if (diff_sd > 0)
      sprintf("Avg+%.1fsd (%.1fx)", diff_sd, band_pes[i])
    else
      sprintf("Avg%.1fsd (%.1fx)", diff_sd, band_pes[i])
  })

  # Colour palette: interpolate across n_bands
  # Fixed anchors: top=blue, upper-mid=red, mean=purple, lower-mid=yellow, bottom=cyan
  anchor_cols <- c("#3b82f6", "#ef4444", "#a855f7", "#eab308", "#06b6d4")
  band_colours <- if (n_bands <= length(anchor_cols)) {
    anchor_cols[seq_len(n_bands)]
  } else {
    colorRampPalette(anchor_cols)(n_bands)
  }

  # ── Daily S&P 500 price from Yahoo (smooth line) ──────────────────────────────
  if (!requireNamespace("quantmod", quietly = TRUE))
    install.packages("quantmod", repos = "https://cloud.r-project.org")

  message("📈 Fetching daily ^GSPC from Yahoo...")
  fetch_from <- if (!is.null(start_date)) as.Date(start_date) - 365
               else as.Date("1950-01-01")

  daily_px <- tryCatch({
    gspc_xts <- suppressMessages(
      quantmod::getSymbols("^GSPC", src = "yahoo",
                            from = fetch_from, auto.assign = FALSE)
    )
    data.frame(
      date  = as.Date(zoo::index(quantmod::Ad(gspc_xts))),
      price = as.numeric(quantmod::Ad(gspc_xts))
    )
  }, error = function(e) {
    message("  ✗ Yahoo daily fetch failed, using monthly Shiller price: ", conditionMessage(e))
    shiller %>% select(date, price)
  })

  message(sprintf("  ✓ Daily prices: %d rows", nrow(daily_px)))

  # ── Build band time series — forward-fill monthly EPS to daily ────────────────
  # EPS updates monthly; for each trading day use the most recent month's EPS.
  # This produces the Bloomberg step-function bands with a smooth price line.
  # One EPS value per calendar month (deduplicated — Shiller may have duplicates)
  eps_monthly <- shiller %>%
    select(date, earnings) %>%
    mutate(ym = format(date, "%Y-%m")) %>%
    group_by(ym) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(ym, earnings)

  daily_band_df <- daily_px %>%
    mutate(ym = format(date, "%Y-%m")) %>%
    left_join(eps_monthly, by = "ym") %>%
    # Forward-fill any gaps (months where Shiller has no data)
    arrange(date) %>%
    fill(earnings, .direction = "down") %>%
    filter(!is.na(earnings)) %>%
    crossing(tibble(band_id    = seq_along(band_pes),
                    pe_band    = band_pes,
                    band_label = band_labels)) %>%
    mutate(
      implied_price = pe_band * earnings,
      band_label    = factor(band_label, levels = band_labels)
    )

  # ── Apply start_date filter ───────────────────────────────────────────────────
  if (!is.null(start_date)) {
    start_date   <- as.Date(start_date)
    daily_px     <- daily_px     %>% filter(date >= start_date)
    daily_band_df <- daily_band_df %>% filter(date >= start_date)
  }

  current      <- last(daily_px$price)
  current_date <- max(daily_px$date)
  current_eps  <- last(shiller$earnings)
  current_pe   <- current / current_eps

  # ── Implied prices at current EPS for annotation ──────────────────────────────
  pe_stats <- tibble(
    Band          = band_labels,
    `P/E`         = sprintf("%.1fx", band_pes),
    `Implied Price` = sprintf("%.0f", band_pes * current_eps),
    Colour        = band_colours
  )

  # ── Theme setup ───────────────────────────────────────────────────────────────
  if (dark) {
    bg_col    <- "#0a0a0a"
    grid_col  <- "#222222"
    text_col  <- "#e5e7eb"
    price_col <- "white"
  } else {
    bg_col    <- "white"
    grid_col  <- "#f3f4f6"
    text_col  <- "#111827"
    price_col <- "#1d3461"
  }

  base_theme <- theme_minimal(base_size = 11) +
    theme(
      plot.background   = element_rect(fill = bg_col,   colour = NA),
      panel.background  = element_rect(fill = bg_col,   colour = NA),
      panel.grid.major  = element_line(colour = grid_col, linewidth = 0.3),
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(colour = text_col, face = "bold", size = 13),
      plot.subtitle     = element_text(colour = if (dark) "#9ca3af" else "grey50", size = 9),
      plot.caption      = element_text(colour = if (dark) "#6b7280" else "grey60", size = 8),
      axis.text         = element_text(colour = text_col),
      axis.title        = element_text(colour = text_col),
      legend.background = element_rect(fill = bg_col, colour = NA),
      legend.text       = element_text(colour = text_col, size = 9),
      legend.title      = element_blank(),
      legend.position   = "top",
      legend.key.width  = unit(1.8, "cm"),
      plot.margin       = margin(10, 10, 10, 10)
    )

  # ── Chart title ───────────────────────────────────────────────────────────────
  eps_op_str <- if (!is.na(latest_op)) sprintf("Op $%.2f", latest_op) else "Op n/a"
  eps_ar_str <- if (!is.na(latest_ar)) sprintf("AR $%.2f", latest_ar) else "AR n/a"
  chart_title <- title %||% sprintf(
    "S&P 500 — P/E Price Bands  |  Current: %.0f  @  %.1fx P/E  (%.0fx %dY avg)  |  EPS: %s  |  %s",
    current, current_pe, pe_mean, n_years, eps_op_str, eps_ar_str
  )

  eps_source <- attr(shiller, "eps_source") %||% "unknown"
  eps_label  <- if (eps_type == "operating") "Operating EPS" else "As-Reported GAAP EPS"

  chart_sub <- sprintf(
    "Bands = %dY Avg \u00b1 1sd / \u00b1 2sd  |  %s: $%.2f  |  Price: Yahoo ^GSPC  |  EPS: %s  |  Bands step as EPS updates",
    n_years, eps_label, current_eps, eps_source
  )

  # ── Plot ──────────────────────────────────────────────────────────────────────
  p <- ggplot() +

    # Band lines (step-function because EPS is monthly)
    geom_step(data      = daily_band_df,
              aes(x     = date,
                  y     = implied_price,
                  colour = band_label),
              linewidth = 0.7, alpha = 0.85, direction = "hv") +

    # Actual price — daily, smooth, bold, on top
    geom_line(data      = daily_px,
              aes(x     = date, y = price),
              colour    = price_col,
              linewidth = 1.1, alpha = 0.95) +

    # Current price dot
    annotate("point",
             x = current_date, y = current,
             colour = price_col, size = 3) +
    annotate("text",
             x = current_date,
             y = current * 1.03,
             label = sprintf("%.0f\n(%.1fx P/E)", current, current_pe),
             colour = price_col, hjust = 1, size = 3, fontface = "bold",
             lineheight = 0.85) +

    # Mean P/E horizontal reference (current implied)
    geom_hline(yintercept = pe_mean * current_eps,
               colour = "#a855f7", linewidth = 0.4,
               linetype = "dashed", alpha = 0.5) +

    # End-of-line P/E labels
    geom_text(
      data = daily_band_df %>%
        group_by(band_label, band_id, pe_band) %>%
        slice_max(date, n = 1) %>%
        ungroup(),
      aes(x     = date,
          y     = implied_price,
          label = sprintf("%.1fx", pe_band),
          colour = band_label),
      hjust   = -0.15, size = 2.8, fontface = "bold",
      show.legend = FALSE
    ) +

    scale_colour_manual(values = setNames(band_colours, band_labels)) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                 expand = expansion(mult = c(0.01, 0.06))) +
    scale_y_continuous(labels = comma_format(accuracy = 1),
                       expand = expansion(mult = c(0.02, 0.05))) +

    coord_cartesian(clip = "off") +

    labs(
      title   = chart_title,
      subtitle = chart_sub,
      caption = sprintf(
        "Implied prices at current EPS ($%.2f):  %s",
        current_eps,
        paste(sprintf("%s → %s", pe_stats$`P/E`, pe_stats$`Implied Price`),
              collapse = "   |   ")
      ),
      x = NULL, y = "S&P 500 Index"
    ) +
    base_theme

  print(p)

  # ── Console summary table ─────────────────────────────────────────────────────
  message(sprintf("\n── S&P 500 P/E Price Bands (%dY lookback) ──────────────────", n_years))
  message(sprintf("Current price : %.0f   P/E: %.1fx   EPS: $%.2f",
                  current, current_pe, current_eps))
  message(sprintf("%-25s %6s %16s", "Band", "P/E", "Implied Price"))
  message(strrep("─", 50))
  for (i in seq_along(band_pes)) {
    message(sprintf("%-25s %6.1fx %14.0f",
                    band_labels[i], band_pes[i], band_pes[i] * current_eps))
  }
  message(strrep("─", 50))
  message(sprintf("%-25s %6.1fx %14.0f  ← CURRENT", "Actual", current_pe, current))
  message(sprintf("\nVs %dY avg P/E (%.1fx): current at %+.0f%% discount/premium\n",
                  n_years, pe_mean,
                  (current_pe / pe_mean - 1) * 100))

  invisible(list(p = p, pe_stats = pe_stats, band_df = daily_band_df,
                 daily_px = daily_px, shiller = shiller,
                 pe_mean = pe_mean, pe_sd = pe_sd, band_pes = band_pes))
}

# ── Quick-run example (guarded) ───────────────────────────────────────────────
if (FALSE) {
  source(here("key_plots/chart_pe_bands.R"))

  # Default: 10Y lookback, dark theme
  out <- plot_pe_bands()

  # 20Y history, light theme
  plot_pe_bands(n_years = 20, dark = FALSE)

  # Force specific band P/E levels (matching Bloomberg screenshot)
  plot_pe_bands(band_pes = c(41.7, 36.9, 32.1, 27.3, 22.5), dark = TRUE)

  # Force refresh from Yale
  plot_pe_bands(force_refresh = TRUE)

  # Shiller CAPE standalone chart
  plot_shiller_cape()
  plot_shiller_cape(start_date = "1990-01-01", dark = FALSE)
}

################################################################################
# FUNCTION: plot_shiller_cape
# PURPOSE : Shiller CAPE (P/E10) history with long-run mean ± sd reference bands
#           and current-level annotation.
#
# PARAMETERS
#   start_date   chr/Date — display start; NULL = full history since 1950
#   dark         logical  — Bloomberg dark background (default TRUE)
#   cache_path   path     — shared cache with plot_pe_bands (default input/sp500_eps.rds)
#   force_refresh logical — ignore cache and re-download (default FALSE)
#   title        chr      — override chart title
#
# RETURNS (invisibly)
#   list: p (ggplot), cape_df (tibble), cape_mean, cape_sd, current_cape
################################################################################

plot_shiller_cape <- function(
    start_date    = NULL,
    dark          = TRUE,
    cache_path    = here("input/sp500_eps.rds"),
    force_refresh = FALSE,
    title         = NULL
) {

  # ── Load CAPE from cache (re-use .load_eps infrastructure) ───────────────────
  # Force as_reported so Shiller source is preferred (it carries CAPE)
  .load_eps(cache_path, eps_type = "as_reported", force_refresh = force_refresh)

  both_eps <- tryCatch(read_rds(cache_path), error = function(e) NULL)
  if (is.null(both_eps) || !"cape" %in% colnames(both_eps))
    stop("CAPE column not found in cache. Run plot_pe_bands(force_refresh=TRUE) first to rebuild.")

  cape_df <- both_eps %>%
    filter(!is.na(cape), cape > 0, cape < 200) %>%
    select(date, cape) %>%
    arrange(date)

  if (!is.null(start_date))
    cape_df <- cape_df %>% filter(date >= as.Date(start_date))

  if (nrow(cape_df) == 0) stop("No CAPE data in requested date range.")

  # ── Stats ─────────────────────────────────────────────────────────────────────
  cape_mean    <- mean(cape_df$cape, na.rm = TRUE)
  cape_sd      <- sd(cape_df$cape,   na.rm = TRUE)
  current_cape <- last(cape_df$cape)
  current_date <- max(cape_df$date)
  since_year   <- year(min(cape_df$date))

  # ── Theme ─────────────────────────────────────────────────────────────────────
  if (dark) {
    bg_col   <- "#0a0a0a"; grid_col <- "#222222"
    text_col <- "#e5e7eb"; line_col <- "#f97316"
  } else {
    bg_col   <- "white";   grid_col <- "#f3f4f6"
    text_col <- "#111827"; line_col <- "#c2410c"
  }

  base_theme <- theme_minimal(base_size = 11) +
    theme(
      plot.background  = element_rect(fill = bg_col,   colour = NA),
      panel.background = element_rect(fill = bg_col,   colour = NA),
      panel.grid.major = element_line(colour = grid_col, linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(colour = text_col, face = "bold", size = 13),
      plot.subtitle    = element_text(colour = if (dark) "#9ca3af" else "grey50", size = 9),
      plot.caption     = element_text(colour = if (dark) "#6b7280" else "grey60", size = 8),
      axis.text        = element_text(colour = text_col),
      axis.title       = element_text(colour = text_col),
      plot.margin      = margin(10, 60, 10, 10)   # right margin for end labels
    )

  chart_title <- title %||% sprintf(
    "Shiller CAPE  |  Current: %.1fx  (Long-run Avg %.1fx since %d)",
    current_cape, cape_mean, since_year
  )
  chart_sub <- sprintf(
    "+1sd: %.1fx   |   Avg: %.1fx   |   -1sd: %.1fx   |   Premium vs avg: %+.0f%%",
    cape_mean + cape_sd, cape_mean, cape_mean - cape_sd,
    (current_cape / cape_mean - 1) * 100
  )

  # ── Plot ──────────────────────────────────────────────────────────────────────
  p <- ggplot(cape_df, aes(x = date, y = cape)) +

    # Reference bands
    geom_hline(yintercept = cape_mean + 2 * cape_sd, colour = "#3b82f6",
               linewidth = 0.35, linetype = "dotted", alpha = 0.5) +
    geom_hline(yintercept = cape_mean + cape_sd,     colour = "#ef4444",
               linewidth = 0.40, linetype = "dashed", alpha = 0.6) +
    geom_hline(yintercept = cape_mean,               colour = "#a855f7",
               linewidth = 0.55, linetype = "dashed", alpha = 0.75) +
    geom_hline(yintercept = cape_mean - cape_sd,     colour = "#3b82f6",
               linewidth = 0.40, linetype = "dashed", alpha = 0.6) +

    # CAPE line
    geom_line(colour = line_col, linewidth = 0.95) +

    # Current level dot
    annotate("point", x = current_date, y = current_cape,
             colour = line_col, size = 3) +

    # End-of-line labels
    annotate("text", x = current_date, y = current_cape,
             label = sprintf("%.1fx", current_cape),
             colour = line_col, hjust = -0.20, size = 3.2, fontface = "bold") +
    annotate("text", x = current_date, y = cape_mean,
             label = sprintf("Avg %.1fx", cape_mean),
             colour = "#a855f7", hjust = -0.15, size = 2.8) +
    annotate("text", x = current_date, y = cape_mean + cape_sd,
             label = sprintf("+1sd %.1fx", cape_mean + cape_sd),
             colour = "#ef4444", hjust = -0.15, size = 2.6) +
    annotate("text", x = current_date, y = cape_mean - cape_sd,
             label = sprintf("-1sd %.1fx", cape_mean - cape_sd),
             colour = "#3b82f6", hjust = -0.15, size = 2.6) +
    annotate("text", x = current_date, y = cape_mean + 2 * cape_sd,
             label = sprintf("+2sd %.1fx", cape_mean + 2 * cape_sd),
             colour = "#3b82f6", hjust = -0.15, size = 2.4, alpha = 0.7) +

    scale_x_date(date_breaks = "5 years", date_labels = "%Y",
                 expand = expansion(mult = c(0.01, 0.01))) +
    scale_y_continuous(labels = number_format(accuracy = 0.1),
                       expand = expansion(mult = c(0.02, 0.05))) +
    coord_cartesian(clip = "off") +

    labs(
      title    = chart_title,
      subtitle = chart_sub,
      caption  = "Source: Robert Shiller / Yale (ie_data.xls) — 10Y inflation-adjusted avg earnings",
      x = NULL, y = "CAPE (P/E10)"
    ) +
    base_theme

  print(p)

  # ── Console summary ───────────────────────────────────────────────────────────
  message(sprintf("\n── Shiller CAPE (%d → %s) ──────────────────────────────",
                  since_year, format(current_date, "%Y-%m")))
  message(sprintf("Current CAPE : %.1fx", current_cape))
  message(sprintf("Long-run avg : %.1fx   sd: %.1fx", cape_mean, cape_sd))
  message(sprintf("Premium/disc : %+.0f%% vs long-run avg", (current_cape / cape_mean - 1) * 100))
  message(sprintf("Vs +1sd band : %.1fx  |  vs -1sd band: %.1fx\n",
                  cape_mean + cape_sd, cape_mean - cape_sd))

  invisible(list(p = p, cape_df = cape_df,
                 cape_mean = cape_mean, cape_sd = cape_sd,
                 current_cape = current_cape))
}
