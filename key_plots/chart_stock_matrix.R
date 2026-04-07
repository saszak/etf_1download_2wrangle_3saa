################################################################################
# CHART TEMPLATE : stock_matrix
# NAME           : Stock Market Return Matrix (Crestmont Research style)
# FILE           : key_plots/chart_stock_matrix.R
#
# WHAT IT SHOWS
#   Triangular matrix of annualised S&P 500 NOMINAL price-only returns.
#   Row = FROM year, Column = TO year (upper triangle: TO > FROM).
#   Cells are colour-coded by annualised return band.
#   White numbers = P/E ratio decreased over the period (headwind from valuation).
#
# COLOUR BANDS
#   Red         < 0%
#   Pink        0% – 3%
#   Blue        3% – 7%
#   Light Green 7% – 10%
#   Dark Green  > 10%
#
# DATA
#   Shiller Yale ie_data.xls — annual average price + CAPE (P/E10)
#   Cached to input/shiller_annual.rds after first download.
#
# INVOKE
#   source(here("key_plots/chart_stock_matrix.R"))
#   plot_stock_matrix()                         # default 1900–latest
#   plot_stock_matrix(from_yr = 1950)           # subset
#   plot_stock_matrix(show_25yr_line = TRUE)    # draw 25-year diagonal
#
# OUTPUT
#   ggplot object; save with ggsave() at large size (≥ 20×20 inches)
################################################################################

library(tidyverse)
library(readxl)
library(httr)
library(scales)
library(here)

# ── Colour palette ────────────────────────────────────────────────────────────
SM_FILL <- c(
  "Red"         = "#c0392b",
  "Pink"        = "#e8a0b4",
  "Blue"        = "#2155a0",
  "LightGreen"  = "#5aab5a",
  "DarkGreen"   = "#1a6b1a"
)

SM_BREAKS <- c(-Inf, 0, 3, 7, 10, Inf)
SM_LABELS <- c("Red", "Pink", "Blue", "LightGreen", "DarkGreen")

# ── Data loading ──────────────────────────────────────────────────────────────
.load_shiller_annual <- function(
    cache_path    = here("input/shiller_annual.rds"),
    force_refresh = FALSE
) {

  if (file.exists(cache_path) && !force_refresh) {
    message("Cache hit: ", cache_path)
    return(readRDS(cache_path))
  }

  message("Downloading Shiller ie_data.xls from Yale...")
  tmp <- tempfile(fileext = ".xls")
  res <- tryCatch(
    download.file("http://www.econ.yale.edu/~shiller/data/ie_data.xls",
                  tmp, mode = "wb", quiet = TRUE),
    error = function(e) -1L
  )
  if (res != 0) stop("Download failed — check internet connection or URL.")

  raw <- suppressMessages(
    suppressWarnings(
      readxl::read_xls(tmp, sheet = "Data", skip = 7,
                       col_types = c("numeric", "numeric", "numeric", "numeric",
                                     "numeric", "numeric", "numeric",
                                     rep("skip", 15)))
    )
  )
  names(raw) <- c("date_dec", "price", "div", "earn", "cpi", "date_frac",
                  "gs10")

  # Parse year + month from decimal date (e.g. 1871.01 = Jan 1871)
  monthly <- raw %>%
    filter(!is.na(date_dec), !is.na(price)) %>%
    mutate(
      year  = as.integer(floor(date_dec)),
      month = as.integer(round((date_dec - floor(date_dec)) * 100))
    ) %>%
    filter(year >= 1870, month %in% 1:12)

  # CAPE: need to reload with all columns to get P/E10
  raw2 <- suppressMessages(
    suppressWarnings(
      readxl::read_xls(tmp, sheet = "Data", skip = 7)
    )
  )
  # CAPE is typically column 13 (named "CAPE")
  cape_col <- which(grepl("^CAPE$", names(raw2), ignore.case = TRUE))[1]
  if (!is.na(cape_col)) {
    monthly$cape <- suppressWarnings(as.numeric(raw2[[cape_col]][seq_len(nrow(monthly))]))
  } else {
    monthly$cape <- NA_real_
  }

  # Annual averages (all 12 months of each year)
  annual <- monthly %>%
    group_by(year) %>%
    summarise(
      price_avg = mean(price, na.rm = TRUE),
      cape_avg  = mean(cape,  na.rm = TRUE),
      n_months  = n(),
      .groups   = "drop"
    ) %>%
    filter(n_months >= 6, year >= 1870) %>%
    arrange(year)

  saveRDS(annual, cache_path)
  message(sprintf("Saved %d years (%d – %d) to %s",
                  nrow(annual), min(annual$year), max(annual$year), cache_path))
  annual
}

# ── Matrix computation ────────────────────────────────────────────────────────
.build_return_matrix <- function(annual, from_yr, to_yr) {

  ann <- annual %>% filter(year >= from_yr, year <= to_yr)
  yrs <- ann$year

  # All pairs where TO > FROM
  grid <- expand.grid(from = yrs, to = yrs) %>%
    filter(to > from) %>%
    left_join(ann %>% select(year, price_avg, cape_avg),
              by = c("from" = "year")) %>%
    rename(p_from = price_avg, cape_from = cape_avg) %>%
    left_join(ann %>% select(year, price_avg, cape_avg),
              by = c("to" = "year")) %>%
    rename(p_to = price_avg, cape_to = cape_avg) %>%
    mutate(
      n_years   = to - from,
      ann_ret   = (p_to / p_from)^(1 / n_years) - 1,
      ann_ret_pct = ann_ret * 100,
      pe_fell   = !is.na(cape_from) & !is.na(cape_to) & (cape_to < cape_from),
      band      = cut(ann_ret_pct, breaks = SM_BREAKS,
                      labels = SM_LABELS, right = FALSE),
      cell_fill = SM_FILL[as.character(band)],
      # Text colour: white when dark fill OR P/E fell; dark when light fill & P/E rose
      text_col  = case_when(
        pe_fell                           ~ "white",
        band %in% c("Red","Blue","DarkGreen") ~ "white",
        TRUE                              ~ "#111111"
      ),
      label     = as.character(round(ann_ret_pct))
    )

  grid
}

# ── Plot function ─────────────────────────────────────────────────────────────
plot_stock_matrix <- function(
    from_yr       = 1900,
    to_yr         = NULL,        # NULL = latest available year
    show_25yr     = TRUE,        # draw 25-year period diagonal
    show_text     = TRUE,        # show annualised return numbers in cells
    show_hgrid    = TRUE,        # horizontal grid lines at every FROM year tick (every 5y)
    title         = "Stock Market Return Matrix",
    subtitle      = "S&P 500 Index Only \u2022 Nominal Price Returns (no dividends)",
    cache_path    = here("input/shiller_annual.rds"),
    force_refresh = FALSE
) {

  annual <- .load_shiller_annual(cache_path, force_refresh)

  if (is.null(to_yr)) to_yr <- max(annual$year)
  from_yr <- max(from_yr, min(annual$year) + 1)  # need at least one prior year for return

  mat <- .build_return_matrix(annual, from_yr - 1, to_yr)
  # filter to display range
  mat <- mat %>% filter(from >= from_yr, to <= to_yr)

  yrs_from <- sort(unique(mat$from))
  yrs_to   <- sort(unique(mat$to))

  # ── Side labels: FROM year + P/E + price ─────────────────────────────────────
  from_meta <- annual %>%
    filter(year %in% yrs_from) %>%
    mutate(
      row_label = sprintf("%d", year),
      pe_label  = ifelse(!is.na(cape_avg), sprintf("%.1f", cape_avg), ""),
      px_label  = ifelse(price_avg < 100,
                         sprintf("%.1f", price_avg),
                         sprintf("%.0f", price_avg))
    )

  # ── Build ggplot ──────────────────────────────────────────────────────────────
  p <- ggplot(mat, aes(x = to, y = from)) +
    geom_tile(aes(fill = band), colour = "white", linewidth = 0.06) +
    scale_fill_manual(
      values = SM_FILL,
      limits = SM_LABELS,
      labels = c("< 0%", "0% \u2013 3%", "3% \u2013 7%", "7% \u2013 10%", "> 10%"),
      name   = "Ann. Return",
      drop   = FALSE
    ) +
    scale_x_continuous(
      breaks = seq(from_yr + 1, to_yr, by = 5),
      expand = c(0, 0),
      position = "top"
    ) +
    scale_y_reverse(
      breaks = seq(from_yr, to_yr - 1, by = 5),
      expand = c(0, 0)
    ) +
    coord_fixed(ratio = 1) +
    labs(
      title    = title,
      subtitle = subtitle,
      x        = "TO year \u2192",
      y        = "\u2190 FROM year",
      caption  = paste0(
        "Annualised nominal price-only returns (no dividends).  ",
        "White numbers = P/E10 (CAPE) decreased over period.  ",
        "Data: Robert Shiller / Yale (ie_data.xls).  ",
        "Inspired by Crestmont Research Stock Market Matrix."
      )
    )

  # ── Cell text ──────────────────────────────────────────────────────────────
  if (show_text) {
    n_yrs <- to_yr - from_yr + 1
    txt_size <- case_when(
      n_yrs > 100 ~ 1.25,
      n_yrs >  80 ~ 1.6,
      n_yrs >  60 ~ 2.0,
      TRUE        ~ 2.5
    )

    p <- p +
      geom_text(
        aes(label = label, colour = I(text_col)),
        size        = txt_size,
        fontface    = "bold",
        lineheight  = 0.9,
        na.rm       = TRUE
      )
  }

  # ── Horizontal grid lines at FROM year ticks (every 5 years) ────────────────
  # Must use geom_segment (not panel.grid — grid is always drawn behind tiles).
  # Lines span only the filled triangle (x from yr+1 to to_yr).
  if (show_hgrid) {
    hgrid_yrs <- seq(from_yr + 5, to_yr - 1, by = 5)
    for (yr in hgrid_yrs) {
      p <- p + geom_segment(
        x    = yr + 1, xend = to_yr,
        y    = yr,     yend = yr,
        inherit.aes = FALSE,
        colour    = "white",
        linewidth = 1.2,
        alpha     = 0.85
      )
    }
  }

  # ── 25-year diagonal ──────────────────────────────────────────────────────
  if (show_25yr) {
    diag_df <- tibble(
      x    = seq(from_yr + 25, to_yr,      by = 1),
      y    = seq(from_yr,      to_yr - 25, by = 1)
    )
    p <- p +
      geom_path(data = diag_df, aes(x = x, y = y),
                inherit.aes = FALSE,
                colour = "black", linewidth = 0.5, linetype = "solid")
  }

  # ── Legend annotation ─────────────────────────────────────────────────────
  p <- p +
    guides(fill = guide_legend(
      override.aes = list(colour = NA),
      title.position = "top",
      nrow = 5
    )) +
    theme_minimal(base_size = 8) +
    theme(
      plot.title        = element_text(face = "bold", size = 11),
      plot.subtitle     = element_text(size  = 8, colour = "#444"),
      plot.caption      = element_text(size  = 6, colour = "#888"),
      axis.title.x.top  = element_text(size  = 7, face = "bold"),
      axis.title.y      = element_text(size  = 7, face = "bold"),
      axis.text.x.top   = element_text(size  = 14, angle = 90, vjust = 0.5, hjust = 0),
      axis.text.y       = element_text(size  = 14.4),
      panel.grid        = element_blank(),
      legend.position   = "bottom",
      legend.key.size   = unit(0.5, "cm"),
      legend.text       = element_text(size = 7),
      legend.title      = element_text(size = 7, face = "bold"),
      plot.margin       = margin(8, 8, 8, 8)
    )

  p
}

# ── Quick-run (guarded) ───────────────────────────────────────────────────────
if (!isTRUE(getOption("knitr.in.progress"))) {
  p <- plot_stock_matrix(from_yr = 1900, show_25yr = TRUE, show_hgrid = TRUE)

  out_path <- here("key_plots/stock_matrix.png")
  ggplot2::ggsave(out_path, p,
                  width = 22, height = 20, dpi = 200, bg = "white")
  message("Saved: ", out_path)
  print(p)
}
