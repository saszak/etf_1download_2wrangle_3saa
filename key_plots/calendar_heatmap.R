# calendar_heatmap.R
# Calendar heatmap of daily log returns for any ticker in xts_ret.
#
# Usage (interactive):
#   source("key_plots/calendar_heatmap.R")          # plots TICKER (default SPY)
#   plot_calendar_heatmap("GLD")                    # single ticker
#   plot_calendar_heatmap("QQQ", n_yr = 5)          # last 5 years
#   plot_calendar_heatmap("AGG", save_png = TRUE)   # save to key_plots/
#
# Dependencies: lattice, chron, xts

# ── Packages ──────────────────────────────────────────────────────────────────
if (!requireNamespace("chron",   quietly = TRUE)) install.packages("chron")
library(lattice)
library(chron)
library(grid)
library(xts)

# ── calendarHeat core (Paul Bleicher, GPL-2) ─────────────────────────────────
.calendarHeat <- function(dates, values, ncolors = 99, color = "r2b",
                          varname = "Values") {
  if (inherits(dates, "character") | inherits(dates, "factor"))
    dates <- as.Date(dates)

  min.date <- as.Date(paste0(format(min(dates), "%Y"), "-01-01"))
  max.date <- as.Date(paste0(format(max(dates), "%Y"), "-12-31"))

  caldat <- data.frame(date.seq = seq(min.date, max.date, by = "days"),
                       value    = NA_real_)
  caldat$value[match(as.Date(dates), caldat$date.seq)] <- values

  caldat$dotw  <- as.numeric(format(caldat$date.seq, "%w"))
  caldat$woty  <- as.numeric(format(caldat$date.seq, "%U")) + 1
  caldat$yr    <- as.factor(format(caldat$date.seq, "%Y"))
  caldat$month <- as.numeric(format(caldat$date.seq, "%m"))

  yrs   <- as.character(unique(caldat$yr))
  d.loc <- integer(0)
  for (m in min(yrs):max(yrs)) {
    d.sub <- which(caldat$yr == m)
    d.loc <- c(d.loc, seq_along(d.sub))
  }
  caldat$seq <- d.loc

  # colour palettes
  pal <- list(
    r2b = c("#0571B0", "#92C5DE", "#F7F7F7", "#F4A582", "#CA0020"),  # blue=up  red=down
    r2g = c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384"),              # green=high
    w2b = c("#045A8D", "#2B8CBE", "#74A9CF", "#BDC9E1", "#F1EEF6")   # white=low
  )
  cal.pal <- colorRampPalette(pal[[color]], space = "Lab")

  nyr <- length(unique(caldat$yr))

  print(levelplot(
    value ~ woty * dotw | yr, data = caldat,
    as.table   = TRUE,
    aspect     = .12,
    layout     = c(1, nyr),
    between    = list(x = 0, y = c(1, 1)),
    strip      = TRUE,
    main       = paste("Calendar Heat Map —", varname),
    scales     = list(
      x = list(at = seq(2.9, 52, by = 4.42), labels = month.abb,
                alternating = c(1, rep(0, nyr - 1)), tck = 0, cex = 0.7),
      y = list(at = 0:6,
               labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
               alternating = 1, cex = 0.6, tck = 0)
    ),
    xlim       = c(0.4, 54.6),
    ylim       = c(6.6, -0.6),
    cuts       = ncolors - 1,
    col.regions = cal.pal(ncolors),
    xlab = "", ylab = "",
    colorkey   = list(col = cal.pal(ncolors), width = 0.6, height = 0.5),
    subscripts = TRUE
  ))
}

# ── Main function ─────────────────────────────────────────────────────────────
#' @param ticker  Character. Must exist in xts_ret (e.g. "SPY", "GLD", "QQQ").
#' @param n_yr    Integer. How many years back to plot (default 5).
#' @param color   "r2b" (red=down, blue=up) | "r2g" | "w2b".
#' @param save_png Logical. If TRUE saves PNG to key_plots/.
plot_calendar_heatmap <- function(ticker  = "SPY",
                                  n_yr    = 5,
                                  color   = "r2b",
                                  save_png = FALSE) {

  xts_ret <- readRDS(here::here("02_data_processed/xts_ret_returns.rds"))

  if (!ticker %in% colnames(xts_ret))
    stop(ticker, " not found in xts_ret. Available: ",
         paste(head(colnames(xts_ret), 10), collapse = ", "), " ...")

  x      <- na.omit(xts_ret[, ticker])
  cutoff <- Sys.Date() - 365 * n_yr
  x      <- x[paste0(cutoff, "/")]
  dates  <- as.Date(zoo::index(x))
  values <- as.numeric(zoo::coredata(x)) * 100   # log return → %

  varname <- paste0(ticker, " Daily Log Return (%)  ·  last ", n_yr, "y")

  if (save_png) {
    n_rows  <- length(unique(format(dates, "%Y")))
    h_px    <- max(400, n_rows * 160 + 150)
    outfile <- here::here(paste0("key_plots/calendar_heatmap_", ticker, ".png"))
    png(outfile, width = 1200, height = h_px, res = 120)
    .calendarHeat(dates, values, color = color, varname = varname)
    dev.off()
    message("Saved: ", outfile)
  } else {
    .calendarHeat(dates, values, color = color, varname = varname)
  }

  invisible(list(ticker = ticker, dates = dates, values = values))
}

# ── Auto-run guard ────────────────────────────────────────────────────────────
if (!isTRUE(getOption("knitr.in.progress"))) {
  plot_calendar_heatmap("SPY", n_yr = 5)
}
