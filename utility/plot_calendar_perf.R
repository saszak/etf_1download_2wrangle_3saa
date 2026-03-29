# ==============================================================================
# CALENDAR YEAR CUMULATIVE PERFORMANCE
# ==============================================================================
#
# Functions:
#   plot_calendar_perf(xts_col, ...)          — single ticker
#   plot_multi_calendar_perf(xts_multi, ...)  — multiple tickers, faceted
#   plot_rel_calendar_perf(ticker, ...)       — absolute vs relative, two panels
#
# Usage:
#   source("utility/plot_calendar_perf.R")
#   xts_ret <- readRDS("02_data_processed/xts_ret_returns.rds")
#
#   plot_calendar_perf(xts_ret[, "SPY"])
#   plot_calendar_perf(xts_ret[, "GLD"], highlight_year = 2024)
# ==============================================================================

library(tidyverse)
library(scales)
library(ggrepel)
library(patchwork)
library(cowplot)

plot_calendar_perf <- function(xts_col,
                               title         = NULL,
                               highlight_year = as.integer(format(Sys.Date(), "%Y"))) {

  ticker <- colnames(xts_col)[1]

  # ── Build base tibble ────────────────────────────────────────────────────────
  df <- tibble(
    date = as.Date(index(xts_col)),
    ret  = as.numeric(xts_col[, 1])
  ) %>%
    filter(!is.na(ret)) %>%
    mutate(year = as.integer(format(date, "%Y"))) %>%
    group_by(year) %>%
    arrange(date) %>%
    mutate(
      tday    = row_number(),
      cum_ret = cumprod(1 + ret) - 1
    ) %>%
    ungroup()

  # ── Add day-0 anchor (cum_ret = 0) per year ──────────────────────────────────
  anchors <- df %>%
    group_by(year) %>%
    slice_min(tday, n = 1) %>%
    mutate(tday = 0L, cum_ret = 0, ret = 0) %>%
    ungroup()

  plot_df <- bind_rows(anchors, df) %>% arrange(year, tday)

  # ── End-of-line labels ───────────────────────────────────────────────────────
  labels_df <- df %>%
    group_by(year) %>%
    slice_max(tday, n = 1) %>%
    ungroup() %>%
    mutate(lbl = paste0(sprintf("%+.1f%%", cum_ret * 100), "  ", year))

  # ── Colour palette ───────────────────────────────────────────────────────────
  years   <- sort(unique(plot_df$year))
  n_years <- length(years)

  base_pal <- colorRampPalette(c("#D5D8DC", "#85929E", "#2C3E50"))(n_years)
  pal      <- setNames(base_pal, as.character(years))
  lwd      <- setNames(rep(0.65, n_years), as.character(years))

  if (as.character(highlight_year) %in% names(pal)) {
    pal[as.character(highlight_year)] <- "#E74C3C"
    lwd[as.character(highlight_year)] <- 1.5
  }

  if (is.null(title)) title <- paste0(ticker, "  —  Calendar Year Cumulative Return")

  # ── Plot ─────────────────────────────────────────────────────────────────────
  ggplot(plot_df,
         aes(x = tday, y = cum_ret,
             group     = factor(year),
             colour    = factor(year),
             linewidth = factor(year))) +

    geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.5, linetype = "solid") +
    geom_line(alpha = 0.88) +

    geom_text_repel(
      data          = labels_df,
      aes(label = lbl),
      size          = 2.4,
      fontface      = "bold",
      hjust         = 0,
      direction     = "y",
      nudge_x       = 4,
      segment.color = "grey75",
      segment.size  = 0.3,
      show.legend   = FALSE
    ) +

    scale_colour_manual(values = pal, name = "Year") +
    scale_linewidth_manual(values = lwd, guide = "none") +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      name   = "Cumulative Return"
    ) +
    scale_x_continuous(
      name   = "Trading Day of Year",
      expand = expansion(mult = c(0.01, 0.18))
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey92", linewidth = 0.4),
      legend.position  = "right",
      legend.text      = element_text(size = 10),
      legend.title     = element_text(size = 10, face = "bold"),
      axis.title       = element_text(size = 11, face = "bold"),
      axis.text        = element_text(size = 10),
      plot.title       = element_text(face = "bold", size = 14),
      plot.subtitle    = element_text(size = 10, colour = "grey45"),
      plot.caption     = element_text(size = 8,  colour = "grey55")
    ) +
    labs(
      title    = title,
      subtitle = sprintf(
        "Indexed to 0%% at Jan 1 each year  |  %d years  |  Current year highlighted in red",
        n_years
      ),
      caption  = paste0("Daily log returns  |  As of ", format(max(df$date), "%d %b %Y"))
    )
}

# ==============================================================================
# RELATIVE VERSION — two panels: absolute | vs SPY, shared Y axis
# ==============================================================================
#
# Usage:
#   xts_ret <- readRDS("02_data_processed/xts_ret_returns.rds")
#   xts_rel <- readRDS("02_data_processed/xts_rel.rds")
#
#   plot_rel_calendar_perf("GLD", xts_ret, xts_rel)
#   plot_rel_calendar_perf("XLK", xts_ret, xts_rel, highlight_year = 2022)
# ==============================================================================

plot_rel_calendar_perf <- function(ticker,
                                   xts_ret,
                                   xts_rel,
                                   highlight_year = as.integer(format(Sys.Date(), "%Y")),
                                   bmk_label      = "SPY") {

  build_df <- function(xts_col) {
    tibble(
      date = as.Date(index(xts_col)),
      ret  = as.numeric(xts_col[, 1])
    ) %>%
      filter(!is.na(ret)) %>%
      mutate(year = as.integer(format(date, "%Y"))) %>%
      group_by(year) %>%
      arrange(date) %>%
      mutate(tday = row_number(), cum_ret = cumprod(1 + ret) - 1) %>%
      ungroup()
  }

  df_abs <- build_df(xts_ret[, ticker])
  df_rel <- build_df(xts_rel[, ticker])

  common_years <- intersect(unique(df_abs$year), unique(df_rel$year))
  df_abs <- filter(df_abs, year %in% common_years)
  df_rel <- filter(df_rel, year %in% common_years)

  all_cum <- c(df_abs$cum_ret, df_rel$cum_ret)
  y_pad   <- diff(range(all_cum, na.rm = TRUE)) * 0.05
  y_lim   <- c(min(all_cum, na.rm = TRUE) - y_pad,
               max(all_cum, na.rm = TRUE) + y_pad)

  years   <- sort(common_years)
  n_years <- length(years)

  base_pal <- colorRampPalette(c("#D5D8DC", "#85929E", "#2C3E50"))(n_years)
  pal      <- setNames(base_pal, as.character(years))
  lwd      <- setNames(rep(0.65, n_years), as.character(years))

  if (as.character(highlight_year) %in% names(pal)) {
    pal[as.character(highlight_year)] <- "#E74C3C"
    lwd[as.character(highlight_year)] <- 1.5
  }

  make_panel <- function(df, panel_title, show_y_axis = TRUE) {

    anchors <- df %>%
      group_by(year) %>%
      slice_min(tday, n = 1) %>%
      mutate(tday = 0L, cum_ret = 0, ret = 0) %>%
      ungroup()

    plot_df <- bind_rows(anchors, df) %>% arrange(year, tday)

    labels_df <- df %>%
      group_by(year) %>%
      slice_max(tday, n = 1) %>%
      ungroup() %>%
      mutate(lbl = paste0(sprintf("%+.0f%%", cum_ret * 100), "  ", year))

    ggplot(plot_df,
           aes(x = tday, y = cum_ret,
               group     = factor(year),
               colour    = factor(year),
               linewidth = factor(year))) +

      geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.5) +
      geom_line(alpha = 0.85) +

      geom_text(
        data        = labels_df,
        aes(label = lbl),
        hjust       = -0.15,
        size        = 2.4,
        fontface    = "bold",
        show.legend = FALSE
      ) +

      scale_colour_manual(values = pal, name = "Year") +
      scale_linewidth_manual(values = lwd, guide = "none") +
      scale_y_continuous(
        labels = percent_format(accuracy = 1),
        limits = y_lim,
        name   = if (show_y_axis) "Cumulative Return" else NULL
      ) +
      scale_x_continuous(
        name   = "Trading Day of Year",
        expand = expansion(mult = c(0.01, 0.22))
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor  = element_blank(),
        panel.grid.major  = element_line(colour = "grey92", linewidth = 0.4),
        legend.position   = "none",
        axis.title        = element_text(size = 11, face = "bold"),
        axis.text         = element_text(size = 10),
        axis.text.y       = if (show_y_axis) element_text(size = 10) else element_blank(),
        axis.ticks.y      = if (show_y_axis) element_line() else element_blank(),
        plot.title        = element_text(face = "bold", size = 12),
        plot.subtitle     = element_blank(),
        plot.caption      = element_blank()
      ) +
      labs(title = panel_title)
  }

  p_abs <- make_panel(df_abs, paste0(ticker, "  —  Absolute Return"),                        show_y_axis = TRUE)
  p_rel <- make_panel(df_rel, paste0(ticker, "  —  Return vs ", bmk_label, " (\u03b1)"),    show_y_axis = FALSE)

  p_legend <- plot_calendar_perf(xts_ret[, ticker], highlight_year = highlight_year) +
    theme(legend.position = "bottom") +
    guides(colour = guide_legend(nrow = 2, override.aes = list(linewidth = 1.2)))

  legend_grob <- cowplot::get_legend(p_legend)

  top_row <- p_abs + p_rel + patchwork::plot_layout(widths = c(1, 1))

  patchwork::wrap_plots(
    top_row,
    legend_grob,
    ncol    = 1,
    heights = c(10, 1)
  ) +
    patchwork::plot_annotation(
      caption = paste0(
        "Daily log returns  |  Shared Y axis  |  Current year in red  |  As of ",
        format(max(df_abs$date), "%d %b %Y")
      ),
      theme = theme(plot.caption = element_text(size = 8, colour = "grey55"))
    )
}

# ==============================================================================
# MULTI-TICKER VERSION — small multiples, one panel per ticker
# ==============================================================================
#
# Usage:
#   plot_multi_calendar_perf(xts_ret[, c("SPY","QQQ","GLD","TLT","AGG")])
#   plot_multi_calendar_perf(xts_ret[, c("SPY","GLD")], ncol = 1)
# ==============================================================================

plot_multi_calendar_perf <- function(xts_multi,
                                     tickers        = NULL,
                                     title          = "Calendar Year Cumulative Return",
                                     highlight_year = as.integer(format(Sys.Date(), "%Y")),
                                     ncol           = NULL) {

  if (!is.null(tickers)) xts_multi <- xts_multi[, tickers]
  all_tickers <- colnames(xts_multi)

  df <- map_dfr(all_tickers, function(tk) {
    tibble(
      ticker = tk,
      date   = as.Date(index(xts_multi)),
      ret    = as.numeric(xts_multi[, tk])
    )
  }) %>%
    filter(!is.na(ret)) %>%
    mutate(year = as.integer(format(date, "%Y"))) %>%
    group_by(ticker, year) %>%
    arrange(date) %>%
    mutate(
      tday    = row_number(),
      cum_ret = cumprod(1 + ret) - 1
    ) %>%
    ungroup()

  anchors <- df %>%
    group_by(ticker, year) %>%
    slice_min(tday, n = 1) %>%
    mutate(tday = 0L, cum_ret = 0, ret = 0) %>%
    ungroup()

  plot_df <- bind_rows(anchors, df) %>%
    arrange(ticker, year, tday) %>%
    mutate(ticker = factor(ticker, levels = all_tickers))

  labels_df <- df %>%
    group_by(ticker, year) %>%
    slice_max(tday, n = 1) %>%
    ungroup() %>%
    mutate(
      ticker = factor(ticker, levels = all_tickers),
      lbl    = paste0(sprintf("%+.0f%%", cum_ret * 100), "  ", year)
    )

  years   <- sort(unique(plot_df$year))
  n_years <- length(years)

  base_pal <- colorRampPalette(c("#D5D8DC", "#85929E", "#2C3E50"))(n_years)
  pal      <- setNames(base_pal, as.character(years))
  lwd      <- setNames(rep(0.65, n_years), as.character(years))

  if (as.character(highlight_year) %in% names(pal)) {
    pal[as.character(highlight_year)] <- "#E74C3C"
    lwd[as.character(highlight_year)] <- 1.4
  }

  if (is.null(ncol)) ncol <- min(3L, length(all_tickers))

  ggplot(plot_df,
         aes(x = tday, y = cum_ret,
             group     = factor(year),
             colour    = factor(year),
             linewidth = factor(year))) +

    geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.4) +
    geom_line(alpha = 0.85) +

    geom_text(
      data        = labels_df,
      aes(label = lbl),
      hjust       = -0.15,
      size        = 2.4,
      fontface    = "bold",
      show.legend = FALSE
    ) +

    facet_wrap(~ ticker, ncol = ncol, scales = "free_y") +

    scale_colour_manual(values = pal, name = "Year") +
    scale_linewidth_manual(values = lwd, guide = "none") +
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       name   = "Cumulative Return") +
    scale_x_continuous(name   = "Trading Day of Year",
                       expand = expansion(mult = c(0.01, 0.22))) +

    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(colour = "grey92", linewidth = 0.35),
      strip.text        = element_text(face = "bold", size = 12),
      strip.background  = element_rect(fill = "#F4F6F7", colour = NA),
      legend.position   = "bottom",
      legend.text       = element_text(size = 9),
      legend.title      = element_text(size = 9, face = "bold"),
      axis.title        = element_text(size = 10, face = "bold"),
      axis.text         = element_text(size = 9),
      plot.title        = element_text(face = "bold", size = 14),
      plot.subtitle     = element_text(size = 10, colour = "grey45"),
      plot.caption      = element_text(size = 8,  colour = "grey55")
    ) +
    guides(colour = guide_legend(nrow = 2, override.aes = list(linewidth = 1.2))) +
    labs(
      title    = title,
      subtitle = sprintf(
        "%d tickers  |  Indexed to 0%% at Jan 1 each year  |  %d years  |  Current year in red",
        length(all_tickers), n_years
      ),
      caption  = paste0("Daily log returns  |  As of ", format(max(df$date), "%d %b %Y"))
    )
}
