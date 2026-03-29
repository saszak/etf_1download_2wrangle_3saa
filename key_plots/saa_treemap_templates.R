################################################################################
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE:    key_plots/saa_treemap_templates.R
# Purpose: Three business-style treemap templates for the SAA portfolio.
#          All functions take a pre-built df (output of build_treemap_df()).
#
# FUNCTIONS
#   build_treemap_df()       — build enriched df from SAA_PORTFOLIO + xts_ret
#   treemap_dark()           — Option A: dark terminal (Bloomberg style)
#   treemap_monochrome()     — Option B: monochrome by asset class (McKinsey)
#   treemap_minimal()        — Option C: white cells, coloured borders (FT/Economist)
#
# USAGE
#   source(here("scripts_saa_taa/saa_tree.R"))
#   source(here("key_plots/saa_treemap_templates.R"))
#   df <- build_treemap_df(xts_ret)
#   treemap_dark(df)
#   treemap_monochrome(df)
#   treemap_minimal(df)
################################################################################

library(tidyverse)
library(scales)
library(treemapify)

# ── Shared builder ─────────────────────────────────────────────────────────────

build_treemap_df <- function(xts_ret = NULL,
                              portfolio = SAA_PORTFOLIO) {
  df <- if (!is.null(xts_ret)) enrich_ytd(portfolio, xts_ret) else portfolio

  df %>% mutate(
    ytd_label  = if ("ytd_label" %in% names(.)) ytd_label else "",
    cell_label = paste0(ticker, "\n",
                        percent(weight, accuracy = 1),
                        if_else(ytd_label != "", paste0("\n", ytd_label), ""))
  )
}

# ── Option A — Dark terminal (Bloomberg style) ─────────────────────────────────

treemap_dark <- function(df, title = "SAA PORTFOLIO") {

  df <- df %>%
    mutate(leaf_fill = if ("ytd" %in% names(.)) {
      case_when(is.na(ytd) ~ ROLE_PAL[role],
                ytd >= 0   ~ "#40916C",
                TRUE       ~ "#C1121F")
    } else ROLE_PAL[role])

  ggplot(df, aes(area = weight, fill = I(leaf_fill),
                 label = cell_label, subgroup = asset_class)) +
    treemapify::geom_treemap(colour = "#1a1a2e", size = 3) +
    treemapify::geom_treemap_subgroup_border(colour = "#1a1a2e", size = 5) +
    treemapify::geom_treemap_subgroup_text(
      place = "topleft", colour = "#aaaaaa",
      fontface = "bold", size = 11, alpha = 1, grow = FALSE
    ) +
    treemapify::geom_treemap_text(
      colour = "white", fontface = "bold", size = 12,
      place = "centre", grow = FALSE
    ) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "#1a1a2e", colour = NA),
      plot.title      = element_text(face = "bold", size = 14,
                                     colour = "white", margin = margin(b = 6)),
      plot.subtitle   = element_text(colour = "#888888", size = 9,
                                     margin = margin(b = 10)),
      plot.margin     = margin(12, 12, 12, 12)
    ) +
    labs(title    = title,
         subtitle = "TREEMAP  |  AREA = WEIGHT  |  COLOUR = YTD")
}

# ── Option B — Monochrome by asset class (McKinsey/consulting style) ───────────

treemap_monochrome <- function(df, title = "SAA Portfolio — Treemap") {

  equity_shades <- c(
    URTH = "#1D3557", SPY  = "#2E5073", QQQ  = "#3D6B94",
    XLK  = "#4E84B5", SMH  = "#6299C5"
  )
  fi_shades <- c(AGG = "#2D6A4F")
  all_shades <- c(equity_shades, fi_shades)

  df <- df %>%
    mutate(leaf_fill = all_shades[ticker])

  ggplot(df, aes(area = weight, fill = I(leaf_fill),
                 label = cell_label, subgroup = asset_class)) +
    treemapify::geom_treemap(colour = "white", size = 4) +
    treemapify::geom_treemap_subgroup_border(colour = "white", size = 6) +
    treemapify::geom_treemap_subgroup_text(
      place = "bottomleft", colour = "white",
      fontface = "bold", size = 14, alpha = 0.4, grow = FALSE
    ) +
    treemapify::geom_treemap_text(
      colour = "white", fontface = "bold", size = 11,
      place = "centre", grow = FALSE
    ) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      plot.title      = element_text(face = "bold", size = 14,
                                     colour = "grey15", margin = margin(b = 6)),
      plot.subtitle   = element_text(colour = "grey50", size = 9,
                                     margin = margin(b = 10)),
      plot.margin     = margin(12, 12, 12, 12)
    ) +
    labs(title    = title,
         subtitle = "Monochrome by asset class  |  Darker = larger weight  |  Area = weight")
}

# ── Option C — White cells, coloured borders (FT / Economist style) ────────────

treemap_minimal <- function(df, title = "SAA Portfolio — Treemap") {

  df <- df %>%
    mutate(border_col = ROLE_PAL[role])

  ggplot(df, aes(area = weight, fill = I("white"),
                 label = cell_label, subgroup = asset_class)) +
    treemapify::geom_treemap(aes(colour = I(border_col)), size = 4) +
    treemapify::geom_treemap_subgroup_border(colour = "grey30", size = 5) +
    treemapify::geom_treemap_subgroup_text(
      place = "topleft", colour = "grey25",
      fontface = "bold", size = 12, alpha = 1, grow = FALSE
    ) +
    treemapify::geom_treemap_text(
      aes(colour = I(border_col)), fontface = "bold", size = 11,
      place = "centre", grow = FALSE
    ) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      plot.title      = element_text(face = "bold", size = 14,
                                     colour = "grey15", margin = margin(b = 6)),
      plot.subtitle   = element_text(colour = "grey50", size = 9,
                                     margin = margin(b = 10)),
      plot.margin     = margin(12, 12, 12, 12)
    ) +
    labs(title    = title,
         subtitle = "Border & text colour = role  |  Area = weight  |  White fill")
}

# ── Run ────────────────────────────────────────────────────────────────────────

if (!isTRUE(getOption("knitr.in.progress"))) {
  if (!exists("xts_ret"))
    xts_ret <- readRDS(here::here("02_data_processed/xts_ret_returns.rds"))
  if (!exists("SAA_PORTFOLIO"))
    source(here::here("scripts_saa_taa/saa_tree.R"))

  df <- build_treemap_df(xts_ret)
  print(treemap_dark(df))
  print(treemap_monochrome(df))
  print(treemap_minimal(df))
}
