################################################################################
# scripts_saa_execution/saa_visual_dashboard.R
# Purpose : Portfolio snapshot visuals — bar chart (Book 3 weights) and
#           radar/spider DNA charts comparing tickers on 5 dimensions.
#           Ported from etf_saa_taa/05_visual_dashboard.R.
#
# Input   : 02_data_processed/saa_policy_list.rds
#           02_data_processed/saa_quantdb.rds
#           saa_metadata, bucket vectors (from saa_bridge_config.R)
# Output  : p_book3_weights  (ggplot, global env)
#           dna_matrix       (tibble, global env)
#           Functions: plot_etf_spider(), plot_etf_spider_grid(),
#                      plot_etf_spider_vertical(), rank_spider_dna()
#           03_reports/saa_portfolio_snapshot.png
################################################################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(fmsb)
library(here)

if (!exists("policy_list")) {
  policy_list <- readRDS(here::here("02_data_processed/saa_policy_list.rds"))
}
if (!exists("etf_quantdb")) {
  etf_quantdb <- readRDS(here::here("02_data_processed/saa_quantdb.rds"))
}
if (!exists("saa_metadata")) {
  source(here::here("scripts_saa_execution/saa_bridge_config.R"))
}

# ── 1. BUILD DNA MATRIX (normalised 0–1 per dimension) ────────────────────────
# Five axes: Alpha, Return, Momentum, RiskParity (vol scalar), Stability
dna_matrix <- etf_quantdb %>%
  mutate(
    Alpha      = scales::rescale(alpha_6y),
    Return     = scales::rescale(ann_ret),
    Momentum   = scales::rescale(mom_d),
    RiskParity = scales::rescale(vol_scalar),
    # Stability: lower |kurtosis| and |skew| → higher score
    Stability  = scales::rescale(-(abs(kurtosis_val) + abs(skew_val)))
  ) %>%
  select(ticker, Alpha, Return, Momentum, RiskParity, Stability)

# ── 2. BOOK 3 BAR CHART ───────────────────────────────────────────────────────
plot_df <- policy_list$TOTAL %>%
  left_join(saa_metadata %>% select(ticker, cluster), by = "ticker") %>%
  mutate(cluster = tidyr::replace_na(cluster, "Sovereign Core")) %>%
  arrange(net_weight)

p_book3_weights <- ggplot(plot_df,
    aes(x = reorder(ticker, net_weight), y = net_weight, fill = cluster)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_text(aes(label = scales::percent(net_weight, accuracy = 0.1)),
            hjust = -0.2, size = 4, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Sovereign Book 3: Total Combined Exposure",
    subtitle = paste0("Net = 60/40 SAA  ±  5% LS Synthetics  |  ",
                      Sys.Date()),
    x        = NULL,
    y        = "Net Portfolio Weight",
    fill     = "Cluster",
    caption  = paste("Generated:", Sys.Date())
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position  = "right",
        plot.title       = element_text(face = "bold", size = 15),
        panel.grid.minor = element_blank())

# ── 3. SPIDER / RADAR FUNCTIONS ───────────────────────────────────────────────

#' Compare tickers on 5 DNA dimensions (single radar panel).
#' @param target_tickers Character vector; SPY is automatically added as anchor.
#' @param data_input     DNA matrix; defaults to global dna_matrix.
plot_etf_spider <- function(target_tickers,
                            data_input = dna_matrix) {
  if (!"SPY" %in% target_tickers) target_tickers <- c("SPY", target_tickers)

  plot_data <- data_input %>%
    filter(ticker %in% target_tickers) %>%
    as.data.frame()
  rownames(plot_data) <- plot_data$ticker
  plot_data$ticker <- NULL

  df_radar <- rbind(rep(1, 5), rep(0, 5), plot_data)

  n_lines       <- nrow(plot_data)
  colors_border <- c("#2E8B57", "#CD5C5C", "#4682B4",
                     "#E67E22", "#8E44AD")[seq_len(n_lines)]
  colors_in     <- paste0(colors_border, "66")

  fmsb::radarchart(df_radar,
                   pcol = colors_border, pfcol = colors_in, plwd = 3,
                   cglcol = "grey80", cglty = 1, axislabcol = "grey30",
                   vlcex = 0.9, title = "Sovereign DNA Comparison")

  legend(x = "bottomright", legend = rownames(plot_data),
         fill = colors_in, bty = "n", cex = 0.8)
}

#' Grid of individual radars — each ticker vs SPY.
#' @param target_tickers Tickers to compare (SPY added automatically).
#' @param cols           Number of columns in the grid.
plot_etf_spider_grid <- function(target_tickers,
                                 cols = 3,
                                 data_input = dna_matrix) {
  if (!"SPY" %in% target_tickers) target_tickers <- c("SPY", target_tickers)
  assets <- target_tickers[target_tickers != "SPY"]

  n    <- length(assets)
  rows <- ceiling(n / cols)
  par(mfrow = c(rows, cols), mar = c(2, 2, 4, 2))
  on.exit(par(mfrow = c(1, 1)))

  for (t in assets) {
    plot_data <- data_input %>%
      filter(ticker %in% c("SPY", t)) %>%
      as.data.frame()
    rownames(plot_data) <- plot_data$ticker
    plot_data$ticker    <- NULL
    df_radar <- rbind(rep(1, 5), rep(0, 5), plot_data)

    fmsb::radarchart(df_radar,
                     pcol  = c("#80808088", "#2E8B57"),
                     pfcol = c("#80808022", "#2E8B5744"),
                     plwd  = c(1, 3), plty = c(2, 1),
                     cglcol = "grey90", vlcex = 0.7,
                     title  = paste(t, "vs SPY"))
  }
}

#' Vertical single-column stack — each ticker vs SPY in its own panel.
#' Best for 2–4 tickers; use grid for larger sets.
plot_etf_spider_vertical <- function(target_tickers,
                                     data_input = dna_matrix) {
  if (!"SPY" %in% target_tickers) target_tickers <- c("SPY", target_tickers)
  assets <- target_tickers[target_tickers != "SPY"]
  n      <- length(assets)
  if (n == 0) return(message("saa_visual: no tickers to plot."))

  n_cols <- if (n <= 2) 1L else 2L
  n_rows <- ceiling(n / n_cols)
  par(mfrow = c(n_rows, n_cols), mar = c(3, 3, 4, 3))
  on.exit(par(mfrow = c(1, 1)))

  for (t in assets) {
    plot_data <- data_input %>%
      filter(ticker %in% c("SPY", t)) %>%
      as.data.frame()
    rownames(plot_data) <- plot_data$ticker
    plot_data$ticker    <- NULL
    df_radar <- rbind(rep(1, 5), rep(0, 5), plot_data)

    fmsb::radarchart(df_radar,
                     axistype     = 1,
                     seg          = 4,
                     caxislabels  = seq(0, 1, 0.25),
                     axislabcol   = "grey50",
                     cglcol       = "grey90", cglty = 1,
                     pcol         = c("#80808066", "#E63946"),
                     pfcol        = c("#80808011", "#E6394633"),
                     plwd         = c(2, 4),
                     plty         = c(2, 1),
                     vlcex        = 1.2,
                     title        = paste0("Strategic DNA: ", t, " vs SPY"))

    legend(x = "topright",
           legend = c("SPY (Anchor)", t),
           col    = c("#80808066", "#E63946"),
           lty    = c(2, 1), lwd = 3, bty = "n", cex = 1.1)
  }
}

#' Rank tickers by weighted DNA score (Alpha + Stability + Momentum).
#' @return Tibble sorted by dna_score descending.
rank_spider_dna <- function(data_input     = dna_matrix,
                             weight_alpha  = 0.4,
                             weight_stab   = 0.3,
                             weight_mom    = 0.3) {
  data_input %>%
    mutate(
      dna_score = (Alpha     * weight_alpha) +
                  (Stability * weight_stab)  +
                  (Momentum  * weight_mom)
    ) %>%
    arrange(desc(dna_score)) %>%
    mutate(rank = row_number())
}

# ── 4. SAVE BOOK 3 SNAPSHOT ───────────────────────────────────────────────────
reports_dir <- here::here("03_reports")
if (!dir.exists(reports_dir)) dir.create(reports_dir, recursive = TRUE)
ggsave(file.path(reports_dir, "saa_portfolio_snapshot.png"),
       plot = p_book3_weights, width = 12, height = 7, dpi = 150)
message("saa_visual: SAVED → 03_reports/saa_portfolio_snapshot.png")

# ── Auto-run guard ─────────────────────────────────────────────────────────────
if (!isTRUE(getOption("knitr.in.progress"))) {

  print(p_book3_weights)

  cat("\n── Top 5 by DNA Score ───────────────────────────────────\n")
  print(rank_spider_dna() %>%
    select(ticker, dna_score, rank, Alpha, Return, Momentum, Stability) %>%
    head(5))

  cat("\n── Defensive Bulwark DNA ────────────────────────────────\n")
  plot_etf_spider_grid(defensive_tickers)

  cat("\n── Cycle & Breadth DNA ──────────────────────────────────\n")
  plot_etf_spider_grid(cycle_tickers)
}
