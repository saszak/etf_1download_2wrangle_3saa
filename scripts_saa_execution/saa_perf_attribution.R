################################################################################
# scripts_saa_execution/saa_perf_attribution.R
# Purpose : Quantify the risk/return profile of the Book 2 TAA LS basket and
#           plot cumulative alpha.
#           Ported from etf_saa_taa/06_performance_attribution.R.
#
# Input   : 02_data_processed/saa_policy_list.rds
#           xts_d_rel  (from saa_bridge_config.R)
# Output  : taa_stats       (tibble, global env)
#           p_taa_cum_alpha (ggplot, global env)
#           02_data_processed/saa_taa_stats.rds
#           03_reports/saa_taa_cum_return.png
################################################################################

library(dplyr)
library(tidyr)
library(xts)
library(ggplot2)
library(scales)
library(here)

if (!exists("xts_d_rel")) source(here::here("scripts_saa_execution/saa_bridge_config.R"))

if (!exists("policy_list")) {
  policy_list <- readRDS(here::here("02_data_processed/saa_policy_list.rds"))
}

ann_factor <- 252

# ── 1. IDENTIFY ACTIVE LS PAIRS ───────────────────────────────────────────────
ls_pairs <- policy_list$TAA %>%
  filter(book == "BOOK_2_TAA_LONG") %>%
  pull(ticker)

if (length(ls_pairs) == 0) {
  message("saa_perf: Book 2 is flat — no active LS pairs to attribute.")
  taa_stats       <- tibble()
  p_taa_cum_alpha <- ggplot() +
    labs(title = "TAA Book 2: Flat (no active LS pairs)",
         x = NULL, y = NULL) +
    theme_minimal()

} else {

  message("saa_perf: attributing LS basket: ",
          paste(ls_pairs, collapse = ", "))

  # ── 2. BASKET ALPHA (equal-weighted relative returns) ───────────────────────
  ls_returns   <- xts_d_rel[, ls_pairs, drop = FALSE]
  basket_alpha <- xts(rowMeans(ls_returns, na.rm = TRUE),
                      order.by = zoo::index(ls_returns))
  basket_alpha[is.nan(basket_alpha)] <- 0

  cum_alpha    <- cumprod(1 + as.numeric(basket_alpha)) - 1

  # ── 3. RISK / RETURN METRICS ─────────────────────────────────────────────────
  alpha_vec <- as.numeric(basket_alpha)
  alpha_vec <- alpha_vec[!is.na(alpha_vec)]

  ann_alpha  <- mean(alpha_vec) * ann_factor
  alpha_vol  <- sd(alpha_vec)   * sqrt(ann_factor)
  sharpe     <- ifelse(alpha_vol != 0, ann_alpha / alpha_vol, NA_real_)

  # Max drawdown of the alpha stream
  cum_ts     <- cumprod(1 + alpha_vec) - 1
  running_hi <- cummax(cum_ts)
  max_dd     <- min(cum_ts - running_hi)

  taa_stats <- tibble(
    Metric = c("Ann. Alpha", "Alpha Vol", "Sharpe (Alpha)", "Max DD (Alpha)"),
    Value  = round(c(ann_alpha, alpha_vol, sharpe, max_dd), 4),
    Pairs  = paste(ls_pairs, collapse = " | ")
  )

  # ── 4. CUMULATIVE ALPHA CHART ────────────────────────────────────────────────
  df_cum <- data.frame(
    Date      = zoo::index(ls_returns),
    CumReturn = cum_alpha
  )

  p_taa_cum_alpha <- ggplot(df_cum, aes(x = Date, y = CumReturn)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
    geom_area(fill = "#2E7D32", alpha = 0.12) +
    geom_line(colour = "#2E7D32", linewidth = 1) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      title    = "Sovereign TAA: Cumulative Alpha — LS Basket",
      subtitle = paste0("Pairs: ", paste(ls_pairs, collapse = ", "),
                        "  |  Equal-weighted relative returns"),
      y = "Cumulative Spread Return",
      x = NULL,
      caption = paste("Generated:", Sys.Date())
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title       = element_text(face = "bold"),
          panel.grid.minor = element_blank())
}

# ── 5. SAVE ───────────────────────────────────────────────────────────────────
saveRDS(taa_stats, here::here("02_data_processed/saa_taa_stats.rds"))

reports_dir <- here::here("03_reports")
if (!dir.exists(reports_dir)) dir.create(reports_dir, recursive = TRUE)
ggsave(file.path(reports_dir, "saa_taa_cum_return.png"),
       plot = p_taa_cum_alpha, width = 10, height = 5, dpi = 150)

message("saa_perf: SAVED → 02_data_processed/saa_taa_stats.rds")
message("saa_perf: SAVED → 03_reports/saa_taa_cum_return.png")

# ── Auto-run guard ─────────────────────────────────────────────────────────────
if (!isTRUE(getOption("knitr.in.progress"))) {

  cat("\n── TAA Risk / Return Profile ────────────────────────────\n")
  print(taa_stats)

  print(p_taa_cum_alpha)
}
