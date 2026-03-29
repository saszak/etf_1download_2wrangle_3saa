################################################################################
# scripts_saa_execution/saa_master_allocator.R
# Purpose : Convert Book 3 net weights into a dollar-denominated execution
#           list and export a trade CSV.
#           Ported from etf_saa_taa/04_master_allocator.R.
#
# Input   : 02_data_processed/saa_drift_report.rds
# Output  : execution_list  (tibble, global env)
#           03_reports/saa_trade_list_<date>.csv
#           03_reports/saa_trade_list_latest.csv   (always overwritten)
################################################################################

library(dplyr)
library(scales)
library(here)

total_aum <- 1e6   # reference portfolio size ($1M); adjust as needed

# ── Load upstream output ──────────────────────────────────────────────────────
if (!exists("drift_report")) {
  dr_path <- here::here("02_data_processed/saa_drift_report.rds")
  if (!file.exists(dr_path))
    stop("saa_allocator: run saa_drift_analysis.R first.")
  drift_report <- readRDS(dr_path)
}

# ── Build execution list ──────────────────────────────────────────────────────
execution_list <- drift_report %>%
  mutate(
    target_value  = net_weight * total_aum,
    action        = case_when(
      target_value > 0  ~ "BUY/HOLD",
      target_value < 0  ~ "SHORT/SELL",
      TRUE              ~ "FLAT"
    ),
    weight_pct    = scales::percent(net_weight, accuracy = 0.1),
    target_value_fmt = scales::dollar(target_value, accuracy = 1)
  ) %>%
  select(ticker, cluster, category,
         net_weight, weight_pct, target_value, target_value_fmt, action) %>%
  arrange(desc(target_value))

# ── Export CSVs ───────────────────────────────────────────────────────────────
reports_dir <- here::here("03_reports")
if (!dir.exists(reports_dir)) dir.create(reports_dir, recursive = TRUE)

# Dated snapshot (keeps history)
dated_file  <- file.path(reports_dir,
                         paste0("saa_trade_list_", Sys.Date(), ".csv"))
# Latest (always current)
latest_file <- file.path(reports_dir, "saa_trade_list_latest.csv")

write.csv(execution_list %>% select(-weight_pct, -target_value_fmt),
          dated_file,  row.names = FALSE)
write.csv(execution_list %>% select(-weight_pct, -target_value_fmt),
          latest_file, row.names = FALSE)

# ── Summary stats ─────────────────────────────────────────────────────────────
long_val  <- sum(execution_list$target_value[execution_list$target_value > 0])
short_val <- sum(execution_list$target_value[execution_list$target_value < 0])

message("saa_allocator: FINAL ALLOCATION COMPLETE")
message("  Dated CSV  : ", dated_file)
message("  Latest CSV : ", latest_file)
message("  Long  exposure : $", format(long_val,  big.mark = ",", nsmall = 0))
message("  Short exposure : $", format(short_val, big.mark = ",", nsmall = 0))
message("  Net   exposure : $", format(long_val + short_val, big.mark = ",", nsmall = 0))

# ── Auto-run guard ─────────────────────────────────────────────────────────────
if (!isTRUE(getOption("knitr.in.progress"))) {

  cat("\n── Execution List ───────────────────────────────────────\n")
  print(execution_list %>%
    select(ticker, cluster, net_weight, weight_pct,
           target_value_fmt, action))

  cat("\n── Portfolio Summary ────────────────────────────────────\n")
  cat("  Reference AUM  : $", format(total_aum,          big.mark = ","), "\n")
  cat("  Long  exposure : $", format(long_val,            big.mark = ","), "\n")
  cat("  Short exposure : $", format(abs(short_val),      big.mark = ","), "\n")
  cat("  Net   exposure : $", format(long_val + short_val,big.mark = ","), "\n")
  cat("  Positions      : ", nrow(execution_list), "\n")
}
