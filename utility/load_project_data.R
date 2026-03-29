################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : utility/load_project_data.R
# Purpose : Load all key processed data artifacts into the calling environment.
#
# USAGE
#   source(here::here("utility/load_project_data.R"))
#   load_project_data()          # loads all, prints summary
#   load_project_data(quiet = TRUE)   # silent
#
# OBJECTS LOADED (into caller's environment)
#   xts_ret               Winsorized log returns          (xts, 60 cols)
#   xts_sigma             Rolling 252-day z-scores        (xts)
#   outlier_regime_report Sigma / regime labels           (tibble)
#   xts_rel               Relative returns vs SPY         (xts)
#   xts_rel_cum           Cumulative alpha, 0-base        (xts)
#   xts_rel_wealth        Wealth index, 1.0-base          (xts)
#   sm_signal_registry    200DMA state machine registry   (xts)
#   ma_technical_anchors  Moving average table
#   technical_summary     Technical signal summary
#   final_allocation      Final SAA/TAA weights
################################################################################

load_project_data <- function(quiet = FALSE) {

  data_dir <- here::here("02_data_processed")

  files <- list(
    xts_ret               = "xts_ret_returns.rds",
    xts_sigma             = "xts_analytical_sigma.rds",
    outlier_regime_report = "outlier_regime_report.rds",
    xts_rel               = "xts_rel.rds",
    xts_rel_cum           = "xts_rel_cum.rds",
    xts_rel_wealth        = "xts_rel_wealth.rds",
    sm_signal_registry    = "sm_signal_registry.rds",
    ma_technical_anchors  = "ma_technical_anchors.rds",
    technical_summary     = "technical_summary.rds",
    final_allocation      = "final_allocation.rds"
  )

  loaded  <- character(0)
  missing <- character(0)

  for (var_name in names(files)) {
    path <- file.path(data_dir, files[[var_name]])
    if (file.exists(path)) {
      assign(var_name, readRDS(path), envir = parent.frame())
      loaded <- c(loaded, var_name)
    } else {
      missing <- c(missing, var_name)
    }
  }

  if (!quiet) {
    cat("\n── load_project_data() ─────────────────────────────────────────────────\n")
    if (length(loaded) > 0)
      cat(sprintf("  OK  : %s\n", loaded), sep = "")
    if (length(missing) > 0)
      cat(sprintf("  MISS: %s  (%s)\n", missing, files[missing]), sep = "")
    cat(sprintf("  %d / %d artifacts loaded\n", length(loaded), length(files)))
    cat("────────────────────────────────────────────────────────────────────────\n\n")
  }

  invisible(list(loaded = loaded, missing = missing))
}

################################################################################
# END OF FILE
################################################################################
