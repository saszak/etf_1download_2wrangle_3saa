# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts/util_functions.R
# Purpose: Global Utility Library for Signal-Project & Technical Logic
# ==============================================================================

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