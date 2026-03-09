# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# REGISTRY: prof_data_script_db
# Purpose: Master Audit with DATA NATURE Specification
# ==============================================================================

prof_data_script_db <- tibble::tibble(
  Script = c(
    "01_init.R", 
    "02_wrangle.R", 
    "03_saa_logic.R", 
    "04_advanced_reporting.R", 
    "05_shiny_builder.R"
  ),
  Data_Nature = c(
    "Metadata / Config", 
    "Non-Stationary (Price) -> Stationary (Log-Returns)", 
    "Stationary (Returns)", 
    "Statistical Artifacts (Z-Scores / Sigma)", 
    "UI/Reactive Environment"
  ),
  Required_Functions = c(
    "set_global_options()", 
    "clean_returns(), handle_outliers()", 
    "calculate_saa()", 
    "theme_sovereign_soft(), get_sigma_scores(), plot_sigma_analysis(), plot_precision_ytd(), plot_synthetic_efficiency(), plot_alpha_tape()", 
    "build_and_launch_shiny()"
  ),
  Exported_Globals = c(
    "etf_metadata, OPTIONS, WATCHLIST", 
    "xts_ret, signal_table", 
    "alloc_plan", 
    "p_sigma, p_ytd, p_eff, p_tape", 
    "ui, server"
  )
)
