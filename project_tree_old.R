# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./project_tree.R
# Purpose: Infrastructure Map (Aligned to Physical Files)
# ==============================================================================

project_tree <- list(
  dirs = list(
    scripts = "./scripts",
    raw     = "./01_data_raw",
    proc    = "./02_data_processed",
    output  = "./03_reports"
  ),
  scripts = list(
    init                = "./scripts/00_init_universe.R",
    wrangle             = "./scripts/01_etf_wrangle.R",
    signals             = "./scripts/01b_technical_signals.R", 
    saa                 = "./scripts/02_etf_saa_taa.R",
    reporting_engine    = "./scripts/03_reporting_engine.R",
    advanced_reporting  = "./scripts/04_advanced_reporting.R",
    shiny_builder       = "./scripts/05_shiny_builder.R",
    sanity_check        = "./scripts/06_download_sanity_check.R"
  ),
  products = list(
    # --- RAW DATA POINTER ---
    raw_p_d     = "./01_data_raw/raw_data.rds", 
    
    # --- PROCESSED DATA POINTERS ---
    ma_table     = "./02_data_processed/ma_technical_anchors.rds",
    tech_summary = "./02_data_processed/technical_summary.rds",
    refined_ret  = "./02_data_processed/xts_ret_returns.rds",
    sigma_mat    = "./02_data_processed/xts_analytical_sigma.rds",
    ref_report   = "./02_data_processed/outlier_regime_report.rds",
    alloc_plan   = "./02_data_processed/final_allocation.rds"
  )
)