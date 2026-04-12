# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./project_tree.R
# Purpose: Infrastructure Map — full directory + script registry
# Last updated: 2026-03-27
# ==============================================================================

project_tree <- list(

  dirs = list(
    shiny_dashboard = "./shiny_dashboard",  # ETF visualisation Shiny app
    scripts         = "./scripts",
    sm              = "./scripts_state_machine",
    spy_regime      = "./scripts_spy_dd_regime",
    spy_cycle       = "./scripts_spy_dd_cycle",
    safe_haven      = "./scripts_safe_haven_regime",
    exante          = "./scripts_exante_hypothesis",
    daa             = "./scripts_daa_saa_taa",
    saa_taa         = "./scripts_saa_taa",
    saa_structured  = "./scripts_saa_structured",
    saa_execution   = "./scripts_saa_execution",
    key_plots       = "./key_plots",
    key_findings    = "./key_findings",
    utility         = "./utility",
    raw             = "./01_data_raw",
    proc            = "./02_data_processed",
    output          = "./03_reports"
  ),

  scripts = list(

    # --- SHINY DASHBOARD ---
    shiny_app       = "./shiny_dashboard/app.R",       # entry point
    shiny_global    = "./shiny_dashboard/global.R",    # root data bridge
    shiny_helpers   = "./shiny_dashboard/utils/helpers.R",
    shiny_mod_table = "./shiny_dashboard/modules/mod_perf_table.R",
    shiny_mod_tmap  = "./shiny_dashboard/modules/mod_treemap.R",
    shiny_mod_plots  = "./shiny_dashboard/modules/mod_plots.R",
    shiny_mod_absrel  = "./shiny_dashboard/modules/mod_absrel.R",
    shiny_mod_calyear = "./shiny_dashboard/modules/mod_calyear.R",
    shiny_mod_comp      = "./shiny_dashboard/modules/mod_comp.R",
    shiny_mod_technical = "./shiny_dashboard/modules/mod_technical.R",
    shiny_mod_regime    = "./shiny_dashboard/modules/mod_regime.R",

    # --- FLOOR 1: DATA INGESTION ---
    init                = "./scripts/00_init_universe.R",        # 96-ticker Sovereign Universe
    wrangle             = "./scripts/01_etf_wrangle.R",          # download + wrangle → xts_ret, xts_rel
    signals             = "./scripts/01b_technical_signals.R",   # 200DMA + momentum signals
    saa                 = "./scripts/02_etf_saa_taa.R",          # SAA/TAA portfolio construction helpers
    reporting_engine    = "./scripts/03_reporting_engine.R",
    advanced_reporting  = "./scripts/04_advanced_reporting.R",
    shiny_builder       = "./scripts/05_shiny_builder.R",        # Sentinel Intelligence flexdashboard
    sanity_check        = "./scripts/06_download_sanity_check.R",
    ticker_scoring      = "./scripts/ticker_scoring.R",
    dashboard_etf       = "./scripts/dashboard_etf_performance.R",

    # --- SIGNAL ENGINE (State Machine) ---
    sm_engine           = "./scripts_state_machine/sm_engine.R",           # Hysteresis Math
    sm_visuals          = "./scripts_state_machine/sm_visuals.R",          # Sentinel Plotting
    sm_utils_visuals    = "./scripts_state_machine/sm_utils_visuals.R",
    sm_audit            = "./scripts_state_machine/execute_sm_audit.R",    # Ticker Audit Logic
    sm_batch            = "./scripts_state_machine/batch_enrich_registry.R", # Registry Builder

    # --- SPY DRAWDOWN REGIME ---
    spy_dd_regime       = "./scripts_spy_dd_regime/spy_dd_regime.R",          # build_regime_table(), plot_regime_overlay()
    spy_dd_regime_rel   = "./scripts_spy_dd_regime/spy_dd_regime_rel.R",      # plot_regime_rel_heatmap(), .outperf_type()
    regime_multi        = "./scripts_spy_dd_regime/regime_multi_ticker.R",    # plot_regime_multi_overlay()
    sm_diagram_rel      = "./scripts_spy_dd_regime/spy_statemachine_diagram_rel.R", # plot_sm_diagram(), plot_200DMA()

    # --- SPY DD CYCLE ---
    spy_dd_cycle        = "./scripts_spy_dd_cycle/AAA_spy_DD_regime-Functionalized.R",
    spy_dd_cycle_v3     = "./scripts_spy_dd_cycle/AAA_spy_ddcycle_plus_ticker_v3.R",
    spy_dual_overlay    = "./scripts_spy_dd_cycle/AAA_plot_dual_regime_overlay.R",
    spy_dd_cycle_final  = "./scripts_spy_dd_cycle/Final11Mar26_spy_DD_regime-Functionalized.R",

    # --- SAFE HAVEN REGIME ---
    safe_m1             = "./scripts_safe_haven_regime/method1_rolling_corr.R",      # rolling corr breakdown
    safe_m2             = "./scripts_safe_haven_regime/method2_logistic_regime.R",   # LASSO logistic (AUC ~0.72)
    safe_m3             = "./scripts_safe_haven_regime/method3_regime_covariance.R", # PCA eigenanalysis

    # --- EXANTE HYPOTHESIS ---
    exante_engine       = "./scripts_exante_hypothesis/exante_engine.R",       # build_exante_metrics()
    exante_visuals      = "./scripts_exante_hypothesis/exante_visuals.R",      # plot_exante_quadrant()
    exante_render_all   = "./scripts_exante_hypothesis/render_all_exante.R",

    # --- DAA / SAA / TAA FRAMEWORK ---
    daa_baseline        = "./scripts_daa_saa_taa/01_saa_baseline.R",          # 60/40 SAA definition
    daa_classifier      = "./scripts_daa_saa_taa/02_daa_classifier.R",        # EQ-Enhancer/Stabilizer/FI-Enhancer
    daa_rules           = "./scripts_daa_saa_taa/03_taa_rules.R",             # Regime → weight tilt engine
    daa_backtest        = "./scripts_daa_saa_taa/04_daa_backtest.R",          # Walk-forward simulation
    daa_stabilizer      = "./scripts_daa_saa_taa/05_dd_stabilizer.R",         # DD stabilizer screen
    daa_trend_quality   = "./scripts_daa_saa_taa/06_trend_quality.R",         # Trendability scoring
    daa_overlay_opt     = "./scripts_daa_saa_taa/07_dd_overlay_optimizer.R",  # Min-cost DD overlay solver
    daa_enhancer_screen = "./scripts_daa_saa_taa/08_enhancer_screen.R",       # 3-stage enhancer ranking

    # --- SAA / TAA ---
    saa_tree            = "./scripts_saa_taa/saa_tree.R",                     # SAA_PORTFOLIO, plot_saa_sunburst/treemap
    saa_portfolio       = "./scripts_saa_taa/05_saa_portfolio.R",             # calc_saa_portfolio(), build_ls_portfolio()
    taa_regime_engine   = "./scripts_saa_taa/taa_regime_engine.R",
    taa_momentum_screen = "./scripts_saa_taa/taa_momentum_screen.R",
    taa_backtest        = "./scripts_saa_taa/taa_portfolio_backtest.R",
    equity_spread       = "./scripts_saa_taa/equity_spread_screen.R",
    efficient_frontier  = "./scripts_saa_taa/efficient_frontier.R",

    # --- SAA STRUCTURED ---
    long_short_overlay  = "./scripts_saa_structured/long_short_overlay.R",   # build_ls_portfolio()
    fractal_saa         = "./scripts_saa_structured/fractal_saa_engine.R",
    meta_asset          = "./scripts_saa_structured/meta_asset_engine.R",
    risk_parity         = "./scripts_saa_structured/risk_parity_saa.R",
    saa_utility         = "./scripts_saa_structured/portfolio_utility.R",
    sunburst_radar      = "./scripts_saa_structured/sunburst_radar.R",

    # --- SAA EXECUTION ---
    saa_master          = "./scripts_saa_execution/saa_master_allocator.R",
    saa_policy          = "./scripts_saa_execution/saa_policy_weights.R",
    saa_drift           = "./scripts_saa_execution/saa_drift_analysis.R",
    saa_perf_attr       = "./scripts_saa_execution/saa_perf_attribution.R",
    saa_quantdb         = "./scripts_saa_execution/saa_quantdb_builder.R",
    saa_quantdb_sel     = "./scripts_saa_execution/saa_quantdb_selector.R",
    saa_alpha_intel     = "./scripts_saa_execution/saa_alpha_intelligence.R",
    saa_bridge          = "./scripts_saa_execution/saa_bridge_config.R",
    saa_dashboard       = "./scripts_saa_execution/saa_visual_dashboard.R",

    # --- KEY PLOTS (chart templates) ---
    chart_pe_bands          = "./key_plots/chart_pe_bands.R",            # Bloomberg P/E bands + plot_shiller_cape()
    chart_dd_frontier       = "./key_plots/chart_dd_frontier.R",         # DD frontier per instrument
    chart_multi_wealth      = "./key_plots/chart_multi_wealth_endlabel.R", # 2-5 portfolio wealth comparison
    chart_ma200             = "./key_plots/chart_ma200_signal_panel.R",  # per-ticker 200DMA grid
    chart_stacked_weight    = "./key_plots/chart_stacked_weight_timeline.R", # stacked weight area chart
    chart_calendar          = "./key_plots/calendar_heatmap.R",
    chart_saa_sunburst      = "./key_plots/saa_sunburst_plotly.R",
    chart_saa_treemap       = "./key_plots/saa_treemap_templates.R",
    chart_regime_alpha_tape = "./key_plots/regime_aware_alpha_tape.R",
    chart_yahoo_range       = "./key_plots/yahoo_finance_range_plot.R",
    chart_regime_overlay_ext = "./key_plots/chart_regime_rel_overlay.R",  # ★ GEM — plot_regime_overlay_ext(): 3-bar regime overlay + per-regime Rel bar + cumulative α panel

    # --- KEY FINDINGS ---
    kf1_anti_div        = "./key_findings/kf1_anti_diversification.R",   # low-ρ → worse MaxDD
    kf2_ls_pair         = "./key_findings/kf2_ls_pair_selection.R",      # L/S pair needs ρ>0.75
    kf3_trend_reversal  = "./key_findings/kf3_trend_reversal.R"          # winner→loser at regime transitions
  ),

  products = list(

    # --- RAW DATA ---
    raw_p_d           = "./01_data_raw/raw_data.rds",

    # --- PROCESSED DATA ---
    ma_table          = "./02_data_processed/ma_technical_anchors.rds",
    tech_summary      = "./02_data_processed/technical_summary.rds",
    refined_ret       = "./02_data_processed/xts_ret_returns.rds",
    derived_ret       = "./02_data_processed/xts_derived_ret.rds",
    sigma_mat         = "./02_data_processed/xts_analytical_sigma.rds",
    ref_report        = "./02_data_processed/outlier_regime_report.rds",

    # --- SIGNAL-PROJECT OUTPUTS ---
    signal_registry   = "./02_data_processed/sm_signal_registry.rds",

    # --- DD OVERLAY OPTIMIZER OUTPUTS ---
    stab_screen       = "./02_data_processed/dd_stabilizer_screen.rds",
    overlay_grid      = "./02_data_processed/overlay_grid.rds",
    pareto_frontier   = "./02_data_processed/pareto_frontier.rds",
    overlay_solutions = "./02_data_processed/overlay_target_solutions.rds",

    # --- FINAL ALLOCATION ---
    alloc_plan        = "./02_data_processed/final_allocation.rds"
  )
)
