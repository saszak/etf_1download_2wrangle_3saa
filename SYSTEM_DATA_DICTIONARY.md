# SYSTEM DATA DICTIONARY
## PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
*Last updated: 2026-03-15 — Source of Truth for naming, paths, and architecture*

---

## 1. DIRECTORY TREE

```text
etf_1download_2wrangle_3saa/
│
├── master_run.R                        # 8-stage pipeline orchestrator
├── project_tree.R                      # All file path definitions (scripts & products)
├── 00_global_params.R                  # OPTIONS list, signal thresholds (+4%/-2%)
├── SYSTEM_DATA_DICTIONARY.md           # This file
│
├── scripts/                            # Core pipeline (Floor 1)
│   ├── 00_init_universe.R              # ETF universe — 60 tickers + metadata
│   ├── 01_etf_wrangle.R                # Download, winsorize, compute returns
│   ├── 01b_technical_signals.R         # Moving averages & technical signals
│   ├── 02_etf_saa_taa.R                # SAA/TAA portfolio weights
│   ├── 03_reporting_engine.R           # Core reports
│   ├── 04_advanced_reporting.R         # Advanced analysis
│   ├── 05_shiny_builder.R              # Shiny dashboard builder
│   ├── 06_download_sanity_check.R      # Finnhub independent audit gate
│   └── utility/
│       ├── util_functions.R            # generate_relative_returns() etc.
│       └── utility.R
│
├── scripts_state_machine/              # Floor 2: 200DMA Hysteresis State Machine
│   ├── sm_engine.R                     # Hysteresis math (thru +4% / thrd -2%)
│   ├── sm_visuals.R                    # Sentinel plotting library
│   ├── batch_enrich_registry.R         # Batch registry builder (all 60 tickers)
│   ├── execute_sm_audit.R              # Single-ticker audit runner
│   └── sm_utils_visuals.R
│
├── scripts_spy_dd_regime/              # SPY Drawdown Regime Engine (3-state)
│   ├── spy_dd_regime.R                 # Core engine + single-ticker plots
│   ├── regime_multi_ticker.R           # Multi-ticker stacked bar overlay
│   ├── spy_dd_regime_rel.R             # Mixed mode: SPY absolute, others alpha
│   └── spy_statemachine_diagram_rel.R  # State machine flow diagram + perf panels
│
├── scripts_saa_structured/             # Structured portfolio models
│   ├── fractal_saa_engine.R            # Fractal SAA allocation engine
│   ├── long_short_overlay.R            # L/S momentum overlay (dollar neutral)
│   ├── risk_parity_saa.R               # Risk parity allocation
│   └── sunburst_radar.R                # Allocation visualisation
│
├── scripts_saa_taa/
│   └── 05_saa_portfolio.R              # SAA rebalancing logic (Return.portfolio)
│
├── key_plots/                          # Standalone chart scripts (exploratory)
├── scripts_spy_dd_cycle/               # (Legacy) Earlier drawdown cycle scripts
├── old/                                # Archived prior project versions
│
├── 01_data_raw/
│   └── raw_data.rds                    # Raw OHLCV from Yahoo Finance
│
├── 02_data_processed/                  # All processed artifacts (do not edit manually)
│   ├── xts_ret_returns.rds             # Winsorized log returns (xts, 60 cols)
│   ├── xts_analytical_sigma.rds        # Rolling 252-day z-scores (xts)
│   ├── outlier_regime_report.rds       # Latest sigma/regime labels (tibble)
│   ├── xts_rel.rds                     # Relative returns vs SPY (xts)
│   ├── xts_rel_cum.rds                 # Cumulative alpha, 0-base (xts)
│   ├── xts_rel_wealth.rds              # Wealth index, 1.0-base (xts)
│   ├── sm_signal_registry.rds          # 200DMA state machine registry (xts)
│   ├── ma_technical_anchors.rds        # Moving average table
│   ├── technical_summary.rds           # Technical signal summary
│   └── final_allocation.rds            # Final SAA/TAA weights
│
├── 03_reports/                         # Rendered report outputs
│
├── spy_regime_statemachine_report.Rmd  # SPY regime engine — all plots & methodology
├── long_short_overlay_report.Rmd       # L/S momentum overlay portfolio analysis
├── etf123_dashboard.Rmd                # Main ETF intelligence dashboard
└── template.Rmd                        # Rmd template for new reports
```

---

## 2. GLOBAL ENVIRONMENT VARIABLE REGISTRY

These objects must exist in the R global environment for downstream scripts to run.

| Variable | Source Script | Type | Description |
|:---|:---|:---|:---|
| `project_tree` | `project_tree.R` | list | All file paths (scripts + products) |
| `OPTIONS` | `master_run.R` | list | Pipeline flags: `force_fresh_sync`, `run_external_audit`, etc. |
| `etf_metadata` | `00_init_universe.R` | tibble | 60-ticker registry: ticker, name, sub_block, strat_func, sigma_limit, winsor_pct |
| `all_tickers` | `00_init_universe.R` | chr vector | All 60 ticker symbols in order |
| `cat_tickers` | `00_init_universe.R` | named list | Tickers grouped by `strat_func` |
| `block_tickers` | `00_init_universe.R` | named list | Tickers grouped by `sub_block` |
| `raw_data` | `01_etf_wrangle.R` | tibble | Raw OHLCV from Yahoo Finance (tidy long format) |
| `xts_ret` | `01_etf_wrangle.R` | xts | Winsorized log returns — **primary returns object** |
| `xts_rel` / `signal_table` | `01_etf_wrangle.R` | xts | Relative returns vs SPY (alpha spread) — same object, two names |
| `xts_rel_cum` | `01_etf_wrangle.R` | xts | Cumulative alpha (0-base) |
| `xts_wlth` | `01_etf_wrangle.R` | xts | Wealth index (1.0-base) |
| `xts_sigma` | `01_etf_wrangle.R` | xts | Rolling 252-day z-scores |
| `outlier_report` | `01_etf_wrangle.R` | tibble | Latest sigma labels per ticker: [STABLE] / [EXHAUSTION-HIGH/LOW] |

---

## 3. DATA FLOW (PIPELINE STAGES)

```
Stage 0  project_tree.R + 00_global_params.R   Path registry + OPTIONS flags
  ↓
Stage 1  00_init_universe.R                    etf_metadata, all_tickers
  ↓
Stage 1  01_etf_wrangle.R                      raw_data → xts_ret, xts_rel, xts_sigma
  ↓
Stage 2  batch_enrich_registry.R               sm_signal_registry.rds (200DMA states)
  ↓
Stage 3  01b_technical_signals.R               ma_technical_anchors.rds
  ↓
Stage 4  02_etf_saa_taa.R                      final_allocation.rds
  ↓
Stage 5  04_advanced_reporting.R               Technical intelligence merge
  ↓
Stage 6  06_download_sanity_check.R            Finnhub audit gate (optional)
  ↓
Stage 7  sm_visuals.R + 05_shiny_builder.R     Sentinel plots + Shiny UI
```

---

## 4. KEY FUNCTION INVENTORY

### Floor 1 — Data & Returns
| Function | File | Purpose |
|:---|:---|:---|
| `generate_relative_returns(xts_abs, bmk)` | `scripts/utility/util_functions.R` | Subtract benchmark column from all columns |

### Floor 2 — 200DMA State Machine
| Function | File | Purpose |
|:---|:---|:---|
| `get_enriched_xts_data(ticker, xts_rel_wlth)` | `sm_engine.R` | SMA200 + hysteresis labeling |
| `plot_200DMA(ticker, ...)` | `sm_visuals.R` | Orchestrates enrichment + plot; registers into `key_plots` |
| `plot_signal_with_dd(...)` | `sm_visuals.R` | Price / oscillator / drawdown 3-panel chart |
| `batch_enrich_registry()` | `batch_enrich_registry.R` | Builds registry for all 60 tickers |

### SPY Drawdown Regime Engine (3-state)
| Function | File | Purpose |
|:---|:---|:---|
| `build_regime_table(xts_col, t_fall)` | `spy_dd_regime.R` | Core engine — Fall / Recovery / Consolidation labels |
| `plot_regime_overlay(xts_col, ...)` | `spy_dd_regime.R` | Cumulative return with regime shading + transition arrows |
| `plot_regime_stats(xts_col, ...)` | `spy_dd_regime.R` | Avg return, duration, % time by state |
| `plot_regime_calendar(xts_col, ...)` | `spy_dd_regime.R` | Monthly calendar heatmap by dominant regime |
| `plot_regime_multi_overlay(xts_ret, tickers, ...)` | `regime_multi_ticker.R` | Stacked per-ticker bar overlay |
| `plot_regime_rel_overlay(xts_ret, tickers, ...)` | `spy_dd_regime_rel.R` | SPY absolute + others alpha per period |
| `plot_regime_rel_heatmap(xts_ret, tickers, ...)` | `spy_dd_regime_rel.R` | Per-period heatmap (diverging scale) |
| `plot_regime_rel_summary(xts_ret, tickers, ...)` | `spy_dd_regime_rel.R` | Avg alpha per ticker × regime type (faceted) |
| `plot_sm_diagram(xts_ret, master, t_fall)` | `spy_statemachine_diagram_rel.R` | State machine flow diagram (triangle layout) |
| `plot_sm_diagram_perf(xts_ret, tickers, ...)` | `spy_statemachine_diagram_rel.R` | Flow diagram + per-ticker performance bars |
| `plot_sm_alpha_heatmap(xts_ret, tickers, ...)` | `spy_statemachine_diagram_rel.R` | Ticker × regime alpha heatmap |
| `plot_sm_transition_matrix(xts_ret, master, ...)` | `spy_statemachine_diagram_rel.R` | Empirical transition probability matrix |

### SAA / Portfolio
| Function | File | Purpose |
|:---|:---|:---|
| `calc_saa_portfolio(...)` | `scripts_saa_taa/05_saa_portfolio.R` | SAA rebalancing using Return.portfolio |
| `build_ls_portfolio(xts_rel, n_long, n_short, ...)` | `scripts_saa_structured/long_short_overlay.R` | Dollar-neutral L/S momentum overlay |
| `plot_ls_weights(weights_history)` | `scripts_saa_structured/long_short_overlay.R` | Weight heatmap over time |

---

## 5. REGIME ENGINE CONSTANTS

| Constant | Value | Meaning |
|:---|:---|:---|
| `t_fall` | 0.10 (default) | SPY drawdown threshold to enter Fall state |
| `REGIME_PAL["Fall"]` | `#D90429` | Red |
| `REGIME_PAL["Recovery"]` | `#F77F00` | Orange |
| `REGIME_PAL["Consolidation"]` | `#2D6A4F` | Green |
| Hysteresis upper (`thru`) | +0.04 | 200DMA bull trigger |
| Hysteresis lower (`thrd`) | -0.02 | 200DMA bear trigger |

---

## 6. REPORTS

| File | Output | Content |
|:---|:---|:---|
| `spy_regime_statemachine_report.Rmd` | HTML + PDF | Full SPY regime engine — 14 sections, all 4 scripts |
| `long_short_overlay_report.Rmd` | HTML | L/S momentum overlay portfolio performance |
| `etf123_dashboard.Rmd` | HTML | Main ETF intelligence dashboard |

---

## 7. NAMING RULES

- **`xts_ret`** — always absolute log returns, Winsorized. Never rename.
- **`xts_rel`** — always relative to SPY (alpha spread). Aliased as `signal_table` for legacy compatibility.
- **`xts_wlth`** — wealth index (1.0-base) used for SMA200 math (avoids log-return instability).
- All `.rds` paths go through `project_tree$products$*` — never hardcode paths in analysis scripts.
- New analysis scripts: always source `project_tree.R` first, consume `xts_ret` / `xts_rel` from global env.
