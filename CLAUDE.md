# CLAUDE.md — ETF Intelligence Project
# Loaded automatically at every session start.

## Project in one sentence
A modular R research project that classifies ETF behaviour relative to SPY across
three data-driven market regimes (Fall / Recovery / Consolidation) and uses those
findings to build a concrete SAA portfolio with institutional mapping.

---

## Risk/Return calculation policy (MANDATORY)
Always use **PerformanceAnalytics** as the primary source for all risk/return metrics.
Never hand-roll annualised return, volatility, Sharpe, drawdown, or IR calculations.

| Metric | Correct call |
|--------|-------------|
| Ann. return | `Return.annualized(r, scale = 252)` |
| Ann. volatility | `StdDev.annualized(r, scale = 252)` |
| Sharpe ratio | `SharpeRatio.annualized(r, Rf = 0, scale = 252)` |
| Max drawdown | `maxDrawdown(r)` |
| Information ratio | `InformationRatio(r, benchmark)` |
| Calmar ratio | `CalmarRatio(r, scale = 252)` |

Rationale: hand-rolled `sd * sqrt(252)` or `prod(1+r)^(252/n) - 1` inflates numbers
when the window is < 1 year; PerformanceAnalytics handles edge cases and annualisation
correctly and consistently.

---

## Key constants (never change without asking)
| Constant | Value | Where used |
|---|---|---|
| `T_FALL` | 0.10 (10%) | Regime engine threshold — `build_regime_table()` |
| `MASTER` | "SPY" | Benchmark throughout all analyses |
| 200DMA thresholds | upper = +4%, lower = −2% | Hysteresis in `plot_200DMA()` |
| Rolling corr window | 60 days | Safe haven rolling correlation (Section 4) |
| `min_persist` | 5 days (default) | Market state confirmation filter — `build_market_state_history()` |
| MS roll_win | 60 days | US premium / EM appetite spread window |
| MS mom_win | 63 days | Sector rotation momentum window (L2) |
| MS EQ range | 25%–75% | Allocation clip bounds in `ms_risk_allocation.R` |

---

## Core data objects (loaded in every Rmd / script)
| Object | File | Description |
|---|---|---|
| `xts_ret` | `02_data_processed/xts_ret_returns.rds` | Daily log returns, clean — matches Bloomberg total return |
| `xts_ret_winsor` | `02_data_processed/xts_ret_winsor.rds` | Winsorized log returns — for sigma/regime/z-score calculations only; distorts cumulative returns |
| `xts_rel` | `02_data_processed/xts_rel.rds` | Relative returns (ticker / SPY) |
| `etf_metadata` | `scripts/00_init_universe.R` | 96-ticker Sovereign Universe with asset_class, tree_level, pf_function |
| `rt` | built via `build_regime_table(spy_ret, T_FALL)` | Regime table: one row per episode — columns: `regime`, `xmin`, `xmax`, `days`, `period_return`, `ann_return`, `label`, `is_thin` |

---

## Script map — active files only
```
scripts/
  00_init_universe.R          ← etf_metadata (96-ticker Sovereign Universe)
  01_etf_wrangle.R            ← download + wrangle → xts_ret, xts_rel
  01b_technical_signals.R     ← 200DMA and momentum signals
  02_etf_saa_taa.R            ← SAA/TAA portfolio construction helpers

scripts_spy_dd_regime/
  spy_dd_regime.R             ← build_regime_table(), plot_regime_overlay(),
                                 plot_regime_stats()
  spy_dd_regime_rel.R         ← plot_regime_rel_overlay(), plot_regime_rel_heatmap(),
                                 plot_regime_rel_summary()
                                 .outperf_type(): HEDGE/STABLE/ALPHA/LAG/LOSS tags
  regime_multi_ticker.R       ← plot_regime_multi_overlay(), plot_regime_ticker_heatmap()
  spy_statemachine_diagram_rel.R ← plot_sm_diagram(), plot_sm_alpha_heatmap(),
                                    plot_200DMA()

scripts_state_machine/
  sm_engine.R                 ← 200DMA state machine core logic
  sm_visuals.R                ← state machine plotting functions
  batch_enrich_registry.R     ← batch ticker enrichment
  execute_sm_audit.R          ← audit runner

scripts_daa_saa_taa/
  01_saa_baseline.R           ← 60/40 SAA definition, benchmark construction
  02_daa_classifier.R         ← classify universe into EQ-Enhancer / EQ-Stabilizer /
                                 FI-Enhancer by regime using .outperf_type()
  03_taa_rules.R              ← 200DMA trend filter: signal heatmap + composition + per-ticker panels
  04_daa_backtest.R           ← walk-forward DAA vs SAA vs Passive SPY/IEF; wealth + DD
  05_dd_stabilizer.R          ← insurance quadrant + DD frontier (screen at w=10%)
  06_trend_quality.R          ← trendability scoring: Hurst + hit rate + whipsaw + capture ratio
  07_dd_overlay_optimizer.R   ← MINIMUM-COST OVERLAY: grid-search combos, Pareto frontier,
                                 solve for MaxDD targets 15/17/19/21% → recommended weights
  08_enhancer_screen.R        ← 3-STAGE ENHANCER SCREEN: candidates (OR-screen) →
                                 context score (rolling IR + regime α + 200DMA + momentum) →
                                 select_enhancers() with ρ guard → plot_xts_pair per selection
  plot_rebal_drift.R          ← REBALANCING DRIFT STUDY (5 strategies vs BaH):
                                 Daily / Quarterly / Annual / Threshold±10% / Threshold±5%
                                 Builds: main_view (M1–M8 + table T), appendix (A1–A8),
                                 fig_D, fig_Q, fig_AR, fig_TR10, fig_TR5 via build_pairwise()
                                 knitr.in.progress guards; sources into rebalancing_drift_guide.Rmd
  saa_overlay_workflow.R      ← END-TO-END SAA OVERLAY WORKFLOW (v1):
                                 build_saa_ret()            → daily-rebal binary SAA → xts_ret
                                 rank_enhancers()           → top N by full_ir vs SAA
                                 rank_stabilizers()         → top N by dd_reduction vs SAA
                                 fingerprint_candidates()   → screen_ticker() per candidate
                                 overlay_summary_heatmap()  → two-panel ranking heatmap
                                 backtest_ls_overlay()      → P1 wealth + P2 spread + P3 stats
                                 run_saa_overlay_workflow() → master orchestrator (all steps)
                                 Overlay: Option B — r_SAA + α*(r_candidate − r_SAA)
                                 Backtest: Long overlay-SAA / Short original-SAA (spread PnL)
  saa_overlay_workflow_guide.Rmd ← pedagogical guide for saa_overlay_workflow.R:
                                 design choices table, all function signatures,
                                 live SPY/IEF 60/40 example, enhancer + stabilizer
                                 backtest interpretation, advanced usage patterns

scripts_saa_taa/
  saa_tree.R                  ← SAA_PORTFOLIO tribble, plot_saa_ggbar(),
                                 plot_saa_sunburst(), plot_saa_treemap(),
                                 calc_ytd(), enrich_ytd()
  05_saa_portfolio.R          ← calc_saa_portfolio(), build_ls_portfolio()

scripts_saa_structured/
  long_short_overlay.R        ← build_ls_portfolio(), L/S overlay engine

scripts_exante_hypothesis/
  exante_engine.R             ← build_exante_metrics(), enrich_with_metadata()
  exante_visuals.R            ← plot_exante_quadrant(), plot_corr_dd()

scripts_safe_haven_regime/
  method1_rolling_corr.R      ← rolling correlation breakdown signal
  method2_logistic_regime.R   ← LASSO logistic (AUC ~0.72 at 63-day horizon)
  method3_regime_covariance.R ← PCA eigenanalysis on Fall-minus-Consolidation covariance

scripts_market_state/
  ms_vector_state.R           ← L1: build_market_state_history(), current_market_state(),
                                 print_market_state()
                                 5 signals: URTH regime | SPY regime | US premium (60d) |
                                 EM appetite (60d) | sector breadth (% above 200DMA)
                                 5 states: Expansion(65%) / US-Led(58%) / Late-Cycle(50%) /
                                           Deterioration(42%) / Contraction(35%)
                                 min_persist param: N days new state must hold before confirmed
  ms_sector_pattern.R         ← L2: build_sector_pattern_history(), current_sector_pattern()
                                 Cyclicals (XLK/XLY/XLB/XLE/XLI) vs Defensives (XLU/XLP/XLV)
                                 63d momentum spread → Strong Risk-On/Risk-On/Neutral/Risk-Off/Strong Risk-Off
                                 Adjustment: ±5pp (strong) / ±2pp (weak) / 0pp (neutral)
  ms_risk_allocation.R        ← L3: build_risk_allocation_history(), print_risk_allocation()
                                 Combines L1 base weight + L2 adjustment → final EQ/FI
                                 Clipped to [25%, 75%]. Passes min_persist through to L1.
  ms_visuals.R                ← plot_ms_timeline(), plot_ms_signal_strip(),
                                 plot_ms_allocation(), plot_ms_dashboard()
                                 Dashboard saved to key_plots/ms_dashboard.png

Rmd/
  executive_summary.Rmd       ← THE master summary — 8 sections, sources all scripts
  market_state_report.Rmd     ← documentation + analysis for 3-layer market state framework
                                 params: min_persist(5), t_fall(0.10), roll_win(60),
                                         ma_win(200), mom_win(63)
  spy_regime_statemachine_report.Rmd
  safe_haven_regime_report.Rmd
  long_short_overlay_report.Rmd
  exante_hypothesis_report.Rmd  ← parameterised: render via render_exante_report()
  pair_analysis_portfolio_construction.Rmd
  universe_pair_analysis_v2.Rmd
  eq_factsheets_batch.Rmd     ← batch factsheet for all equity ETFs
                                 params: bmk, t_fall, tickers, show_full_factsheet
  rebalancing_drift_guide.Rmd ← pedagogical guide: volatility harvesting theory + math,
                                 drift mechanics, 5-strategy pairwise comparisons,
                                 optimal frequency discussion; sources plot_rebal_drift.R
```

---

## SAA Portfolio definition
```r
SAA_PORTFOLIO <- tribble(
  ~ticker, ~weight, ~asset_class,  ~role,
  "AGG",   0.35,    "Fixed Income", "Core-Stabilizer",
  "URTH",  0.25,    "Equity",       "Anchor",
  "SPY",   0.25,    "Equity",       "Anchor",
  "QQQ",   0.05,    "Equity",       "Core-Growth",
  "XLK",   0.05,    "Equity",       "Core-Growth",
  "SMH",   0.05,    "Equity",       "Satellite"
)
# Equity 65% / Fixed Income 35%
```

---

## Outperformance type taxonomy
Used in `plot_regime_rel_heatmap()` via `.outperf_type()`:

| Type   | Condition                  | Meaning                        |
|--------|----------------------------|--------------------------------|
| HEDGE  | α>0, abs>0, spy<0          | Ticker rose while SPY fell     |
| STABLE | α>0, abs<0, spy<0          | Both fell — ticker fell less   |
| ALPHA  | α>0, spy>0                 | Both rose — ticker rose more   |
| LAG    | α<0, spy>0                 | Underperformed rising SPY      |
| LOSS   | α<0, spy<0                 | Both fell — ticker fell worse  |

---

## Critical analytical findings (do not contradict without strong reason)

**1. Anti-diversification (core finding — confirmed across Sections 5, 6, 7)**
For a long-only blended portfolio, MaxDD is NOT minimised by minimising correlation.
It is minimised by minimising the ticker's own standalone drawdown.
A ticker with ρ=0.8 and shallow 15% DD beats a ticker with ρ=0.1 and 40% DD.

**2. Safe havens LAG (relative) in Recovery — they do NOT lose (absolute)**
GLD and IEF record negative alpha in Recovery (SPY rebounds faster) but their
absolute returns often stay positive. "Give back" framing is wrong — it is an
opportunity cost, not an actual loss.

**3. 3-state regime transition matrix is trivially 100% by construction**
Fall → Recovery → Consolidation is deterministic. Never show a transition matrix
for this regime system — it adds no information.

**4. XLK is a pure growth satellite, not a hedge**
ρ ≈ 0.95 vs SPY. ALPHA in Consolidation/Recovery, LOSS in Fall. Appropriate
at 5% SAA weight only.

**5. TAA top calls (institutional sheet)**
- Sell: US Govt bonds (completely eliminated), US equity (−11pp GRO)
- Buy: Gold (doubled 3.5→7%), Value equity (+9.7pp), EUR/CHF vs USD
- FX: USD derisking −10.6pp (INC) to −15.9pp (GRO)

**6. Regime-rotation TAA does NOT beat passive 60/40 (confirmed)**
The walk-forward DAA produced ~6.7% vs passive ~7.6% over the full history.
Root causes: (1) SAA universe drag from international diversification during US-dominated decade,
(2) regime detection lag — Fall fires only after 10% drawdown already absorbed,
(3) tilts too small to overcome base drag.
Do NOT attempt to improve return via regime rotation. The right objective is DD reduction.

**7. Rebalancing drift and volatility harvesting (confirmed)**
Daily rebalancing beats Buy & Hold in total wealth for SPY/IEF 60/40 despite SPY's clear long-run outperformance.
Mechanism: rebalancing bonus ≈ ½ × w_SPY × w_IEF × σ²(SPY − IEF) — always positive, always a free lunch from variance.
Drift (BaH − daily rebal) is negative in aggregate because BaH enters every Fall maximally overweight (~80% SPY) while daily rebal always enters at 60% — Fall-regime spikes dominate the cumulative total.
Optimal real-world frequency: monthly/quarterly calendar or ±5% threshold (captures ~85% of daily bonus at 1/20th of the trading activity).
Threshold rebalancing dominates calendar: it fires when the bonus is largest (volatile periods = wide spread variance) and is implicitly regime-aware.
Source: `scripts_daa_saa_taa/plot_rebal_drift.R` + `Rmd/rebalancing_drift_guide.Rmd`.

**8. DD Optimizer objective (confirmed)**
The project's primary portfolio-construction question is:
"Given we accept SPY/IEF 60/40 return, what is the minimum-cost overlay
that reduces MaxDD from ~23% to ~15%?"
Answer comes from `07_dd_overlay_optimizer.R` — Pareto frontier of all single/pair/triple
combinations screened from `05_dd_stabilizer.R`. Never go back to regime-rotation framing.

---

## DD Overlay Optimizer — design (07_dd_overlay_optimizer.R)

**Grid search scope:**
- Candidates: top 8 tickers by insurance_ratio from stabilizer_screen (dd_reduction > 0)
- Singles: 5%, 10%, 15%, 20%
- Pairs: 5%–15% each, total ≤ 25%
- Triples (top 5 candidates): 5%–10% each, total ≤ 25%
- Each combo: `(1 - sum(w)) × passive_60_40 + w_1 × tk_1 + ... + w_n × tk_n`

**Outputs:**
- `overlay_grid` — all combinations with MaxDD + AnnRet
- `pareto_frontier` — Pareto-efficient combos (min return_drag at each MaxDD bucket)
- `target_solutions` — cheapest combo per target: ≤15%, ≤17%, ≤19%, ≤21%

**Key constants (optimizer):**
- `MAX_OVERLAY = 0.25` — total overlay cap (25% of portfolio)
- `DD_TARGETS = c(0.15, 0.17, 0.19, 0.21)` — MaxDD targets in absolute terms

---

## Chart templates — reusable across projects

Three canonical chart formats live in `key_plots/`. Invoke by sourcing the file.

| Template name | File | Function | Use case |
|---|---|---|---|
| `stacked_weight_timeline` | `key_plots/chart_stacked_weight_timeline.R` | `plot_stacked_weight_timeline(weight_tbl, saa_tbl, rt)` | Portfolio weight composition over time — stacked area, regime shading, ticker order by SAA bucket |
| `ma200_signal_panel` | `key_plots/chart_ma200_signal_panel.R` | `plot_ma200_signal_panel(trend_signals, saa_tbl)` → `$eq` / `$fi` | Per-ticker price + 200DMA grid — red shading when below MA, rebased to 100, patchwork 2-col layout |
| `multi_wealth_endlabel` | `key_plots/chart_multi_wealth_endlabel.R` | `plot_multi_wealth_endlabel(ret_list, rt, colours, labels)` | 2–5 portfolio wealth comparison — direct end-of-line labels with ann return, regime shading, no legend |
| `dd_frontier` | `key_plots/chart_dd_frontier.R` | `plot_dd_frontier(dd_frontier_data, weight_marks, title, subtitle)` | Per-instrument DD vs Return path as overlay weight sweeps 0→25% — arrowed paths, passive diamond anchor, weight markers at 10%/20% |
| `pe_bands` | `key_plots/chart_pe_bands.R` | `plot_pe_bands(n_years, band_pes, dark, force_refresh)` | Bloomberg-style S&P 500 P/E price bands — 5 coloured step-function bands (Avg±1sd/±2sd) vs actual price; Shiller data auto-downloaded and cached |

**Defaulting convention — apply to all chart templates**
`colours` and `labels` always default from the data so a minimal two-argument call works:
```r
plot_multi_wealth_endlabel(ret_list = list(SPY = xts_ret[,"SPY"], GLD = xts_ret[,"GLD"]), rt = rt)
```
- `labels  = NULL` → filled from `names(ret_list)` (or equivalent data names)
- `colours = NULL` → filled from fixed canonical palette first (purple/blue/grey/amber/green/red), then `hue_pal(n)` for >6 series
- Callers override only what they need; the function never errors on missing aesthetics.
- Apply this pattern to every new chart template added to `key_plots/`.

**Design rules for all three templates**
- Regime shading: Fall = `#fca5a5` α=0.18 | Recovery = `#bbf7d0` α=0.18
- `multi_wealth_endlabel`: purple `#7c3aed` = active strategy | blue `#3b82f6` = SAA | grey `#9ca3af` = passive
- `ma200_signal_panel`: dark blue `#1d3461` = price | amber `#f59e0b` dashed = 200DMA | red `#ef4444` = trend OFF
- `stacked_weight_timeline`: white separator lines between bands (linewidth=0.10), legend right side

---

## Plotting rules

**Sunburst + Treemap are always a pair**
`plot_saa_sunburst()` and `plot_saa_treemap()` must always be updated, shown, and
discussed together. Changing one means changing both.

**plotly is HTML-only**
In Rmd: use `eval=knitr::is_html_output()` for sunburst/treemap chunks.
In .R scripts: guard with `if (!isTRUE(getOption("knitr.in.progress")) || ...)`
Never call `library(plotly)` unconditionally in a sourced script.

**PDF rendering**
Use `latex_engine: xelatex` in YAML for Unicode support.
Static ggplot functions are the PDF fallback for every interactive plotly chart.

**Heatmap colour convention**
- Text: dark blue `"#1B3A6B"` for positive, dark red `"#7B1010"` for negative
- Fill: light palette so dark text is always readable
- SPY (master row): show abs return at full alpha-layer font size; empty abs layer

---

## Auto-run guards
Every script that produces plots must wrap its run-on-source block:
```r
if (!isTRUE(getOption("knitr.in.progress"))) {
  # plots / print() calls here
}
```
This prevents double-execution when sourced inside an Rmd render.

---

## key_findings/ — canonical empirical discoveries

Self-contained scripts, each loads its own data, produces labelled plots, prints summary table.

| File | Finding | Key result |
|---|---|---|
| `kf1_anti_diversification.R` | Low-ρ tickers have WORSE empirical MaxDD than Magdon-Ismail predicts | Confirmed across full 96-ticker universe; high-ρ tickers track theory |
| `kf2_ls_pair_selection.R` | L/S pair to replace SPY requires ρ > 0.75 | Only SMH (IR=0.90), XLK (IR=0.51), QQQ (IR=0.48) pass — all Equity |

KF2 screening parameters: `RHO_SCREEN = 0.75`, `MDD_SCREEN = -0.30`
KF2 plot C: IR (x) vs ΔMaxDD (y), circle size = P(spread > 0 over 63-day window)

---

## Key analytical functions — quick reference

### Session startup (fresh R session)
```r
source("00_libraries.R")                                        # all packages
source(here("scripts/00_init_universe.R"))                      # etf_metadata
source(here("scripts/01_etf_wrangle.R"))                        # xts_ret, xts_ret_winsor, rt (auto-audit runs)
source(here("scripts_spy_dd_regime/spy_dd_regime.R"))           # build_regime_table() lives here
rt <- build_regime_table(xts_ret[, "SPY"])                      # MUST assign explicitly — stats::rt shadows it
```

### Table-returning functions (atomic building blocks)
| Function | Signature | Returns |
|---|---|---|
| `build_regime_table()` | `(xts_ret_col)` | Episode table: regime / xmin / xmax / days / return per Fall·Recovery·Consolidation |
| `label_daily_regime()` | `(xts_ret_col)` | Date → regime label mapping (one row per day) |

### Factsheet
```r
source(here("utility/factsheet_functions.R"))
fs_render("GLD", bmk = "SPY", ret_xts = xts_ret, rt = rt)
# 7 panels: P1 wealth | P2 L/S spread | P3 DD | P4 dist | P5 rolling ρ | P6 regime ρ | P7 rolling IR
# rt must be a data.frame — if accidentally passes stats::rt, fs_render auto-rebuilds it
```

### Bundle runners (call multiple build/plot functions, return list)
| Function | File | What it runs |
|---|---|---|
| `run_regime_analysis()` | `spy_dd_regime.R` | Full SPY regime analysis |
| `run_regime_rel_analysis()` | `spy_dd_regime_rel.R` | Relative regime (ticker vs SPY) |
| `run_multi_ticker_regime()` | `regime_multi_ticker.R` | Multi-ticker regime heatmap + summary |
| `run_sm_diagram_rel()` | `spy_statemachine_diagram_rel.R` | State machine diagram bundle |

### Market state framework (3-layer)
```r
source(here("scripts_market_state/ms_vector_state.R"))     # L1
source(here("scripts_market_state/ms_sector_pattern.R"))   # L2
source(here("scripts_market_state/ms_risk_allocation.R"))  # L3
source(here("scripts_market_state/ms_visuals.R"))          # plots

ms_hist <- build_market_state_history(xts_ret, min_persist = 5L)
sp_hist <- build_sector_pattern_history(xts_ret)
ra_hist <- build_risk_allocation_history(xts_ret, min_persist = 5L)

print_market_state(ms_hist)       # console summary with current state
print_risk_allocation(ra_hist)    # current EQ/FI allocation
plot_ms_dashboard(xts_ret)        # 3-panel patchwork dashboard
```

### Rendering parameterised Rmds
Always use `envir = new.env()` to avoid `params already exists` errors:
```r
rmarkdown::render(
  here::here("Rmd/market_state_report.Rmd"),
  output_file = here::here("03_reports/market_state_report.html"),
  params = list(min_persist = 5, t_fall = 0.10),
  envir  = new.env()
)
# output_file must be absolute path (here::here()) when pointing outside Rmd directory
# Never use relative paths for output_file in parameterised renders
```

---

## Tickers requiring new download (not yet in Sovereign Universe)
| Ticker | Name | Reason needed |
|---|---|---|
| ILF  | iShares Latin America 40 | Institutional SAA line item |
| SH   | ProShares Short S&P 500 | S&P 500 Hedge line item |
| DBMF | iM DBi Managed Futures | AI Hedge Funds proxy |
