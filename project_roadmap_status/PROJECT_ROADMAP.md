# PROJECT ROADMAP
## ETF Intelligence Project — Milestone Tracker & Decision Log
*Last updated: 2026-04-12 · Branch: `main`*

---

## Project in one sentence
A modular R research project that classifies ETF behaviour relative to SPY across
three data-driven market regimes (Fall / Recovery / Consolidation) and uses those
findings to build a concrete SAA portfolio with institutional mapping.

---

## Status legend
| Symbol | Meaning |
|--------|---------|
| ✅ | Complete — committed, tested, documented |
| 🔄 | In progress — exists but uncommitted or partially wired |
| 📋 | Planned — scoped, not yet started |
| ❌ | Blocked / deprioritised |

---

## Phase 1 — Data Infrastructure ✅

| Item | File | Status |
|------|------|--------|
| 96-ticker Sovereign Universe | `scripts/00_init_universe.R` | ✅ |
| Download + winsorise → `xts_ret` | `scripts/01_etf_wrangle.R` | ✅ |
| Technical signals (200DMA, momentum) | `scripts/01b_technical_signals.R` | ✅ |
| Global params + project tree | `00_global_params.R`, `project_tree.R` | ✅ |
| Derived Universe registry (constructed portfolios) | `scripts/00_derived_universe.R` | 🔄 uncommitted |

**Decision:** Daily log returns stored as `xts_ret`; winsorized copy (`xts_ret_winsor`)
used only for sigma/regime/z-score — never for cumulative return plots.

---

## Phase 2 — Regime Engine ✅

| Item | File | Status |
|------|------|--------|
| 3-state SPY drawdown regime | `scripts_spy_dd_regime/spy_dd_regime.R` | ✅ |
| Relative regime heatmap + HEDGE/STABLE/ALPHA/LAG/LOSS | `spy_dd_regime_rel.R` | ✅ |
| Multi-ticker overlays | `regime_multi_ticker.R` | ✅ |
| 200DMA hysteresis state machine | `scripts_state_machine/sm_engine.R` | ✅ |
| State machine visuals + diagram | `sm_visuals.R`, `spy_statemachine_diagram_rel.R` | ✅ |
| Batch enrichment registry | `batch_enrich_registry.R` | ✅ |

**Decision:** `T_FALL = 0.10` — regime engine fires only after 10% drawdown absorbed.
Intentional: detects confirmed bear, not false alarms. This lag is a known limitation (see KF finding #6).

---

## Phase 3 — SAA Portfolio Construction ✅

| Item | File | Status |
|------|------|--------|
| SAA definition (AGG 35 / URTH 25 / SPY 25 / QQQ 5 / XLK 5 / SMH 5) | `scripts_saa_taa/saa_tree.R` | ✅ |
| SAA sunburst + treemap visualisation | `plot_saa_sunburst()`, `plot_saa_treemap()` | ✅ |
| Walk-forward DAA backtest vs passive | `scripts_daa_saa_taa/04_daa_backtest.R` | ✅ |
| Rebalancing drift study (5 strategies) | `scripts_daa_saa_taa/plot_rebal_drift.R` | ✅ |
| Rebalancing drift guide (Rmd) | `Rmd/rebalancing_drift_guide.Rmd` | ✅ |

**Decision:** Regime-rotation TAA does NOT beat passive 60/40 (~6.7% vs ~7.6%). Project
objective reoriented from return enhancement to **MaxDD reduction** (see KF finding #8).

---

## Phase 4 — DD Overlay Optimizer ✅

| Item | File | Status |
|------|------|--------|
| Insurance quadrant + DD frontier | `scripts_daa_saa_taa/05_dd_stabilizer.R` | ✅ |
| Trend quality scoring (Hurst + hit rate) | `06_trend_quality.R` | ✅ |
| Grid search: singles / pairs / triples | `07_dd_overlay_optimizer.R` | ✅ |
| Pareto frontier + 4 MaxDD targets (15/17/19/21%) | `07_dd_overlay_optimizer.R` | ✅ |
| 3-stage enhancer screen | `08_enhancer_screen.R` | ✅ |
| SAA overlay workflow end-to-end | `saa_overlay_workflow.R` | ✅ |
| SAA overlay workflow guide (Rmd) | `saa_overlay_workflow_guide.Rmd` | ✅ |
| L/S overlay backtest (Phase 2 report) | `Rmd/long_short_overlay_report.Rmd` | ✅ |

**Decision:** Primary objective = minimum-cost overlay to reduce MaxDD from ~23% to ≤15%.
Answer: Pareto-efficient triple combo from top 8 insurance_ratio candidates, total overlay ≤ 25%.

---

## Phase 5 — Market State Framework ✅

| Item | File | Status |
|------|------|--------|
| L1 vector state (5 signals → 5 states) | `scripts_market_state/ms_vector_state.R` | ✅ |
| L2 sector rotation (cyclical vs defensive) | `ms_sector_pattern.R` | ✅ |
| L3 EQ/FI risk allocation (clipped 25–75%) | `ms_risk_allocation.R` | ✅ |
| Visuals: timeline, strip, allocation, dashboard | `ms_visuals.R` | ✅ |
| Market state report (parameterised Rmd) | `Rmd/market_state_report.Rmd` | ✅ |

**Current state (as of 2026-04-12):** Late-Cycle / Risk-On → ~52% EQ allocation.

---

## Phase 6 — Key Findings (Canonical Scripts) ✅

| # | Finding | File | Status |
|---|---------|------|--------|
| KF1 | Anti-diversification: low-ρ → worse MaxDD than theory | `key_findings/kf1_anti_diversification.R` | ✅ |
| KF2 | L/S pair requires ρ > 0.75 (only SMH/XLK/QQQ qualify) | `key_findings/kf2_ls_pair_selection.R` | ✅ |
| KF3 | Trend reversals cluster at regime transitions | `key_findings/kf3_trend_reversal.R` | ✅ |
| KF4 | Phase 2 overlay framework (FULL/PARTIAL/CONDITIONAL + A/B/C) | `key_findings/kf4_*.R` | ✅ |

---

## Phase 7 — ETF_EQ_Dashboard (Shiny App) 🔄

**App location:** `etf_eq_shiny_dashboard/`
**Rename:** `shiny_dashboard/` → `etf_eq_shiny_dashboard/` (in progress, uncommitted)

| Tab | Module | Status |
|-----|--------|--------|
| Table | `mod_perf_table.R` | ✅ |
| Treemap | `mod_treemap.R` | 🔄 modified, uncommitted |
| Plots | `mod_plots.R` | ✅ |
| Comp | `mod_comp.R` | ✅ |
| Technical | `mod_technical.R` | ✅ |
| Regime | `mod_regime.R` | 🔄 modified, uncommitted |
| RiskRet | `mod_riskret.R` | ✅ |
| Patterns | `mod_patterns.R` | ✅ |
| AbsRel | `mod_absrel.R` | ✅ |
| CalYear | `mod_calyear.R` | ✅ |
| Surprise | `mod_surprise.R` | ✅ |
| **Archetypes** | `mod_archetypes.R` | 🔄 built, not yet wired into app.R |
| **Sectors** | — | 📋 planned |
| **Macro** | — | 📋 planned |

---

## Phase 8 — Derived Universe Registry 🔄

**File:** `scripts/00_derived_universe.R` (new, uncommitted)

Introduces a `dU` registry for constructed portfolios (EUR 60/40, BAL SAA, etc.)
that are computed from `iU` ticker components — not downloadable directly.

| Item | Status |
|------|--------|
| Registry schema (id / name / components / weights / rebal / currency / version) | 🔄 |
| `current_derived()` helper | 🔄 |
| `build_derived_returns()` integration | 📋 |
| EUR-hedged portfolio entries (IWDE/IUES/IHYE/IGLD) | 📋 |

---

## Pending / Backlog

### Tickers needing download (not yet in Sovereign Universe)
| Ticker | Name | Reason |
|--------|------|--------|
| ILF | iShares Latin America 40 | Institutional SAA line item |
| SH | ProShares Short S&P 500 | S&P 500 Hedge line item |
| DBMF | iM DBi Managed Futures | AI Hedge Funds proxy |

### Dashboard planned integrations
- **Regime Archetypes bar** → wire `mod_archetypes.R` into `app.R` Regime tab
- **Sectors tab** — sector bars (2025 vs YTD) + momentum bubble
- **Macro tab** — SPX YoY vs EPS Growth scatter (live Yardeni data)

### Infrastructure
- Regime plot kernel refactor: shared kernel + decorator pattern for `spy_dd_regime` plot functions
- FMP analyst targets: live 12M price targets via FMP free API (requires sign-up)
- MA spread: add MA50/MA200 spread or z-score annotation to `plot_ma_gauge()`

---

## Risk/Return Calculation Policy (MANDATORY)

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

> Hand-rolled `sd * sqrt(252)` or `prod(1+r)^(252/n) - 1` inflates numbers when the
> window is < 1 year. PerformanceAnalytics handles edge cases and annualisation correctly.

---

## Key Decisions Log

| Date | Decision | Rationale |
|------|----------|-----------|
| — | `MASTER = "SPY"` as universal benchmark | Liquid, total-return, US equity anchor |
| — | `T_FALL = 0.10` (10%) for regime detection | Balances sensitivity vs false positives |
| — | Winsorized returns for risk metrics only | Distorts cumulative returns if used for wealth |
| — | `xts_ret_winsor` never used for wealth curves | Confirmed across all reporting functions |
| — | Regime-rotation TAA abandoned as return driver | Walk-forward DAA ~6.7% vs passive ~7.6% |
| — | MaxDD reduction as primary objective | DD Optimizer target: 23% → ≤15% at ≤25% overlay |
| — | Threshold ±5% rebalancing as practical optimum | Captures ~85% of daily bonus at 1/20th trades |
| — | `min_persist = 5` days for market state confirmation | Prevents noise-driven state flips |
| — | `REBAL_FREQ = "Quarterly"` as project default | Set in `00_global_params.R`; override per portfolio |
| 2026-04-12 | App renamed: `shiny_dashboard/` → `etf_eq_shiny_dashboard/` | Reflects actual scope (equity ETF screen) |

---

*ETF Intelligence Project · Sovereign Universe 96 tickers · Benchmark: SPY*
