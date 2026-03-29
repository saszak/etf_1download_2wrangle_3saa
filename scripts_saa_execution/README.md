# SAA Execution Subproject

## In one sentence
A 9-script pipeline that reads the parent project's processed data products,
builds a per-ticker quant database, identifies Spring-Load / Exhaustion signals,
constructs a 3-book SAA + TAA overlay, and emits a dollar-denominated execution
CSV — all without touching the T7 SSD or any external volume.

---

## Where this lives
```
etf_1download_2wrangle_3saa/
└── scripts_saa_execution/   ← YOU ARE HERE
    ├── README.md
    ├── saa_bridge_config.R
    ├── saa_quantdb_builder.R
    ├── saa_alpha_intelligence.R
    ├── saa_policy_weights.R
    ├── saa_drift_analysis.R
    ├── saa_master_allocator.R
    ├── saa_visual_dashboard.R
    ├── saa_perf_attribution.R
    └── saa_quantdb_selector.R

utility/
└── plot_sovereign_tapes.R   ← regime tape functions (shared with parent)

Rmd/
└── saa_execution_report.Rmd ← master deliverable report

02_data_processed/           ← inputs (parent) + outputs (this subproject)
03_reports/                  ← CSVs + PNGs emitted by this pipeline
```

---

## Pipeline map

```
[Parent project outputs]
  xts_ret_returns.rds   daily log returns  (96 tickers)
  xts_rel.rds           daily alpha vs SPY (96 tickers)
  ma_technical_anchors.rds  MA200, dist_200, trend_regime
          │
          ▼
  saa_bridge_config.R  ──────────────────────────────────── STAGE 0
    · Loads parent rds products
    · Builds weekly / monthly / quarterly cubes (apply.weekly etc.)
    · Defines 34-ticker saa_metadata with cluster column
    · Creates bucket vectors: defensive / growth / cycle / intl / anchors
    · Exports: xts_d/w/m/q_abs, xts_d/w/m/q_rel, saa_metadata, tickers
          │
          ▼
  saa_quantdb_builder.R ─────────────────────────────────── STAGE 1
    · L1 abs  : ann_ret, ann_vol, max_dd, kurtosis, skew,
                count_dd_10, max_recovery_d
    · L2 rel  : alpha_6y (cumulative), ir_ratio, beta_spy,
                corr_to_bmk, vol_scalar (risk-parity scalar)
    · L4 mom  : mom_d (daily z), mom_w (weekly EMA4),
                mom_m (monthly EMA3), is_confirmed, mom_stretch
    · Saves  : 02_data_processed/saa_quantdb.rds
          │
          ▼
  saa_alpha_intelligence.R ──────────────────────────────── STAGE 2
    · alpha_velocity_z  : 20d rolling mean / 252d rolling sd of xts_rel
    · range_percentile  : (price − 52w_low) / (52w_high − 52w_low)
                          derived from cumulative wealth of xts_ret
    · dist_200          : from ma_technical_anchors.rds
    · Spring-Load       : range_pct < 0.30  AND  dist_200 > 0
    · Exhaustion        : range_pct > 0.70  AND  dist_200 < 0
    · Quadrant          : Spring-Load + z > 0  → SPRING_LOAD (LONG)
                          Exhaustion  + z < 0  → EXHAUSTION  (SHORT)
    · Saves  : 02_data_processed/saa_selection_matrix.rds
          │
          ▼
  saa_policy_weights.R ──────────────────────────────────── STAGE 3
    · BOOK 1 SAA   : 60% SPY  +  40% AGG  (static anchor)
    · BOOK 2 TAA   : top-3 Spring-Loads  @ +5% each
                     category benchmark  @ −5% each (SPY/AGG)
    · BOOK 3 TOTAL : net sum of Book 1 + Book 2
    · Saves  : 02_data_processed/saa_policy_list.rds
          │
          ▼
  saa_drift_analysis.R ──────────────────────────────────── STAGE 4
    · Joins Book 3 weights with cluster metadata
    · Cluster crowding : net exposure summed by cluster
    · Category split   : Equity / Fixed / Alt / Multi
    · Saves  : 02_data_processed/saa_drift_report.rds
          │
          ▼
  saa_master_allocator.R ────────────────────────────────── STAGE 5
    · Converts net weights → dollar amounts (reference AUM = $1M)
    · Action flags : BUY/HOLD  |  SHORT/SELL  |  FLAT
    · Saves  : 03_reports/saa_trade_list_<date>.csv
               03_reports/saa_trade_list_latest.csv

[Analytics branches — can run in any order after Stage 1]

  saa_visual_dashboard.R ────────────────────────────────── VIZ A
    · dna_matrix        : rescaled 5-axis scores (Alpha/Return/Momentum/
                          RiskParity/Stability)
    · p_book3_weights   : ggplot bar chart of Book 3 net weights
    · plot_etf_spider()            single radar panel
    · plot_etf_spider_grid()       grid of ticker-vs-SPY radars
    · plot_etf_spider_vertical()   stacked vertical radars
    · rank_spider_dna()            weighted DNA score ranking
    · Saves  : 03_reports/saa_portfolio_snapshot.png

  saa_perf_attribution.R ────────────────────────────────── VIZ B
    · basket_alpha      : equal-weighted mean of Book 2 LS relative returns
    · cum_alpha         : cumulative basket alpha series
    · taa_stats         : Ann.Alpha / Alpha Vol / Sharpe / MaxDD
    · p_taa_cum_alpha   : ggplot cumulative alpha chart
    · Saves  : 02_data_processed/saa_taa_stats.rds
               03_reports/saa_taa_cum_return.png

  saa_quantdb_selector.R ────────────────────────────────── VIZ C
    · enhancers         : top-3 by (alpha_6y×0.7 + mom_d×0.3),
                          is_confirmed == YES, alpha > 0
    · dampeners         : top-2 by stability score (−|kurt|−|skew|),
                          corr_to_bmk < 0.50
    · sovereign_list    : the combined Sovereign 5 roster
    · Saves  : 02_data_processed/saa_sovereign_list.rds

  utility/plot_sovereign_tapes.R ────────────────────────── VIZ D
    · plot_sovereign_stress_bars()
    · plot_sovereign_heatmap_continuous()
    · plot_sovereign_full_spectrum_V2()
    · plot_sovereign_multi_tape_v2()
    · plot_sovereign_alpha_tape_v12()   ← primary: SPY abs + others alpha
```

---

## Execution universe (34 tickers)

| Cluster | Tickers |
|---|---|
| Broad Market | SPY |
| Factor Play | QUAL VTV MTUM USMV |
| Growth Laggards | XLK |
| Defensive Bulwark | XLV SHY IEF TLT TIP AOK GLD SLV |
| Cycle & Breadth | XLF XLE XLI XLP XLY LQD HYG AOM AOR IYR |
| Pipes & Power | XLU IGF |
| Multipolar Alpha | EWY EWJ EMB EMLC |
| Cash/Ultra-Short | SGOV |
| Broad Bonds | AGG BND |

Tickers absent from the parent's `xts_ret` are silently dropped at bridge load.

---

## Key objects (global env after full run)

| Object | Type | Description |
|---|---|---|
| `saa_metadata` | tibble | 34-ticker universe + cluster + parent metadata |
| `xts_d/w/m/q_abs` | xts | Multi-frequency absolute returns |
| `xts_d/w/m/q_rel` | xts | Multi-frequency relative returns vs SPY |
| `etf_quantdb` | tibble | 8-layer quant database, one row per ticker |
| `selection_matrix` | tibble | Spring-Load / Exhaustion / velocity z-score |
| `policy_list` | list | `$SAA` `$TAA` `$TOTAL` — the 3 books |
| `drift_report` | tibble | Book 3 weights + cluster + dollar exposure |
| `execution_list` | tibble | Trade list with BUY/HOLD / SHORT/SELL flags |
| `dna_matrix` | tibble | Normalised 5-axis DNA scores |
| `sovereign_list` | tibble | Sovereign 5 roster (3 Enhancers + 2 Dampeners) |
| `taa_stats` | tibble | TAA basket risk/return metrics |

---

## Data products saved

| File | Script | Description |
|---|---|---|
| `02_data_processed/saa_quantdb.rds` | quantdb_builder | 8-layer quant DB |
| `02_data_processed/saa_selection_matrix.rds` | alpha_intelligence | Spring-Load signals |
| `02_data_processed/saa_policy_list.rds` | policy_weights | 3-book weights |
| `02_data_processed/saa_drift_report.rds` | drift_analysis | Cluster crowding |
| `02_data_processed/saa_taa_stats.rds` | perf_attribution | TAA metrics |
| `02_data_processed/saa_sovereign_list.rds` | quantdb_selector | Sovereign 5 |
| `03_reports/saa_trade_list_<date>.csv` | master_allocator | Dated execution CSV |
| `03_reports/saa_trade_list_latest.csv` | master_allocator | Current execution CSV |
| `03_reports/saa_portfolio_snapshot.png` | visual_dashboard | Book 3 bar chart |
| `03_reports/saa_taa_cum_return.png` | perf_attribution | Cumulative alpha chart |

---

## Key constants

| Constant | Value | Location | Purpose |
|---|---|---|---|
| `saa_ratio` | 0.60 | bridge_config | Book 1 equity weight |
| `taa_ratio` | 0.30 | bridge_config | TAA budget |
| `taa_slot_size` | 0.05 | policy_weights | Per LS pair notional |
| `n_enhancers` | 3 | quantdb_selector | Enhancer roster size |
| `n_dampeners` | 2 | quantdb_selector | Dampener roster size |
| `corr_threshold` | 0.50 | quantdb_selector | Max corr for dampener eligibility |
| `total_aum` | 1 000 000 | policy_weights / master_allocator | Reference portfolio size |
| Spring-Load threshold | range_pct < 0.30 | alpha_intelligence | Oversold boundary |
| Exhaustion threshold | range_pct > 0.70 | alpha_intelligence | Overbought boundary |
| Alpha velocity window | 20d / 252d | alpha_intelligence | Momentum / vol windows |
| DD threshold | 0.10 (10%) | tape functions | SPY stress event trigger |

---

## How to run

### Full pipeline (fresh run)
```r
library(here)
source(here("scripts_saa_execution/saa_bridge_config.R"))
source(here("scripts_saa_execution/saa_quantdb_builder.R"))
source(here("scripts_saa_execution/saa_alpha_intelligence.R"))
source(here("scripts_saa_execution/saa_policy_weights.R"))
source(here("scripts_saa_execution/saa_drift_analysis.R"))
source(here("scripts_saa_execution/saa_master_allocator.R"))
# Analytics branches (order-independent after Stage 1)
source(here("scripts_saa_execution/saa_visual_dashboard.R"))
source(here("scripts_saa_execution/saa_perf_attribution.R"))
source(here("scripts_saa_execution/saa_quantdb_selector.R"))
source(here("utility/plot_sovereign_tapes.R"))
```

### Just the execution CSV (stages 0–5 only)
```r
for (s in c("saa_bridge_config", "saa_quantdb_builder",
            "saa_alpha_intelligence", "saa_policy_weights",
            "saa_drift_analysis", "saa_master_allocator")) {
  source(here(paste0("scripts_saa_execution/", s, ".R")))
}
```

### Render the deliverable report
```r
rmarkdown::render(here("Rmd/saa_execution_report.Rmd"),
                  output_dir = here("03_reports"))
```

---

## Relationship to parent project

This subproject is **Phase 3** of the parent pipeline:

| Phase | Folder | Produces |
|---|---|---|
| 1 — Download & Wrangle | `scripts/` | `xts_ret`, `xts_rel`, `etf_metadata` |
| 2 — Regime & Alpha | `scripts_spy_dd_regime/`, `scripts_state_machine/` | `rt`, `sm_signal_registry` |
| **3 — SAA Execution** | **`scripts_saa_execution/`** | **`etf_quantdb`, `policy_list`, trade CSV** |

The subproject **reads but never writes** parent data products in
`02_data_processed/`.  All its own outputs use the `saa_` prefix to avoid
collisions.
