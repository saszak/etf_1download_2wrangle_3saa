# TAA Ex-Ante Decision Framework
# Status: Draft v2 — 2026-03-22
# Purpose: Structured repeating process for tactical tilts around SAA (BAL USD)
# Updated as framework evolves — this is a living document

---

## 1. Guiding Principle

TAA is a series of **repeating structured questions** applied identically to every
instrument. The goal is to avoid decision chaos by separating:

- **Structural Prior** — what history says about a spread (stable, quarterly review)
- **Regime Conditioning** — what the current cycle says (changes per episode)
- **Current State** — what today's signals say (changes daily)

The intersection of all three drives the leeway position.

**Key insight:** Before using any leeway, we know a great deal about the past
behaviour of each L/S spread — its distribution in general AND conditional on the
SPY regime. This ex-ante knowledge is what prevents decision chaos.

---

## 2. Portfolio Anchor — SAA BAL USD

Source: `input/ic_grid_signals.R` → `bal_saa_full`
Loaded via: `source(here("input/ic_grid_signals.R")); source(here("input/bal_saa_portfolios.R"))`

| Bucket       | Tickers             | SAA Weight |
|--------------|---------------------|------------|
| Cash         | SGOV                | 5.0%       |
| Fixed Income | LQD, IEF, HYG, EMB  | 40.2%      |
| Equity       | URTH, SPY, AAXJ     | 49.3%      |
| Alternatives | GLD, AOK            | 5.5%       |

**Core benchmarks (simplification — start here, expand later):**
- Equity → **SPY**
- Fixed Income → **AGG**

Note: AGG is not in `bal_saa_full` directly but is the standard FI benchmark
for spread computation. All FI spreads are measured vs AGG.

---

## 3. Leeway = L/S Spread Within a Bucket

**Core concept:** Leeway is NOT a simple weight tilt. It is a L/S spread position
expressed within a bucket, sized so that worst-case portfolio drawdown impact stays
within a risk budget.

**Why L/S within a bucket?**
- Keeps total bucket exposure anchored to SAA (market-neutral within bucket)
- The short leg (SPY or AGG) hedges the long leg's market beta
- What remains is **pure relative return** — cleaner, lower volatility spread
- We already have all spread return series in `xts_rel`

**Example — Equity bucket:**
```
SAA:  URTH 27.5% + SPY 16.5% = 44% total equity
TAA:  Signal says US tech strong → +XLK / −SPY  (5% notional)
Net:  URTH 27.5%, SPY 11.5%, XLK 5.0%  — still 44% equity
```

**Leeway sizing rule:**
```
leeway_notional ≤ risk_budget / spread_MaxDD_in_worst_regime
```
Example: risk budget = 0.5% portfolio impact, spread MaxDD in Fall = −22%
→ leeway ≤ 0.5% / 22% = **2.3% notional**

**Correlation pre-screen (hard gate):**
- ρ(ticker, benchmark) > 0.75 required — from KF2 finding
- Low-ρ spreads have unpredictable MaxDD → not suitable for leeway
- This is also a **dynamic gate**: if rolling 60-day ρ drops below threshold
  during stress, reduce or close the leeway regardless of other signals

---

## 4. What We Know About Each Spread Before Using It

Before opening any leeway position, the following is known from history:

**Unconditional (full history)**
- Mean / median spread return
- Volatility, skew, kurtosis
- Information Ratio (IR) of the spread
- MaxDD of cumulative spread (`xts_rel_cum[, ticker]`)
- Current sigma position (`xts_sigma[, ticker]`) — are we stretched?

**Conditional on SPY regime (Fall / Recovery / Consolidation)**
- Mean spread return per regime
- Outperformance type tag: HEDGE / ALPHA / STABLE / LAG / LOSS
- **MaxDD per regime** — the binding constraint for leeway sizing
  (Fall regime typically produces worst spread behaviour)

**Key deduction:** Spread MaxDD in Fall is the binding constraint — not
full-history MaxDD. Fall regime produces the worst spread behaviour for most
equity L/S pairs.

---

## 5. Regime Stack

Two independent regime layers applied sequentially.

### 5.1 Regime 1 — SPY Cycle (built)
Source: `build_regime_table(xts_ret[, "SPY"])` in `scripts_spy_dd_regime/spy_dd_regime.R`

| State         | Definition                            |
|---------------|---------------------------------------|
| Fall          | SPY drawdown > T_FALL (10%) from peak |
| Recovery      | SPY rebounding from Fall trough       |
| Consolidation | SPY in steady state / grinding higher |

Answers: *"Where are we in the equity cycle?"*

**Usage note:** Always assign explicitly — `stats::rt` shadows the `rt` variable:
```r
rt <- build_regime_table(xts_ret[, "SPY"])
```

### 5.2 Regime 2 — Risk-On / Risk-Off (to be built)
Source: TBD — new script in `scripts_safe_haven_regime/` or dedicated `scripts_taa/`

| State      | Definition                                             |
|------------|--------------------------------------------------------|
| Risk-On    | Credit spreads tight, vol low, safe havens lagging     |
| Risk-Off   | Credit spreads widening, vol spiking, safe havens bid  |
| Transition | Conflicting signals                                    |

Answers: *"What is the broad market appetite right now?"*

**Candidate input signals (all already computed):**
- `xts_rel[, "GLD"]` — gold vs SPY (safe haven bid?)
- `xts_rel[, "IEF"]` — treasuries vs SPY (flight to quality?)
- `xts_rel[, "HYG"]` — HY vs AGG credit spread (credit appetite?)
- `xts_sigma[, "SPY"]` — SPY exhaustion z-score
- 200DMA state of SPY (from state machine: ABOVE / BELOW / TRANSITION)

**Status:** Definition in progress — to be formalised in next session.

---

## 6. The Repeating TAA Question

Applied identically to every spread on every review:

```
SPREAD: [TICKER] vs [BENCHMARK]

── STRUCTURAL PRIOR (quarterly, stable) ──────────────────────────────
Q1.  Correlation screen:      ρ(ticker, benchmark) > 0.75?  VALID / INVALID
Q2.  Full-history IR:         IR > 0 → Evergreen / Conditional / Structural UW
Q3.  Spread MaxDD (full):     worst historical drawdown of cumulative spread
Q4.  Spread MaxDD per regime: Fall / Recovery / Consolidation
     → Fall MaxDD is the binding constraint for leeway sizing

── SPY REGIME LAYER (changes per episode) ────────────────────────────
Q5.  Current SPY regime:      Fall / Recovery / Consolidation
Q6.  Spread behaviour in this regime: HEDGE / ALPHA / STABLE / LAG / LOSS
     → does regime confirm or contradict the structural prior?

── RISK-ON/OFF LAYER (changes daily) ─────────────────────────────────
Q7.  Current Risk regime:     Risk-On / Risk-Off / Transition  [TBD]
Q8.  Spread behaviour in Risk-Off: confirm / reduce / close

── CURRENT STATE (daily) ─────────────────────────────────────────────
Q9.  Spread sigma:            stretched / normal / oversold  (xts_sigma)
Q10. 200DMA state (ticker):   ABOVE / BELOW / TRANSITION
Q11. 200DMA state (benchmark):ABOVE / BELOW / TRANSITION
Q12. Rolling ρ (60-day):      above 0.75 threshold? (dynamic correlation gate)

── DECISION ──────────────────────────────────────────────────────────
Structural + Regime + Risk + State → OW / HOLD / UW + leeway size (% notional)
```

**Example output:**
```
XLK-SPY:
  Q1.  ρ = 0.95             → VALID
  Q2.  IR = 0.51            → Evergreen Long
  Q3.  MaxDD (full) = −18%
  Q4.  MaxDD Fall = −22%    → leeway ≤ 2.3% at 0.5% budget
  Q5.  SPY regime = Consolidation
  Q6.  XLK in Consolidation = ALPHA (avg +1.8%)  → confirms Long
  Q7.  Risk regime = [TBD]
  Q9.  Spread sigma = +0.8  → normal, not stretched
  Q10. XLK 200DMA = ABOVE   → confirming
  Q11. SPY 200DMA = ABOVE   → both above, spread stable
  Q12. Rolling ρ = 0.93     → gate open
  →  OW: sit at SAA + leeway (+2% notional on XLK / −2% SPY)
```

---

## 7. Spread Universe (initial)

### Equity vs SPY

| Spread    | Classification  | ρ    | IR    | MaxDD full | MaxDD Fall | Notes                         |
|-----------|-----------------|------|-------|------------|------------|-------------------------------|
| XLK-SPY   | Evergreen Long  | 0.95 | 0.51  | −18%       | −22%       | Pure US tech tilt             |
| QQQ-SPY   | Evergreen Long  | 0.94 | 0.48  | −15%       | TBD        | Nasdaq vs S&P                 |
| SMH-SPY   | Evergreen Long  | 0.88 | 0.90  | −25%       | TBD        | Highest IR, highest DD        |
| VTV-SPY   | Conditional     | 0.92 | 0.20  | −12%       | TBD        | Value — regime dependent      |
| AAXJ-SPY  | Structural UW   | 0.78 | −0.15 | −35%       | TBD        | EM Asia structural drag vs US |
| AAXJ-URTH | Structural UW   | TBD  | TBD   | TBD        | TBD        | EM Asia vs World — to compute |

### Fixed Income vs AGG

| Spread  | Classification | Notes                           |
|---------|----------------|---------------------------------|
| HYG-AGG | Risk-On Long   | Credit appetite signal          |
| EMB-AGG | Conditional    | EM premium, regime dependent    |
| IEF-AGG | Defensive Long | Duration tilt, Risk-Off signal  |

### Alternatives (directional vs SAA weight)

| Asset | Classification | Notes                                   |
|-------|----------------|-----------------------------------------|
| GLD   | Risk-Off Long  | Primary RORO signal + safe haven        |
| AOK   | Hold           | HF proxy — no strong directional prior  |

---

## 8. Data Objects — Where Everything Lives

| Object           | File                                      | Description                              |
|------------------|-------------------------------------------|------------------------------------------|
| `xts_ret`        | `02_data_processed/xts_ret_returns.rds`   | Clean daily log returns (Bloomberg-comparable) |
| `xts_ret_winsor` | `02_data_processed/xts_ret_winsor.rds`    | Winsorized returns — sigma/regime use ONLY; distorts cumulative returns |
| `xts_rel`        | `02_data_processed/xts_rel.rds`           | **Spread return series** — all tickers vs SPY |
| `xts_rel_cum`    | `02_data_processed/xts_rel_cum.rds`       | Cumulative spread — for MaxDD calculation |
| `xts_sigma`      | `02_data_processed/xts_sigma.rds`         | Rolling z-score (252d window, winsorized) |
| `bal_saa_full`   | `input/bal_saa_portfolios.R`              | BAL USD SAA portfolio with IC weights    |
| `rt`             | built via `build_regime_table()`          | SPY regime table (Fall/Recovery/Consol.) |

**Critical distinction:**
- `xts_ret` = clean, use for performance / total return
- `xts_ret_winsor` = winsorized, use for sigma / z-score / regime classification only
- Mixing the two distorts cumulative returns — confirmed vs Bloomberg 2026-03

---

## 9. Functions to Build

| Function              | Purpose                                                | Status  |
|-----------------------|--------------------------------------------------------|---------|
| `spread_profile()`    | Returns full Q1–Q12 answer table for any ticker        | Planned |
| `build_roro_regime()` | Constructs Risk-On/Off regime from signal inputs       | Planned |
| `taa_decision()`      | Combines profile + regimes → OW/HOLD/UW + leeway size  | Planned |

**Custom benchmark support needed:**
- `generate_relative_returns()` currently only supports SPY as benchmark
- Need to support arbitrary benchmark (e.g. AAXJ vs URTH, HYG vs AGG)
- To be added to `scripts/01_etf_wrangle.R`

---

## 10. Key Analytical Deductions (do not contradict without strong reason)

1. **High ρ is required for leeway validity** — low-ρ spreads have unpredictable
   MaxDD. KF2 screen: ρ > 0.75 required. Also a dynamic gate via rolling 60d ρ.

2. **Spread MaxDD in Fall is the binding constraint** — not full-history MaxDD.
   Fall regime produces worst spread behaviour for most equity L/S pairs.

3. **Evergreen longs (XLK, QQQ, SMH) are persistent** — positive IR across all
   regimes. The question is only how much to OW, not whether to hold.

4. **AAXJ is a structural underperformer vs SPY and URTH** — negative full-history
   IR. Only hold at SAA weight. No OW case without strong regime catalyst.

5. **GLD is the primary Risk-Off signal** — rolling ρ(GLD, SPY) breaks down in
   stress. A rising `xts_rel[,"GLD"]` is an early Risk-Off indicator.

6. **Anti-diversification** — for a long-only blended portfolio, MaxDD is NOT
   minimised by minimising correlation. It is minimised by minimising the ticker's
   own standalone drawdown. ρ=0.8 + shallow 15% DD beats ρ=0.1 + 40% DD. (KF1)

7. **Safe havens LAG in Recovery — they do NOT lose** — GLD and IEF record negative
   alpha in Recovery (SPY rebounds faster) but their absolute returns often stay
   positive. "Give back" framing is wrong — it is opportunity cost, not actual loss.

8. **xts_rel already contains all spread series** — no new calculation needed to
   get L/S spread returns. `xts_rel[, "XLK"]` IS the XLK-SPY daily spread return.

---

## 11. Open Questions

- [ ] Define Risk-On/Off regime formally (signals, thresholds, hysteresis)
- [ ] Compute AAXJ-URTH spread (needs custom benchmark in generate_relative_returns)
- [ ] Compute per-regime MaxDD for all spreads in universe table
- [ ] Formalise risk budget per bucket (acceptable leeway MaxDD impact)
- [ ] Decide review frequency: structural prior = quarterly, state = daily, regime = ?
- [ ] Add ILF, SH, DBMF to universe (noted in CLAUDE.md as needed tickers)
