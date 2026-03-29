# key_findings/

Standalone, self-contained R scripts that document the **most important empirical
discoveries** of this project. Each script loads its own data, produces labelled
plots, and prints a summary table. Run them directly in RStudio or `source()` them.

---

## KF1 — Anti-Diversification (`kf1_anti_diversification.R`)

**Finding:** Low-correlation tickers (ρ < 0.2 vs SPY) have empirical MaxDD of
their spread that is *systematically worse* than the Magdon-Ismail theoretical
prediction. High-correlation tickers (ρ > 0.75) track theory closely.

**Implication:** Adding a "diversifier" does not reduce drawdown in a long-only
blended portfolio. Conventional diversification theory breaks down here.
MaxDD is minimised by minimising the ticker's own standalone drawdown, not
its correlation.

Plots produced:
- `KF1-A` — Scatter: theoretical vs empirical MaxDD, coloured by ρ band
- `KF1-B` — Residual bar chart: (empirical − theory), sorted by ρ

---

## KF2 — L/S Pair Selection (`kf2_ls_pair_selection.R`)

**Finding:** To replace SPY weight in an SAA portfolio using a long-ticker /
short-SPY overlay, the ticker must have ρ > 0.75 vs SPY. Low-ρ tickers produce
spread drawdowns too severe to use as SAA substitutes. Sector ETFs (XLK, XLV,
XLF) are the primary valid candidates; non-equity assets (GLD, IEF, TLT) are
standalone allocations, never L/S legs.

**Screen:** `ρ ≥ 0.75  AND  spread MaxDD ≥ −30%`

Plots produced:
- `KF2-A` — Screening scatter: ρ vs spread-MaxDD with valid-candidate zone
- `KF2-B` — IR ranking of high-ρ candidates
- `KF2-C` — ΔMaxDD: does the 10% swap actually improve portfolio-level DD?
