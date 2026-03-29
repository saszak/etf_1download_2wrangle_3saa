################################################################################
# FILE    : scripts/ticker_scoring.R
# Purpose : Score and rank all Category A and B tickers to shortlist top 10
#           for full pair analysis.
#
# SCORING LOGIC
#   For both categories the primary lens is the L/S spread vs the category
#   benchmark (SPY for A, AGG for B).
#
#   Three components, each min-max normalised within its category (→ [0,1]):
#     IR_score    (50%) : Ann IR = E[spread] / σ[spread]
#                         → primary quality of the active bet
#     Calmar_score(30%) : E[spread] / |MaxDD_spread|
#                         → persistence / drawdown efficiency
#     Rho_score   (20%) : for A: reward HIGH ρ (lower TE per notional)
#                         for B: reward LOW ρ (stronger diversification benefit)
#
#   composite = 0.50 × IR_score + 0.30 × Calmar_score + 0.20 × Rho_score
#
# OUTPUTS
#   cat_a_scores  — tibble, all Category A tickers ranked
#   cat_b_scores  — tibble, all Category B tickers ranked
#   top10_a / top10_b — top-10 shortlists (printed to console)
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(xts)
  library(here)
})

# ── Data ──────────────────────────────────────────────────────────────────────
if (!exists("xts_ret"))
  xts_ret <- readRDS(here("02_data_processed/xts_ret_returns.rds"))

available <- colnames(xts_ret)

# ── Category definitions ──────────────────────────────────────────────────────
cat_a_tickers <- c(
  # US sub-market
  "QQQ", "IJH", "IWM",
  # Global / DM broad
  "URTH", "ACWI", "ACWX", "IEFA",
  # EM broad
  "VWO",
  # DM countries
  "EWJ", "DXJ", "FEZ", "DAX", "EWQ", "EWL", "EWU", "EWC", "EWA", "EWI", "EWP",
  # EM countries
  "INDA", "FXI", "EWT", "EWY", "EWZ",
  # US sectors
  "XLK", "XLC", "XLV", "XLF", "XLI", "XLP", "XLY", "XLE", "XLB", "XLRE", "XLU",
  # Factors
  "QUAL", "MTUM", "USMV", "VTV", "DGRW", "COWZ", "ACWV",
  # Satellites HC
  "VHT", "IHI", "IBB", "XBI",
  # Satellites Tech
  "SMH", "CIBR", "WCLD", "AIQ",
  # Satellites other
  "ITA", "ITB", "KRE", "IYT", "PSP", "IPO", "JEPI"
)

cat_b_tickers <- c(
  # Precious metals
  "GLD", "SLV",
  # Broad commodities
  "PDBC", "DJP",
  # Agriculture
  "MOO", "DBA",
  # Metals / Resources
  "COPX", "URA", "LIT",
  # REIT / Real Estate
  "IYR", "VNQI",
  # Infrastructure
  "IGF"
)

# Filter to tickers actually present in the return data
cat_a_tickers <- intersect(cat_a_tickers, available)
cat_b_tickers <- intersect(cat_b_tickers, available)

# ── Helper functions ──────────────────────────────────────────────────────────
max_dd <- function(r) {
  w  <- cumprod(1 + r)
  dd <- (w - cummax(w)) / cummax(w)
  min(dd, na.rm = TRUE)
}

magdon_mdd <- function(sigma, mu, T_years) {
  if (is.na(mu) || is.na(sigma) || sigma <= 0) return(NA_real_)
  if (mu <= 0) return(sigma * sqrt(T_years))
  S <- mu / sigma
  (sigma / S) * (log(S * sqrt(T_years)) + 0.35)
}

# Compute all spread stats for one ticker vs a benchmark
spread_stats <- function(ticker, benchmark, xts_ret) {
  both <- merge(xts_ret[, ticker], xts_ret[, benchmark], join = "inner")
  colnames(both) <- c("tkr", "bmk")
  both <- na.omit(both)
  if (nrow(both) < 252) return(NULL)   # require at least 1 year of overlap

  r_tkr  <- as.numeric(both[, "tkr"])
  r_bmk  <- as.numeric(both[, "bmk"])
  r_spr  <- r_tkr - r_bmk
  T_yrs  <- nrow(both) / 252

  mu_tkr    <- mean(r_tkr, na.rm = TRUE) * 252
  sig_tkr   <- sd(r_tkr,   na.rm = TRUE) * sqrt(252)
  mdd_tkr   <- max_dd(r_tkr)

  mu_spr    <- mean(r_spr, na.rm = TRUE) * 252
  sig_spr   <- sd(r_spr,   na.rm = TRUE) * sqrt(252)
  mdd_spr   <- max_dd(r_spr)
  ir        <- if (sig_spr > 0) mu_spr / sig_spr else NA_real_
  calmar    <- if (abs(mdd_spr) > 0) mu_spr / abs(mdd_spr) else NA_real_
  rho       <- cor(r_tkr, r_bmk, use = "complete.obs")
  mdd_theo  <- magdon_mdd(sig_spr, mu_spr, T_yrs)

  tibble(
    ticker      = ticker,
    benchmark   = benchmark,
    n_days      = nrow(both),
    T_yrs       = round(T_yrs, 1),
    # Individual asset
    mu_tkr      = mu_tkr,
    sig_tkr     = sig_tkr,
    sharpe_tkr  = mu_tkr / sig_tkr,
    mdd_tkr     = mdd_tkr,
    # Spread
    mu_spr      = mu_spr,
    sig_spr     = sig_spr,
    ir          = ir,
    mdd_spr     = mdd_spr,
    calmar      = calmar,
    mdd_theory  = mdd_theo,
    rho         = rho,
    te_10pct    = 0.10 * sig_spr     # TE added at 10% notional
  )
}

# Min-max scaler (returns 0–1; handles NA)
minmax <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(0.5, length(x)))
  (x - rng[1]) / diff(rng)
}

# ── Compute stats ─────────────────────────────────────────────────────────────
cat("Computing Category A spread stats (vs SPY)...\n")
raw_a <- map_dfr(cat_a_tickers, ~spread_stats(.x, "SPY", xts_ret))

cat("Computing Category B spread stats (vs AGG)...\n")
raw_b <- map_dfr(cat_b_tickers, ~spread_stats(.x, "AGG", xts_ret))

# ── Score: Category A ─────────────────────────────────────────────────────────
# For A: Rho_score = reward HIGH ρ (collapses spread vol → small TE)
cat_a_scores <- raw_a %>%
  filter(!is.na(ir), !is.na(calmar)) %>%
  mutate(
    ir_score     = minmax(ir),
    calmar_score = minmax(calmar),
    rho_score    = minmax(rho),          # higher ρ → higher score
    composite    = 0.50 * ir_score +
                   0.30 * calmar_score +
                   0.20 * rho_score
  ) %>%
  arrange(desc(composite))

# ── Score: Category B ─────────────────────────────────────────────────────────
# For B: Rho_score = reward LOW ρ (stronger cross-asset diversification)
cat_b_scores <- raw_b %>%
  filter(!is.na(ir), !is.na(calmar)) %>%
  mutate(
    ir_score     = minmax(ir),
    calmar_score = minmax(calmar),
    rho_score    = minmax(-rho),         # lower/more-negative ρ → higher score
    composite    = 0.50 * ir_score +
                   0.30 * calmar_score +
                   0.20 * rho_score
  ) %>%
  arrange(desc(composite))

# ── Top 10 shortlists ─────────────────────────────────────────────────────────
top10_a <- cat_a_scores %>% slice_head(n = 10)
top10_b <- cat_b_scores %>% slice_head(n = 10)

# ── Print: Category A ─────────────────────────────────────────────────────────
fmt_pct <- function(x) sprintf("%+.1f%%", x * 100)
fmt_dec <- function(x) sprintf("%.2f",    x)

cat("\n")
cat("════════════════════════════════════════════════════════════════════════\n")
cat("  CATEGORY A — EQUITY SWAP (vs SPY)  |  Top 10\n")
cat("════════════════════════════════════════════════════════════════════════\n")
cat(sprintf("  %-6s  %-5s  %-8s  %-8s  %-8s  %-7s  %-7s  %-8s  %-8s\n",
            "Ticker", "T_yrs", "E[spr]", "σ(spr)", "IR",
            "Calmar", "ρ(SPY)", "MaxDD", "Score"))
cat(sprintf("  %-6s  %-5s  %-8s  %-8s  %-8s  %-7s  %-7s  %-8s  %-8s\n",
            "──────", "─────", "───────", "───────", "───────",
            "──────", "──────", "───────", "───────"))
walk(seq_len(nrow(top10_a)), function(i) {
  r <- top10_a[i, ]
  cat(sprintf("  %-6s  %-5.1f  %-8s  %-8s  %-8s  %-7s  %-7s  %-8s  %-8s\n",
              r$ticker, r$T_yrs,
              fmt_pct(r$mu_spr), fmt_pct(r$sig_spr), fmt_dec(r$ir),
              fmt_dec(r$calmar), fmt_dec(r$rho),
              fmt_pct(r$mdd_spr), fmt_dec(r$composite)))
})

cat("\n")
cat("════════════════════════════════════════════════════════════════════════\n")
cat("  CATEGORY A — Full Rankings (all tickers)\n")
cat("════════════════════════════════════════════════════════════════════════\n")
print(
  cat_a_scores %>%
    transmute(
      Ticker   = ticker,
      T_yrs,
      `E[spr]` = fmt_pct(mu_spr),
      `σ(spr)` = fmt_pct(sig_spr),
      IR       = round(ir, 2),
      Calmar   = round(calmar, 2),
      `ρ(SPY)` = round(rho, 2),
      MaxDD    = fmt_pct(mdd_spr),
      TE_10pct = fmt_pct(te_10pct),
      Score    = round(composite, 3)
    ),
  n = Inf
)

# ── Print: Category B ─────────────────────────────────────────────────────────
cat("\n")
cat("════════════════════════════════════════════════════════════════════════\n")
cat("  CATEGORY B — CROSS-ASSET (vs AGG)  |  Top 10\n")
cat("════════════════════════════════════════════════════════════════════════\n")
cat(sprintf("  %-6s  %-5s  %-8s  %-8s  %-8s  %-7s  %-7s  %-8s  %-8s\n",
            "Ticker", "T_yrs", "E[spr]", "σ(spr)", "IR",
            "Calmar", "ρ(AGG)", "MaxDD", "Score"))
cat(sprintf("  %-6s  %-5s  %-8s  %-8s  %-8s  %-7s  %-7s  %-8s  %-8s\n",
            "──────", "─────", "───────", "───────", "───────",
            "──────", "──────", "───────", "───────"))
walk(seq_len(nrow(top10_b)), function(i) {
  r <- top10_b[i, ]
  cat(sprintf("  %-6s  %-5.1f  %-8s  %-8s  %-8s  %-7s  %-7s  %-8s  %-8s\n",
              r$ticker, r$T_yrs,
              fmt_pct(r$mu_spr), fmt_pct(r$sig_spr), fmt_dec(r$ir),
              fmt_dec(r$calmar), fmt_dec(r$rho),
              fmt_pct(r$mdd_spr), fmt_dec(r$composite)))
})

cat("\n")
cat("════════════════════════════════════════════════════════════════════════\n")
cat("  CATEGORY B — Full Rankings (all tickers)\n")
cat("════════════════════════════════════════════════════════════════════════\n")
print(
  cat_b_scores %>%
    transmute(
      Ticker   = ticker,
      T_yrs,
      `E[spr]` = fmt_pct(mu_spr),
      `σ(spr)` = fmt_pct(sig_spr),
      IR       = round(ir, 2),
      Calmar   = round(calmar, 2),
      `ρ(AGG)` = round(rho, 2),
      MaxDD    = fmt_pct(mdd_spr),
      TE_10pct = fmt_pct(te_10pct),
      Score    = round(composite, 3)
    ),
  n = Inf
)

cat("\n")
cat("────────────────────────────────────────────────────────────────────────\n")
cat("  Scoring formula:\n")
cat("    composite = 0.50 × IR_score  +  0.30 × Calmar_score  +  0.20 × Rho_score\n")
cat("  All components min-max normalised within category.\n")
cat("  Rho_score (A) = reward HIGH ρ  |  Rho_score (B) = reward LOW/negative ρ\n")
cat("────────────────────────────────────────────────────────────────────────\n")

invisible(list(
  cat_a_scores = cat_a_scores,
  cat_b_scores = cat_b_scores,
  top10_a      = top10_a,
  top10_b      = top10_b
))
