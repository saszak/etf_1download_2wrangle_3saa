################################################################################
# FILE    : scripts_daa_saa_taa/portfolio_maker_demo.R
# Purpose : Demonstrate portfolio_maker() — build a custom SAA baseline and
#           use it with screen_ticker()
#
# RUN     : source this file, or step through interactively
################################################################################

library(here)
if (!exists("portfolio_maker")) source(here("scripts_daa_saa_taa/screen_ticker.R"))

# ==============================================================================
# 1. BUILD SYNTHETIC BASELINES
# ==============================================================================

# Classic 60/40  — label must NOT start with a digit (xts prepends "X" otherwise)
bl_6040 <- portfolio_maker(
  tickers = c("SPY", "IEF"),
  weights = c(0.60,  0.40),
  xts_ret = xts_ret,
  label   = "BL6040"
)

# Sovereign SAA — 6-asset portfolio
my_saa <- portfolio_maker(
  tickers = c("AGG",  "URTH", "SPY",  "QQQ",  "XLK",  "SMH"),
  weights = c( 0.35,   0.25,   0.25,   0.05,   0.05,   0.05),
  xts_ret = xts_ret,
  label   = "MySAA"
)

# ==============================================================================
# 2. INJECT INTO xts_ret  (one merge per series — join only works for 2 objects)
# ==============================================================================

xts_ret <- merge(xts_ret, bl_6040, join = "left")
xts_ret <- merge(xts_ret, my_saa,  join = "left")

message("Columns added: ", paste(c("BL6040","MySAA"), collapse=", "))
message("xts_ret now has ", ncol(xts_ret), " columns")

# ==============================================================================
# 3. QUICK STATS COMPARISON
# ==============================================================================

.ann_r <- function(r) prod(1+as.numeric(r), na.rm=TRUE)^(252/sum(!is.na(r)))-1
.vol_r <- function(r) sd(as.numeric(r), na.rm=TRUE) * sqrt(252)
.dd_r  <- function(r) { w <- cumprod(1+as.numeric(na.omit(r))); min((w-cummax(w))/cummax(w)) }
.ir_r  <- function(r) { v <- .vol_r(r); if(is.na(v)||v==0) NA_real_ else .ann_r(r)/v }

series <- na.omit(merge(xts_ret[,"SPY"], bl_6040, my_saa))

cat("\n── Portfolio comparison ────────────────────────────────\n")
cat(sprintf("  %-14s  %8s  %8s  %8s  %6s\n", "Series", "Ann Ret", "MaxDD", "Vol", "IR"))
cat(strrep("─", 58), "\n")
for (tk in colnames(series)) {
  r <- series[, tk]
  cat(sprintf("  %-14s  %+7.1f%%  %7.1f%%  %7.1f%%  %6.2f\n",
              tk,
              .ann_r(r)*100, .dd_r(r)*100, .vol_r(r)*100, .ir_r(r)))
}
cat(strrep("─", 58), "\n\n")

# ==============================================================================
# 4. CUMULATIVE RETURN CHART
# ==============================================================================

if (!isTRUE(getOption("knitr.in.progress"))) {
  library(PerformanceAnalytics)
  chart.CumReturns(
    series,
    wealth.index = TRUE,
    legend.loc   = "topleft",
    main         = "MySAA vs BL6040 vs SPY",
    colorset     = c("#9ca3af", "#3b82f6", "#7c3aed")
  )
}

# ==============================================================================
# 5. SCREEN A TICKER AGAINST MySAA
# ==============================================================================

# screen_ticker() works with any column name in xts_ret as baseline
# screen_ticker("GLD",  baseline = "MySAA")
# screen_ticker("GLD",  baseline = "MySAA", report = TRUE)
# screen_ticker("PDBC", baseline = "BL6040")

message("✅ Done.  Use screen_ticker(\"GLD\", baseline = \"MySAA\") to screen any ticker.")
