################################################################################
# FILE    : scripts_daa_saa_taa/plot_rebal_drift.R
# Purpose : Pedagogical 6-panel guide to the rebalancing drift phenomenon.
#
# Causal chain told across three acts (2 panels each, displayed side-by-side):
#
#   ACT 1 — Building blocks
#     P1  Individual asset returns: SPY dominates, IEF cushions Falls
#     P2  Implied SPY weight: BaH drifts to ~80%; annual rebal snaps back
#
#   ACT 2 — The mechanism (why drift hurts in Falls)
#     P3  Daily SPY−IEF spread: IEF beats SPY in every Fall
#     P4  Daily drift = (excess SPY weight) × spread: large neg spikes in Falls
#
#   ACT 3 — The consequence
#     P5  Cumulative drift: Fall losses > bull-run gains → negative overall
#     P6  Cumulative wealth: daily rebal wins; rebalancing bonus is real
#
# Reading order: left column top-to-bottom, then right column, mirrors the
# cause-and-effect logic. Fall episodes (red) align across all 6 panels.
################################################################################

library(tidyverse); library(xts); library(zoo); library(patchwork)
library(scales); library(here)

if (!exists("xts_ret"))                      source(here(project_tree$scripts$wrangle))
if (!exists("rt") || !is.data.frame(rt))     rt <- build_regime_table(xts_ret[,"SPY"])

W_EQ <- 0.60; W_FI <- 0.40

# ── Raw series ─────────────────────────────────────────────────────────────────
common <- na.omit(merge(xts_ret[,"SPY"], xts_ret[,"IEF"]))
spy_r  <- as.numeric(common[,"SPY"])
ief_r  <- as.numeric(common[,"IEF"])
dates  <- as.Date(index(common))
n      <- length(dates)

# Daily-rebalanced (fixed weights every day)
ret_rebal <- W_EQ * spy_r + W_FI * ief_r

# Buy-and-hold (weights drift with performance)
w_spy   <- cumprod(1 + spy_r) * W_EQ
w_ief   <- cumprod(1 + ief_r) * W_FI
total   <- w_spy + w_ief
ret_bah <- c(NA, diff(log(total)))

# Drift = daily excess of BaH over rebalanced
drift  <- ret_bah - ret_rebal

# Yearly rebalancing (reset at each year boundary)
years    <- as.integer(format(dates, "%Y"))
spy_w_yr <- numeric(n);  ief_w_yr <- numeric(n)
spy_w_yr[1] <- W_EQ;     ief_w_yr[1] <- W_FI
for (i in 2:n) {
  spy_w_yr[i] <- spy_w_yr[i-1] * (1 + spy_r[i])
  ief_w_yr[i] <- ief_w_yr[i-1] * (1 + ief_r[i])
  if (years[i] != years[i-1]) {
    tot          <- spy_w_yr[i] + ief_w_yr[i]
    spy_w_yr[i]  <- tot * W_EQ
    ief_w_yr[i]  <- tot * W_FI
  }
}
total_yr         <- spy_w_yr + ief_w_yr
ret_annual       <- c(NA, diff(log(total_yr)))
cum_annual       <- total_yr / total_yr[1]
drift_annual     <- ret_annual - ret_rebal
cum_drift_annual <- cumsum(replace(drift_annual, is.na(drift_annual), 0))

# Quarterly rebalancing (reset at each quarter boundary)
qtrs     <- as.integer(format(dates, "%Y")) * 4L + (as.integer(format(dates, "%m")) - 1L) %/% 3L
spy_w_qtr <- numeric(n);  ief_w_qtr <- numeric(n)
spy_w_qtr[1] <- W_EQ;     ief_w_qtr[1] <- W_FI
for (i in 2:n) {
  spy_w_qtr[i] <- spy_w_qtr[i-1] * (1 + spy_r[i])
  ief_w_qtr[i] <- ief_w_qtr[i-1] * (1 + ief_r[i])
  if (qtrs[i] != qtrs[i-1]) {
    tot           <- spy_w_qtr[i] + ief_w_qtr[i]
    spy_w_qtr[i]  <- tot * W_EQ
    ief_w_qtr[i]  <- tot * W_FI
  }
}
total_qtr   <- spy_w_qtr + ief_w_qtr
ret_qtr     <- c(NA, diff(log(total_qtr)))
cum_qtr     <- total_qtr / total_qtr[1]

# Threshold rebalancing ±10%
spy_w_thr10 <- numeric(n);  ief_w_thr10 <- numeric(n)
spy_w_thr10[1] <- W_EQ;     ief_w_thr10[1] <- W_FI
for (i in 2:n) {
  spy_w_thr10[i] <- spy_w_thr10[i-1] * (1 + spy_r[i])
  ief_w_thr10[i] <- ief_w_thr10[i-1] * (1 + ief_r[i])
  tot <- spy_w_thr10[i] + ief_w_thr10[i]
  if (abs(spy_w_thr10[i] / tot - W_EQ) > 0.10) {
    spy_w_thr10[i] <- tot * W_EQ
    ief_w_thr10[i] <- tot * W_FI
  }
}
total_thr10  <- spy_w_thr10 + ief_w_thr10
cum_thr10    <- total_thr10 / total_thr10[1]
ret_thr10    <- c(NA, diff(log(total_thr10)))

# Threshold rebalancing ±5%
spy_w_thr05 <- numeric(n);  ief_w_thr05 <- numeric(n)
spy_w_thr05[1] <- W_EQ;     ief_w_thr05[1] <- W_FI
for (i in 2:n) {
  spy_w_thr05[i] <- spy_w_thr05[i-1] * (1 + spy_r[i])
  ief_w_thr05[i] <- ief_w_thr05[i-1] * (1 + ief_r[i])
  tot <- spy_w_thr05[i] + ief_w_thr05[i]
  if (abs(spy_w_thr05[i] / tot - W_EQ) > 0.05) {
    spy_w_thr05[i] <- tot * W_EQ
    ief_w_thr05[i] <- tot * W_FI
  }
}
total_thr05  <- spy_w_thr05 + ief_w_thr05
cum_thr05    <- total_thr05 / total_thr05[1]
ret_thr05    <- c(NA, diff(log(total_thr05)))

# Cumulative wealth
cum_rebal <- cumprod(1 + ret_rebal)
cum_bah   <- c(1, cumprod(1 + na.omit(ret_bah)))
cum_bah   <- c(NA, cum_bah[-length(cum_bah)])   # align to dates
cum_drift <- cumsum(replace(drift, is.na(drift), 0))

df <- tibble(
  date              = dates,
  rebal             = cum_rebal,
  bah               = cum_bah,
  annual            = cum_annual,
  quarterly         = cum_qtr,
  thr10             = cum_thr10,
  thr05             = cum_thr05,
  drift_cum         = cum_drift,
  drift_cum_annual  = cum_drift_annual,
  drift_daily       = drift,
  drift_annual      = drift_annual,
  spread            = spy_r - ief_r,
  spy_weight_bah    = w_spy  / total,
  spy_weight_annual = spy_w_yr / total_yr,
  spy_weight_qtr    = spy_w_qtr / total_qtr,
  spy_weight_thr10  = spy_w_thr10 / total_thr10,
  spy_weight_thr05  = spy_w_thr05 / total_thr05,
  spy_weight_daily  = rep(W_EQ, n),        # constant: reset every day
  ret_rebal         = ret_rebal,
  ret_bah           = ret_bah,
  ret_annual        = ret_annual,
  ret_qtr           = ret_qtr,
  ret_thr10         = ret_thr10,
  ret_thr05         = ret_thr05,
  cum_spy           = cumprod(1 + spy_r),
  cum_ief           = cumprod(1 + ief_r)
)

# ── Regime shading ─────────────────────────────────────────────────────────────
regime_rect <- rt %>%
  filter(regime %in% c("Fall","Recovery")) %>%
  mutate(
    xmin = as.Date(xmin),
    xmax = as.Date(xmax),
    fill = if_else(regime == "Fall", "#fca5a5", "#bbf7d0")
  )

# Standard shading (line/area panels — fill scale free)
.shading <- function(p) {
  p + geom_rect(data = regime_rect,
                aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf, fill=fill),
                alpha=0.18, inherit.aes=FALSE) +
    scale_fill_identity()
}

# Bar panels already use fill for sign colouring — add rects WITHOUT fill scale
.shading_bar <- function(p) {
  for (i in seq_len(nrow(regime_rect))) {
    p <- p + annotate("rect",
                      xmin  = regime_rect$xmin[i],
                      xmax  = regime_rect$xmax[i],
                      ymin  = -Inf, ymax = Inf,
                      fill  = regime_rect$fill[i],
                      alpha = 0.18)
  }
  p
}

.theme_panel <- function() {
  theme_minimal(base_size = 11) +
    theme(legend.position  = "top",
          panel.grid.minor = element_blank(),
          axis.text.y      = element_text(colour="#1B3A6B", face="bold"))
}

# ══════════════════════════════════════════════════════════════════════════════
# ACT 1 — BUILDING BLOCKS
# ══════════════════════════════════════════════════════════════════════════════

# P1: Individual asset returns ─────────────────────────────────────────────────
p1 <- df %>%
  select(date, SPY = cum_spy, IEF = cum_ief) %>%
  pivot_longer(-date) %>%
  ggplot(aes(date, value, colour=name)) +
  geom_line(linewidth=0.8) +
  scale_colour_manual(values=c("SPY"="#ef4444", "IEF"="#3b82f6")) +
  scale_y_continuous(labels=dollar_format(prefix="$")) +
  scale_x_date(date_labels="%Y") +
  labs(title    = "A1 \u00b7 Building blocks: SPY dominates, IEF cushions Falls",
       subtitle = "$1 invested in each asset independently",
       x=NULL, y="Wealth ($)", colour=NULL) +
  .theme_panel()
p1 <- .shading(p1)

# P2: Implied SPY weight ───────────────────────────────────────────────────────
p2 <- df %>%
  select(date, `Buy & Hold` = spy_weight_bah, `Annual Rebal` = spy_weight_annual) %>%
  pivot_longer(-date) %>%
  ggplot(aes(date, value, colour=name)) +
  geom_hline(yintercept=W_EQ, linetype="dashed", colour="#9ca3af", linewidth=0.4) +
  geom_line(linewidth=0.8) +
  scale_colour_manual(values=c("Annual Rebal"="#f59e0b", "Buy & Hold"="#7c3aed")) +
  scale_x_date(date_labels="%Y") +
  scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0.40, 0.90)) +
  labs(title    = "A2 \u00b7 SPY weight drifts to ~80% — maximally overweight entering every Fall",
       subtitle = sprintf("Annual rebal snaps back to %.0f%% each January; BaH never does",
                          W_EQ*100),
       x=NULL, y="SPY weight", colour=NULL) +
  .theme_panel()
p2 <- .shading(p2)

# ══════════════════════════════════════════════════════════════════════════════
# ACT 2 — THE MECHANISM
# ══════════════════════════════════════════════════════════════════════════════

# P3: Daily SPY−IEF spread ─────────────────────────────────────────────────────
p3 <- df %>%
  mutate(bar_fill = if_else(spread >= 0, "#86efac", "#fca5a5")) %>%
  ggplot(aes(date, spread * 100, fill=bar_fill)) +
  geom_col(width=1, show.legend=FALSE) +
  scale_fill_identity() +
  scale_x_date(date_labels="%Y") +
  scale_y_continuous(labels=function(x) paste0(x, "%")) +
  labs(title    = "A3 \u00b7 Signal: daily SPY \u2212 IEF return spread",
       subtitle = "Red bars = IEF beats SPY — bad for an overweight-equity portfolio",
       x=NULL, y="Spread (% per day)") +
  .theme_panel() + theme(legend.position="none")
p3 <- .shading_bar(p3)

# P4: Daily drift = (excess SPY weight) × spread ──────────────────────────────
p4 <- df %>%
  mutate(bar_fill = if_else(drift_daily >= 0 | is.na(drift_daily),
                             "#86efac", "#fca5a5")) %>%
  ggplot(aes(date, drift_daily * 100, fill=bar_fill)) +
  geom_col(width=1, show.legend=FALSE) +
  scale_fill_identity() +
  scale_x_date(date_labels="%Y") +
  scale_y_continuous(labels=function(x) paste0(x, "%")) +
  labs(title    = "A4 \u00b7 Damage: daily drift = (excess SPY weight) \u00d7 spread",
       subtitle = "BaH enters every Fall maximally overweight \u2192 largest negative spikes there",
       x=NULL, y="Daily drift (%)") +
  .theme_panel() + theme(legend.position="none")
p4 <- .shading_bar(p4)

# ══════════════════════════════════════════════════════════════════════════════
# ACT 3 — THE CONSEQUENCE
# ══════════════════════════════════════════════════════════════════════════════

# P5: Cumulative drift ─────────────────────────────────────────────────────────
p5 <- df %>%
  select(date, `Buy & Hold` = drift_cum, `Annual Rebal` = drift_cum_annual) %>%
  pivot_longer(-date) %>%
  ggplot(aes(date, value, colour=name)) +
  geom_hline(yintercept=0, linetype="dashed", colour="#9ca3af", linewidth=0.4) +
  geom_line(linewidth=0.8) +
  scale_colour_manual(values=c("Annual Rebal"="#f59e0b", "Buy & Hold"="#7c3aed")) +
  scale_x_date(date_labels="%Y") +
  scale_y_continuous(labels=percent_format(accuracy=0.1)) +
  labs(title    = "A5 \u00b7 Scorecard: Fall losses outweigh bull-run gains \u2192 drift stays negative",
       subtitle = "Positive = momentum paid  |  Negative = rebalancing bonus paid",
       x=NULL, y="Cumulative drift", colour=NULL) +
  .theme_panel()
p5 <- .shading(p5)

# P6: Cumulative wealth ────────────────────────────────────────────────────────
p6 <- df %>%
  select(date, `Daily Rebal` = rebal, `Annual Rebal` = annual, `Buy & Hold` = bah) %>%
  pivot_longer(-date) %>%
  mutate(name = factor(name, levels=c("Buy & Hold","Annual Rebal","Daily Rebal"))) %>%
  ggplot(aes(date, value, colour=name)) +
  geom_line(linewidth=0.8) +
  scale_colour_manual(values=c("Daily Rebal"="#3b82f6",
                                "Annual Rebal"="#f59e0b",
                                "Buy & Hold"  ="#7c3aed")) +
  scale_y_continuous(labels=dollar_format(prefix="$")) +
  scale_x_date(date_labels="%Y") +
  labs(title    = "A6 \u00b7 Bottom line: daily rebal wins — sell high / buy low every day",
       subtitle = "Rebalancing bonus = volatility harvesting; frequency matters at the margin",
       x=NULL, y="Wealth ($)", colour=NULL) +
  .theme_panel()
p6 <- .shading(p6)

# ══════════════════════════════════════════════════════════════════════════════
# ACT 4 — WHEN IT HAPPENED
# ══════════════════════════════════════════════════════════════════════════════

# P7: Annual drift attribution — which years cost BaH vs daily rebal? ──────────
# Sum daily drift within each calendar year
annual_drift_tbl <- df %>%
  mutate(year = as.integer(format(date, "%Y"))) %>%
  group_by(year) %>%
  summarise(
    bah_drift  = sum(drift_daily,   na.rm=TRUE),
    ann_drift  = sum(drift_daily,   na.rm=TRUE),   # placeholder; overwrite below
    .groups    = "drop"
  )

# Recompute properly per-year for each strategy
annual_drift_tbl <- df %>%
  mutate(year = as.integer(format(date, "%Y")),
         drift_ann_daily = drift_annual) %>%
  group_by(year) %>%
  summarise(
    `Buy & Hold`   = sum(drift_daily,     na.rm=TRUE),
    `Annual Rebal` = sum(drift_ann_daily, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-year, names_to="strategy", values_to="drift_yr")

# fall years for bar colouring
fall_years <- regime_rect %>%
  filter(fill == "#fca5a5") %>%
  mutate(yr = as.integer(format(xmin, "%Y"))) %>%
  pull(yr) %>% unique()

p7 <- annual_drift_tbl %>%
  mutate(
    bar_fill = case_when(
      year %in% fall_years & drift_yr < 0 ~ "#ef4444",   # Fall year, negative
      drift_yr < 0                         ~ "#fca5a5",   # other negative
      TRUE                                 ~ "#86efac"    # positive
    )
  ) %>%
  ggplot(aes(year, drift_yr * 100, fill=bar_fill)) +
  geom_col(position="dodge", show.legend=FALSE) +
  geom_hline(yintercept=0, colour="#9ca3af", linewidth=0.3) +
  scale_fill_identity() +
  scale_x_continuous(breaks=scales::pretty_breaks(n=8)) +
  scale_y_continuous(labels=function(x) paste0(x, "%")) +
  facet_wrap(~strategy, ncol=1) +
  labs(title    = "A7 \u00b7 Annual drift by year: Falls deliver the knockout blows",
       subtitle = "Dark red = Fall year with negative drift  |  Each bar = yearly sum of daily drifts",
       x=NULL, y="Yearly drift (%)") +
  theme_minimal(base_size=11) +
  theme(panel.grid.minor=element_blank(),
        strip.text=element_text(face="bold"),
        axis.text.y=element_text(colour="#1B3A6B", face="bold"))

# P8: Rolling 12-month drift — is drift currently positive? ───────────────────
roll_days <- 252L
df_roll <- df %>%
  mutate(
    roll_bah = zoo::rollsum(replace(drift_daily,   is.na(drift_daily),   0),
                             k=roll_days, fill=NA, align="right"),
    roll_ann = zoo::rollsum(replace(drift_annual,  is.na(drift_annual),  0),
                             k=roll_days, fill=NA, align="right")
  )

p8 <- df_roll %>%
  select(date, `Buy & Hold` = roll_bah, `Annual Rebal` = roll_ann) %>%
  pivot_longer(-date) %>%
  ggplot(aes(date, value, colour=name)) +
  geom_hline(yintercept=0, linetype="dashed", colour="#9ca3af", linewidth=0.4) +
  geom_line(linewidth=0.8) +
  scale_colour_manual(values=c("Annual Rebal"="#f59e0b", "Buy & Hold"="#7c3aed")) +
  scale_x_date(date_labels="%Y") +
  scale_y_continuous(labels=percent_format(accuracy=0.1)) +
  labs(title    = "A8 \u00b7 Rolling 12-month drift: negative during and after every Fall",
       subtitle = "Crosses zero as Falls end — rebalancing bonus turns on exactly when recovery begins",
       x=NULL, y="Trailing 12m drift", colour=NULL) +
  .theme_panel()
p8 <- .shading(p8)

# ══════════════════════════════════════════════════════════════════════════════
# MAIN VIEW — two narrative columns, each with 4 stacked panels
#
#   LEFT  — Daily Rebal column
#     M1  Cumulative wealth: all 3 strategies
#     M2  Drawdown: daily rebal limits damage in Falls
#     M3  Annual returns: strategies track closely; advantage in Fall years
#     M4  Rolling 3-year annualised return: who leads at each point in time?
#
#   RIGHT — Drift column
#     M5  Cumulative drift with shaded area (hero)
#     M6  SPY weight: why drift exists — compounding away from 60%
#     M7  Annual drift by year: 2008 / 2022 are the knockout blows
#     M8  Rolling 12-month drift: rebalancing bonus turns on in Recovery
# ══════════════════════════════════════════════════════════════════════════════

# ── Shared helper colours / theme ─────────────────────────────────────────────
COL <- c("Daily Rebal"="#3b82f6", "Annual Rebal"="#f59e0b", "Buy & Hold"="#7c3aed")

.theme_col <- function(base=11, leg="top") {
  theme_minimal(base_size=base) +
    theme(legend.position=leg, panel.grid.minor=element_blank(),
          axis.text.y=element_text(colour="#1B3A6B", face="bold"),
          plot.title=element_text(face="bold", size=base+1),
          plot.subtitle=element_text(size=base-1, colour="#6b7280"))
}

# ── Derived series needed for column panels ────────────────────────────────────
running_dd <- function(x) {
  peak <- cummax(ifelse(is.na(x), -Inf, x))
  peak[is.infinite(peak)] <- NA_real_
  ifelse(is.na(peak) | peak == 0, NA_real_, (x - peak) / peak)
}

df <- df %>%
  mutate(
    dd_rebal  = running_dd(rebal),
    dd_bah    = running_dd(bah),
    dd_annual = running_dd(annual),
    r_rebal   = log(rebal)  - lag(log(rebal)),
    r_bah     = log(bah)    - lag(log(bah)),
    r_annual  = log(annual) - lag(log(annual))
  )

# Annual returns per strategy
ann_ret_tbl <- df %>%
  mutate(year = format(date, "%Y")) %>%
  group_by(year) %>%
  summarise(
    `Daily Rebal`  = exp(sum(r_rebal,  na.rm=TRUE)) - 1,
    `Annual Rebal` = exp(sum(r_annual, na.rm=TRUE)) - 1,
    `Buy & Hold`   = exp(sum(r_bah,    na.rm=TRUE)) - 1,
    .groups="drop"
  ) %>%
  pivot_longer(-year, names_to="strategy", values_to="ret") %>%
  mutate(strategy = factor(strategy, levels=names(COL)))

# Rolling 3-year annualised return (756 trading days)
roll3 <- 756L
df_roll3 <- df %>%
  mutate(
    roll_rebal  = zoo::rollapply(r_rebal,  roll3, function(x) exp(sum(x,na.rm=T))^(252/roll3)-1,
                                  fill=NA, align="right"),
    roll_bah    = zoo::rollapply(r_bah,    roll3, function(x) exp(sum(x,na.rm=T))^(252/roll3)-1,
                                  fill=NA, align="right"),
    roll_annual = zoo::rollapply(r_annual, roll3, function(x) exp(sum(x,na.rm=T))^(252/roll3)-1,
                                  fill=NA, align="right")
  )

# ── LEFT COLUMN ───────────────────────────────────────────────────────────────

# M1: Cumulative wealth
m1 <- df %>%
  select(date, `Daily Rebal`=rebal, `Annual Rebal`=annual, `Buy & Hold`=bah) %>%
  pivot_longer(-date) %>%
  mutate(name=factor(name, levels=names(COL))) %>%
  ggplot(aes(date, value, colour=name)) +
  geom_line(linewidth=0.9) +
  scale_colour_manual(values=COL) +
  scale_y_continuous(labels=dollar_format(prefix="$")) +
  scale_x_date(date_labels="%Y") +
  labs(title="M1 · Wealth: all three strategies from $1",
       subtitle=paste0("Daily rebal = sell winner / buy loser every day\n",
                       "Annual rebal = reset to 60/40 each January  |  Buy & Hold = never touch"),
       x=NULL, y="Wealth ($)", colour=NULL) +
  .theme_col(base=12)
m1 <- .shading(m1)

# M2: Drawdown
m2 <- df %>%
  select(date, `Daily Rebal`=dd_rebal, `Annual Rebal`=dd_annual, `Buy & Hold`=dd_bah) %>%
  pivot_longer(-date) %>%
  mutate(name=factor(name, levels=names(COL))) %>%
  ggplot(aes(date, value, colour=name)) +
  geom_line(linewidth=0.7) +
  geom_hline(yintercept=0, colour="#9ca3af", linewidth=0.3) +
  scale_colour_manual(values=COL) +
  scale_x_date(date_labels="%Y") +
  scale_y_continuous(labels=percent_format(accuracy=1)) +
  labs(title="M2 · Drawdown: daily rebal limits damage in every Fall",
       subtitle="Buy & Hold enters Falls maximally overweight equity \u2192 deepest drawdowns",
       x=NULL, y="Drawdown from peak", colour=NULL) +
  .theme_col(leg="none")
m2 <- .shading(m2)

# M3: Annual returns bar
m3 <- ann_ret_tbl %>%
  ggplot(aes(year, ret, fill=strategy)) +
  geom_col(position="dodge", width=0.75, show.legend=FALSE) +
  geom_hline(yintercept=0, colour="#9ca3af", linewidth=0.3) +
  scale_fill_manual(values=COL) +
  scale_y_continuous(labels=percent_format(accuracy=1)) +
  scale_x_discrete(breaks=function(x) x[seq(1,length(x),by=4)]) +
  labs(title="M3 · Annual returns: strategies track closely — gap widens in Fall years",
       subtitle="Daily rebal advantage concentrates in 2008, 2020, 2022 via faster mean reversion",
       x=NULL, y="Annual return") +
  .theme_col(leg="none") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

# M4: Rolling 3-year annualised return — who leads at each moment?
m4 <- df_roll3 %>%
  select(date, `Daily Rebal`=roll_rebal, `Annual Rebal`=roll_annual, `Buy & Hold`=roll_bah) %>%
  pivot_longer(-date) %>%
  mutate(name=factor(name, levels=names(COL))) %>%
  ggplot(aes(date, value, colour=name)) +
  geom_hline(yintercept=0, linetype="dashed", colour="#9ca3af", linewidth=0.3) +
  geom_line(linewidth=0.7) +
  scale_colour_manual(values=COL) +
  scale_x_date(date_labels="%Y") +
  scale_y_continuous(labels=percent_format(accuracy=1)) +
  labs(title="M4 · Rolling 3-year annualised return: daily rebal rarely falls behind",
       subtitle="Each point = compound annual return of trailing 3 years — regime shading shows why dips occur",
       x=NULL, y="Trailing 3Y ann. return", colour=NULL) +
  .theme_col(leg="none")
m4 <- .shading(m4)

# ── RIGHT COLUMN ──────────────────────────────────────────────────────────────

# M5: Cumulative drift — hero panel with shaded area
m5 <- df %>%
  select(date, `Buy & Hold`=drift_cum, `Annual Rebal`=drift_cum_annual) %>%
  pivot_longer(-date) %>%
  ggplot(aes(date, value, colour=name)) +
  geom_hline(yintercept=0, linetype="dashed", colour="#9ca3af", linewidth=0.5) +
  geom_ribbon(
    data = df %>% mutate(ymin=pmin(drift_cum,0), ymax=pmax(drift_cum,0)),
    aes(x=date, ymin=ymin, ymax=ymax), inherit.aes=FALSE,
    fill="#7c3aed", alpha=0.12
  ) +
  geom_line(linewidth=0.9) +
  scale_colour_manual(values=c("Annual Rebal"="#f59e0b", "Buy & Hold"="#7c3aed")) +
  scale_x_date(date_labels="%Y") +
  scale_y_continuous(labels=percent_format(accuracy=0.1)) +
  labs(title="M5 · Cumulative drift = BaH \u2212 Daily-Rebal (running total)",
       subtitle=paste0("Positive = momentum paid (BaH wins)  |  Negative = rebalancing bonus paid\n",
                       "Shaded area = magnitude of rebalancing bonus accumulated to date"),
       x=NULL, y="Cumulative drift", colour=NULL) +
  .theme_col(base=12)
m5 <- .shading(m5)

# M6: Implied SPY weight — why drift exists
m6 <- df %>%
  select(date, `Buy & Hold`=spy_weight_bah, `Annual Rebal`=spy_weight_annual) %>%
  pivot_longer(-date) %>%
  ggplot(aes(date, value, colour=name)) +
  geom_hline(yintercept=W_EQ, linetype="dashed", colour="#9ca3af", linewidth=0.4) +
  geom_line(linewidth=0.7) +
  scale_colour_manual(values=c("Annual Rebal"="#f59e0b", "Buy & Hold"="#7c3aed")) +
  scale_x_date(date_labels="%Y") +
  scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0.40, 0.90)) +
  labs(title=sprintf("M6 · SPY weight: Buy & Hold drifts to ~80%% — Annual rebal resets to %.0f%%", W_EQ*100),
       subtitle="This is the source of drift: BaH carries excess equity exposure into every Fall",
       x=NULL, y="SPY weight in portfolio", colour=NULL) +
  .theme_col(leg="none")
m6 <- .shading(m6)

# M7: Annual drift cost by year
m7 <- annual_drift_tbl %>%
  filter(strategy == "Buy & Hold") %>%
  mutate(bar_fill = case_when(
    year %in% fall_years & drift_yr < 0 ~ "#ef4444",
    drift_yr < 0                         ~ "#fca5a5",
    TRUE                                 ~ "#86efac"
  )) %>%
  ggplot(aes(year, drift_yr * 100, fill=bar_fill)) +
  geom_col(show.legend=FALSE) +
  geom_hline(yintercept=0, colour="#9ca3af", linewidth=0.3) +
  scale_fill_identity() +
  scale_x_continuous(breaks=scales::pretty_breaks(n=8)) +
  scale_y_continuous(labels=function(x) paste0(x, "%")) +
  labs(title="M7 · Annual drift cost (Buy & Hold \u2212 Daily Rebal): Falls deliver the blows",
       subtitle="Dark red = Fall year  |  Each bar = sum of daily drifts that year",
       x=NULL, y="Yearly drift") +
  .theme_col(leg="none")

# M8: Rolling 12-month drift
m8 <- df_roll %>%
  select(date, `Buy & Hold`=roll_bah, `Annual Rebal`=roll_ann) %>%
  pivot_longer(-date) %>%
  ggplot(aes(date, value, colour=name)) +
  geom_hline(yintercept=0, linetype="dashed", colour="#9ca3af", linewidth=0.4) +
  geom_line(linewidth=0.7) +
  scale_colour_manual(values=c("Annual Rebal"="#f59e0b", "Buy & Hold"="#7c3aed")) +
  scale_x_date(date_labels="%Y") +
  scale_y_continuous(labels=percent_format(accuracy=0.1)) +
  labs(title="M8 · Rolling 12-month drift: turns negative at Fall onset, recovers in Recovery",
       subtitle="Crosses zero exactly as regime transitions — rebalancing bonus is regime-conditional",
       x=NULL, y="Trailing 12m drift", colour=NULL) +
  .theme_col(leg="none")
m8 <- .shading(m8)

# ── Summary statistics table ───────────────────────────────────────────────────
.cagr    <- function(x) { x <- na.omit(x); (tail(x,1)/head(x,1))^(252/length(x)) - 1 }
.ann_vol <- function(r)   sd(r, na.rm=TRUE) * sqrt(252)
.max_dd  <- function(x) { x <- na.omit(x); pk <- cummax(x); min((x-pk)/pk) }

stats_num <- tibble(
  Strategy   = factor(c("Daily Rebal","Annual Rebal","Buy & Hold"), levels=names(COL)),
  `Final $1` = c(tail(df$rebal,1),  tail(df$annual,1),  tail(na.omit(df$bah),1)),
  `Ann Ret`  = c(.cagr(df$rebal),   .cagr(df$annual),   .cagr(df$bah)),
  `Ann Vol`  = c(.ann_vol(df$r_rebal), .ann_vol(df$r_annual), .ann_vol(df$r_bah)),
  `Max DD`   = c(.max_dd(df$rebal),  .max_dd(df$annual),  .max_dd(df$bah))
) %>%
  mutate(
    Sharpe = `Ann Ret` / `Ann Vol`,
    Calmar = `Ann Ret` / abs(`Max DD`)
  )

# Higher-is-better metrics (Max DD negative so higher = less severe = better)
hib <- c("Final $1","Ann Ret","Sharpe","Calmar")
lib <- c("Ann Vol","Max DD")

stats_long <- stats_num %>%
  pivot_longer(-Strategy, names_to="Metric", values_to="val_num") %>%
  group_by(Metric) %>%
  mutate(
    rank3 = if_else(Metric %in% hib, rank(val_num), rank(-val_num)),
    tile  = case_when(rank3 == 3 ~ "#bbf7d0",
                      rank3 == 2 ~ "#fef9c3",
                      TRUE       ~ "#fecaca"),
    # formatted label
    label = case_when(
      Metric == "Final $1" ~ sprintf("$%.2f",   val_num),
      Metric %in% c("Ann Ret","Ann Vol","Max DD") ~ sprintf("%.1f%%", val_num*100),
      TRUE                  ~ sprintf("%.2f",   val_num)
    )
  ) %>%
  ungroup() %>%
  mutate(
    Metric   = factor(Metric,   levels=c("Final $1","Ann Ret","Ann Vol","Max DD","Sharpe","Calmar")),
    Strategy = factor(Strategy, levels=rev(names(COL)))
  )

# Strategy colour dots for y-axis labels (via point geom on a dummy column)
strat_cols <- c("Daily Rebal"="#3b82f6","Annual Rebal"="#f59e0b","Buy & Hold"="#7c3aed")

mt <- stats_long %>%
  ggplot(aes(Metric, Strategy)) +
  geom_tile(aes(fill=tile), colour="white", linewidth=1.2) +
  geom_text(aes(label=label, colour=Strategy), size=3.8, fontface="bold") +
  scale_fill_identity() +
  scale_colour_manual(values=strat_cols, guide="none") +
  labs(title    = "T · Summary statistics: three rebalancing strategies",
       subtitle = paste0(
         "Green = best in metric  |  Yellow = middle  |  Red = worst\n",
         "Ann Ret & Vol = annualised  |  Max DD = peak-to-trough  |  ",
         "Sharpe = Ann Ret / Ann Vol  |  Calmar = Ann Ret / |Max DD|"
       ),
       x=NULL, y=NULL) +
  theme_minimal(base_size=11) +
  theme(panel.grid      = element_blank(),
        axis.text.x     = element_text(face="bold", colour="#374151", size=10),
        axis.text.y     = element_text(face="bold", size=10),
        plot.title      = element_text(face="bold"),
        plot.subtitle   = element_text(colour="#6b7280", size=9),
        legend.position = "none")

# ── Combine into two columns + table ──────────────────────────────────────────
left_col  <- (m1 / m2 / m3 / m4) + plot_layout(heights=c(2, 1.2, 1.2, 1.2))
right_col <- (m5 / m6 / m7 / m8) + plot_layout(heights=c(2, 1.2, 1.2, 1.2))

main_view <- ((left_col | right_col) / mt) +
  plot_layout(heights=c(8, 1.4)) +
  plot_annotation(
    title    = "Rebalancing Drift — SPY/IEF 60/40",
    subtitle = paste0(
      "LEFT (M1\u2013M4): Daily Rebalancing — wealth, drawdown, annual returns, rolling performance\n",
      "RIGHT (M5\u2013M8): Drift = cost of not rebalancing — cumulative, weight source, annual damage, rolling signal\n",
      "BOTTOM (T): summary statistics  \u2502  Fall = red  \u2502  Recovery = green  \u2502  ",
      "Drift \u2261 Buy & Hold \u2212 Daily-Rebal return"
    ),
    theme = theme(
      plot.title    = element_text(size=15, face="bold"),
      plot.subtitle = element_text(size=9,  colour="#6b7280")
    )
  )

if (!isTRUE(getOption("knitr.in.progress"))) print(main_view)

# ══════════════════════════════════════════════════════════════════════════════
# APPENDIX — full 4-act pedagogical breakdown
# ══════════════════════════════════════════════════════════════════════════════

appendix <- (p1 | p2) / (p3 | p4) / (p5 | p6) / (p7 | p8) +
  plot_annotation(
    title    = "Appendix: Pedagogical Breakdown — Rebalancing Drift SPY/IEF 60/40",
    subtitle = paste0(
      "Act 1: why drift builds  \u2502  Act 2: how it hurts in Falls  \u2502  ",
      "Act 3: the net result  \u2502  Act 4: when it happened\n",
      "Fall = red shading  \u2502  Recovery = green shading  \u2502  ",
      "Drift \u2261 BaH return \u2212 Daily-Rebal return  (positive = momentum paid)"
    ),
    theme = theme(
      plot.title    = element_text(size=14, face="bold"),
      plot.subtitle = element_text(size=9,  colour="#6b7280")
    )
  )

if (!isTRUE(getOption("knitr.in.progress"))) print(appendix)

# ══════════════════════════════════════════════════════════════════════════════
# PAIRWISE COMPARISON BUILDER
# Produces a labelled 4+4+table figure for any strategy vs Buy & Hold.
#
# Panels: prefix1–prefix4 (left col: wealth/DD/annual/rolling)
#         prefix5–prefix8 (right col: drift/SPY-weight/ann-drift/roll-drift)
#         T_prefix         (bottom: summary stats table)
# ══════════════════════════════════════════════════════════════════════════════

build_pairwise <- function(
    strat_label,           # display name, e.g. "Daily Rebal"
    prefix,                # panel prefix, e.g. "D"
    cum_strat,             # cumulative wealth vector (length n, aligned to dates)
    ret_strat,             # daily log return vector  (NA-padded, length n)
    spy_w_strat,           # implied SPY weight vector (length n)
    col_strat,             # hex colour for strategy line
    cum_bah     = df$bah,
    ret_bah     = df$ret_bah,
    spy_w_bah   = df$spy_weight_bah,
    bah_label   = "Buy & Hold",
    col_bah     = "#7c3aed"
) {
  # ── helpers ─────────────────────────────────────────────────────────────────
  run_dd <- function(x) {
    x   <- replace(x, is.na(x), NA_real_)
    pk  <- cummax(ifelse(is.na(x), -Inf, x))
    pk[is.infinite(pk)] <- NA_real_
    ifelse(is.na(pk) | pk == 0, NA_real_, (x - pk) / pk)
  }
  .cagr    <- function(x) { x <- na.omit(x); (tail(x,1)/head(x,1))^(252/length(x)) - 1 }
  .ann_vol <- function(r)   sd(r, na.rm=TRUE) * sqrt(252)
  .max_dd  <- function(x) { x <- na.omit(x); pk <- cummax(x); min((x-pk)/pk) }
  .sh  <- function(p) {
    p + geom_rect(data=regime_rect,
                  aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf, fill=fill),
                  alpha=0.18, inherit.aes=FALSE) +
      scale_fill_identity()
  }
  .tc <- function(base=11, leg="top") {
    theme_minimal(base_size=base) +
      theme(legend.position=leg, panel.grid.minor=element_blank(),
            axis.text.y=element_text(colour="#1B3A6B", face="bold"),
            plot.title=element_text(face="bold", size=base+1),
            plot.subtitle=element_text(size=base-1, colour="#6b7280"))
  }
  COL2 <- setNames(c(col_strat, col_bah), c(strat_label, bah_label))

  # ── derived series ───────────────────────────────────────────────────────────
  drift_d   <- ret_strat - ret_bah
  drift_cum <- cumsum(replace(drift_d, is.na(drift_d), 0))
  roll_d12  <- zoo::rollsum(replace(drift_d, is.na(drift_d), 0),
                             k=252L, fill=NA, align="right")
  r_s <- log(cum_strat) - dplyr::lag(log(cum_strat))
  r_b <- log(replace(cum_bah, is.na(cum_bah), NA)) -
           dplyr::lag(log(replace(cum_bah, is.na(cum_bah), NA)))
  roll_s3 <- zoo::rollapply(replace(r_s, is.na(r_s), 0), 756L,
                              function(x) exp(sum(x))^(252/756)-1,
                              fill=NA, align="right")
  roll_b3 <- zoo::rollapply(replace(r_b, is.na(r_b), 0), 756L,
                              function(x) exp(sum(x))^(252/756)-1,
                              fill=NA, align="right")

  fall_yrs <- regime_rect %>%
    filter(fill == "#fca5a5") %>%
    mutate(yr = as.integer(format(xmin, "%Y"))) %>%
    pull(yr) %>% unique()

  ann_tbl <- tibble(date=dates, yr=as.integer(format(dates, "%Y")),
                    r_s=r_s, r_b=r_b, drift_d=drift_d) %>%
    group_by(yr) %>%
    summarise(strat_a = exp(sum(r_s, na.rm=TRUE)) - 1,
              bah_a   = exp(sum(r_b, na.rm=TRUE)) - 1,
              drift_y = sum(drift_d, na.rm=TRUE),
              .groups="drop") %>%
    mutate(bar_fill = case_when(
      yr %in% fall_yrs & drift_y < 0 ~ "#ef4444",
      drift_y < 0 ~ "#fca5a5",
      TRUE ~ "#86efac"))

  df_p <- tibble(date=dates, strat=cum_strat, bah=cum_bah,
                 dd_s=run_dd(cum_strat), dd_b=run_dd(replace(cum_bah,is.na(cum_bah),NA)),
                 spy_s=spy_w_strat, spy_b=spy_w_bah,
                 drift_cum=drift_cum, roll_d12=roll_d12,
                 roll_s3=roll_s3, roll_b3=roll_b3)

  # ── LEFT COLUMN ──────────────────────────────────────────────────────────────
  lev <- names(COL2)

  l1 <- df_p %>%
    select(date, !!strat_label:=strat, !!bah_label:=bah) %>%
    pivot_longer(-date) %>% mutate(name=factor(name,lev)) %>%
    ggplot(aes(date,value,colour=name)) + geom_line(linewidth=0.9) +
    scale_colour_manual(values=COL2) +
    scale_y_continuous(labels=dollar_format(prefix="$")) +
    scale_x_date(date_labels="%Y") +
    labs(title=sprintf("%s1 \u00b7 Wealth: %s vs Buy & Hold", prefix, strat_label),
         subtitle="$1 at inception",  x=NULL, y="Wealth ($)", colour=NULL) +
    .tc(base=12)
  l1 <- .sh(l1)

  l2 <- df_p %>%
    select(date, !!strat_label:=dd_s, !!bah_label:=dd_b) %>%
    pivot_longer(-date) %>% mutate(name=factor(name,lev)) %>%
    ggplot(aes(date,value,colour=name)) +
    geom_hline(yintercept=0, colour="#9ca3af", linewidth=0.3) +
    geom_line(linewidth=0.7) +
    scale_colour_manual(values=COL2) + scale_x_date(date_labels="%Y") +
    scale_y_continuous(labels=percent_format(accuracy=1)) +
    labs(title=sprintf("%s2 \u00b7 Drawdown: %s vs Buy & Hold", prefix, strat_label),
         subtitle="Shallower = more defensive", x=NULL, y="Drawdown from peak", colour=NULL) +
    .tc(leg="none")
  l2 <- .sh(l2)

  l3 <- ann_tbl %>%
    select(year=yr, !!strat_label:=strat_a, !!bah_label:=bah_a) %>%
    pivot_longer(-year, names_to="strategy", values_to="ret") %>%
    mutate(strategy=factor(strategy,lev)) %>%
    ggplot(aes(year,ret,fill=strategy)) +
    geom_col(position="dodge", width=0.75, show.legend=FALSE) +
    geom_hline(yintercept=0, colour="#9ca3af", linewidth=0.3) +
    scale_fill_manual(values=COL2) +
    scale_y_continuous(labels=percent_format(accuracy=1)) +
    scale_x_continuous(breaks=scales::pretty_breaks(n=8)) +
    labs(title=sprintf("%s3 \u00b7 Annual returns: %s vs Buy & Hold", prefix, strat_label),
         subtitle="Gap concentrates in Fall years", x=NULL, y="Annual return") +
    .tc(leg="none") + theme(axis.text.x=element_text(angle=45, hjust=1))

  l4 <- df_p %>%
    select(date, !!strat_label:=roll_s3, !!bah_label:=roll_b3) %>%
    pivot_longer(-date) %>% mutate(name=factor(name,lev)) %>%
    ggplot(aes(date,value,colour=name)) +
    geom_hline(yintercept=0, linetype="dashed", colour="#9ca3af", linewidth=0.3) +
    geom_line(linewidth=0.7) +
    scale_colour_manual(values=COL2) + scale_x_date(date_labels="%Y") +
    scale_y_continuous(labels=percent_format(accuracy=1)) +
    labs(title=sprintf("%s4 \u00b7 Rolling 3Y ann. return: %s vs Buy & Hold", prefix, strat_label),
         subtitle="Trailing 3-year compound annual return at each date",
         x=NULL, y="Trailing 3Y ann. return", colour=NULL) +
    .tc(leg="none")
  l4 <- .sh(l4)

  # ── RIGHT COLUMN ─────────────────────────────────────────────────────────────
  r5 <- df_p %>%
    ggplot(aes(date, drift_cum)) +
    geom_hline(yintercept=0, linetype="dashed", colour="#9ca3af", linewidth=0.5) +
    geom_ribbon(aes(ymin=pmin(drift_cum,0), ymax=pmax(drift_cum,0)),
                fill=col_strat, alpha=0.15) +
    geom_line(colour=col_strat, linewidth=0.9) +
    scale_x_date(date_labels="%Y") +
    scale_y_continuous(labels=percent_format(accuracy=0.1)) +
    labs(title=sprintf("%s5 \u00b7 Cumulative drift = %s \u2212 Buy & Hold", prefix, strat_label),
         subtitle="Positive = strategy wins that day  |  Negative = BaH wins",
         x=NULL, y="Cumulative drift") +
    .tc(base=12, leg="none")
  r5 <- .sh(r5)

  r6 <- df_p %>%
    select(date, !!strat_label:=spy_s, !!bah_label:=spy_b) %>%
    pivot_longer(-date) %>% mutate(name=factor(name,lev)) %>%
    ggplot(aes(date,value,colour=name)) +
    geom_hline(yintercept=W_EQ, linetype="dashed", colour="#9ca3af", linewidth=0.4) +
    geom_line(linewidth=0.7) +
    scale_colour_manual(values=COL2) + scale_x_date(date_labels="%Y") +
    scale_y_continuous(labels=percent_format(accuracy=1), limits=c(0.35, 0.95)) +
    labs(title=sprintf("%s6 \u00b7 SPY weight: %s vs Buy & Hold", prefix, strat_label),
         subtitle=sprintf("Dashed = %.0f%% target — excess weight drives drift", W_EQ*100),
         x=NULL, y="SPY weight", colour=NULL) +
    .tc(leg="none")
  r6 <- .sh(r6)

  r7 <- ann_tbl %>%
    ggplot(aes(yr, drift_y*100, fill=bar_fill)) +
    geom_col(show.legend=FALSE) +
    geom_hline(yintercept=0, colour="#9ca3af", linewidth=0.3) +
    scale_fill_identity() +
    scale_x_continuous(breaks=scales::pretty_breaks(n=8)) +
    scale_y_continuous(labels=function(x) paste0(x, "%")) +
    labs(title=sprintf("%s7 \u00b7 Annual drift (%s \u2212 Buy & Hold)", prefix, strat_label),
         subtitle="Dark red = Fall year  |  Each bar = yearly sum of daily drifts",
         x=NULL, y="Yearly drift") +
    .tc(leg="none")

  r8 <- df_p %>%
    ggplot(aes(date, roll_d12)) +
    geom_hline(yintercept=0, linetype="dashed", colour="#9ca3af", linewidth=0.4) +
    geom_line(colour=col_strat, linewidth=0.7) +
    scale_x_date(date_labels="%Y") +
    scale_y_continuous(labels=percent_format(accuracy=0.1)) +
    labs(title=sprintf("%s8 \u00b7 Rolling 12m drift: %s \u2212 Buy & Hold", prefix, strat_label),
         subtitle="Negative at Fall onset — rebalancing bonus is regime-conditional",
         x=NULL, y="Trailing 12m drift") +
    .tc(leg="none")
  r8 <- .sh(r8)

  # ── SUMMARY TABLE ────────────────────────────────────────────────────────────
  hib <- c("Final $1","Ann Ret","Sharpe","Calmar")
  stats_n <- tibble(
    Strategy   = factor(c(strat_label, bah_label), levels=c(strat_label, bah_label)),
    `Final $1` = c(tail(na.omit(cum_strat),1),   tail(na.omit(cum_bah),1)),
    `Ann Ret`  = c(.cagr(cum_strat),              .cagr(cum_bah)),
    `Ann Vol`  = c(.ann_vol(ret_strat),            .ann_vol(ret_bah)),
    `Max DD`   = c(.max_dd(cum_strat),
                   .max_dd(replace(cum_bah, is.na(cum_bah), NA)))
  ) %>% mutate(Sharpe=`Ann Ret`/`Ann Vol`, Calmar=`Ann Ret`/abs(`Max DD`))

  tbl <- stats_n %>%
    pivot_longer(-Strategy, names_to="Metric", values_to="val_num") %>%
    group_by(Metric) %>%
    mutate(
      rank2 = if_else(Metric %in% hib, rank(val_num), rank(-val_num)),
      tile  = if_else(rank2==2, "#bbf7d0", "#fecaca"),
      label = case_when(
        Metric == "Final $1" ~ sprintf("$%.2f", val_num),
        Metric %in% c("Ann Ret","Ann Vol","Max DD") ~ sprintf("%.1f%%", val_num*100),
        TRUE ~ sprintf("%.2f", val_num)
      )
    ) %>% ungroup() %>%
    mutate(Metric   = factor(Metric, levels=c("Final $1","Ann Ret","Ann Vol","Max DD","Sharpe","Calmar")),
           Strategy = factor(Strategy, levels=rev(c(strat_label, bah_label)))) %>%
    ggplot(aes(Metric, Strategy)) +
    geom_tile(aes(fill=tile), colour="white", linewidth=1.2) +
    geom_text(aes(label=label, colour=Strategy), size=3.8, fontface="bold") +
    scale_fill_identity() +
    scale_colour_manual(values=setNames(c(col_strat,col_bah),
                                         c(strat_label,bah_label)), guide="none") +
    labs(title=sprintf("T_%s \u00b7 Summary: %s vs Buy & Hold", prefix, strat_label),
         subtitle="Green = better  |  Red = worse  |  Sharpe = Ann Ret/Vol  |  Calmar = Ann Ret/|Max DD|",
         x=NULL, y=NULL) +
    theme_minimal(base_size=11) +
    theme(panel.grid=element_blank(),
          axis.text.x=element_text(face="bold", colour="#374151", size=10),
          axis.text.y=element_text(face="bold", size=10),
          plot.title=element_text(face="bold"),
          plot.subtitle=element_text(colour="#6b7280", size=9),
          legend.position="none")

  # ── ASSEMBLE ─────────────────────────────────────────────────────────────────
  left_col  <- (l1/l2/l3/l4) + plot_layout(heights=c(2,1.2,1.2,1.2))
  right_col <- (r5/r6/r7/r8) + plot_layout(heights=c(2,1.2,1.2,1.2))

  ((left_col | right_col) / tbl) +
    plot_layout(heights=c(8,1.4)) +
    plot_annotation(
      title    = sprintf("Rebalancing Drift — %s vs Buy & Hold  (SPY/IEF 60/40)", strat_label),
      subtitle = paste0(
        sprintf("LEFT (%s1\u2013%s4): wealth, drawdown, annual returns, rolling 3Y return\n", prefix, prefix),
        sprintf("RIGHT (%s5\u2013%s8): cumulative drift, SPY weight, annual drift, rolling 12m drift\n", prefix, prefix),
        sprintf("BOTTOM (T_%s): summary stats  \u2502  Fall = red  \u2502  Recovery = green  \u2502  ", prefix),
        sprintf("Drift \u2261 %s \u2212 Buy & Hold", strat_label)
      ),
      theme=theme(plot.title   =element_text(size=14, face="bold"),
                  plot.subtitle=element_text(size=9,  colour="#6b7280"))
    )
}

# ══════════════════════════════════════════════════════════════════════════════
# PAIRWISE FIGURES  2–5
# ══════════════════════════════════════════════════════════════════════════════

# Store figures as named objects so the Rmd can access them individually
fig_D    <- build_pairwise(
  strat_label = "Daily Rebal",   prefix = "D",
  cum_strat   = df$rebal,        ret_strat   = df$ret_rebal,
  spy_w_strat = df$spy_weight_daily,
  col_strat   = "#3b82f6"
)
fig_Q    <- build_pairwise(
  strat_label = "Quarterly Rebal",  prefix = "Q",
  cum_strat   = df$quarterly,       ret_strat   = df$ret_qtr,
  spy_w_strat = df$spy_weight_qtr,
  col_strat   = "#10b981"
)
fig_AR   <- build_pairwise(
  strat_label = "Annual Rebal",  prefix = "AR",
  cum_strat   = df$annual,       ret_strat   = df$ret_annual,
  spy_w_strat = df$spy_weight_annual,
  col_strat   = "#f59e0b"
)
fig_TR10 <- build_pairwise(
  strat_label = "Threshold Rebal (\u00b110%)",  prefix = "TR10",
  cum_strat   = df$thr10,                        ret_strat   = df$ret_thr10,
  spy_w_strat = df$spy_weight_thr10,
  col_strat   = "#f97316"
)
fig_TR5  <- build_pairwise(
  strat_label = "Threshold Rebal (\u00b15%)",   prefix = "TR5",
  cum_strat   = df$thr05,                        ret_strat   = df$ret_thr05,
  spy_w_strat = df$spy_weight_thr05,
  col_strat   = "#dc2626"
)

# Print all figures when run interactively — suppressed when knitting
if (!isTRUE(getOption("knitr.in.progress"))) {
  print(main_view)
  print(appendix)
  print(fig_D)
  print(fig_Q)
  print(fig_AR)
  print(fig_TR10)
  print(fig_TR5)
}
