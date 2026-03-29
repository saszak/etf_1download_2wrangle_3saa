################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : scripts_daa_saa_taa/05_dd_stabilizer.R
# Purpose : Find the best DrawDown stabilizers to overlay on a passive 60/40.
#
# PREMISE
#   Passive SPY/IEF 60/40 delivers adequate return.
#   Objective: reduce MaxDD and Fall-regime drawdown with minimum return sacrifice.
#
# FRAMEWORK
#   For each candidate instrument at allocation weight w (0 → 25%):
#     portfolio = (1 - w) × passive_60_40 + w × instrument
#   This is funded from the passive portfolio proportionally (SPY/IEF both shrink).
#
# SCREENING METRICS (per instrument)
#   1. dd_reduction   : MaxDD(passive) − MaxDD(blended at w=10%)       → bigger = better
#   2. return_drag    : AnnRet(passive) − AnnRet(blended at w=10%)     → smaller = better
#   3. insurance_ratio: dd_reduction / max(return_drag, 0.001)          → efficiency
#   4. fall_corr      : rolling correlation vs passive_60/40 in Fall    → lower = better
#   5. own_maxdd      : instrument standalone MaxDD                     → lower = better
#
# OUTPUTS
#   stabilizer_screen   tibble — all metrics per ticker at w=10%
#   dd_frontier_data    tibble — MaxDD + return at each w for top candidates
#   p_insurance_quad    ggplot — x=return_drag, y=dd_reduction, bubble=fall_corr
#   p_dd_frontier       ggplot — DD frontier: w from 0→25% for top picks
#   p_stabilizer_wealth ggplot — cumulative wealth: passive vs + best stabilizer
#   p_stabilizer_dd     ggplot — drawdown: passive vs + best stabilizer
################################################################################

library(tidyverse)
library(xts)
library(PerformanceAnalytics)
library(patchwork)
library(scales)
library(here)

if (!exists("project_tree"))       source(here("project_tree.R"))
if (!exists("etf_metadata"))       source(here(project_tree$scripts$init))
if (!exists("xts_ret"))            source(here(project_tree$scripts$wrangle))
if (!exists("build_regime_table")) source(here("scripts_spy_dd_regime/spy_dd_regime.R"))
if (!exists("rt") || !is.data.frame(rt))
  rt <- build_regime_table(xts_ret[, "SPY"])
if (!exists("SAA_60_40"))          source(here("scripts_daa_saa_taa/01_saa_baseline.R"))

# ==============================================================================
# 1. PASSIVE BASELINE
# ==============================================================================
# SPY 60% + IEF 40% — the benchmark we are trying to protect

passive_ret <- xts(
  as.numeric(xts_ret[, "SPY"]) * 0.60 +
  as.numeric(xts_ret[, "IEF"]) * 0.40,
  order.by = index(xts_ret)
)
colnames(passive_ret) <- "Passive_60_40"

.ann_ret <- function(r) {
  r <- as.numeric(r[!is.na(r)])
  prod(1 + r)^(252 / length(r)) - 1
}
.ann_vol <- function(r) sd(as.numeric(r), na.rm = TRUE) * sqrt(252)
.max_dd  <- function(r) {
  w <- cumprod(1 + as.numeric(r[!is.na(r)]))
  min((w - cummax(w)) / cummax(w))
}

passive_ann_ret <- .ann_ret(passive_ret)
passive_max_dd  <- .max_dd(passive_ret)
passive_ann_vol <- .ann_vol(passive_ret)

message(sprintf("📊 Passive 60/40 baseline — Return: %+.1f%%  MaxDD: %.1f%%  Vol: %.1f%%",
                passive_ann_ret * 100, passive_max_dd * 100, passive_ann_vol * 100))

# ==============================================================================
# 2. BLENDED PORTFOLIO HELPER
# ==============================================================================
# Replace fraction w of the passive portfolio with instrument x.
# Both SPY and IEF shrink proportionally; x makes up the difference.

blend <- function(passive_r, instrument_r, w) {
  common   <- merge(passive_r, instrument_r, join = "inner")
  p_r      <- as.numeric(common[, 1])
  x_r      <- as.numeric(common[, 2])
  blended  <- xts((1 - w) * p_r + w * x_r, order.by = index(common))
  colnames(blended) <- "Blended"
  blended
}

# ==============================================================================
# 3. FULL UNIVERSE SCREEN AT w = 10%
# ==============================================================================

SCREEN_WEIGHT <- 0.10   # evaluate all instruments at this overlay weight

# Regime daily labels for correlation conditioning
regime_daily <- rt %>%
  rowwise() %>%
  mutate(date = list(seq(as.Date(xmin), as.Date(xmax), by = "day"))) %>%
  unnest(date) %>%
  select(date, regime) %>%
  ungroup()

message(sprintf("🔍 Screening %d tickers as DD stabilizers (w = %.0f%%)...",
                ncol(xts_ret), SCREEN_WEIGHT * 100))

stabilizer_screen <- purrr::map_dfr(colnames(xts_ret), function(tk) {

  if (tk %in% c("SPY", "IEF")) return(NULL)
  tk_r <- xts_ret[, tk]

  # Align to passive
  common <- merge(passive_ret, tk_r, join = "inner")
  if (nrow(common) < 252) return(NULL)   # need at least 1 year
  p_r  <- common[, 1]
  x_r  <- common[, 2]

  # Blended at screen weight
  bl   <- blend(p_r, x_r, SCREEN_WEIGHT)

  # Core metrics
  dd_reduction  <- .max_dd(p_r) - .max_dd(bl)    # positive = stabiliser improved DD
  return_drag   <- .ann_ret(p_r) - .ann_ret(bl)  # positive = stabiliser cost return
  own_maxdd     <- .max_dd(x_r)
  own_ret       <- .ann_ret(x_r)
  own_vol       <- .ann_vol(x_r)

  # Insurance ratio: how much DD improvement per unit of return drag
  insurance_ratio <- dd_reduction / max(abs(return_drag), 0.001)

  # Fall-regime correlation with passive
  dates      <- as.Date(index(common))
  p_num      <- as.numeric(p_r)
  x_num      <- as.numeric(x_r)
  fall_idx   <- dates %in% (regime_daily %>% filter(regime == "Fall") %>% pull(date))
  fall_corr  <- if (sum(fall_idx) > 20)
                  cor(p_num[fall_idx], x_num[fall_idx], use = "complete.obs")
                else NA_real_

  full_corr  <- cor(p_num, x_num, use = "complete.obs")

  tibble(
    ticker          = tk,
    dd_reduction    = dd_reduction,
    return_drag     = return_drag,
    insurance_ratio = insurance_ratio,
    fall_corr       = fall_corr,
    full_corr       = full_corr,
    own_maxdd       = own_maxdd,
    own_ret         = own_ret,
    own_vol         = own_vol,
    n_days          = nrow(common)
  )
}) %>%
  left_join(etf_metadata %>% select(ticker, name, asset_class, pf_function),
            by = "ticker") %>%
  arrange(desc(insurance_ratio))

message("✅ Screen complete.")

# ==============================================================================
# 4. CONSOLE SUMMARY — TOP 20 STABILIZERS
# ==============================================================================

message("\n🛡️  Top 20 DD Stabilizers (ranked by insurance ratio at w=10%):\n")
stabilizer_screen %>%
  slice_head(n = 20) %>%
  mutate(
    dd_reduction    = sprintf("%+.1f pp", dd_reduction * 100),
    return_drag     = sprintf("%+.1f pp", return_drag  * 100),
    insurance_ratio = sprintf("%.2f",     insurance_ratio),
    fall_corr       = sprintf("%+.2f",    fall_corr),
    own_maxdd       = sprintf("%.1f%%",   own_maxdd * 100),
    own_ret         = sprintf("%+.1f%%",  own_ret   * 100)
  ) %>%
  select(ticker, asset_class, dd_reduction, return_drag, insurance_ratio,
         fall_corr, own_maxdd, own_ret) %>%
  print(n = 20)

message("\n❌ Worst (destroys DD instead of reducing it):\n")
stabilizer_screen %>%
  filter(dd_reduction < 0) %>%
  slice_tail(n = 10) %>%
  select(ticker, asset_class, dd_reduction, return_drag, insurance_ratio, fall_corr) %>%
  mutate(across(c(dd_reduction, return_drag), ~ sprintf("%+.1f pp", . * 100))) %>%
  print()

# ==============================================================================
# 5. DD FRONTIER — top 8 picks across weights 0 → 25%
# ==============================================================================

TOP_N    <- 8L
top_picks <- stabilizer_screen %>%
  filter(!is.na(fall_corr), dd_reduction > 0) %>%
  slice_head(n = TOP_N) %>%
  pull(ticker)

sweep_weights <- seq(0, 0.25, by = 0.02)

dd_frontier_data <- purrr::map_dfr(top_picks, function(tk) {
  tk_r   <- xts_ret[, tk]
  common <- merge(passive_ret, tk_r, join = "inner")
  p_r    <- common[, 1]
  x_r    <- common[, 2]

  purrr::map_dfr(sweep_weights, function(w) {
    bl <- blend(p_r, x_r, w)
    tibble(
      ticker     = tk,
      weight     = w,
      maxdd      = .max_dd(bl),
      ann_ret    = .ann_ret(bl),
      ann_vol    = .ann_vol(bl)
    )
  })
})

# ==============================================================================
# 6. PLOT A — INSURANCE QUADRANT
#    x = return drag (cost),  y = DD reduction (benefit)
#    bubble = fall correlation with 60/40 (lower = more negative = better)
#    Best instruments: top-right quadrant, cold-coloured bubbles
# ==============================================================================

plot_insurance_quad <- function(stabilizer_screen) {
  df <- stabilizer_screen %>%
    filter(!is.na(fall_corr), !is.na(dd_reduction)) %>%
    mutate(
      label_show = dd_reduction > 0.01 | abs(return_drag) < 0.005,
      asset_class = coalesce(asset_class, "Unknown")
    )

  passive_dd  <- passive_max_dd
  passive_ret_val <- passive_ann_ret

  ggplot(df, aes(x = return_drag * 100, y = dd_reduction * 100)) +
    # Quadrant reference lines
    geom_hline(yintercept = 0,  colour = "#6b7280", linewidth = 0.4, linetype = "dashed") +
    geom_vline(xintercept = 0,  colour = "#6b7280", linewidth = 0.4, linetype = "dashed") +
    # Quadrant labels
    annotate("text", x =  3,  y =  12, label = "✓ Best: less DD, some drag",
             colour = "#166534", size = 3.2, fontface = "italic") +
    annotate("text", x = -2,  y =  12, label = "✓✓ Ideal: less DD, free",
             colour = "#14532d", size = 3.2, fontface = "bold") +
    annotate("text", x =  3,  y = -3,  label = "✗ Avoid: costs return AND increases DD",
             colour = "#7f1d1d", size = 3.2, fontface = "italic") +
    # Bubbles
    geom_point(aes(size  = abs(fall_corr),
                   fill  = fall_corr,
                   shape = asset_class),
               alpha = 0.82, colour = "white", stroke = 0.3) +
    scale_fill_gradient2(
      low      = "#1d4ed8",   # deep blue  = negative fall_corr = great hedge
      mid      = "#d1d5db",
      high     = "#dc2626",   # red        = positive fall_corr = sells off with market
      midpoint = 0,
      name     = "Fall ρ\nvs 60/40",
      limits   = c(-1, 1)
    ) +
    scale_size_continuous(range = c(2, 9), guide = "none") +
    scale_shape_manual(
      values = c("Equity" = 21, "FixedIncome" = 22, "Alternative" = 23,
                 "RealAsset" = 24, "MultiAsset" = 25, "Commodity" = 21,
                 "Unknown" = 21),
      name = "Asset class"
    ) +
    # Label best picks
    ggrepel::geom_text_repel(
      data = df %>% filter(dd_reduction > 0.02 | insurance_ratio > 1),
      aes(label = ticker),
      size = 3, fontface = "bold", max.overlaps = 15,
      box.padding = 0.3, colour = "#111827"
    ) +
    scale_x_continuous(labels = function(x) paste0(x, " pp"),
                       name   = "Return drag at 10% allocation (pp/yr)\n← free / pays you      costs you →") +
    scale_y_continuous(labels = function(x) paste0(x, " pp"),
                       name   = "MaxDD reduction at 10% allocation (pp)\n↑ more protection") +
    labs(
      title    = "DD Stabilizer Quadrant — Cost vs Benefit at 10% Overlay",
      subtitle = sprintf(
        "Passive 60/40 baseline — MaxDD: %.1f%%  AnnRet: %+.1f%%\nBlue bubble = negative Fall correlation (true hedge)  |  Bubble size = |Fall ρ|",
        passive_max_dd * 100, passive_ann_ret * 100
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(colour = "grey50", size = 9),
      legend.position  = "right"
    )
}

# ==============================================================================
# 7. PLOT B — DD FRONTIER  (function lives in key_plots/chart_dd_frontier.R)
# ==============================================================================

source(here("key_plots/chart_dd_frontier.R"))

# ==============================================================================
# 8. PLOT C — WEALTH + DD COMPARISON: passive vs best stabilizer combo
# ==============================================================================

plot_stabilizer_vs_passive <- function(top_tickers, overlay_weight = 0.10,
                                       passive_ret, xts_ret, rt) {

  # Equal-weighted blend of top 3 stabilizers
  top3 <- top_tickers[seq_len(min(3, length(top_tickers)))]

  stab_tickers_avail <- top3[top3 %in% colnames(xts_ret)]
  stab_equal_w       <- 1 / length(stab_tickers_avail)

  stab_ret <- Reduce("+",
    lapply(stab_tickers_avail, function(tk)
      as.numeric(xts_ret[, tk]) * stab_equal_w
    )
  )
  stab_xts <- xts(stab_ret, order.by = index(xts_ret))

  common   <- merge(passive_ret, stab_xts, join = "inner")
  p_r      <- common[, 1]
  s_r      <- common[, 2]
  blend_r  <- xts((1 - overlay_weight) * as.numeric(p_r) +
                    overlay_weight      * as.numeric(s_r),
                  order.by = index(common))

  colnames(p_r)     <- "Passive 60/40"
  colnames(blend_r) <- sprintf("60/40 + %.0f%% Stabilizer", overlay_weight * 100)

  # Wealth
  dates  <- as.Date(index(common))
  w1     <- cumprod(1 + as.numeric(p_r))
  w2     <- cumprod(1 + as.numeric(blend_r))
  dd1    <- (w1 - cummax(w1)) / cummax(w1)
  dd2    <- (w2 - cummax(w2)) / cummax(w2)

  wealth_df <- tibble(
    date      = dates,
    Passive   = w1,
    Stabilized = w2
  ) %>%
    pivot_longer(-date, names_to = "portfolio", values_to = "wealth")

  dd_df <- tibble(date = dates, Passive = dd1, Stabilized = dd2) %>%
    pivot_longer(-date, names_to = "portfolio", values_to = "dd")

  regime_rect <- rt %>%
    filter(regime == "Fall") %>%
    mutate(xmin = as.Date(xmin), xmax = as.Date(xmax))

  colours <- c("Passive" = "#9ca3af", "Stabilized" = "#7c3aed")

  # Annotations
  stats <- tibble(
    portfolio = c("Passive", "Stabilized"),
    ret  = c(.ann_ret(p_r), .ann_ret(blend_r)),
    mdd  = c(.max_dd(p_r),  .max_dd(blend_r)),
    vol  = c(.ann_vol(p_r), .ann_vol(blend_r))
  ) %>%
    mutate(label = sprintf("%s\nRet %+.1f%%  DD %.1f%%  Vol %.1f%%",
                           portfolio, ret*100, mdd*100, vol*100))

  last_w <- wealth_df %>% group_by(portfolio) %>% slice_max(date, n=1) %>%
    left_join(stats %>% select(portfolio, label), by = "portfolio")

  pW <- ggplot(wealth_df, aes(date, wealth, colour = portfolio)) +
    geom_rect(data = regime_rect,
              aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf),
              inherit.aes=FALSE, fill="#fca5a5", alpha=0.18) +
    geom_line(linewidth = 0.9) +
    geom_text(data = last_w, aes(label = label),
              hjust = 0, nudge_x = 80, size = 2.8,
              fontface = "bold", lineheight = 0.85) +
    scale_colour_manual(values = colours, guide = "none") +
    scale_y_continuous(labels = dollar_format(prefix = "$"),
                       limits = c(0.7, NA),
                       expand = expansion(mult = c(0, 0.05))) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    coord_cartesian(clip = "off") +
    labs(title    = sprintf("Passive 60/40  vs  60/40 + %.0f%% Stabilizer Overlay",
                            overlay_weight * 100),
         subtitle = sprintf("Stabilizer basket: %s (equal weight)\nPink = Fall regime",
                            paste(stab_tickers_avail, collapse=" + ")),
         x = NULL, y = "Wealth ($)") +
    theme_minimal(base_size = 11) +
    theme(plot.margin = margin(5,160,5,5), panel.grid.minor = element_blank(),
          plot.title  = element_text(face="bold", size=13))

  pDD <- ggplot(dd_df, aes(date, dd * 100, colour = portfolio)) +
    geom_rect(data = regime_rect,
              aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=0),
              inherit.aes=FALSE, fill="#fca5a5", alpha=0.15) +
    geom_hline(yintercept = 0, colour="grey40", linewidth=0.3) +
    geom_line(linewidth = 0.8) +
    scale_colour_manual(values = colours, name = NULL) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_x_date(date_breaks="1 year", date_labels="%Y") +
    labs(title    = "Drawdown Comparison",
         subtitle = "Purple = stabilized  |  Grey = passive  |  Pink = Fall episodes",
         x = NULL, y = "Drawdown (%)") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          legend.position  = "top",
          plot.title       = element_text(face="bold", size=12))

  pW / pDD
}

# ==============================================================================
# 9. RUN ALL PLOTS
# ==============================================================================

if (!requireNamespace("ggrepel", quietly = TRUE))
  install.packages("ggrepel", repos = "https://cloud.r-project.org")

# Save screen
write_rds(stabilizer_screen, here("02_data_processed/dd_stabilizer_screen.rds"))
write_rds(dd_frontier_data,  here("02_data_processed/dd_frontier_data.rds"))
message("💾 Saved: dd_stabilizer_screen, dd_frontier_data")

if (!isTRUE(getOption("knitr.in.progress"))) {

  p_insurance_quad <- plot_insurance_quad(stabilizer_screen)
  p_dd_frontier    <- plot_dd_frontier(dd_frontier_data)
  p_stab_vs_pass   <- plot_stabilizer_vs_passive(
    top_picks, overlay_weight = 0.10, passive_ret, xts_ret, rt
  )

  print(p_insurance_quad)
  print(p_dd_frontier)
  print(p_stab_vs_pass)
}

message("✅ Stage DAA-05 complete: DD stabilizer screen finished.")
