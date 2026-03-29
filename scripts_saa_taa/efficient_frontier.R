# ==============================================================================
# EFFICIENT FRONTIER — Sovereign Universe
# ==============================================================================
#
# Computes and plots the mean-variance efficient frontier for the full
# Sovereign Universe, with:
#   - Individual tickers plotted (risk vs return)
#   - Current SAA portfolio marked
#   - Global Minimum Variance (GMV) portfolio marked
#   - Maximum Sharpe Ratio (tangency) portfolio marked
#
# Optimization: quadprog::solve.QP — long-only, fully-invested
# ==============================================================================

library(tidyverse)
library(xts)
library(scales)
library(ggrepel)
library(quadprog)
library(here)

source(here("scripts/00_init_universe.R"))

xts_ret <- readRDS(here("02_data_processed/xts_ret_returns.rds"))

# ── Parameters ────────────────────────────────────────────────────────────────
RF         <- 0.045          # risk-free rate (annualised) for Sharpe calculation
N_POINTS   <- 80             # number of frontier points
MIN_OBS    <- 252            # minimum trading days required per ticker

# ── SAA portfolio ─────────────────────────────────────────────────────────────
SAA_PORTFOLIO <- tribble(
  ~ticker, ~weight,
  "AGG",   0.35,
  "URTH",  0.25,
  "SPY",   0.25,
  "QQQ",   0.05,
  "XLK",   0.05,
  "SMH",   0.05
)

# ── Filter universe to tickers with sufficient history ────────────────────────
n_obs <- colSums(!is.na(xts_ret))
valid_tickers <- names(n_obs[n_obs >= MIN_OBS])
# Keep only tickers present in etf_metadata
valid_tickers <- intersect(valid_tickers, etf_metadata$ticker)

ret_mat <- xts_ret[, valid_tickers]

# Use pairwise complete observations — align to common date range
# Drop rows where ANY ticker is NA (simplest approach for covariance)
ret_clean <- na.omit(ret_mat)

n <- ncol(ret_clean)
cat(sprintf("Universe: %d tickers  |  %d trading days after na.omit\n",
            n, nrow(ret_clean)))

# ── Annualised inputs ─────────────────────────────────────────────────────────
mu    <- colMeans(ret_clean) * 252
Sigma <- cov(ret_clean) * 252

tickers <- names(mu)

# ── Quadratic programming solver (long-only, fully invested) ──────────────────
# Minimise: 0.5 * w' D w
# s.t.      w' mu    = target   (return constraint)
#           sum(w)   = 1        (budget)
#           w_i      >= 0       (long-only)
#
# solve.QP format: min -d'b + 0.5 b' D b
#   s.t. A' b >= b0
# We use: equality constraints first (meq=2), then inequality (long-only)

min_var_portfolio <- function(target_return, mu, Sigma) {
  n   <- length(mu)
  Dmat <- 2 * Sigma
  dvec <- rep(0, n)

  # Constraint matrix: [sum=1, mu=target, w>=0 each]
  Amat <- cbind(
    rep(1, n),          # budget constraint
    mu,                 # return constraint
    diag(n)             # long-only
  )
  bvec <- c(1, target_return, rep(0, n))

  tryCatch({
    sol <- solve.QP(Dmat, dvec, Amat, bvec, meq = 2)
    list(weights = sol$solution, var = sol$value)
  }, error = function(e) NULL)
}

# ── Frontier: sweep return targets ────────────────────────────────────────────
# Bounds: min = GMV return, max = max individual asset return
gmv_target <- optimize(
  function(r) {
    res <- min_var_portfolio(r, mu, Sigma)
    if (is.null(res)) Inf else res$var
  },
  interval = c(min(mu), max(mu)),
  tol = 1e-6
)$minimum

ret_targets <- seq(gmv_target, quantile(mu, 0.95), length.out = N_POINTS)

frontier_pts <- map_dfr(ret_targets, function(tgt) {
  res <- min_var_portfolio(tgt, mu, Sigma)
  if (is.null(res)) return(NULL)
  tibble(
    ret  = tgt,
    vol  = sqrt(res$var),
    sharpe = (tgt - RF) / sqrt(res$var)
  )
})

# ── Special portfolios ────────────────────────────────────────────────────────
# Global Minimum Variance
gmv_res  <- min_var_portfolio(gmv_target, mu, Sigma)
gmv_wts  <- setNames(gmv_res$weights, tickers)
gmv_pt   <- tibble(ret = gmv_target, vol = sqrt(gmv_res$var),
                   label = "GMV", sharpe = (gmv_target - RF) / sqrt(gmv_res$var))

# Max Sharpe (tangency) — find frontier point with highest Sharpe
tan_idx  <- which.max(frontier_pts$sharpe)
tan_pt   <- frontier_pts[tan_idx, ] %>% mutate(label = "Max Sharpe")

# SAA portfolio
saa_tickers <- SAA_PORTFOLIO$ticker
saa_w       <- SAA_PORTFOLIO$weight
saa_mu      <- sum(saa_w * mu[saa_tickers])
saa_var     <- as.numeric(t(saa_w) %*% Sigma[saa_tickers, saa_tickers] %*% saa_w)
saa_pt      <- tibble(ret = saa_mu, vol = sqrt(saa_var),
                      label = "SAA", sharpe = (saa_mu - RF) / sqrt(saa_var))

cat(sprintf("\nGMV:       vol=%.1f%%  ret=%.1f%%  Sharpe=%.2f\n",
            gmv_pt$vol * 100, gmv_pt$ret * 100, gmv_pt$sharpe))
cat(sprintf("Max Sharpe:vol=%.1f%%  ret=%.1f%%  Sharpe=%.2f\n",
            tan_pt$vol * 100, tan_pt$ret * 100, tan_pt$sharpe))
cat(sprintf("SAA:       vol=%.1f%%  ret=%.1f%%  Sharpe=%.2f\n",
            saa_pt$vol * 100, saa_pt$ret * 100, saa_pt$sharpe))

# ── Plot class: split Equity into sub-groups for better colour distinction ─────
us_broad_sb    <- c("US_Broad", "US_LargeCap", "US_SmallCap", "US_MidCap",
                    "Momentum", "MinVol", "Quality", "DivGrowth", "CashFlow",
                    "Eq_Income", "Value")
us_sector_sb   <- c("Energy", "Financials", "HealthCare", "HC_Broad",
                    "MedDevices", "Biotech", "Industrials", "Materials",
                    "Discretionary", "Comms", "Housing", "Reg_Banks",
                    "REIT_Sector", "Defense")
intl_dm_sb     <- c("DM_Broad", "Europe", "Germany", "France", "Italy",
                    "Australia", "Canada", "Japan", "Japan_Hdg",
                    "Korea", "Switzerland")
em_sb          <- c("EM_Broad", "China", "India", "Brazil")
thematic_sb    <- c("Semis", "AI", "Cloud", "Cyber", "IPO",
                    "Private_Eq", "MinVol_Glbl", "Agri_Equity", "Lithium",
                    "Uranium")

ticker_pts <- tibble(
  ticker = tickers,
  ret    = mu,
  vol    = sqrt(diag(Sigma)),
  sharpe = (mu - RF) / sqrt(diag(Sigma))
) %>%
  left_join(etf_metadata %>% dplyr::select(ticker, asset_class, sub_block),
            by = "ticker") %>%
  mutate(
    in_saa     = ticker %in% SAA_PORTFOLIO$ticker,
    plot_class = case_when(
      asset_class == "Equity" & sub_block %in% us_broad_sb   ~ "Eq · US Broad/Factor",
      asset_class == "Equity" & sub_block %in% us_sector_sb  ~ "Eq · US Sector",
      asset_class == "Equity" & sub_block %in% intl_dm_sb    ~ "Eq · Intl DM",
      asset_class == "Equity" & sub_block %in% em_sb         ~ "Eq · EM",
      asset_class == "Equity" & sub_block %in% thematic_sb   ~ "Eq · Thematic",
      asset_class == "Equity"                                 ~ "Eq · Other",
      asset_class == "FixedIncome"                            ~ "Fixed Income",
      asset_class == "Commodity"                              ~ "Commodity",
      asset_class == "FX"                                     ~ "FX",
      asset_class == "RealAsset"                              ~ "Real Asset",
      asset_class == "MultiAsset"                             ~ "Multi-Asset",
      TRUE                                                    ~ asset_class
    )
  )

# ── Colour palette by plot_class ──────────────────────────────────────────────
AC_PAL <- c(
  "Eq · US Broad/Factor" = "#1B3A6B",   # dark navy
  "Eq · US Sector"       = "#4A90D9",   # mid blue
  "Eq · Intl DM"         = "#7EB8E8",   # light blue
  "Eq · EM"              = "#00897B",   # teal
  "Eq · Thematic"        = "#AB47BC",   # purple
  "Eq · Other"           = "#90A4AE",   # grey-blue
  "Fixed Income"         = "#27AE60",   # green
  "Commodity"            = "#E67E22",   # orange
  "FX"                   = "#F1C40F",   # yellow
  "Real Asset"           = "#C0392B",   # red
  "Multi-Asset"          = "#7F8C8D",   # grey
  "Alternative"          = "#BDC3C7",   # light grey
  "Signal"               = "#BDC3C7"
)

# ── PLOT ──────────────────────────────────────────────────────────────────────
p_frontier <- ggplot() +

  # Individual tickers (background layer)
  geom_point(
    data = ticker_pts,
    aes(x = vol, y = ret, colour = plot_class,
        shape = in_saa, size = in_saa),
    alpha = 0.70
  ) +
  geom_text_repel(
    data = ticker_pts %>% filter(in_saa | sharpe > 0.8 | ret > 0.20 | vol < 0.08),
    aes(x = vol, y = ret, label = ticker, colour = plot_class),
    size = 3, fontface = "bold", max.overlaps = 25,
    segment.color = "grey70", show.legend = FALSE
  ) +

  # Efficient frontier curve
  geom_path(
    data = frontier_pts,
    aes(x = vol, y = ret),
    colour = "#2c3e50", linewidth = 1.2, alpha = 0.9
  ) +

  # GMV portfolio
  geom_point(data = gmv_pt,  aes(x = vol, y = ret),
             shape = 23, fill = "#27ae60", colour = "white",
             size = 5, stroke = 1.5) +
  geom_label(data = gmv_pt,  aes(x = vol, y = ret, label = "GMV"),
             nudge_y = 0.008, size = 3.5, fontface = "bold",
             fill = "#27ae60", colour = "white", label.size = 0) +

  # Max Sharpe portfolio
  geom_point(data = tan_pt,  aes(x = vol, y = ret),
             shape = 24, fill = "#e67e22", colour = "white",
             size = 5, stroke = 1.5) +
  geom_label(data = tan_pt,  aes(x = vol, y = ret, label = "Max Sharpe"),
             nudge_y = 0.008, size = 3.5, fontface = "bold",
             fill = "#e67e22", colour = "white", label.size = 0) +

  # SAA portfolio
  geom_point(data = saa_pt,  aes(x = vol, y = ret),
             shape = 21, fill = "#D90429", colour = "white",
             size = 6, stroke = 1.5) +
  geom_label(data = saa_pt,  aes(x = vol, y = ret, label = "SAA"),
             nudge_x = 0.018, nudge_y = 0.012, size = 3.5, fontface = "bold",
             fill = "#D90429", colour = "white", label.size = 0) +

  scale_colour_manual(values = AC_PAL, name = "Asset Class / Group",
                      na.value = "grey70") +
  scale_shape_manual(values = c("TRUE" = 18, "FALSE" = 16),
                     guide = "none") +
  scale_size_manual(values  = c("TRUE" = 3.5, "FALSE" = 1.8),
                    guide = "none") +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     name   = "Annualised Volatility") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(-0.05, 0.30),
                     name   = "Annualised Return") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor  = element_blank(),
    legend.position   = "right",
    legend.text       = element_text(size = 11),
    legend.title      = element_text(size = 11, face = "bold"),
    axis.title        = element_text(size = 12),
    axis.text         = element_text(size = 11),
    plot.title        = element_text(face = "bold", size = 15),
    plot.subtitle     = element_text(size = 11, colour = "grey40"),
    plot.caption      = element_text(size = 9, colour = "grey55")
  ) +
  labs(
    title    = "Efficient Frontier — Sovereign Universe",
    subtitle = sprintf(
      "Long-only, fully-invested | %d tickers | RF = %.1f%% | SAA = current strategic allocation",
      n, RF * 100
    ),
    caption  = sprintf(
      "GMV Sharpe=%.2f  |  Max Sharpe=%.2f  |  SAA Sharpe=%.2f  |  SAA vol=%.1f%%  |  SAA ret=%.1f%%",
      gmv_pt$sharpe, tan_pt$sharpe, saa_pt$sharpe,
      saa_pt$vol * 100, saa_pt$ret * 100
    )
  )

# ── GMV weights table ─────────────────────────────────────────────────────────
build_gmv_table <- function() {
  tibble(ticker = tickers, weight = gmv_wts) %>%
    filter(weight > 0.005) %>%
    arrange(desc(weight)) %>%
    left_join(etf_metadata %>% dplyr::select(ticker, asset_class), by = "ticker") %>%
    mutate(weight = scales::percent(weight, accuracy = 0.1))
}

# ── Max Sharpe weights table ───────────────────────────────────────────────────
build_tan_table <- function() {
  res <- min_var_portfolio(frontier_pts$ret[tan_idx], mu, Sigma)
  tibble(ticker = tickers, weight = res$weights) %>%
    filter(weight > 0.005) %>%
    arrange(desc(weight)) %>%
    left_join(etf_metadata %>% dplyr::select(ticker, asset_class), by = "ticker") %>%
    mutate(weight = scales::percent(weight, accuracy = 0.1))
}

# ── Auto-run guard ─────────────────────────────────────────────────────────────
if (!isTRUE(getOption("knitr.in.progress"))) {
  print(p_frontier)

  cat("\n── GMV Portfolio (top holdings) ──────────────────────────────────────\n")
  print(build_gmv_table(), n = 20)

  cat("\n── Max Sharpe Portfolio (top holdings) ───────────────────────────────\n")
  print(build_tan_table(), n = 20)
}
