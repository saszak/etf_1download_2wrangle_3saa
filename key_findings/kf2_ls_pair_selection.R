# ==============================================================================
# KEY FINDING 2 — L/S Pair Selection: High Correlation is the Prerequisite
#                 for Replacing SPY in a Strategic Asset Allocation
# ==============================================================================
#
# FINDING IN ONE SENTENCE:
#   To replace SPY with an L/S spread (long ticker / short SPY) inside an SAA
#   portfolio, the ticker must have HIGH correlation vs SPY (ρ > 0.75).
#   Low-ρ tickers produce spreads with extreme volatility and deep drawdowns —
#   making them unsuitable as SPY substitutes regardless of their standalone
#   returns.
#
# WHY IT MATTERS:
#   The Exante SAA logic (Section 5 of executive_summary) tests whether adding
#   a ticker at 10% weight (with −10% SPY) improves risk-adjusted outcomes.
#   This finding establishes the SCREENING RULE: only high-ρ candidates
#   (XLK, XLV, XLF, sector ETFs) are valid candidates for this substitution.
#   Near-zero ρ "diversifiers" (GLD, IEF, TLT) are NOT valid L/S pair legs
#   for SPY replacement — they are standalone allocations.
#
# PLOT 1 — Scatter: ρ vs spread-MaxDD, annotated with ticker labels for
#           candidates that pass the L/S pair screen (|mdd_spread| < threshold)
# PLOT 2 — Ranked bar: IR of L/S spread, filtered to high-ρ tickers only,
#           showing the viable substitution set
#
# Data requirements:
#   xts_ret      — loaded from 02_data_processed/xts_ret_returns.rds
#   etf_metadata — loaded from scripts/00_init_universe.R
# ==============================================================================

library(tidyverse)
library(xts)
library(scales)
library(ggrepel)
library(here)
library(patchwork)

source(here("scripts/00_init_universe.R"))

xts_ret <- readRDS(here("02_data_processed/xts_ret_returns.rds"))

# ── Parameters ────────────────────────────────────────────────────────────────
MASTER       <- "SPY"
RHO_SCREEN   <- 0.75    # minimum ρ to qualify as an L/S pair candidate
MDD_SCREEN   <- -0.30   # maximum tolerable spread-MaxDD (−30%)

# ── Build spread metrics for every ticker vs SPY ──────────────────────────────
T_yrs    <- as.numeric(diff(range(index(xts_ret)))) / 365
universe <- setdiff(colnames(xts_ret), MASTER)

spread_tbl <- map_dfr(universe, function(tk) {
  r_tk  <- as.numeric(na.omit(xts_ret[, tk]))
  r_spy <- as.numeric(na.omit(xts_ret[, MASTER]))
  n     <- min(length(r_tk), length(r_spy))
  if (n < 252) return(NULL)
  r_tk  <- tail(r_tk,  n)
  r_spy <- tail(r_spy, n)

  spread  <- r_tk - r_spy
  mu_spr  <- mean(spread, na.rm = TRUE) * 252
  sig_spr <- sd(spread,   na.rm = TRUE) * sqrt(252)
  mdd_spr <- {
    w <- cumprod(1 + spread)
    min((w - cummax(w)) / cummax(w), na.rm = TRUE)
  }
  rho     <- cor(r_tk, r_spy, use = "complete.obs")

  # Also compute blended portfolio (90% SPY + 10% ticker) MaxDD
  blend   <- 0.90 * r_spy + 0.10 * r_tk
  mdd_blend <- {
    w <- cumprod(1 + blend)
    min((w - cummax(w)) / cummax(w), na.rm = TRUE)
  }
  mdd_spy <- {
    w <- cumprod(1 + r_spy)
    min((w - cummax(w)) / cummax(w), na.rm = TRUE)
  }

  # Empirical probability that a 1-Quarter (63 trading day) rolling spread
  # return is positive — fraction of all 63-day windows where long-ticker /
  # short-SPY delivered a gain.
  WIN_1Q   <- 63L
  roll_ret_1q <- zoo::rollapply(spread, WIN_1Q,
                                function(x) prod(1 + x) - 1,
                                fill = NA, align = "right")
  prob_1q  <- mean(roll_ret_1q > 0, na.rm = TRUE)

  tibble(
    ticker    = tk,
    rho       = rho,
    mu_spr    = mu_spr,
    sig_spr   = sig_spr,
    ir        = mu_spr / sig_spr,
    mdd_spr   = mdd_spr,
    mdd_blend = mdd_blend,
    mdd_spy   = mdd_spy,
    dd_delta  = mdd_blend - mdd_spy,   # positive = blend has LESS severe DD
    prob_1q   = prob_1q                # P(spread > 0 over 63-day window)
  )
}) %>%
  left_join(
    etf_metadata %>% dplyr::select(ticker, asset_class, sub_block),
    by = "ticker"
  ) %>%
  mutate(
    passes_screen = rho >= RHO_SCREEN & mdd_spr >= MDD_SCREEN,
    rho_band = cut(rho,
      breaks = c(-Inf, 0.2, 0.5, 0.75, Inf),
      labels = c("ρ < 0.2", "0.2–0.5", "0.5–0.75", "ρ > 0.75")
    )
  )

# ── Palette ───────────────────────────────────────────────────────────────────
RHO_PAL <- c(
  "ρ < 0.2"  = "#d73027",
  "0.2–0.5"  = "#fc8d59",
  "0.5–0.75" = "#4575b4",
  "ρ > 0.75" = "#1a1a6e"
)

SCREEN_LINE_COL <- "#27ae60"

# ── PLOT 1: ρ vs Spread-MaxDD — screening diagram ─────────────────────────────
# The valid L/S pair candidates sit in the top-right quadrant:
#   high ρ (x-axis right) + shallow spread-DD (y-axis near zero)

p1 <- ggplot(spread_tbl, aes(x = rho, y = mdd_spr, colour = rho_band)) +
  # Screening boundaries
  geom_vline(xintercept = RHO_SCREEN, linetype = "dashed",
             colour = SCREEN_LINE_COL, linewidth = 0.8) +
  geom_hline(yintercept = MDD_SCREEN, linetype = "dashed",
             colour = SCREEN_LINE_COL, linewidth = 0.8) +
  annotate("rect",
           xmin = RHO_SCREEN, xmax = Inf,
           ymin = MDD_SCREEN, ymax = 0,
           fill = "#eafaf1", alpha = 0.4) +
  annotate("text", x = RHO_SCREEN + 0.01, y = -0.01,
           label = "VALID L/S PAIR\nCANDIDATES",
           hjust = 0, vjust = 1, size = 3.2,
           colour = SCREEN_LINE_COL, fontface = "bold") +
  geom_point(aes(size = abs(ir)), alpha = 0.75) +
  geom_text_repel(
    data = spread_tbl %>% filter(passes_screen | rho > 0.80),
    aes(label = ticker),
    size = 2.8, max.overlaps = 20,
    segment.color = "grey60", show.legend = FALSE
  ) +
  scale_colour_manual(values = RHO_PAL, name = "Correlation\nvs SPY") +
  scale_size_continuous(range = c(1.5, 5), name = "|IR|") +
  scale_x_continuous(labels = number_format(accuracy = 0.01)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position  = "right",
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, colour = "grey40")
  ) +
  labs(
    title    = "KF2-A: L/S Pair Screening — ρ vs Spread MaxDD",
    subtitle = paste0(
      sprintf("Green zone: ρ > %.2f AND spread MaxDD > %.0f%%  =  valid SPY-replacement candidates\n", RHO_SCREEN, MDD_SCREEN * 100),
      "Low-ρ tickers have deep spread-DD — unsuitable as L/S legs regardless of standalone returns"
    ),
    x = "Correlation vs SPY (ρ)",
    y = "Spread MaxDD (Long ticker − Short SPY)"
  )

# ── PLOT 2: IR ranking — high-ρ candidates only ───────────────────────────────
# Among valid candidates, rank by Information Ratio.
# This is the actionable output: who should replace SPY weight in the SAA?

p2_data <- spread_tbl %>%
  filter(rho >= RHO_SCREEN) %>%
  arrange(ir) %>%
  mutate(
    ticker    = factor(ticker, levels = ticker),
    ir_colour = if_else(ir > 0, "#1a1a6e", "#d73027")
  )

p2 <- ggplot(p2_data, aes(x = ticker, y = ir, fill = ir > 0)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = 0, linewidth = 0.6, colour = "grey30") +
  scale_fill_manual(values = c("TRUE" = "#1a1a6e", "FALSE" = "#d73027"),
                    guide = "none") +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x        = element_text(angle = 60, hjust = 1, size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(size = 10, colour = "grey40")
  ) +
  labs(
    title    = sprintf("KF2-B: Information Ratio of L/S Spread vs SPY — High-ρ Candidates (ρ > %.2f)", RHO_SCREEN),
    subtitle = paste0(
      "Positive IR = ticker systematically outperforms SPY on a risk-adjusted basis\n",
      "These are the ONLY tickers suitable for a long-ticker / short-SPY swap in SAA"
    ),
    x = NULL,
    y = "Information Ratio (Ann excess return / Ann tracking error)"
  )

# ── PLOT 3: Scatter — IR (x) vs ΔMaxDD (y), size = P(1Q improvement) ─────────
# The "sweet spot" is top-right: positive IR AND positive DD improvement.
# Circle size encodes how reliably the ticker beats SPY over a 1-quarter window.
# This is the single most actionable plot: it combines return edge (IR),
# risk reduction (ΔMaxDD), and consistency (prob_1q) in one view.

p3_data <- spread_tbl %>%
  filter(rho >= RHO_SCREEN) %>%
  mutate(
    quadrant = case_when(
      ir > 0 & dd_delta > 0 ~ "Best: +IR, +ΔDD",
      ir > 0 & dd_delta <= 0 ~ "Return edge only",
      ir <= 0 & dd_delta > 0 ~ "DD protection only",
      TRUE                    ~ "Neither"
    ),
    quadrant = factor(quadrant, levels = c(
      "Best: +IR, +ΔDD", "Return edge only", "DD protection only", "Neither"
    ))
  )

QUAD_PAL <- c(
  "Best: +IR, +ΔDD"    = "#1a1a6e",
  "Return edge only"   = "#2196F3",
  "DD protection only" = "#27ae60",
  "Neither"            = "#bdbdbd"
)

p3 <- ggplot(p3_data,
             aes(x = ir, y = dd_delta, size = prob_1q, colour = quadrant)) +
  # Quadrant lines
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.7) +
  # Quadrant labels (background)
  annotate("text", x =  0.95, y =  max(p3_data$dd_delta, na.rm=TRUE) * 0.92,
           label = "Best zone\n+IR  +ΔDD", hjust = 1, size = 3.5,
           colour = "#1a1a6e", fontface = "italic") +
  annotate("text", x = -0.6,  y =  max(p3_data$dd_delta, na.rm=TRUE) * 0.92,
           label = "DD protection\nonly", hjust = 0, size = 3.2,
           colour = "#27ae60", fontface = "italic") +
  annotate("text", x =  0.95, y =  min(p3_data$dd_delta, na.rm=TRUE) * 0.92,
           label = "Return edge\nonly", hjust = 1, size = 3.2,
           colour = "#2196F3", fontface = "italic") +
  geom_point(alpha = 0.80) +
  geom_text_repel(
    aes(label = ticker),
    size = 3.5, fontface = "bold", max.overlaps = 30,
    segment.color = "grey60", show.legend = FALSE
  ) +
  scale_colour_manual(values = QUAD_PAL, name = "Quadrant") +
  scale_size_continuous(
    range  = c(3, 14),
    name   = "P(spread > 0\nover 1 Quarter)",
    labels = percent_format(accuracy = 1),
    breaks = c(0.45, 0.50, 0.55, 0.60, 0.65)
  ) +
  scale_x_continuous(labels = number_format(accuracy = 0.01)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor  = element_blank(),
    legend.position   = "right",
    legend.text       = element_text(size = 11),
    legend.title      = element_text(size = 11, face = "bold"),
    axis.title        = element_text(size = 12),
    axis.text         = element_text(size = 11),
    plot.title        = element_text(face = "bold", size = 14),
    plot.subtitle     = element_text(size = 11, colour = "grey40")
  ) +
  labs(
    title    = sprintf("KF2-C: IR vs ΔMaxDD — High-ρ Candidates (ρ > %.2f)", RHO_SCREEN),
    subtitle = paste0(
      "X = Information Ratio of spread  |  Y = MaxDD improvement vs pure SPY\n",
      "Circle size = empirical P(spread > 0 over any 63-day window)  |  Best zone: top-right"
    ),
    x = "Information Ratio (spread vs SPY)",
    y = "ΔMaxDD: blend vs pure SPY  (positive = improvement)"
  )

# ── PLOT 4: DD improvement bar — ranked view ───────────────────────────────────
p4_data <- spread_tbl %>%
  filter(rho >= RHO_SCREEN) %>%
  arrange(dd_delta) %>%
  mutate(ticker = factor(ticker, levels = ticker))

p4 <- ggplot(p4_data, aes(x = ticker, y = dd_delta, fill = dd_delta > 0)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = 0, linewidth = 0.6, colour = "grey30") +
  scale_fill_manual(values = c("TRUE" = "#27ae60", "FALSE" = "#d73027"),
                    guide = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x        = element_text(angle = 60, hjust = 1, size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(size = 10, colour = "grey40")
  ) +
  labs(
    title    = sprintf("KF2-D: MaxDD Improvement from 10%% Swap (High-ρ Only, ρ > %.2f)", RHO_SCREEN),
    subtitle = "Green = 90% SPY + 10% ticker has LESS severe MaxDD than 100% SPY",
    x = NULL,
    y = "ΔMaxDD: blend vs pure SPY  (positive = improvement)"
  )

# ── Print ──────────────────────────────────────────────────────────────────────
if (!isTRUE(getOption("knitr.in.progress"))) {
  print(p1)
  print(p2)
  print(p3)
  print(p4)

  cat("\n── Candidate summary table ────────────────────────────────\n")
  spread_tbl %>%
    filter(passes_screen) %>%
    arrange(desc(ir)) %>%
    transmute(
      ticker,
      asset_class,
      rho       = round(rho, 2),
      IR        = round(ir, 2),
      mdd_spr   = scales::percent(mdd_spr,   accuracy = 0.1),
      dd_delta  = scales::percent(dd_delta,  accuracy = 0.1),
      prob_1q   = scales::percent(prob_1q,   accuracy = 1)
    ) %>%
    print(n = 30)
}
