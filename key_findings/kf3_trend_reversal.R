# ==============================================================================
# KEY FINDING 3 — Trend Reversal: "Winners" Becoming "Losers" in
#                 Cross-Sectional Relative Momentum
# ==============================================================================
#
# FINDING IN ONE SENTENCE:
#   Cross-sectional relative momentum (ticker vs SPY) shows persistent cohort
#   membership WITHIN regimes, but systematic reversals AT regime transitions —
#   especially Fall-to-Recovery, where HEDGE tickers (GLD, IEF) flip from
#   winner to loser and high-beta laggards (SMH, XLK) flip to leaders.
#
# WHY IT MATTERS:
#   A naive trailing-return TAA screen is structurally wrong at regime turning
#   points. A ticker screening as a "winner" late in a Fall episode (e.g. GLD
#   rising while SPY falls) will become a systematic loser in Recovery with no
#   change in fundamentals — purely because the regime context reversed.
#   Knowing WHEN reversals cluster lets the TAA framework apply a
#   regime-conditional momentum filter rather than a naive trailing rank.
#
# PLOT A — Quintile Transition Matrix: how persistently do tickers stay in
#           their weekly quintile? High diagonal = momentum; low = mean reversion
# PLOT B — Reversal frequency by regime: where do W→L and L→W events cluster?
# PLOT C — Forward return post-reversal: do reversal signals have predictive
#           power? Box plot of 20d and 63d relative return after each event type
#
# Parameters:
#   REBAL_FREQ   = 5   trading days  (weekly rebalance cadence)
#   LOOKBACK     = 60  trading days  (formation window for cross-sectional rank)
#   PERSIST_N    = 3   consecutive rebalance periods to qualify as persistent
#   TRANSIT_WIN  = 8   max rebalance gap (periods) to call it a reversal event
#
# Noise-reduction rationale:
#   1. Relative returns (xts_rel) remove the common SPY beta from every ticker,
#      so rank reflects genuine cross-sectional alpha rotation, not regime beta.
#   2. Winsorised at 1% tails before ranking — prevents one outlier episode
#      from monopolising Q1 / Q5 slots.
#   3. Persistence filter (PERSIST_N) eliminates one-week quintile flukes.
#   4. Weekly (not daily) rebalance cadence reduces microstructure noise.
#
# Data requirements:
#   xts_ret      — 02_data_processed/xts_ret_returns.rds
#   xts_rel      — 02_data_processed/xts_rel.rds
#   etf_metadata — scripts/00_init_universe.R
#   rt           — build_regime_table(xts_ret[, "SPY"])
# ==============================================================================

library(tidyverse)
library(xts)
library(zoo)
library(scales)
library(ggrepel)
library(here)
library(patchwork)

source(here("00_libraries.R"))
source(here("scripts_spy_dd_regime/spy_dd_regime.R"))   # build_regime_table()
source(here("scripts/00_init_universe.R"))

xts_ret <- readRDS(here("02_data_processed/xts_ret_returns.rds"))
xts_rel <- readRDS(here("02_data_processed/xts_rel.rds"))
rt      <- build_regime_table(xts_ret[, "SPY"])   # explicit assign — stats::rt shadows

# ── Parameters ────────────────────────────────────────────────────────────────
MASTER       <- "SPY"
REBAL_FREQ   <- 5L    # weekly rebalance (trading days)
LOOKBACK     <- 60L   # formation window
PERSIST_N    <- 3L    # min consecutive periods for persistent cohort label
TRANSIT_WIN  <- 8L    # max rebalance gap to qualify as a reversal event
FWD_WINDOWS  <- c(20L, 63L)

# ── Rebalance dates and universe ──────────────────────────────────────────────
all_dates <- index(xts_rel)
reb_dates <- all_dates[seq(LOOKBACK + 1L, length(all_dates), by = REBAL_FREQ)]
universe  <- setdiff(colnames(xts_rel), MASTER)

# ── Helper: 1%-tail winsorise ─────────────────────────────────────────────────
winsorise_vec <- function(x, p = 0.01) {
  lo <- quantile(x, p,       na.rm = TRUE)
  hi <- quantile(x, 1 - p,   na.rm = TRUE)
  pmax(pmin(x, hi), lo)
}

# ── Cross-sectional rank at each rebalance date ───────────────────────────────
# Cumulative relative log-return over the LOOKBACK window, winsorised, then ranked.
# Quintile 5 = winner (top 20%), Quintile 1 = loser (bottom 20%).
message("Building cross-sectional rank table (", length(reb_dates), " rebalance dates) ...")

rank_tbl <- map_dfr(reb_dates, function(d) {
  win <- tail(xts_rel[paste0("/", format(d)), universe], LOOKBACK)
  if (nrow(win) < as.integer(LOOKBACK * 0.8)) return(NULL)  # need 80% coverage

  cum_rel   <- colSums(win, na.rm = TRUE)            # sum of log-rets ≈ cumulative
  cum_rel_w <- winsorise_vec(as.numeric(cum_rel))    # winsorise before ranking
  names(cum_rel_w) <- universe

  n_valid  <- sum(!is.na(cum_rel_w))
  raw_rank <- rank(cum_rel_w, na.last = "keep", ties.method = "average")
  quintile <- pmin(ceiling(raw_rank / n_valid * 5), 5L)

  tibble(
    date     = d,
    ticker   = universe,
    cum_rel  = as.numeric(cum_rel),
    quintile = as.integer(quintile)
  )
}) %>%
  filter(!is.na(quintile))

# ── Persistence filter ────────────────────────────────────────────────────────
# Count consecutive rebalance periods a ticker has stayed in the SAME quintile.
# Only label as "persistent" when consec >= PERSIST_N.
rank_tbl <- rank_tbl %>%
  arrange(ticker, date) %>%
  group_by(ticker) %>%
  mutate(
    consec = {
      q   <- quintile
      out <- integer(length(q))
      out[1] <- 1L
      for (i in seq_along(q)[-1]) {
        out[i] <- if (!is.na(q[i]) && !is.na(q[i - 1]) && q[i] == q[i - 1])
          out[i - 1] + 1L else 1L
      }
      out
    },
    is_persistent_winner = (quintile == 5L) & (consec >= PERSIST_N),
    is_persistent_loser  = (quintile == 1L) & (consec >= PERSIST_N)
  ) %>%
  ungroup()

# ── Quintile transition matrix (all tickers, all periods) ────────────────────
trans_matrix <- rank_tbl %>%
  arrange(ticker, date) %>%
  group_by(ticker) %>%
  mutate(next_quintile = lead(quintile)) %>%
  ungroup() %>%
  filter(!is.na(quintile), !is.na(next_quintile)) %>%
  count(quintile, next_quintile) %>%
  group_by(quintile) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    q_from = factor(quintile,      levels = 5:1, labels = paste0("Q", 5:1)),
    q_to   = factor(next_quintile, levels = 1:5, labels = paste0("Q", 1:5)),
    is_diag = quintile == next_quintile
  )

# ── Reversal detection ────────────────────────────────────────────────────────
# A reversal event (W→L or L→W) requires:
#   1. Ticker was in a persistent state (FROM),
#   2. That persistent state ENDS (last period of FROM),
#   3. Within TRANSIT_WIN rebalance periods, the ticker ENTERS a persistent
#      state in the OPPOSITE extreme (TO),
#   4. That TO state was not already active before the FROM exit.

detect_reversals <- function(df, from_flag, to_flag, label) {
  ticker_name <- df$ticker[1]

  # Last period of FROM run = TRUE where FROM is TRUE and next is FALSE
  from_exit <- from_flag & !dplyr::lead(from_flag, default = FALSE)
  # First period of TO run = TRUE where TO is TRUE and previous is FALSE
  to_entry  <- to_flag & !dplyr::lag(to_flag, default = FALSE)

  from_dates <- df$date[from_exit]
  to_dates   <- df$date[to_entry]

  if (length(from_dates) == 0 || length(to_dates) == 0) return(tibble())

  purrr::map_dfr(from_dates, function(fd) {
    candidates  <- to_dates[to_dates > fd]
    if (length(candidates) == 0) return(NULL)
    nearest     <- candidates[1]
    gap_periods <- sum(reb_dates > fd & reb_dates <= nearest)
    if (gap_periods > TRANSIT_WIN) return(NULL)
    tibble(
      ticker        = ticker_name,
      reversal_type = label,
      exit_date     = fd,        # end of the old regime (winner or loser)
      entry_date    = nearest,   # start of the new regime (loser or winner)
      gap_periods   = gap_periods
    )
  })
}

message("Detecting reversal events ...")
reversals <- rank_tbl %>%
  group_by(ticker) %>%
  group_split() %>%
  purrr::map_dfr(function(df) {
    dplyr::bind_rows(
      detect_reversals(df, df$is_persistent_winner, df$is_persistent_loser, "W\u2192L"),
      detect_reversals(df, df$is_persistent_loser,  df$is_persistent_winner, "L\u2192W")
    )
  })

# ── Regime tagging at reversal exit date ──────────────────────────────────────
get_regime <- function(d, rt) {
  hit <- rt[rt$xmin <= d & rt$xmax >= d, ]
  if (nrow(hit) == 0) "Consolidation" else as.character(hit$regime[1])
}

reversals <- reversals %>%
  mutate(
    regime_at_exit = map_chr(exit_date, get_regime, rt = rt)
  ) %>%
  left_join(
    etf_metadata %>% select(ticker, asset_class, sub_block),
    by = "ticker"
  )

# ── Forward relative returns post-reversal ────────────────────────────────────
# Measure how the ticker performs (relative to SPY) in the N days after entry_date.
# Positive = ticker outperforms SPY in the period following the reversal.
message("Computing forward returns post-reversal ...")

fwd_returns <- reversals %>%
  filter(!is.na(entry_date)) %>%
  mutate(
    fwd_20d = map2_dbl(ticker, entry_date, function(tk, d) {
      fwd <- all_dates[all_dates > d]
      if (length(fwd) < FWD_WINDOWS[1]) return(NA_real_)
      sum(as.numeric(xts_rel[head(fwd, FWD_WINDOWS[1]), tk]), na.rm = TRUE)
    }),
    fwd_63d = map2_dbl(ticker, entry_date, function(tk, d) {
      fwd <- all_dates[all_dates > d]
      if (length(fwd) < FWD_WINDOWS[2]) return(NA_real_)
      sum(as.numeric(xts_rel[head(fwd, FWD_WINDOWS[2]), tk]), na.rm = TRUE)
    })
  ) %>%
  pivot_longer(
    cols      = c(fwd_20d, fwd_63d),
    names_to  = "window",
    values_to = "fwd_rel_ret"
  ) %>%
  mutate(
    window = factor(window,
                    levels = c("fwd_20d", "fwd_63d"),
                    labels = c("20-day fwd", "63-day fwd"))
  )

# ── Palettes ───────────────────────────────────────────────────────────────────
REVERSAL_PAL <- c(
  "W\u2192L" = "#d73027",   # red — winner becoming loser
  "L\u2192W" = "#1a1a6e"    # dark blue — loser becoming winner
)

REGIME_PAL <- c(
  "Fall"          = "#d73027",
  "Recovery"      = "#1a1a6e",
  "Consolidation" = "#4575b4"
)

# ── Plot helper functions ──────────────────────────────────────────────────────
# Shared plot builders accept data + a title prefix so the same visual style
# can be applied to both the full universe and the equity-only sub-universe.

.make_trans_plot <- function(tm, title_prefix) {
  ggplot(tm, aes(x = q_to, y = q_from, fill = pct)) +
    geom_tile(colour = "white", linewidth = 0.8) +
    geom_text(
      aes(label  = percent(pct, accuracy = 1),
          colour = is_diag),
      size     = 4.5, fontface = "bold"
    ) +
    scale_fill_gradient2(
      low = "#f7f7f7", mid = "#74add1", high = "#1a1a6e", midpoint = 0.20,
      labels = percent_format(accuracy = 1), name = "Transition\nprobability"
    ) +
    scale_colour_manual(values = c("TRUE" = "#ffffff", "FALSE" = "#1B3A6B"),
                        guide = "none") +
    scale_x_discrete(position = "bottom") +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid      = element_blank(),
      axis.title      = element_text(size = 12),
      axis.text       = element_text(size = 12),
      legend.position = "right",
      legend.text     = element_text(size = 11),
      legend.title    = element_text(size = 11, face = "bold"),
      plot.title      = element_text(face = "bold", size = 14),
      plot.subtitle   = element_text(size = 11, colour = "grey40")
    ) +
    labs(
      title    = paste0(title_prefix, ": Weekly Quintile Transition Matrix"),
      subtitle = paste0(
        "Diagonal = momentum persistence (ticker stays in same quintile next week)\n",
        "Off-diagonal top-right corner = reversals (Q5\u2192Q1);  formation = ",
        LOOKBACK, " days, rebalance = ", REBAL_FREQ, " days"
      ),
      x = "Next period quintile  (Q1 = loser, Q5 = winner)",
      y = "Current quintile"
    )
}

.make_reversal_count_plot <- function(rev_df, title_prefix) {
  counts <- rev_df %>%
    count(regime_at_exit, reversal_type) %>%
    mutate(regime_at_exit = factor(regime_at_exit,
                                   levels = c("Fall", "Recovery", "Consolidation")))
  ggplot(counts, aes(x = regime_at_exit, y = n, fill = reversal_type)) +
    geom_col(position = "dodge", alpha = 0.87, width = 0.6) +
    geom_text(aes(label = n), position = position_dodge(width = 0.6),
              vjust = -0.4, size = 4, fontface = "bold") +
    scale_fill_manual(values = REVERSAL_PAL, name = "Reversal type") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.title         = element_text(size = 12),
      axis.text          = element_text(size = 12),
      legend.position    = "top",
      plot.title         = element_text(face = "bold", size = 14),
      plot.subtitle      = element_text(size = 11, colour = "grey40")
    ) +
    labs(
      title    = paste0(title_prefix, ": Reversal Event Count by Regime at Exit"),
      subtitle = paste0(
        "Regime tagged at end of the FROM state (exit_date)\n",
        "Persistence filter: Q1/Q5 for \u2265", PERSIST_N,
        " consecutive weeks; transition window \u2264", TRANSIT_WIN, " weeks"
      ),
      x = "Regime at reversal exit",
      y = "Number of reversal events"
    )
}

.make_fwd_return_plot <- function(fwd_df, title_prefix) {
  ggplot(fwd_df %>% filter(!is.na(fwd_rel_ret)),
         aes(x = reversal_type, y = fwd_rel_ret, fill = reversal_type)) +
    facet_wrap(~ window, scales = "free_y") +
    geom_hline(yintercept = 0, linewidth = 0.8, colour = "grey30", linetype = "dashed") +
    geom_boxplot(alpha = 0.70, outlier.shape = NA, width = 0.45) +
    geom_jitter(aes(colour = reversal_type), width = 0.15, size = 1.8,
                alpha = 0.45, show.legend = FALSE) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 4,
                 fill = "white", colour = "grey20", show.legend = FALSE) +
    scale_fill_manual(values   = REVERSAL_PAL, guide = "none") +
    scale_colour_manual(values = REVERSAL_PAL, guide = "none") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      strip.text         = element_text(face = "bold", size = 12),
      axis.title         = element_text(size = 12),
      axis.text          = element_text(size = 11),
      plot.title         = element_text(face = "bold", size = 14),
      plot.subtitle      = element_text(size = 11, colour = "grey40")
    ) +
    labs(
      title    = paste0(title_prefix, ": Forward Relative Return (vs SPY) Post-Reversal"),
      subtitle = paste0(
        "Measured from reversal entry_date; positive = ticker outperforms SPY\n",
        "Diamond = mean; box = IQR; W\u2192L should trend negative, L\u2192W positive"
      ),
      x = "Reversal type",
      y = "Cumulative relative return post-reversal"
    )
}

# ── Full-universe plots ────────────────────────────────────────────────────────
p_A <- .make_trans_plot(trans_matrix, "KF3-A (Full Universe)")
p_B <- .make_reversal_count_plot(reversals, "KF3-B (Full Universe)")
p_C <- .make_fwd_return_plot(fwd_returns, "KF3-C (Full Universe)")

# ==============================================================================
# EQUITY-ONLY SUB-UNIVERSE
# ==============================================================================
# Rationale: hedge ETFs (GLD, IEF, TLT, AGG, etc.) have no idiosyncratic
# "eigen-move". Their relative return vs SPY is mechanically driven by SPY's
# beta flip during Fall — they outperform solely because SPY falls, not because
# they have genuine sector momentum. This inflates Q5 during Fall with tickers
# that will reverse the instant Recovery begins, producing spurious W→L signals.
#
# By restricting to Equity ETFs only, the cross-sectional rank reflects genuine
# sector/factor rotation — the kind of momentum a TAA screen should actually use.
# ==============================================================================

# ── Equity universe ───────────────────────────────────────────────────────────
universe_eq <- etf_metadata %>%
  filter(asset_class == "Equity") %>%
  pull(ticker) %>%
  intersect(universe)

message("Equity-only universe: ", length(universe_eq), " tickers")

# ── Re-rank within equity subset ──────────────────────────────────────────────
# Reuse the already-computed cum_rel (raw, un-winsorised) from rank_tbl.
# Re-winsorise and re-rank cross-sectionally within equity only.
# This is essential: a ticker that was Q3 in the full universe may be Q5 within
# equities alone — the quintile label must be relative to the peer group.
rank_tbl_eq <- rank_tbl %>%
  filter(ticker %in% universe_eq) %>%
  group_by(date) %>%
  mutate(
    cum_rel_w  = winsorise_vec(cum_rel),
    n_valid    = sum(!is.na(cum_rel_w)),
    raw_rank   = rank(cum_rel_w, na.last = "keep", ties.method = "average"),
    quintile   = pmin(ceiling(raw_rank / n_valid * 5), 5L)
  ) %>%
  ungroup() %>%
  select(-cum_rel_w, -n_valid, -raw_rank)

# ── Persistence filter — equity ───────────────────────────────────────────────
rank_tbl_eq <- rank_tbl_eq %>%
  arrange(ticker, date) %>%
  group_by(ticker) %>%
  mutate(
    consec = {
      q   <- quintile
      out <- integer(length(q))
      out[1] <- 1L
      for (i in seq_along(q)[-1]) {
        out[i] <- if (!is.na(q[i]) && !is.na(q[i - 1]) && q[i] == q[i - 1])
          out[i - 1] + 1L else 1L
      }
      out
    },
    is_persistent_winner = (quintile == 5L) & (consec >= PERSIST_N),
    is_persistent_loser  = (quintile == 1L) & (consec >= PERSIST_N)
  ) %>%
  ungroup()

# ── Transition matrix — equity ────────────────────────────────────────────────
trans_matrix_eq <- rank_tbl_eq %>%
  arrange(ticker, date) %>%
  group_by(ticker) %>%
  mutate(next_quintile = lead(quintile)) %>%
  ungroup() %>%
  filter(!is.na(quintile), !is.na(next_quintile)) %>%
  count(quintile, next_quintile) %>%
  group_by(quintile) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    q_from  = factor(quintile,      levels = 5:1, labels = paste0("Q", 5:1)),
    q_to    = factor(next_quintile, levels = 1:5, labels = paste0("Q", 1:5)),
    is_diag = quintile == next_quintile
  )

# ── Reversal detection — equity ───────────────────────────────────────────────
message("Detecting equity-only reversal events ...")
reversals_eq <- rank_tbl_eq %>%
  group_by(ticker) %>%
  group_split() %>%
  purrr::map_dfr(function(df) {
    dplyr::bind_rows(
      detect_reversals(df, df$is_persistent_winner, df$is_persistent_loser, "W\u2192L"),
      detect_reversals(df, df$is_persistent_loser,  df$is_persistent_winner, "L\u2192W")
    )
  }) %>%
  mutate(regime_at_exit = map_chr(exit_date, get_regime, rt = rt)) %>%
  left_join(etf_metadata %>% select(ticker, asset_class, sub_block), by = "ticker")

# ── Forward returns — equity ──────────────────────────────────────────────────
message("Computing equity-only forward returns ...")
fwd_returns_eq <- reversals_eq %>%
  filter(!is.na(entry_date)) %>%
  mutate(
    fwd_20d = map2_dbl(ticker, entry_date, function(tk, d) {
      fwd <- all_dates[all_dates > d]
      if (length(fwd) < FWD_WINDOWS[1]) return(NA_real_)
      sum(as.numeric(xts_rel[head(fwd, FWD_WINDOWS[1]), tk]), na.rm = TRUE)
    }),
    fwd_63d = map2_dbl(ticker, entry_date, function(tk, d) {
      fwd <- all_dates[all_dates > d]
      if (length(fwd) < FWD_WINDOWS[2]) return(NA_real_)
      sum(as.numeric(xts_rel[head(fwd, FWD_WINDOWS[2]), tk]), na.rm = TRUE)
    })
  ) %>%
  pivot_longer(cols = c(fwd_20d, fwd_63d), names_to = "window",
               values_to = "fwd_rel_ret") %>%
  mutate(window = factor(window, levels = c("fwd_20d", "fwd_63d"),
                         labels = c("20-day fwd", "63-day fwd")))

# ── Equity-only plots ─────────────────────────────────────────────────────────
p_A_eq <- .make_trans_plot(trans_matrix_eq, "KF3-A (Equity Only)")
p_B_eq <- .make_reversal_count_plot(reversals_eq, "KF3-B (Equity Only)")
p_C_eq <- .make_fwd_return_plot(fwd_returns_eq, "KF3-C (Equity Only)")

# ==============================================================================
# CANARY ANALYSIS — Leading & Confirming Indicators (Equity Universe)
# ==============================================================================
# Two questions:
#   1. TRANSITION CANARY: Which equity ETFs reverse (exit persistent Q5/Q1) in
#      the days BEFORE a regime transition? A consistent lead time > 0 means
#      the reversal signal anticipates the regime change — a forward warning.
#
#   2. PERSISTENCE CONFIRMATION: Which equity ETFs stay locked in Q5 or Q1
#      for the ENTIRETY (or majority) of a regime episode? High intra-regime
#      persistence means "this ticker is confirming the regime is still alive."
# ==============================================================================

# ── Regime transition reference dates ─────────────────────────────────────────
# Fall onset = xmin of each Fall episode
# Recovery onset = xmin of each Recovery episode
fall_onsets     <- as.Date(rt$xmin[rt$regime == "Fall"])
recovery_onsets <- as.Date(rt$xmin[rt$regime == "Recovery"])

# Helper: nearest transition within ±CANARY_WIN days; returns signed lead time
# (positive = reversal preceded the transition = canary)
CANARY_WIN <- 45L   # search window in calendar days

.nearest_lead <- function(exit_dates, transition_dates, win = CANARY_WIN) {
  map_dbl(as.Date(exit_dates), function(d) {
    deltas <- as.numeric(as.Date(transition_dates) - d)
    valid  <- abs(deltas) <= win
    if (!any(valid, na.rm = TRUE)) return(NA_real_)
    deltas[valid][which.min(abs(deltas[valid]))]
  })
}

# ── Lead time per reversal event ───────────────────────────────────────────────
canary_lead_tbl <- reversals_eq %>%
  mutate(
    lead_days = case_when(
      reversal_type == "W\u2192L" ~
        .nearest_lead(exit_date, fall_onsets),
      reversal_type == "L\u2192W" ~
        .nearest_lead(exit_date, recovery_onsets)
    )
  ) %>%
  filter(!is.na(lead_days))

# ── Canary score per ticker ────────────────────────────────────────────────────
# Score = (fraction of events that are leading) × n_events
# Tickers that CONSISTENTLY lead transitions score highest.
canary_scores <- canary_lead_tbl %>%
  group_by(ticker, reversal_type) %>%
  summarise(
    n_canary      = n(),
    median_lead   = median(lead_days, na.rm = TRUE),
    pct_leading   = mean(lead_days > 0, na.rm = TRUE),   # >0 = led the transition
    mean_lead     = mean(lead_days,     na.rm = TRUE),
    canary_raw    = n_canary * pct_leading,
    .groups = "drop"
  ) %>%
  group_by(reversal_type) %>%
  mutate(
    canary_score = round(100 * (canary_raw - min(canary_raw)) /
                           (max(canary_raw) - min(canary_raw) + 1e-9))
  ) %>%
  ungroup() %>%
  left_join(etf_metadata %>% select(ticker, sub_block), by = "ticker")

# ── Intra-regime persistence ───────────────────────────────────────────────────
# For each regime episode, compute fraction of rebalance dates where each
# equity ticker was in Q5 (leaders) or Q1 (laggards).
# High Q5 persistence in Recovery = reliable confirmation of regime strength.
# High Q1 persistence in Fall     = reliable confirmation of regime stress.

persistence_tbl <- rt %>%
  select(regime, ep_start = xmin, ep_end = xmax) %>%
  mutate(ep_start = as.Date(ep_start), ep_end = as.Date(ep_end)) %>%
  purrr::pmap_dfr(function(regime, ep_start, ep_end) {
    ep_dates <- reb_dates[reb_dates >= ep_start & reb_dates <= ep_end]
    if (length(ep_dates) < 2) return(tibble())

    rank_tbl_eq %>%
      filter(date %in% ep_dates) %>%
      group_by(ticker) %>%
      summarise(
        pct_q5 = mean(quintile == 5L, na.rm = TRUE),
        pct_q1 = mean(quintile == 1L, na.rm = TRUE),
        n_obs  = n(),
        .groups = "drop"
      ) %>%
      mutate(regime = regime, ep_start = ep_start)
  }) %>%
  group_by(ticker, regime) %>%
  summarise(
    avg_pct_q5 = mean(pct_q5, na.rm = TRUE),
    avg_pct_q1 = mean(pct_q1, na.rm = TRUE),
    n_episodes = n(),
    .groups = "drop"
  ) %>%
  left_join(etf_metadata %>% select(ticker, sub_block), by = "ticker") %>%
  mutate(regime = factor(regime, levels = c("Fall", "Recovery", "Consolidation")))

# ── PLOT D: Canary Lead Time Distribution ─────────────────────────────────────
# X = ticker, Y = signed lead time (days), faceted by reversal type.
# Tickers with boxes above zero are consistently LEADING the regime transition.
# Tickers below zero are lagging — the market moved before they reversed.

canary_lead_plot_data <- canary_lead_tbl %>%
  left_join(canary_scores %>% select(ticker, reversal_type, canary_score),
            by = c("ticker", "reversal_type")) %>%
  filter(!is.na(canary_score), canary_score >= 25) %>%   # top-half canaries only
  mutate(ticker = fct_reorder(ticker, lead_days, .fun = median))

p_D <- ggplot(canary_lead_plot_data,
              aes(x = ticker, y = lead_days, fill = reversal_type)) +
  facet_wrap(~ reversal_type, scales = "free_x", ncol = 1) +
  geom_hline(yintercept = 0, linewidth = 0.9, colour = "grey30",
             linetype = "dashed") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0,   ymax =  CANARY_WIN,
           fill = "#eafaf1", alpha = 0.25) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -CANARY_WIN, ymax = 0,
           fill = "#fdf2f2", alpha = 0.25) +
  geom_boxplot(alpha = 0.75, outlier.shape = NA, width = 0.55) +
  geom_jitter(aes(colour = reversal_type), width = 0.18, size = 1.8,
              alpha = 0.55, show.legend = FALSE) +
  scale_fill_manual(values   = REVERSAL_PAL, guide = "none") +
  scale_colour_manual(values = REVERSAL_PAL, guide = "none") +
  scale_y_continuous(
    breaks = seq(-CANARY_WIN, CANARY_WIN, by = 15),
    labels = function(x) paste0(x, "d")
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    strip.text         = element_text(face = "bold", size = 12),
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(size = 11, colour = "grey40")
  ) +
  labs(
    title    = "KF3-D: Canary Lead Time — How Early Do Equity ETFs Signal Regime Transitions?",
    subtitle = paste0(
      "Green zone = reversal PRECEDED the transition (early warning / canary)\n",
      "Red zone = reversal FOLLOWED the transition (lagging). Only canary_score \u2265 25 shown."
    ),
    x = NULL,
    y = paste0("Lead time: transition_date \u2212 exit_date  (days, \u00b1", CANARY_WIN, "d window)")
  )

# ── PLOT E: Intra-Regime Persistence Heatmap ──────────────────────────────────
# Shows which equity ETFs are reliable WITHIN each regime.
# High Q5 score in Recovery = "this ticker confirms Recovery is running."
# High Q1 score in Fall = "this ticker confirms Fall stress is ongoing."
# We show Q5 persistence by default (the leaders during each regime type).

# Select tickers with avg_pct_q5 > 0.30 in at least one regime
top_persistent <- persistence_tbl %>%
  group_by(ticker) %>%
  filter(max(avg_pct_q5, na.rm = TRUE) > 0.30) %>%
  ungroup()

# Order tickers by Recovery Q5 persistence (most useful regime to confirm)
ticker_order_e <- top_persistent %>%
  filter(regime == "Recovery") %>%
  arrange(desc(avg_pct_q5)) %>%
  pull(ticker)

p_E <- top_persistent %>%
  filter(ticker %in% ticker_order_e) %>%
  mutate(ticker = factor(ticker, levels = rev(ticker_order_e))) %>%
  ggplot(aes(x = regime, y = ticker, fill = avg_pct_q5)) +
  geom_tile(colour = "white", linewidth = 0.7) +
  geom_text(
    aes(label  = percent(avg_pct_q5, accuracy = 1),
        colour = avg_pct_q5 > 0.45),
    size = 3.8, fontface = "bold"
  ) +
  scale_fill_gradient2(
    low      = "#f7f7f7",
    mid      = "#74add1",
    high     = "#1a1a6e",
    midpoint = 0.25,
    labels   = percent_format(accuracy = 1),
    name     = "Avg fraction\nof time in Q5"
  ) +
  scale_colour_manual(
    values = c("TRUE" = "#ffffff", "FALSE" = "#1B3A6B"),
    guide  = "none"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid  = element_blank(),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_text(size = 11),
    axis.title  = element_text(size = 12),
    legend.position = "right",
    legend.text     = element_text(size = 11),
    legend.title    = element_text(size = 11, face = "bold"),
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(size = 11, colour = "grey40")
  ) +
  labs(
    title    = "KF3-E: Intra-Regime Q5 Persistence (Equity Only)",
    subtitle = paste0(
      "Fraction of weekly rebalance dates within each regime episode where\n",
      "the ticker occupied Q5 (top 20% of equity universe). Averaged across all episodes."
    ),
    x = NULL,
    y = NULL
  )

# ── Print ──────────────────────────────────────────────────────────────────────
if (!isTRUE(getOption("knitr.in.progress"))) {
  cat("\n══ FULL UNIVERSE ═══════════════════════════════════════════════════════\n")
  print(p_A)
  print(p_B)
  print(p_C)

  cat("\n── Reversal event summary (full) ───────────────────────────────────────\n")
  reversals %>%
    count(reversal_type, regime_at_exit) %>%
    arrange(reversal_type, regime_at_exit) %>%
    print()

  cat("\n── Forward return medians (full) ───────────────────────────────────────\n")
  fwd_returns %>%
    filter(!is.na(fwd_rel_ret)) %>%
    group_by(reversal_type, window) %>%
    summarise(
      n           = n(),
      median_fwd  = median(fwd_rel_ret, na.rm = TRUE),
      pct_correct = mean(if_else(reversal_type == "W\u2192L",
                                 fwd_rel_ret < 0, fwd_rel_ret > 0), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(c(median_fwd, pct_correct), ~ percent(.x, accuracy = 0.1))) %>%
    print()

  cat("\n══ EQUITY ONLY ══════════════════════════════════════════════════════════\n")
  print(p_A_eq)
  print(p_B_eq)
  print(p_C_eq)

  cat("\n── Reversal event summary (equity) ─────────────────────────────────────\n")
  reversals_eq %>%
    count(reversal_type, regime_at_exit) %>%
    arrange(reversal_type, regime_at_exit) %>%
    print()

  cat("\n── Forward return medians (equity) ─────────────────────────────────────\n")
  fwd_returns_eq %>%
    filter(!is.na(fwd_rel_ret)) %>%
    group_by(reversal_type, window) %>%
    summarise(
      n           = n(),
      median_fwd  = median(fwd_rel_ret, na.rm = TRUE),
      pct_correct = mean(if_else(reversal_type == "W\u2192L",
                                 fwd_rel_ret < 0, fwd_rel_ret > 0), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(c(median_fwd, pct_correct), ~ percent(.x, accuracy = 0.1))) %>%
    print()

  cat("\n── Top equity reversal tickers ─────────────────────────────────────────\n")
  reversals_eq %>%
    count(ticker, reversal_type, sub_block) %>%
    arrange(desc(n)) %>%
    print(n = 20)

  cat("\n══ CANARY ANALYSIS ══════════════════════════════════════════════════════\n")
  print(p_D)
  print(p_E)

  cat("\n── Top transition canaries ─────────────────────────────────────────────\n")
  canary_scores %>%
    arrange(reversal_type, desc(canary_score)) %>%
    print(n = 30)

  cat("\n── Top regime-persistence confirmers (Q5) ──────────────────────────────\n")
  persistence_tbl %>%
    filter(regime == "Recovery") %>%
    arrange(desc(avg_pct_q5)) %>%
    select(ticker, sub_block, regime, avg_pct_q5, n_episodes) %>%
    print(n = 20)
}
