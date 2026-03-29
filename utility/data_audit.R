################################################################################
# utility/data_audit.R
# Purpose : Data quality audit for xts_ret — 4 layers:
#             1. Structural  — row count, NAs, date gaps
#             2. Return      — annualised return plausibility by asset class
#             3. Anchors     — SPY-centric cross-validation (correlation + rank)
#             4. Tiingo      — total return cross-check vs Tiingo (requires TIINGO_API_KEY in .Renviron)
#
# USAGE (integrated — called automatically at end of 01_etf_wrangle.R)
#   Runs silently; prints WARN/FAIL rows only unless verbose = TRUE
#
# USAGE (ad-hoc)
#   source(here("utility/data_audit.R"))
#   audit <- run_data_audit(xts_ret, etf_metadata, verbose = TRUE)
#   audit$report    # full tibble
#   audit$summary   # pass/warn/fail counts
#
# RETURNS  invisible list: $report, $summary, $passed (logical)
################################################################################

library(dplyr)
library(tibble)
library(xts)

run_data_audit <- function(xts_ret,
                           etf_metadata,
                           verbose      = FALSE,
                           min_rows     = 1800L,
                           max_rows     = 2400L,
                           max_na_pct   = 0.05,
                           max_gap_days = 10L,
                           n_random     = 5L) {

  results <- list()

  # ── LAYER 1: STRUCTURAL ────────────────────────────────────────────────────

  # 1a. Row count
  n <- nrow(xts_ret)
  results$rows <- tibble(
    layer  = "Structural",
    check  = "Row count",
    value  = as.character(n),
    target = paste0(min_rows, "–", max_rows, " US trading days"),
    status = ifelse(n >= min_rows & n <= max_rows, "PASS", "FAIL"),
    detail = paste0("nrow = ", n)
  )

  # 1b. SPY NA count (must be zero)
  spy_na <- sum(is.na(xts_ret[, "SPY"]))
  results$spy_na <- tibble(
    layer  = "Structural",
    check  = "SPY NA count",
    value  = as.character(spy_na),
    target = "0",
    status = ifelse(spy_na == 0, "PASS", "FAIL"),
    detail = paste0(spy_na, " NA rows in SPY")
  )

  # 1c. Per-ticker NA rate
  na_rates <- colMeans(is.na(xts_ret))
  bad_na   <- na_rates[na_rates > max_na_pct]
  results$na_rate <- tibble(
    layer  = "Structural",
    check  = "Ticker NA rate",
    value  = ifelse(length(bad_na) == 0, "0 tickers", paste0(length(bad_na), " tickers")),
    target = paste0("< ", max_na_pct * 100, "% NA"),
    status = ifelse(length(bad_na) == 0, "PASS", "WARN"),
    detail = ifelse(length(bad_na) == 0, "All clean",
                    paste(names(bad_na), scales::percent(bad_na, accuracy = 0.1), collapse = "  "))
  )

  # 1d. Date gaps in SPY
  spy_dates  <- as.Date(index(xts_ret[!is.na(xts_ret[, "SPY"]), ]))
  date_gaps  <- as.numeric(diff(spy_dates))
  max_gap    <- max(date_gaps, na.rm = TRUE)
  gap_dates  <- spy_dates[which(date_gaps > max_gap_days) + 1]
  results$gaps <- tibble(
    layer  = "Structural",
    check  = "Max date gap (SPY)",
    value  = paste0(max_gap, " calendar days"),
    target = paste0("≤ ", max_gap_days, " calendar days"),
    status = ifelse(max_gap <= max_gap_days, "PASS", "WARN"),
    detail = ifelse(max_gap <= max_gap_days, "No gaps",
                    paste("Gap after:", format(gap_dates, "%Y-%m-%d"), collapse = ", "))
  )

  # ── LAYER 2: RETURN PLAUSIBILITY ──────────────────────────────────────────

  # Bounds by asset_class
  bounds <- tribble(
    ~asset_class,   ~lo,   ~hi,
    "Equity",       -0.05,  0.30,
    "FixedIncome",  -0.05,  0.12,
    "Commodity",    -0.10,  0.25,
    "RealAsset",    -0.05,  0.20,
    "MultiAsset",   -0.05,  0.20,
    "Alternative",  -0.20,  1.00,
    "FX",           -0.10,  0.10,
    "Signal",       -0.50,  0.50
  )

  # Annualised return from clean log returns
  n_years  <- nrow(xts_ret) / 252
  ann_ret  <- exp(colSums(xts_ret, na.rm = TRUE) / n_years) - 1
  meta_cols <- intersect(c("ticker", "asset_class"), colnames(etf_metadata))
  ann_df   <- tibble(ticker = names(ann_ret), ann_ret = as.numeric(ann_ret)) %>%
    left_join(etf_metadata %>% select(all_of(meta_cols)), by = "ticker") %>%
    { if (!"asset_class" %in% colnames(.)) mutate(., asset_class = NA_character_) else . } %>%
    left_join(bounds, by = "asset_class") %>%
    filter(!is.na(lo)) %>%
    mutate(
      status = case_when(
        ann_ret < lo ~ "FAIL",
        ann_ret > hi ~ "WARN",
        TRUE         ~ "PASS"
      )
    )

  flagged_ret <- ann_df %>% filter(status != "PASS")

  results$returns <- tibble(
    layer  = "Return",
    check  = "Ann.Ret plausibility",
    value  = ifelse(nrow(flagged_ret) == 0, "All pass",
                    paste0(nrow(flagged_ret), " flagged")),
    target = "Within asset-class bounds",
    status = ifelse(nrow(flagged_ret) == 0, "PASS",
                    ifelse(any(flagged_ret$status == "FAIL"), "FAIL", "WARN")),
    detail = ifelse(nrow(flagged_ret) == 0, "All tickers within bounds",
                    paste(flagged_ret$ticker,
                          scales::percent(flagged_ret$ann_ret, accuracy = 0.1),
                          paste0("[", flagged_ret$status, "]"),
                          collapse = "  "))
  )

  # ── LAYER 3: SPY ANCHORS ──────────────────────────────────────────────────

  anchor_checks <- function(t1, t2, min_cor = NULL, max_cor = NULL, label) {
    if (!all(c(t1, t2) %in% colnames(xts_ret))) return(NULL)
    r <- cor(as.numeric(xts_ret[, t1]), as.numeric(xts_ret[, t2]), use = "complete.obs")
    if (!is.null(min_cor)) {
      tibble(layer = "Anchor", check = label,
             value = as.character(round(r, 3)), target = paste0("> ", min_cor),
             status = ifelse(r >= min_cor, "PASS", "FAIL"),
             detail = paste0("ρ(", t1, ",", t2, ") = ", round(r, 3)))
    } else {
      tibble(layer = "Anchor", check = label,
             value = as.character(round(r, 3)), target = paste0("< ", max_cor),
             status = ifelse(r <= max_cor, "PASS", "WARN"),
             detail = paste0("ρ(", t1, ",", t2, ") = ", round(r, 3)))
    }
  }

  results$cor_urth <- anchor_checks("SPY", "URTH",  min_cor = 0.85, label = "ρ SPY–URTH  (> 0.85)")
  results$cor_agg  <- anchor_checks("SPY", "AGG",   max_cor = 0.20, label = "ρ SPY–AGG   (< 0.20)")
  results$cor_gld  <- anchor_checks("SPY", "GLD",   max_cor = 0.30, label = "ρ SPY–GLD   (< 0.30)")
  results$cor_ief  <- anchor_checks("SPY", "IEF",   max_cor = 0.10, label = "ρ SPY–IEF   (< 0.10)")

  # Return rank: SPY total ret > AGG total ret
  spy_tr  <- exp(sum(xts_ret[, "SPY"],  na.rm = TRUE)) - 1
  agg_tr  <- exp(sum(xts_ret[, "AGG"],  na.rm = TRUE)) - 1
  results$rank <- tibble(
    layer  = "Anchor",
    check  = "Total return rank SPY > AGG",
    value  = paste0("SPY=", scales::percent(spy_tr, 0.1),
                    " AGG=", scales::percent(agg_tr, 0.1)),
    target = "SPY > AGG",
    status = ifelse(spy_tr > agg_tr, "PASS", "WARN"),
    detail = paste0("Equity risk premium check")
  )

  # ── LAYER 4: TIINGO CROSS-CHECK ───────────────────────────────────────────
  # Pulls total return for anchor tickers from Tiingo and compares to xts_ret.
  # Skipped silently if TIINGO_API_KEY is not set or Tiingo call fails.

  tiingo_key <- Sys.getenv("RIINGO_TOKEN")
  if (nchar(tiingo_key) == 0) tiingo_key <- Sys.getenv("TIINGO_API_KEY")
  tiingo_tickers <- c("SPY", "URTH", "AGG", "GLD", "IEF")
  tiingo_tickers <- tiingo_tickers[tiingo_tickers %in% colnames(xts_ret)]

  if (nchar(tiingo_key) > 0 && requireNamespace("riingo", quietly = TRUE)) {
    tryCatch({
      riingo::riingo_set_token(tiingo_key)

      date_from <- as.Date(index(xts_ret)[1])
      date_to   <- as.Date(index(xts_ret)[nrow(xts_ret)])

      tiingo_raw <- riingo::riingo_prices(tiingo_tickers,
                                          start_date = date_from,
                                          end_date   = date_to) %>%
        group_by(ticker) %>%
        arrange(date) %>%
        summarise(tr_tiingo = (last(adjClose) / first(adjClose)) - 1,
                  .groups = "drop") %>%
        rename(symbol = ticker)

      xts_tr <- tibble(
        symbol   = tiingo_tickers,
        tr_yahoo = map_dbl(tiingo_tickers,
                           ~exp(sum(xts_ret[, .x], na.rm = TRUE)) - 1)
      )

      tiingo_cmp <- tiingo_raw %>%
        left_join(xts_tr, by = "symbol") %>%
        mutate(
          diff_pp = (tr_yahoo - tr_tiingo) * 100,
          status  = case_when(
            abs(diff_pp) <= 2  ~ "PASS",
            abs(diff_pp) <= 5  ~ "WARN",
            TRUE               ~ "FAIL"
          )
        )

      for (i in seq_len(nrow(tiingo_cmp))) {
        r <- tiingo_cmp[i, ]
        results[[paste0("tiingo_", r$symbol)]] <- tibble(
          layer  = "Tiingo",
          check  = paste0(r$symbol, " total return"),
          value  = scales::percent(r$tr_yahoo,  accuracy = 0.1),
          target = scales::percent(r$tr_tiingo, accuracy = 0.1),
          status = r$status,
          detail = sprintf("Yahoo %.1f%%  Tiingo %.1f%%  Δ%.1fpp",
                           r$tr_yahoo * 100, r$tr_tiingo * 100, r$diff_pp)
        )
      }
    }, error = function(e) {
      message("Tiingo cross-check skipped: ", conditionMessage(e))
    })
  }

  # ── LAYER 5: TIINGO RANDOM SPOT-CHECK ─────────────────────────────────────
  # Randomly samples n_random tickers from the universe each run.
  # Skipped silently if Tiingo key not set or Tiingo call fails.

  tiingo_key5 <- Sys.getenv("RIINGO_TOKEN")
  if (nchar(tiingo_key5) == 0) tiingo_key5 <- Sys.getenv("TIINGO_API_KEY")

  # Only US-listed tickers supported by Tiingo (exclude .AS / .L / .DE etc.)
  us_tickers <- colnames(xts_ret)[!grepl("\\.", colnames(xts_ret))]
  # Exclude the anchors already checked in Layer 4
  anchor_tickers <- c("SPY", "URTH", "AGG", "GLD", "IEF")
  pool <- setdiff(us_tickers, anchor_tickers)

  if (nchar(tiingo_key5) > 0 && length(pool) >= 1 && n_random >= 1 &&
      requireNamespace("riingo", quietly = TRUE)) {
    set.seed(as.integer(format(Sys.Date(), "%Y%m%d")))  # reproducible per day
    spot_tickers <- sample(pool, min(n_random, length(pool)))

    tryCatch({
      riingo::riingo_set_token(tiingo_key5)

      date_from <- as.Date(index(xts_ret)[1])
      date_to   <- as.Date(index(xts_ret)[nrow(xts_ret)])

      spot_raw <- riingo::riingo_prices(spot_tickers,
                                        start_date = date_from,
                                        end_date   = date_to) %>%
        group_by(ticker) %>%
        arrange(date) %>%
        summarise(tr_tiingo = (last(adjClose) / first(adjClose)) - 1,
                  .groups = "drop") %>%
        rename(symbol = ticker)

      spot_yahoo <- tibble(
        symbol   = spot_tickers,
        tr_yahoo = map_dbl(spot_tickers,
                           ~exp(sum(xts_ret[, .x], na.rm = TRUE)) - 1)
      )

      spot_cmp <- spot_raw %>%
        left_join(spot_yahoo, by = "symbol") %>%
        mutate(
          diff_pp = (tr_yahoo - tr_tiingo) * 100,
          status  = case_when(
            abs(diff_pp) <= 2  ~ "PASS",
            abs(diff_pp) <= 5  ~ "WARN",
            TRUE               ~ "FAIL"
          )
        )

      for (i in seq_len(nrow(spot_cmp))) {
        r <- spot_cmp[i, ]
        results[[paste0("spot_", r$symbol)]] <- tibble(
          layer  = "Spot-Check",
          check  = paste0(r$symbol, " total return"),
          value  = scales::percent(r$tr_yahoo,  accuracy = 0.1),
          target = scales::percent(r$tr_tiingo, accuracy = 0.1),
          status = r$status,
          detail = sprintf("Yahoo %.1f%%  Tiingo %.1f%%  Δ%.1fpp",
                           r$tr_yahoo * 100, r$tr_tiingo * 100, r$diff_pp)
        )
      }
    }, error = function(e) {
      message("Tiingo spot-check skipped: ", conditionMessage(e))
    })
  }

  # ── COMPILE ───────────────────────────────────────────────────────────────

  report <- bind_rows(results) %>%
    mutate(value  = as.character(value),
           status = factor(status, levels = c("FAIL", "WARN", "PASS")))

  summary <- report %>%
    count(status, .drop = FALSE) %>%
    tidyr::pivot_wider(names_from = status, values_from = n, values_fill = 0)

  passed <- !any(report$status == "FAIL")

  # ── PRINT ─────────────────────────────────────────────────────────────────

  cat("\n══ DATA AUDIT ══════════════════════════════════════════════════════\n")
  cat(sprintf("  Period : %s → %s  (%d rows)\n",
              format(as.Date(index(xts_ret)[1]),   "%Y-%m-%d"),
              format(as.Date(index(xts_ret)[nrow(xts_ret)]), "%Y-%m-%d"),
              nrow(xts_ret)))

  n_fail <- sum(report$status == "FAIL")
  n_warn <- sum(report$status == "WARN")
  n_pass <- sum(report$status == "PASS")
  cat(sprintf("  Result : %d PASS  %d WARN  %d FAIL\n\n", n_pass, n_warn, n_fail))

  to_print <- if (verbose) report else report %>% filter(status != "PASS")

  if (nrow(to_print) == 0) {
    cat("  ✓ All checks passed.\n")
  } else {
    for (i in seq_len(nrow(to_print))) {
      r <- to_print[i, ]
      icon <- switch(as.character(r$status), FAIL = "✗", WARN = "△", PASS = "✓")
      cat(sprintf("  %s [%s] %-30s %s\n",
                  icon, r$layer, r$check, r$detail))
    }
  }
  cat("════════════════════════════════════════════════════════════════════\n\n")

  invisible(list(report = report, summary = summary, passed = passed,
                 ann_ret = ann_df))
}

message("data_audit: loaded  |  call run_data_audit(xts_ret, etf_metadata)")
