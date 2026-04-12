################################################################################
# PROJECT : ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE    : scripts/00_derived_universe.R
# Purpose : Derived / Synthetic portfolio registry (dU).
#           Constructed portfolios — not downloadable, computed from iU components.
#           Source AFTER 00_init_universe.R and 00_global_params.R.
#
# SCHEMA
#   id           Unique identifier — lowercase, snake_case (e.g. "eur6040_v1")
#   name         Human-readable label
#   components   Character vector of iU tickers
#   weights      Numeric vector (same length as components, must sum to 1)
#   rebal        Rebalancing frequency — defaults to REBAL_FREQ from 00_global_params.R
#                Valid: "Daily" | "Weekly" | "Monthly" | "Quarterly" | "Annual"
#   currency     Portfolio base currency ("usd" | "eur" | "chf" | "gbp")
#   version      Integer version counter — bump when components/weights change
#   description  One-line rationale
#
# VERSIONING RULE
#   id encodes the concept; version encodes the iteration.
#   When components or weights change, increment version — never overwrite.
#   current_derived("eur6040") returns the highest-version row for that prefix.
#
# REBALANCING OVERRIDE
#   Each row uses REBAL_FREQ by default. Override at row level:
#     rebal = "Monthly"   — or —
#   Override at call time:
#     build_derived_returns(du_row, xts_ret, rebal = "Daily")
################################################################################

if (!exists("REBAL_FREQ"))   source(here::here("00_global_params.R"))
if (!exists("etf_metadata")) source(here::here("scripts/00_init_universe.R"))

# ── Registry ──────────────────────────────────────────────────────────────────

DERIVED_UNIVERSE <- tibble::tribble(
  ~id,             ~name,                         ~components,                    ~weights,           ~rebal,      ~currency, ~version, ~description,

  # ── USD balanced benchmarks ─────────────────────────────────────────────────
  "usd6040",       "USD 60/40",                   list(c("SPY","AGG")),           list(c(0.60,0.40)), REBAL_FREQ,  "usd",     1L,       "Classic SPY/AGG 60/40 — primary USD balanced benchmark",
  "usd_saa",       "USD SAA (project baseline)",  list(c("AGG","URTH","SPY","QQQ","XLK","SMH")),
                                                                                  list(c(0.35,0.25,0.25,0.05,0.05,0.05)),
                                                                                                      REBAL_FREQ,  "usd",     1L,       "Project SAA: 65% EQ / 35% FI per CLAUDE.md definition",

  # ── EUR balanced benchmarks ─────────────────────────────────────────────────
  # v1: EM equity (EUR-listed) + EUR govt bonds — primary EUR 60/40
  "eur6040_v1",    "EUR 60/40 (EM + EUR Govt)",   list(c("EUNM.DE","IEGA.DE")),  list(c(0.60,0.40)), REBAL_FREQ,  "eur",     1L,       "EM equity (EUR UCITS) + EUR govt bonds — natively EUR, zero FX cost",

  # v2: European equity (EUR-listed) + EUR govt bonds
  "eur6040_v2",    "EUR 60/40 (EU Eq + EUR Govt)",list(c("EUNK.DE","IEGA.DE")),  list(c(0.60,0.40)), REBAL_FREQ,  "eur",     2L,       "European equity (MSCI Europe EUR) + EUR govt bonds",

  # v3: World EUR-hedged + Global Agg EUR-hedged — fully hedged global 60/40
  "eur6040_v3",    "EUR 60/40 (World Hdg + GAgg)",list(c("IWDE.AS","EUNA.DE")),  list(c(0.60,0.40)), REBAL_FREQ,  "eur",     3L,       "MSCI World EUR-hedged + Global Agg EUR-hedged — global exposure, no FX drag",

  # ── CHF balanced benchmarks ─────────────────────────────────────────────────
  # Note: no CHF-hedged ETFs in iU yet; using EUR instruments as closest proxy
  # (EUR/CHF more stable than USD/CHF). Label clearly in app.
  "chf6040_v1",    "CHF 60/40 (EUR proxy)",       list(c("IWDE.AS","EUNA.DE")),  list(c(0.60,0.40)), REBAL_FREQ,  "chf",     2L,       "EUR proxy for CHF 60/40: MSCI World EUR-hdg (IWDE.AS) + Global Agg EUR-hdg (EUNA.DE) — v2: replaced EM equity + undownloadable IEGA.DE"
)

# ── Validation ────────────────────────────────────────────────────────────────

#' validate_derived_universe
#' Checks all components exist in iU and weights sum to 1 (within tolerance).
#' Prints a clean report; returns invisible(TRUE/FALSE).

validate_derived_universe <- function(du = DERIVED_UNIVERSE,
                                       etf_meta = etf_metadata,
                                       tol = 1e-6) {
  ok <- TRUE

  for (i in seq_len(nrow(du))) {
    row  <- du[i, ]
    tks  <- row$components[[1]][[1]]
    wts  <- row$weights[[1]][[1]]

    # weight sum check
    wsum <- sum(wts)
    if (abs(wsum - 1) > tol) {
      message("⚠️  [", row$id, "] weights sum to ", round(wsum, 6), " (expected 1)")
      ok <- FALSE
    }

    # length match
    if (length(tks) != length(wts)) {
      message("⚠️  [", row$id, "] components length (", length(tks),
              ") != weights length (", length(wts), ")")
      ok <- FALSE
    }

    # components in iU
    missing <- setdiff(tks, etf_meta$ticker)
    if (length(missing) > 0) {
      message("⚠️  [", row$id, "] components not in iU: ",
              paste(missing, collapse = ", "))
      ok <- FALSE
    }
  }

  if (ok) message("✅  derived_universe validated — ", nrow(du), " portfolios, all components in iU")
  invisible(ok)
}

# ── Inception helper ──────────────────────────────────────────────────────────

#' get_derived_inception
#' Returns effective start date = latest component inception (limiting factor).

get_derived_inception <- function(du_row, etf_meta = etf_metadata) {
  tks <- du_row$components[[1]][[1]]
  dates <- etf_meta %>%
    dplyr::filter(ticker %in% tks, !is.na(inception)) %>%
    dplyr::pull(inception) %>%
    as.Date()
  if (length(dates) == 0) return(NA_Date_)
  max(dates)   # latest inception is the binding constraint
}

# ── Return builder ────────────────────────────────────────────────────────────

#' build_derived_returns
#'
#' Constructs a daily return xts for one derived portfolio row.
#' Applies periodic rebalancing (constant-weight).
#'
#' @param du_row   Single-row tibble from DERIVED_UNIVERSE
#' @param xts_ret  xts of daily returns (from 01_etf_wrangle.R)
#' @param rebal    Override rebalancing frequency (NULL = use du_row$rebal)
#' @param start    Optional Date to clip the series
#'
#' @return Named single-column xts

build_derived_returns <- function(du_row,
                                   xts_ret,
                                   rebal = NULL,
                                   start = NULL) {
  freq <- if (!is.null(rebal)) rebal else du_row$rebal
  tks  <- du_row$components[[1]][[1]]
  wts  <- du_row$weights[[1]][[1]]
  id   <- du_row$id

  # validate components present
  missing <- setdiff(tks, colnames(xts_ret))
  if (length(missing) > 0)
    stop("[", id, "] components missing from xts_ret: ", paste(missing, collapse = ", "))

  r <- xts_ret[, tks]
  r <- r[complete.cases(r), ]               # trim to common history
  if (!is.null(start)) r <- r[paste0(format(start, "%Y-%m-%d"), "/")]
  if (nrow(r) < 2) stop("[", id, "] insufficient data after alignment")

  # rebalancing endpoint index
  ep <- switch(freq,
    "Daily"     = zoo::index(r),
    "Weekly"    = xts::endpoints(r, on = "weeks"),
    "Monthly"   = xts::endpoints(r, on = "months"),
    "Quarterly" = xts::endpoints(r, on = "quarters"),
    "Annual"    = xts::endpoints(r, on = "years"),
    stop("Unknown rebal frequency: ", freq)
  )

  # walk-forward rebalancing
  if (identical(ep, zoo::index(r))) {
    # daily: simple weighted sum each day
    ret_out <- xts::xts(
      as.numeric(as.matrix(r) %*% wts),
      order.by = zoo::index(r)
    )
  } else {
    chunks <- vector("list", length(ep) - 1)
    for (k in seq_along(chunks)) {
      blk <- r[(ep[k] + 1):ep[k + 1], ]
      # compound within block at fixed weights
      cum  <- apply(blk, 2, function(x) cumprod(1 + x))
      pf   <- cum %*% wts
      ret_blk <- xts::xts(
        diff(log(c(1, as.numeric(pf)))),
        order.by = zoo::index(blk)
      )
      chunks[[k]] <- ret_blk
    }
    ret_out <- do.call(rbind, chunks)
  }

  colnames(ret_out) <- id
  ret_out
}

# ── Convenience selector ──────────────────────────────────────────────────────

#' current_derived
#' Returns the latest-version row for a given id prefix.
#' current_derived("eur6040")  →  eur6040_v3 row

current_derived <- function(prefix, du = DERIVED_UNIVERSE) {
  hits <- du[startsWith(du$id, prefix), ]
  if (nrow(hits) == 0) stop("No derived portfolio found with prefix: ", prefix)
  hits[which.max(hits$version), ]
}

# ── Auto-run: validate on source ──────────────────────────────────────────────

if (!isTRUE(getOption("knitr.in.progress"))) {
  validate_derived_universe()
  message("--- DERIVED UNIVERSE READY: ", nrow(DERIVED_UNIVERSE), " portfolios ---")
  message("  IDs: ", paste(DERIVED_UNIVERSE$id, collapse = " | "))
}

################################################################################
# END OF FILE
################################################################################
