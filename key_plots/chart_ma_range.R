################################################################################
# CHART TEMPLATE : ma_range
# FILE           : key_plots/chart_ma_range.R
#
# FUNCTIONS
# ─────────────────────────────────────────────────────────────────────────────
# plot_ma_range(ticker, xts_ret, from_date, ma1, ma2, win_52w, compact)
#
#   compact = FALSE (default) → 3-panel:
#       P1  Cumulative return + MA lines + 52W high/low band
#       P2  % distance from 52W High (≤0) and 52W Low (≥0)
#       P3  % distance from 50DMA and 200DMA
#
#   compact = TRUE → 2-panel:
#       P1  same as above
#       P2  all 4 distances on ONE strip (52W Hi / 52W Lo / 50DMA / 200DMA)
#
# plot_ma_gauge(tickers, xts_ret, ma1, ma2, win_52w, flip)
#   Single horizontal ruler — one row per ticker, all 5 references on one line:
#   52W Lo ←───[200DMA]─[50DMA]─◆ PRICE ─────────→ 52W Hi
#   X-axis = % distance TO reference FROM current price
#   State-coloured diamond (Bear/Trending/Stretched/Pullback/Stressed/Correction)
#   DD shading from 0 to peak. State + ticker label at right edge.
#   flip=TRUE: vertical layout (first ticker on left)
#
# plot_gauge_spc(tickers, xts_ret, ma1, ma2, win_52w)
#   SPC cross-sectional control chart (Shewhart 1924 inspired):
#   Centre = 200DMA (0%)  |  Measurement = % from 200DMA
#   Zones: green ±1σ  amber ±2σ  (σ = median annualised 52W vol across tickers)
#   Grey bar = 52W Hi–Lo  |  Yellow circle = 50DMA  |  State-coloured lollipop
#
# INVOKE
#   source(here("key_plots/chart_ma_range.R"))
#   plot_ma_range("URTH", xts_ret)                             # 3-panel
#   plot_ma_range("URTH", xts_ret, compact = TRUE)             # 2-panel condensed
#   plot_ma_gauge("URTH", xts_ret)                             # single ruler
#   plot_ma_gauge(c("SPY","QQQ","GLD","AGG"), xts_ret)         # multi-ticker ruler
#   plot_ma_gauge(c("SPY","QQQ","GLD","AGG"), xts_ret, flip=TRUE)  # flipped
#   plot_gauge_spc(c("SPY","QQQ","URTH","AGG","XLK","SMH"), xts_ret)  # SPC chart
#   plot_gauge_spc(..., sort_by = "norm_pos")                          # sorted oversold→overbought
#   build_gauge_vectors(tickers, xts_ret)                              # fingerprint table
#   plot_gauge_similarity(tickers, xts_ret)                            # z-score distance heatmap
################################################################################

library(tidyverse)
library(xts)
library(zoo)
library(patchwork)
library(scales)

plot_ma_range <- function(
    ticker,
    xts_ret,
    from_date = "2020-01-01",
    ma1       = 50,
    ma2       = 200,
    win_52w   = 252,       # trading-day window for 52W high/low
    compact   = FALSE      # TRUE = merge P2+P3 into one distance strip
) {

  if (!ticker %in% colnames(xts_ret))
    stop("ticker '", ticker, "' not found in xts_ret")

  r <- as.numeric(coredata(xts_ret[, ticker]))
  r[is.na(r)] <- 0

  # ── Build full history (need win_52w look-back before from_date) ──────────
  df_full <- tibble(
    date  = as.Date(index(xts_ret)),
    price = cumprod(1 + r)
  ) %>%
    mutate(
      ma_s     = as.numeric(zoo::rollmean(price, ma1,    align = "right", fill = NA)),
      ma_l     = as.numeric(zoo::rollmean(price, ma2,    align = "right", fill = NA)),
      hi_52w   = as.numeric(zoo::rollmax( price, win_52w, align = "right", fill = NA)),
      lo_52w   = as.numeric(zoo::rollapply(price, win_52w,
                                           FUN = min, align = "right", fill = NA)),
      peak     = cummax(price),
      dd       = (price - peak) / peak,
      # % distances
      d_hi   = (price / hi_52w - 1) * 100,   # ≤ 0  (0 = at 52W high)
      d_lo   = (price / lo_52w - 1) * 100,   # ≥ 0  (0 = at 52W low)
      d_ma_s = (price / ma_s   - 1) * 100,
      d_ma_l = (price / ma_l   - 1) * 100
    )

  df <- df_full %>% filter(date >= as.Date(from_date))

  last   <- tail(df, 1)
  d_rng  <- diff(range(df$date))

  # ── Snapshot values for annotations ───────────────────────────────────────
  snap <- sprintf(
    "52W Hi: %+.1f%%   52W Lo: %+.1f%%   %dDMA: %+.1f%%   %dDMA: %+.1f%%",
    last$d_hi, last$d_lo, ma1, last$d_ma_s, ma2, last$d_ma_l
  )

  # ── Colour palette ─────────────────────────────────────────────────────────
  COL_HI  <- "#dc2626"   # red    — distance from 52W high
  COL_LO  <- "#16a34a"   # green  — distance from 52W low
  COL_S   <- "#f59e0b"   # amber  — short MA
  COL_L   <- "#1d3461"   # navy   — long MA

  # ── P1: Cumulative return + MAs + 52W band ────────────────────────────────
  p1 <- ggplot(df, aes(x = date)) +

    # 52W high/low band
    geom_ribbon(aes(ymin = lo_52w, ymax = hi_52w),
                fill = "#e5e7eb", alpha = 0.55, na.rm = TRUE) +

    # MA lines
    geom_line(aes(y = ma_l), colour = COL_L, linewidth = 0.8,
              linetype = "dashed", na.rm = TRUE) +
    geom_line(aes(y = ma_s), colour = COL_S, linewidth = 0.7,
              linetype = "dashed", na.rm = TRUE) +

    # 52W high / low edges
    geom_line(aes(y = hi_52w), colour = COL_HI, linewidth = 0.5,
              linetype = "dotted", na.rm = TRUE) +
    geom_line(aes(y = lo_52w), colour = COL_LO, linewidth = 0.5,
              linetype = "dotted", na.rm = TRUE) +

    # Price
    geom_line(aes(y = price), colour = COL_L, linewidth = 1.0) +

    # End-point labels
    annotate("text", x = last$date + as.integer(d_rng * 0.015),
             y = last$price,  hjust = 0, size = 2.7, fontface = "bold",
             colour = COL_L,
             label = sprintf("%s\n%.2f", ticker, last$price)) +
    annotate("text", x = last$date + as.integer(d_rng * 0.015),
             y = last$hi_52w, hjust = 0, size = 2.3, colour = COL_HI,
             label = sprintf("52W Hi\n%.2f", last$hi_52w)) +
    annotate("text", x = last$date + as.integer(d_rng * 0.015),
             y = last$lo_52w, hjust = 0, size = 2.3, colour = COL_LO,
             label = sprintf("52W Lo\n%.2f", last$lo_52w)) +
    annotate("text", x = last$date + as.integer(d_rng * 0.015),
             y = last$ma_s,   hjust = 0, size = 2.2, colour = COL_S,
             label = sprintf("%dDMA", ma1)) +
    annotate("text", x = last$date + as.integer(d_rng * 0.015),
             y = last$ma_l,   hjust = 0, size = 2.2, colour = COL_L, alpha = 0.65,
             label = sprintf("%dDMA", ma2)) +

    scale_x_date(expand = expansion(mult = c(0, 0.13))) +
    scale_y_continuous(labels = label_number(accuracy = 0.01)) +
    labs(
      title    = paste0(ticker, "  \u2014  Range & MA Position"),
      subtitle = snap,
      x = NULL, y = "Wealth index (base 1.0)"
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          plot.title    = element_text(face = "bold"),
          plot.subtitle = element_text(size = 8.5, colour = "#444",
                                       family = "mono"))

  # ── P2: Distance from 52W High and Low ────────────────────────────────────
  p2 <- ggplot(df, aes(x = date)) +
    geom_hline(yintercept = 0, colour = "#9ca3af", linewidth = 0.4) +
    geom_line(aes(y = d_hi), colour = COL_HI, linewidth = 0.8, na.rm = TRUE) +
    geom_line(aes(y = d_lo), colour = COL_LO, linewidth = 0.8, na.rm = TRUE) +
    annotate("text", x = last$date + as.integer(d_rng * 0.015),
             y = last$d_hi, hjust = 0, size = 2.5, fontface = "bold",
             colour = COL_HI,
             label = sprintf("%+.1f%%\n52W Hi", last$d_hi)) +
    annotate("text", x = last$date + as.integer(d_rng * 0.015),
             y = last$d_lo, hjust = 0, size = 2.5, fontface = "bold",
             colour = COL_LO,
             label = sprintf("%+.1f%%\n52W Lo", last$d_lo)) +
    scale_x_date(expand = expansion(mult = c(0, 0.13))) +
    scale_y_continuous(labels = percent_format(scale = 1, accuracy = 1)) +
    labs(x = NULL, y = "vs 52W Hi / Lo") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())

  # ── P3: Distance from MAs ─────────────────────────────────────────────────
  p3 <- ggplot(df, aes(x = date)) +
    geom_hline(yintercept = 0, colour = "#9ca3af", linewidth = 0.4) +
    geom_line(aes(y = d_ma_s), colour = COL_S, linewidth = 0.8, na.rm = TRUE) +
    geom_line(aes(y = d_ma_l), colour = COL_L,  linewidth = 0.8, na.rm = TRUE) +
    annotate("text", x = last$date + as.integer(d_rng * 0.015),
             y = last$d_ma_s, hjust = 0, size = 2.5, fontface = "bold",
             colour = COL_S,
             label = sprintf("%+.1f%%\n%dDMA", last$d_ma_s, ma1)) +
    annotate("text", x = last$date + as.integer(d_rng * 0.015),
             y = last$d_ma_l, hjust = 0, size = 2.5, fontface = "bold",
             colour = COL_L,
             label = sprintf("%+.1f%%\n%dDMA", last$d_ma_l, ma2)) +
    scale_x_date(expand = expansion(mult = c(0, 0.13))) +
    scale_y_continuous(labels = percent_format(scale = 1, accuracy = 1)) +
    labs(x = NULL, y = "vs MA") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())

  # ── P2 compact: all 4 distances on one strip ──────────────────────────────
  p2_compact <- ggplot(df, aes(x = date)) +
    geom_hline(yintercept = 0, colour = "#9ca3af", linewidth = 0.4) +
    geom_line(aes(y = d_hi),   colour = COL_HI, linewidth = 0.75, na.rm = TRUE) +
    geom_line(aes(y = d_lo),   colour = COL_LO, linewidth = 0.75, na.rm = TRUE) +
    geom_line(aes(y = d_ma_s), colour = COL_S,  linewidth = 0.75, linetype = "dashed", na.rm = TRUE) +
    geom_line(aes(y = d_ma_l), colour = COL_L,  linewidth = 0.75, linetype = "dashed", na.rm = TRUE) +
    # end labels — stagger x slightly to avoid overlap
    annotate("text", x = last$date + as.integer(d_rng * 0.015),
             y = last$d_hi,   hjust = 0, size = 2.4, fontface = "bold",
             colour = COL_HI, label = sprintf("%+.1f%% 52W Hi", last$d_hi)) +
    annotate("text", x = last$date + as.integer(d_rng * 0.015),
             y = last$d_lo,   hjust = 0, size = 2.4, fontface = "bold",
             colour = COL_LO, label = sprintf("%+.1f%% 52W Lo", last$d_lo)) +
    annotate("text", x = last$date + as.integer(d_rng * 0.015),
             y = last$d_ma_s, hjust = 0, size = 2.4, fontface = "bold",
             colour = COL_S,  label = sprintf("%+.1f%% %dDMA", last$d_ma_s, ma1)) +
    annotate("text", x = last$date + as.integer(d_rng * 0.015),
             y = last$d_ma_l, hjust = 0, size = 2.4, fontface = "bold",
             colour = COL_L,  label = sprintf("%+.1f%% %dDMA", last$d_ma_l, ma2)) +
    scale_x_date(expand = expansion(mult = c(0, 0.16))) +
    scale_y_continuous(labels = percent_format(scale = 1, accuracy = 1)) +
    labs(x = NULL, y = "% distance") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())

  # ── Combine ────────────────────────────────────────────────────────────────
  if (compact)
    p1 / p2_compact + plot_layout(heights = c(3, 1.5))
  else
    p1 / p2 / p3 + plot_layout(heights = c(3, 1.2, 1.2))
}

# ══════════════════════════════════════════════════════════════════════════════
# plot_ma_gauge — single horizontal ruler, one row per ticker
# ══════════════════════════════════════════════════════════════════════════════
plot_ma_gauge <- function(
    tickers,
    xts_ret,
    ma1     = 50,
    ma2     = 200,
    win_52w = 252,
    flip    = FALSE   # TRUE = vertical layout, first ticker on left
) {

  tickers <- tickers[tickers %in% colnames(xts_ret)]
  if (length(tickers) == 0) stop("No valid tickers found in xts_ret")

  # ── Ticker state classifier ───────────────────────────────────────────────
  .ticker_state <- function(px, ma_s, ma_l, dd_pct, hi_52w) {
    case_when(
      (px / hi_52w - 1) < -0.20                         ~ "Bear",
      px > ma_s & ma_s > ma_l & dd_pct > -0.05          ~ "Trending",
      px > ma_l & dd_pct > -0.10                         ~ "Stretched",
      px < ma_s & px > ma_l                              ~ "Pullback",
      px < ma_l & dd_pct > -0.20                         ~ "Stressed",
      TRUE                                               ~ "Correction"
    )
  }
  STATE_COL <- c(
    Bear       = "#7f1d1d",
    Trending   = "#16a34a",
    Stretched  = "#38bdf8",
    Pullback   = "#fbbf24",
    Stressed   = "#f97316",
    Correction = "#dc2626"
  )

  # ── Compute snapshot for each ticker ──────────────────────────────────────
  snap_meta <- purrr::map_dfr(tickers, function(tk) {
    r     <- as.numeric(coredata(xts_ret[, tk])); r[is.na(r)] <- 0
    price <- cumprod(1 + r)
    n     <- length(price)
    w     <- price[max(1, n - win_52w + 1):n]

    hi_52w <- max(w, na.rm = TRUE)
    lo_52w <- min(w, na.rm = TRUE)
    ma_s   <- if (n >= ma1) mean(tail(price, ma1)) else NA_real_
    ma_l   <- if (n >= ma2) mean(tail(price, ma2)) else NA_real_
    px     <- price[n]
    peak   <- max(price, na.rm = TRUE)
    dd_pct <- (px / peak) - 1

    tibble(
      ticker   = tk,
      px       = px,
      peak_pct = (peak / px - 1) * 100,   # % to recover to ATH
      dd_pct   = dd_pct * 100,
      state    = .ticker_state(px, ma_s, ma_l, dd_pct, hi_52w)
    )
  })

  snap <- purrr::map_dfr(tickers, function(tk) {
    r     <- as.numeric(coredata(xts_ret[, tk])); r[is.na(r)] <- 0
    price <- cumprod(1 + r)
    n     <- length(price)
    w     <- price[max(1, n - win_52w + 1):n]

    hi_52w <- max(w, na.rm = TRUE)
    lo_52w <- min(w, na.rm = TRUE)
    ma_s   <- if (n >= ma1) mean(tail(price, ma1)) else NA_real_
    ma_l   <- if (n >= ma2) mean(tail(price, ma2)) else NA_real_
    px     <- price[n]
    peak   <- max(price, na.rm = TRUE)

    tibble(
      ticker   = tk,
      ref      = c("52W Hi", sprintf("%dDMA", ma2), sprintf("%dDMA", ma1),
                   "Price", "Peak", "52W Lo"),
      pct      = c(
        (hi_52w / px - 1) * 100,
        (ma_l   / px - 1) * 100,
        (ma_s   / px - 1) * 100,
        0,
        (peak   / px - 1) * 100,
        (lo_52w / px - 1) * 100
      ),
      col      = c("#4b5563", "#3b82f6", "#f59e0b", "#1d3461", "#4b5563", "#4b5563"),
      shape    = c(124, 124, 124, 18, 124, 124),
      sz       = c(4, 4, 4, 5, 4, 4),
      lbl_side = c("top", "bottom", "top", "top", "bottom", "bottom")
    )
  })

  # DD shading data: from 0% to peak_pct per ticker
  dd_shade <- snap_meta %>% select(ticker, peak_pct)

  # Colour the Price diamond by ticker state
  snap <- snap %>%
    left_join(snap_meta %>% select(ticker, state), by = "ticker") %>%
    mutate(col = if_else(ref == "Price", STATE_COL[state], col)) %>%
    select(-state)

  # Full labels on first ticker row; % only on remaining rows
  lbl_above      <- snap %>% filter(lbl_side == "top",    ticker == tickers[1])
  lbl_below      <- snap %>% filter(lbl_side == "bottom", ticker == tickers[1])
  lbl_pct_above  <- snap %>% filter(lbl_side == "top",    ticker != tickers[1])
  lbl_pct_below  <- snap %>% filter(lbl_side == "bottom", ticker != tickers[1])

  # Horizontal: first ticker on top  → rev(tickers)
  # Flipped:    first ticker on left → tickers (natural order, left = level 1...
  #             but after coord_flip left = bottom of original = level 1)
  lvls <- if (flip) tickers else rev(tickers)

  snap          <- snap          %>% mutate(ticker = factor(ticker, levels = lvls))
  lbl_above     <- lbl_above     %>% mutate(ticker = factor(ticker, levels = lvls))
  lbl_below     <- lbl_below     %>% mutate(ticker = factor(ticker, levels = lvls))
  lbl_pct_above <- lbl_pct_above %>% mutate(ticker = factor(ticker, levels = lvls))
  lbl_pct_below <- lbl_pct_below %>% mutate(ticker = factor(ticker, levels = lvls))
  dd_shade  <- dd_shade  %>% mutate(ticker = factor(ticker, levels = lvls))
  snap_meta <- snap_meta %>% mutate(ticker = factor(ticker, levels = lvls))

  x_pad <- diff(range(snap$pct, na.rm = TRUE)) * 0.12
  x_lo  <- min(snap$pct, na.rm = TRUE) - x_pad
  x_hi  <- max(snap$pct, na.rm = TRUE) + x_pad

  # ── Build plot ─────────────────────────────────────────────────────────────
  p <- ggplot(snap, aes(x = pct, y = ticker)) +

    # 52W range bar (Lo → Hi) per ticker
    geom_segment(
      data = snap %>%
        filter(ref %in% c("52W Hi", "52W Lo")) %>%
        select(ticker, ref, pct) %>%
        tidyr::pivot_wider(names_from = ref, values_from = pct),
      aes(x = `52W Lo`, xend = `52W Hi`, y = ticker, yend = ticker),
      inherit.aes = FALSE,
      colour = "#e5e7eb", linewidth = 5, lineend = "round"
    ) +

    # DD shading: 0% → peak (orange fill = recovery needed)
    geom_rect(
      data = dd_shade,
      aes(xmin = 0, xmax = peak_pct,
          ymin = as.numeric(ticker) - 0.22,
          ymax = as.numeric(ticker) + 0.22),
      inherit.aes = FALSE,
      fill = "#f97316", alpha = 0.15
    ) +

    # Zero line (current price)
    geom_vline(xintercept = 0, colour = "#6b7280", linewidth = 0.5,
               linetype = "dashed") +

    # Pipe markers: fixed height, thick linewidth
    geom_segment(
      data = snap %>% filter(ref != "Price"),
      aes(x = pct, xend = pct,
          y    = as.numeric(ticker) - 0.08,
          yend = as.numeric(ticker) + 0.08,
          colour = I(col)),
      inherit.aes = FALSE,
      linewidth = 1.5
    ) +

    # Diamond: Price only, original size
    geom_point(
      data = snap %>% filter(ref == "Price"),
      aes(x = pct, y = ticker, colour = I(col)),
      inherit.aes = FALSE,
      shape = 18, size = 5
    ) +

    # State label on the right
    geom_text(
      data = snap_meta,
      aes(x = x_hi, y = ticker,
          label = sprintf("%s  %.1f%%", state, dd_pct),
          colour = I(STATE_COL[state])),
      inherit.aes = FALSE,
      hjust = 1, vjust = 1.8, size = 2.8, fontface = "bold"
    ) +

    # Ticker name in black just below state label
    geom_text(
      data = snap_meta,
      aes(x = x_hi, y = ticker, label = ticker),
      inherit.aes = FALSE,
      hjust = 1, vjust = 3.2, size = 2.8,
      colour = "black", fontface = "bold"
    ) +

    # Labels above
    ggrepel::geom_text_repel(
      data        = lbl_above,
      aes(label = sprintf("%s\n%+.1f%%", ref, pct), colour = I(col)),
      size        = 2.8, fontface = "bold",
      nudge_y     = 0.35, direction = "x",
      segment.size = 0.3, segment.colour = "#aaaaaa",
      min.segment.length = 0.2, show.legend = FALSE
    ) +

    # Labels below
    ggrepel::geom_text_repel(
      data        = lbl_below,
      aes(label = sprintf("%s\n%+.1f%%", ref, pct), colour = I(col)),
      size        = 2.8, fontface = "bold",
      nudge_y     = -0.35, direction = "x",
      segment.size = 0.3, segment.colour = "#aaaaaa",
      min.segment.length = 0.2, show.legend = FALSE
    ) +

    # % only — above (non-first tickers)
    ggrepel::geom_text_repel(
      data        = lbl_pct_above,
      aes(label = sprintf("%+.1f%%", pct), colour = I(col)),
      size        = 2.4,
      nudge_y     = 0.28, direction = "x",
      segment.size = 0.2, segment.colour = "#cccccc",
      min.segment.length = 0.2, show.legend = FALSE
    ) +

    # % only — below (non-first tickers)
    ggrepel::geom_text_repel(
      data        = lbl_pct_below,
      aes(label = sprintf("%+.1f%%", pct), colour = I(col)),
      size        = 2.4,
      nudge_y     = -0.28, direction = "x",
      segment.size = 0.2, segment.colour = "#cccccc",
      min.segment.length = 0.2, show.legend = FALSE
    ) +

    scale_x_continuous(
      labels = function(x) sprintf("%+.0f%%", x),
      limits = c(x_lo, x_hi)
    ) +
    labs(
      title    = "Price Position Ruler",
      subtitle = sprintf(
        "As of %s  |  0%% = current price  |  orange band = DD to ATH peak  |  state: Trending / Stretched / Pullback / Stressed / Correction",
        format(as.Date(tail(index(xts_ret), 1)))
      ),
      x = "% distance from current price",
      y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(face = "bold"),
      plot.subtitle      = element_text(size = 9, colour = "#555"),
      axis.text.y        = element_text(face = "bold", size = 10)
    )

  if (flip) p <- p + coord_flip()

  p
}

# ══════════════════════════════════════════════════════════════════════════════
# build_gauge_vectors — fingerprint table for a set of tickers
#
# Returns a tibble with one row per ticker:
#   Raw distances : d_52w_hi, d_200dma, d_50dma, dd_pct, d_52w_lo  (% from price)
#   Norm position : norm_pos ∈ [0,1]  (0 = 52W low, 1 = 52W high)
#   Z-scores      : z_52w_hi, z_200dma, z_50dma, z_dd, z_52w_lo  (distance ÷ σ)
#   State label   : state
#
# INVOKE
#   gv <- build_gauge_vectors(c("SPY","QQQ","GLD","AGG"), xts_ret)
# ══════════════════════════════════════════════════════════════════════════════
build_gauge_vectors <- function(
    tickers,
    xts_ret,
    ma1     = 50,
    ma2     = 200,
    win_52w = 252
) {
  tickers <- tickers[tickers %in% colnames(xts_ret)]
  if (length(tickers) == 0) stop("No valid tickers found in xts_ret")

  .ts <- function(px, ma_s, ma_l, dd_raw, hi_52w) {
    dplyr::case_when(
      (px / hi_52w - 1) < -0.20                      ~ "Bear",
      px > ma_s & ma_s > ma_l & dd_raw > -0.05       ~ "Trending",
      px > ma_l & dd_raw > -0.10                      ~ "Stretched",
      px < ma_s & px > ma_l                           ~ "Pullback",
      px < ma_l & dd_raw > -0.20                      ~ "Stressed",
      TRUE                                            ~ "Correction"
    )
  }

  purrr::map_dfr(tickers, function(tk) {
    r      <- as.numeric(coredata(xts_ret[, tk])); r[is.na(r)] <- 0
    price  <- cumprod(1 + r)
    n      <- length(price)
    w      <- price[max(1, n - win_52w + 1):n]
    px     <- price[n]
    peak   <- max(price, na.rm = TRUE)

    hi_52w <- max(w, na.rm = TRUE)
    lo_52w <- min(w, na.rm = TRUE)
    ma_s   <- if (n >= ma1) mean(tail(price, ma1)) else NA_real_
    ma_l   <- if (n >= ma2) mean(tail(price, ma2)) else NA_real_
    dd_raw <- (px / peak) - 1

    r_52w  <- diff(log(w))
    sigma  <- sd(r_52w, na.rm = TRUE) * sqrt(252) * 100

    d_52w_hi <- (px / hi_52w - 1) * 100
    d_200dma <- (px / ma_l   - 1) * 100
    d_50dma  <- (px / ma_s   - 1) * 100
    d_52w_lo <- (px / lo_52w - 1) * 100
    dd_pct   <- dd_raw * 100
    norm_pos <- (px - lo_52w) / (hi_52w - lo_52w)

    tibble(
      ticker   = tk,
      as_of    = as.Date(tail(index(xts_ret), 1)),
      state    = .ts(px, ma_s, ma_l, dd_raw, hi_52w),
      d_52w_hi = d_52w_hi,
      d_200dma = d_200dma,
      d_50dma  = d_50dma,
      dd_pct   = dd_pct,
      d_52w_lo = d_52w_lo,
      norm_pos = norm_pos,
      sigma    = sigma,
      z_52w_hi = d_52w_hi / sigma,
      z_200dma = d_200dma / sigma,
      z_50dma  = d_50dma  / sigma,
      z_dd     = dd_pct   / sigma,
      z_52w_lo = d_52w_lo / sigma
    )
  })
}

# ══════════════════════════════════════════════════════════════════════════════
# plot_gauge_spc — SPC-inspired cross-sectional control chart
#
# Inspired by Shewhart control charts (1924):
#   • Centre line  = 200DMA  (the "process mean")
#   • Measurement  = % distance of current price from 200DMA
#   • Control zones derived from 52W daily return volatility (σ):
#       Zone 1  |d| < 1σ   → green   "In Control"
#       Zone 2  |d| < 2σ   → amber   "Warning"
#       Zone 3  |d| > 2σ   → red     "Action"
#   • 52W Hi/Lo shown as range bars (like measurement uncertainty)
#   • 50DMA shown as secondary marker
#   • Lollipop from 0 to current value, coloured by ticker state
#
# INVOKE
#   source(here("key_plots/chart_ma_range.R"))
#   plot_gauge_spc(c("SPY","QQQ","URTH","AGG","XLK","SMH"), xts_ret)
# ══════════════════════════════════════════════════════════════════════════════
plot_gauge_spc <- function(
    tickers,
    xts_ret,
    ma1     = 50,
    ma2     = 200,
    win_52w = 252,
    sort_by = c("none", "norm_pos")   # "norm_pos": oversold left → overbought right
) {
  sort_by <- match.arg(sort_by)

  tickers <- tickers[tickers %in% colnames(xts_ret)]
  if (length(tickers) == 0) stop("No valid tickers found in xts_ret")

  STATE_COL <- c(
    Bear       = "#7f1d1d",
    Trending   = "#16a34a",
    Stretched  = "#38bdf8",
    Pullback   = "#fbbf24",
    Stressed   = "#f97316",
    Correction = "#dc2626"
  )

  .ticker_state <- function(px, ma_s, ma_l, dd_pct, hi_52w) {
    dplyr::case_when(
      (px / hi_52w - 1) < -0.20                     ~ "Bear",
      px > ma_s & ma_s > ma_l & dd_pct > -0.05      ~ "Trending",
      px > ma_l & dd_pct > -0.10                     ~ "Stretched",
      px < ma_s & px > ma_l                          ~ "Pullback",
      px < ma_l & dd_pct > -0.20                     ~ "Stressed",
      TRUE                                           ~ "Correction"
    )
  }

  # ── Compute per-ticker metrics ─────────────────────────────────────────────
  spc <- purrr::map_dfr(tickers, function(tk) {
    r     <- as.numeric(coredata(xts_ret[, tk])); r[is.na(r)] <- 0
    price <- cumprod(1 + r)
    n     <- length(price)
    w     <- price[max(1, n - win_52w + 1):n]
    px    <- price[n]
    peak  <- max(price, na.rm = TRUE)

    hi_52w <- max(w, na.rm = TRUE)
    lo_52w <- min(w, na.rm = TRUE)
    ma_s   <- if (n >= ma1) mean(tail(price, ma1)) else NA_real_
    ma_l   <- if (n >= ma2) mean(tail(price, ma2)) else NA_real_
    dd_pct <- (px / peak) - 1

    # Annualised σ from 52W daily returns
    r_52w <- diff(log(w))
    sigma <- sd(r_52w, na.rm = TRUE) * sqrt(252) * 100   # in %

    # Measurement: % from 200DMA (the "process mean")
    meas   <- (px   / ma_l   - 1) * 100
    d_ma_s <- (px   / ma_s   - 1) * 100
    d_hi   <- (hi_52w / px   - 1) * 100    # positive: hi above price
    d_lo   <- (lo_52w / px   - 1) * 100    # negative: lo below price

    tibble(
      ticker  = tk,
      meas    = meas,
      d_ma_s  = d_ma_s,
      d_hi    = d_hi,
      d_lo    = d_lo,
      sigma   = sigma,
      dd_pct  = dd_pct * 100,
      state   = .ticker_state(px, ma_s, ma_l, dd_pct, hi_52w)
    )
  }) %>%
    mutate(
      norm_pos = -d_lo / (d_hi - d_lo),   # 0 = at 52W low, 1 = at 52W high
      zone     = case_when(
        abs(meas) <= sigma     ~ "In Control",
        abs(meas) <= 2 * sigma ~ "Warning",
        TRUE                   ~ "Action"
      )
    )

  # Sort factor levels by normalised 52W position if requested
  lvls <- if (sort_by == "norm_pos") {
    spc %>% arrange(norm_pos) %>% pull(ticker)
  } else {
    tickers
  }
  spc <- spc %>% mutate(ticker = factor(ticker, levels = lvls))

  # ── Control zone bands (same σ for all — use median σ across tickers) ─────
  med_sigma <- median(spc$sigma, na.rm = TRUE)
  y_lim     <- max(abs(c(spc$meas, spc$d_hi, -abs(spc$d_lo))),
                   2.2 * med_sigma, na.rm = TRUE) * 1.15

  # ── Build plot ─────────────────────────────────────────────────────────────
  ggplot(spc, aes(x = ticker)) +

    # Zone bands
    annotate("rect", xmin = -Inf, xmax = Inf,
             ymin = -2 * med_sigma, ymax = 2 * med_sigma,
             fill = "#fef3c7", alpha = 0.5) +
    annotate("rect", xmin = -Inf, xmax = Inf,
             ymin = -med_sigma, ymax = med_sigma,
             fill = "#dcfce7", alpha = 0.6) +

    # Fine grid lines every 5% (drawn after zone bands so they show through)
    geom_hline(yintercept = seq(-100, 100, by = 5),
               colour = "#d1d5db", linewidth = 0.3) +

    # Zone boundary lines
    geom_hline(yintercept = c(-2*med_sigma, 2*med_sigma),
               linetype = "dashed", colour = "#ef4444", linewidth = 0.5) +
    geom_hline(yintercept = c(-med_sigma, med_sigma),
               linetype = "dashed", colour = "#f59e0b", linewidth = 0.5) +
    geom_hline(yintercept = 0,
               colour = "#1e3a8a", linewidth = 0.7) +

    # 52W Hi/Lo range bar (uncertainty band)
    geom_segment(aes(x = ticker, xend = ticker,
                     y = d_lo, yend = d_hi),
                 colour = "#d1d5db", linewidth = 2.5,
                 lineend = "round") +

    # Lollipop stem: 0 → measurement
    geom_segment(aes(x = ticker, xend = ticker,
                     y = 0, yend = meas,
                     colour = I(STATE_COL[state])),
                 linewidth = 1.2) +

    # 50DMA marker
    geom_point(aes(y = d_ma_s),
               shape = 21, size = 3,
               fill = "#f59e0b", colour = "white", stroke = 0.5) +

    # Current price dot (state-coloured)
    geom_point(aes(y = meas, colour = I(STATE_COL[state])),
               shape = 18, size = 5) +

    # Zone annotations (right margin)
    annotate("text", x = length(tickers) + 0.45, y =  1.5 * med_sigma,
             label = "+1\u03c3", size = 2.5, colour = "#f59e0b", hjust = 0) +
    annotate("text", x = length(tickers) + 0.45, y =  2 * med_sigma,
             label = "+2\u03c3  Action", size = 2.5, colour = "#ef4444", hjust = 0) +
    annotate("text", x = length(tickers) + 0.45, y = -2 * med_sigma,
             label = "-2\u03c3  Action", size = 2.5, colour = "#ef4444", hjust = 0) +

    # ── Legend (right margin, lower half) ──────────────────────────────────────
    annotate("text",    x = length(tickers) + 0.45, y = -y_lim * 0.45,
             label = "Symbols", hjust = 0, size = 2.6, colour = "#374151",
             fontface = "bold") +
    annotate("segment",
             x = length(tickers) + 0.50, xend = length(tickers) + 0.85,
             y = -y_lim * 0.54, yend = -y_lim * 0.54,
             colour = "#d1d5db", linewidth = 2.5, lineend = "round") +
    annotate("text",    x = length(tickers) + 0.95, y = -y_lim * 0.54,
             label = "52W Hi\u2013Lo", hjust = 0, size = 2.4, colour = "#6b7280") +
    annotate("point",   x = length(tickers) + 0.65, y = -y_lim * 0.63,
             shape = 21, size = 3, fill = "#f59e0b", colour = "white", stroke = 0.5) +
    annotate("text",    x = length(tickers) + 0.95, y = -y_lim * 0.63,
             label = "50DMA", hjust = 0, size = 2.4, colour = "#6b7280") +
    annotate("point",   x = length(tickers) + 0.65, y = -y_lim * 0.72,
             shape = 18, size = 4, colour = "#1d3461") +
    annotate("text",    x = length(tickers) + 0.95, y = -y_lim * 0.72,
             label = "Current price", hjust = 0, size = 2.4, colour = "#6b7280") +
    annotate("segment",
             x = length(tickers) + 0.65, xend = length(tickers) + 0.65,
             y = -y_lim * 0.85, yend = -y_lim * 0.78,
             colour = "#1d3461", linewidth = 1.2) +
    annotate("text",    x = length(tickers) + 0.95, y = -y_lim * 0.815,
             label = "% from 200DMA", hjust = 0, size = 2.4, colour = "#6b7280") +

    # Ticker label at bottom of 52W range bar
    geom_text(aes(y = d_lo, label = ticker),
              vjust = 1.5, size = 3.5, colour = "#9ca3af", fontface = "bold") +

    # Value labels above dots
    geom_text(aes(y = meas,
                  label = sprintf("%+.1f%%", meas),
                  colour = I(STATE_COL[state])),
              vjust = -0.8, size = 5.5, fontface = "bold") +

    scale_y_continuous(
      labels = function(x) sprintf("%+.0f%%", x),
      limits = c(-y_lim, y_lim),
      breaks = seq(-100, 100, by = 5)
    ) +
    scale_x_discrete(expand = expansion(add = c(0.5, 2.5))) +
    labs(
      title    = "SPC Position Chart — % Distance from 200DMA",
      subtitle = sprintf(
        "As of %s  |  Centre = 200DMA (0%%)  |  Green = \u00b11\u03c3  Amber = \u00b12\u03c3  |  Grey bar = 52W Hi\u2013Lo  |  \u25cf = 50DMA",
        format(as.Date(tail(index(xts_ret), 1)))
      ),
      x = NULL, y = "% from 200DMA"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "#e5e7eb", linewidth = 0.4),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(face = "bold"),
      plot.subtitle      = element_text(size = 8.5, colour = "#555"),
      axis.text.x        = element_text(face = "bold", size = 11)
    )
}

# ══════════════════════════════════════════════════════════════════════════════
# plot_gauge_similarity — pairwise z-score distance heatmap
#
# Computes pairwise distances on the z-score vector
#   [z_52w_hi, z_200dma, z_50dma, z_dd, z_52w_lo]
# and displays as a clustered heatmap.
# Blue = similar posture, Red = dissimilar posture.
#
# INVOKE
#   plot_gauge_similarity(c("SPY","QQQ","GLD","AGG","XLK","SMH"), xts_ret)
#   plot_gauge_similarity(..., method = "euclidean")
# ══════════════════════════════════════════════════════════════════════════════
plot_gauge_similarity <- function(
    tickers,
    xts_ret,
    ma1     = 50,
    ma2     = 200,
    win_52w = 252,
    method  = c("euclidean", "cosine")
) {
  method <- match.arg(method)

  gv <- build_gauge_vectors(tickers, xts_ret, ma1, ma2, win_52w)

  STATE_COL <- c(
    Bear       = "#7f1d1d",
    Trending   = "#16a34a",
    Stretched  = "#38bdf8",
    Pullback   = "#fbbf24",
    Stressed   = "#f97316",
    Correction = "#dc2626"
  )

  z_mat <- gv %>%
    select(ticker, z_52w_hi, z_200dma, z_50dma, z_dd, z_52w_lo) %>%
    tibble::column_to_rownames("ticker") %>%
    as.matrix()

  # Pairwise distance
  if (method == "cosine") {
    nrm      <- z_mat / sqrt(rowSums(z_mat^2, na.rm = TRUE))
    sim_mat  <- nrm %*% t(nrm)
    dist_obj <- as.dist(1 - sim_mat)
  } else {
    dist_obj <- dist(z_mat, method = "euclidean")
  }

  # Cluster order (Ward D2 hierarchical)
  hc_ord  <- hclust(dist_obj, method = "ward.D2")
  tk_ord  <- rownames(z_mat)[hc_ord$order]
  dist_mat <- as.matrix(dist_obj)

  dist_tbl <- as_tibble(dist_mat, rownames = "from") %>%
    tidyr::pivot_longer(-from, names_to = "to", values_to = "dist") %>%
    mutate(
      from = factor(from, levels = tk_ord),
      to   = factor(to,   levels = tk_ord)
    )

  mid_dist <- median(dist_tbl$dist, na.rm = TRUE)

  ggplot(dist_tbl, aes(x = to, y = from, fill = dist)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_text(
      aes(label   = sprintf("%.2f", dist),
          colour  = I(ifelse(dist < mid_dist * 0.6, "white", "#374151"))),
      size = 3.2, fontface = "bold"
    ) +
    scale_fill_gradient2(
      low      = "#1d3461",
      mid      = "#f3f4f6",
      high     = "#dc2626",
      midpoint = mid_dist,
      name     = if (method == "cosine") "Cosine\ndist." else "Euclidean\ndist."
    ) +
    coord_fixed() +
    labs(
      title    = "Technical Posture Similarity",
      subtitle = sprintf(
        "Z-score vector [52W Hi | 200DMA | 50DMA | DD | 52W Lo]  \u2022  Method: %s  \u2022  Clustered Ward D2  \u2022  As of %s",
        method, format(gv$as_of[1])
      ),
      x = NULL, y = NULL,
      caption = "Blue = similar posture  \u2022  Red = dissimilar  \u2022  Diagonal = 0 (self)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid      = element_blank(),
      plot.title      = element_text(face = "bold"),
      plot.subtitle   = element_text(size = 8.5, colour = "#555"),
      plot.caption    = element_text(size = 7, colour = "#888"),
      axis.text.x     = element_text(angle = 45, hjust = 1, face = "bold", size = 10),
      axis.text.y     = element_text(face = "bold", size = 10),
      legend.position = "right"
    )
}

# ── Quick-run (guarded) ───────────────────────────────────────────────────────
if (!isTRUE(getOption("knitr.in.progress"))) {
  library(here)
  if (!exists("xts_ret"))
    xts_ret <- readRDS(here("02_data_processed/xts_ret_returns.rds"))

  tk <- c("SPY", "QQQ", "URTH", "AGG", "XLK", "SMH")

  print(plot_ma_range("URTH", xts_ret))
  print(plot_ma_gauge(c("SPY", "QQQ", "GLD", "AGG", "URTH"), xts_ret))
  print(plot_gauge_spc(tk, xts_ret))
  print(plot_gauge_spc(tk, xts_ret, sort_by = "norm_pos"))
  print(plot_gauge_similarity(tk, xts_ret))                        # euclidean default
  print(plot_gauge_similarity(tk, xts_ret, method = "cosine"))     # only useful in mixed regimes
  print(build_gauge_vectors(tk, xts_ret))
}
