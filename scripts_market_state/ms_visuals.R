################################################################################
# scripts_market_state/ms_visuals.R
#
# PURPOSE
#   Visualisation layer for the 3-layer market state framework.
#
# PUBLIC FUNCTIONS
#   plot_ms_timeline(ms_history)
#     → ggplot: daily state colour timeline + eq_weight line overlay
#
#   plot_ms_heatmap(ms_history)
#     → ggplot: 5 signals × time heatmap (year × month, coloured by state)
#
#   plot_ms_allocation(ra_history)
#     → ggplot: final EQ vs FI weight over time (stacked area + state shading)
#
#   plot_ms_dashboard(xts_ret, ...)
#     → patchwork: all three panels stacked for a single-page dashboard
#
# DEPENDENCIES
#   scripts_market_state/ms_vector_state.R    — MS_STATE_PAL, MS_STATE_LEVELS
#   scripts_market_state/ms_sector_pattern.R  — SP_PATTERN_PAL
#   scripts_market_state/ms_risk_allocation.R — build_risk_allocation_history()
################################################################################

library(tidyverse)
library(patchwork)
library(scales)

# ── Shared theme ─────────────────────────────────────────────────────────────
.ms_theme <- function() {
  theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title         = element_text(face = "bold", size = 12),
      plot.subtitle      = element_text(size = 9, colour = "#555555"),
      legend.position    = "right",
      legend.key.size    = unit(0.45, "cm"),
      legend.text        = element_text(size = 9)
    )
}

# ==============================================================================
# plot_ms_timeline()
#   State-coloured tile bar + eq_weight line overlay
# ==============================================================================
plot_ms_timeline <- function(ms_history, title = "Market State Timeline") {

  valid <- ms_history %>% filter(!is.na(market_state))
  if (nrow(valid) == 0) {
    message("plot_ms_timeline: no valid states")
    return(ggplot() + theme_void())
  }

  # Normalise eq_weight to [0,1] range of y for overlay — map 0.25–0.75 → 0–1
  eq_scaled <- valid %>%
    mutate(
      eq_norm = (eq_weight - 0.25) / (0.75 - 0.25)
    )

  p <- ggplot(valid, aes(x = date)) +
    # State colour tiles — full height bar
    geom_tile(aes(y = 0.5, fill = market_state), height = 1, width = 1) +
    scale_fill_manual(
      values = MS_STATE_PAL,
      limits = MS_STATE_LEVELS,
      name   = "Market State"
    ) +

    # EQ weight line (white so it reads on any colour)
    geom_line(data = eq_scaled, aes(y = eq_norm),
              colour = "white", linewidth = 0.8, alpha = 0.85) +

    # Axis: right side shows EQ weight %
    scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0.25, 0.42, 0.50, 0.58, 0.65) %>%
        { (. - 0.25) / 0.50 },
      labels = c("35%", "42%", "50%", "58%", "65%"),
      name   = "EQ weight"
    ) +
    scale_x_date(expand = c(0, 0)) +
    labs(
      title    = title,
      subtitle = paste0(
        "Current state: ", as.character(tail(valid$market_state, 1)),
        " | EQ weight: ", round(tail(valid$eq_weight, 1) * 100), "%",
        " | As of: ", format(tail(valid$date, 1))
      ),
      x = NULL
    ) +
    .ms_theme() +
    theme(
      axis.text.y.left  = element_blank(),
      axis.ticks.y.left = element_blank(),
      panel.grid        = element_blank()
    )

  p
}

# ==============================================================================
# plot_ms_signal_strip()
#   5 horizontal signal strips (one per signal) colour-coded by value
# ==============================================================================
plot_ms_signal_strip <- function(ms_history) {

  valid <- ms_history %>%
    filter(!is.na(market_state)) %>%
    select(date, urth_regime, spy_regime,
           us_premium, em_appetite, sector_breadth, market_state)

  # Build long-format signal frame
  strips <- bind_rows(
    valid %>% transmute(date,
      signal = "URTH regime",
      value  = case_when(
        urth_regime == "Fall"          ~ -1,
        urth_regime == "Recovery"      ~  0.5,
        urth_regime == "Consolidation" ~  1,
        TRUE                           ~ NA_real_
      )
    ),
    valid %>% transmute(date,
      signal = "SPY regime",
      value  = case_when(
        spy_regime == "Fall"          ~ -1,
        spy_regime == "Recovery"      ~  0.5,
        spy_regime == "Consolidation" ~  1,
        TRUE                          ~ NA_real_
      )
    ),
    valid %>% transmute(date,
      signal = "US premium",
      value  = as.numeric(us_premium)    # +1 / -1
    ),
    valid %>% transmute(date,
      signal = "EM appetite",
      value  = as.numeric(em_appetite)   # +1 / -1
    ),
    valid %>% transmute(date,
      signal = "Sector breadth",
      value  = sector_breadth            # 0 – 1
    )
  ) %>%
    mutate(signal = factor(signal, levels = c(
      "URTH regime", "SPY regime", "US premium", "EM appetite", "Sector breadth"
    )))

  ggplot(strips, aes(x = date, y = signal, fill = value)) +
    geom_tile(height = 0.85, width = 1) +
    scale_fill_gradient2(
      low      = "#dc2626",   # red = negative/Fall
      mid      = "#e5e7eb",   # grey = neutral
      high     = "#16a34a",   # green = positive
      midpoint = 0,
      limits   = c(-1, 1),
      na.value = "#d1d5db",
      name     = "Signal\nvalue"
    ) +
    scale_x_date(expand = c(0, 0)) +
    labs(
      title    = "Market State — Signal Strips",
      subtitle = "Red = risk-off / Fall | Green = risk-on / Expansion",
      x = NULL, y = NULL
    ) +
    .ms_theme() +
    theme(panel.grid = element_blank())
}

# ==============================================================================
# plot_ms_allocation()
#   Stacked area: final EQ vs FI weight over time
# ==============================================================================
plot_ms_allocation <- function(ra_history, title = "Dynamic Allocation from Market State") {

  valid <- ra_history %>% filter(!is.na(final_eq_weight))
  if (nrow(valid) == 0) {
    message("plot_ms_allocation: no valid allocation")
    return(ggplot() + theme_void())
  }

  long <- valid %>%
    select(date, market_state, final_eq_weight, fi_weight) %>%
    pivot_longer(cols = c(final_eq_weight, fi_weight),
                 names_to  = "asset_class",
                 values_to = "weight") %>%
    mutate(asset_class = recode(asset_class,
      final_eq_weight = "Equity",
      fi_weight       = "Fixed Income"
    ))

  # State shading from Layer 1
  state_runs <- valid %>%
    mutate(
      state_grp = with(rle(as.character(market_state)), {
        rep(seq_along(lengths), lengths)
      })
    ) %>%
    group_by(state_grp) %>%
    summarise(
      xmin  = min(date),
      xmax  = max(date),
      state = first(market_state),
      .groups = "drop"
    )

  # Stress shading — use geom_rect with fixed colour (no fill aes) to avoid scale conflict
  stress_runs <- state_runs %>%
    filter(state %in% c("Contraction", "Deterioration"))

  p <- ggplot() +
    # Stacked area (primary fill scale)
    geom_area(
      data = long,
      aes(x = date, y = weight, fill = asset_class),
      position = "stack"
    ) +
    scale_fill_manual(
      values = c("Equity" = "#2563eb", "Fixed Income" = "#9ca3af"),
      name   = NULL
    )

  # Overlay stress shading without adding a second fill scale
  if (nrow(stress_runs) > 0) {
    for (i in seq_len(nrow(stress_runs))) {
      col <- if (stress_runs$state[i] == "Contraction") "#dc2626" else "#ea580c"
      p <- p + annotate("rect",
        xmin = stress_runs$xmin[i], xmax = stress_runs$xmax[i],
        ymin = 0, ymax = 1,
        fill = col, alpha = 0.12
      )
    }
  }

  p +
    # Passive 60/40 reference lines
    geom_hline(yintercept = 0.60, linetype = "dashed",
               colour = "#2563eb", linewidth = 0.4, alpha = 0.7) +
    geom_hline(yintercept = 1.00, linetype = "dashed",
               colour = "#9ca3af", linewidth = 0.4, alpha = 0.7) +

    scale_y_continuous(labels = percent_format(), limits = c(0, 1), expand = c(0, 0)) +
    scale_x_date(expand = c(0, 0)) +
    labs(
      title    = title,
      subtitle = paste0(
        "Latest: EQ ", round(tail(valid$final_eq_weight, 1) * 100), "% / ",
        "FI ",  round(tail(valid$fi_weight,       1) * 100), "%",
        " | Dashed = passive 60/40"
      ),
      x = NULL, y = "Portfolio weight"
    ) +
    .ms_theme()
}

# ==============================================================================
# plot_ms_dashboard()
#   Three panels stacked: timeline + signal strips + allocation
# ==============================================================================
plot_ms_dashboard <- function(xts_ret,
                              roll_win = 60,
                              ma_win   = 200,
                              t_fall   = 0.10,
                              mom_win  = 63) {

  # Build all three layers
  ms <- build_market_state_history(xts_ret,
    roll_win = roll_win, ma_win = ma_win, t_fall = t_fall)
  ra <- build_risk_allocation_history(xts_ret,
    roll_win = roll_win, ma_win = ma_win, t_fall = t_fall, mom_win = mom_win)

  p1 <- plot_ms_timeline(ms)
  p2 <- plot_ms_signal_strip(ms)
  p3 <- plot_ms_allocation(ra)

  (p1 / p2 / p3) +
    plot_layout(heights = c(1.2, 1, 1.5)) +
    plot_annotation(
      title   = "Market State Framework — Full Dashboard",
      caption = paste0(
        "Layers: L1 URTH\u2192SPY\u2192Sector vector | ",
        "L2 Cyclical/Defensive rotation | ",
        "L3 Adjusted EQ allocation"
      ),
      theme = theme(
        plot.title   = element_text(face = "bold", size = 14),
        plot.caption = element_text(size = 8, colour = "#6b7280")
      )
    )
}

# ==============================================================================
# Quick-run (guarded)
# ==============================================================================
if (!isTRUE(getOption("knitr.in.progress"))) {
  library(here)

  if (!exists("xts_ret"))
    xts_ret <- readRDS(here("02_data_processed/xts_ret_returns.rds"))

  if (!exists("build_market_state_history"))
    source(here("scripts_market_state/ms_vector_state.R"))

  if (!exists("build_sector_pattern_history"))
    source(here("scripts_market_state/ms_sector_pattern.R"))

  if (!exists("build_risk_allocation_history"))
    source(here("scripts_market_state/ms_risk_allocation.R"))

  dash <- plot_ms_dashboard(xts_ret)
  print(dash)
}
