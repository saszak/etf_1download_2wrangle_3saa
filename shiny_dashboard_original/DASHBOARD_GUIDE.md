# ETF Universe — Performance Dashboard
## Developer & Extension Guide

---

## 1. Purpose

Bloomberg-style interactive ETF performance screen for the full Sovereign Universe (96 tickers).
Displays price performance, regime signals, and 52-week positioning in a dark-themed Shiny app
built with `bslib`, `reactable`, and `plotly`.

**Run with:**
```r
shiny::runApp("shiny_dashboard")
```

---

## 2. Directory Structure

```
shiny_dashboard/
├── app.R                        UI definition + server + shinyApp()
├── global.R                     Data bridge to root project (auto-sourced by Shiny)
├── DASHBOARD_GUIDE.md           This file
│
├── utils/
│   └── helpers.R                Theme constants · .svg_spark() · .ret_cell() · col_defs
│
└── modules/
    ├── mod_perf_table.R         Performance table module (reactable)
    └── mod_treemap.R            Finviz-style treemap module (plotly)
```

---

## 3. How to Run

```r
# From project root — Shiny finds global.R automatically
shiny::runApp("shiny_dashboard")

# Or from any location using here()
shiny::runApp(here::here("shiny_dashboard"))
```

> **Note:** `here()` must resolve to the project root (`etf_1download_2wrangle_3saa/`).
> Always launch from the root `.Rproj` or with `here` initialised correctly.

---

## 4. Architecture

### Data flow

```
Root project (02_data_processed/)
        │
        ▼
global.R  ←─ sources project_tree.R + 00_init_universe.R
        │     loads: raw_data · tech_summary · outlier_report
        │     builds: perf_data · CLUSTER_MAP · spy_row
        │
        ├──► utils/helpers.R        (theme + renderers + col_defs)
        ├──► modules/mod_perf_table.R
        └──► modules/mod_treemap.R
                    │
                    ▼
              app.R  (UI + server + shinyApp)
```

### Module pattern

Each tab is a self-contained Shiny module:
- `mod_xyz_ui(id)` — returns `tagList` of UI elements
- `mod_xyz_server(id, filtered, ...)` — receives `filtered` reactive + any required inputs as reactives

The `filtered` reactive lives in the main server and is passed down — modules never read `input$` directly from the parent session.

---

## 5. File Reference

### `global.R`

Auto-sourced by Shiny before `app.R`. Responsible for:

| Block | What it does |
|---|---|
| `library()` calls | Loads all packages |
| Root bridge | Sources `project_tree.R` and `00_init_universe.R` |
| Data loading | Reads `raw_data.rds`, `technical_summary.rds`, `outlier_regime_report.rds` |
| Date constants | `today`, `ytd_start`, `mtd_start`, `w52_start` |
| `source()` calls | Loads `helpers.R`, `mod_perf_table.R`, `mod_treemap.R` |
| `perf_data` | Main denormalised table: prices → returns → metadata join |
| `CLUSTER_MAP` | Assigns tickers to display clusters (Core Assets / SPY Sectors / Rest) |
| `spy_row` | Single-row benchmark reference for KPI strip |

**Root data objects used:**

| Object | Source file | Content |
|---|---|---|
| `raw_data` | `01_data_raw/raw_data.rds` | Daily OHLCV — `symbol`, `date`, `adjusted` |
| `etf_metadata` | `scripts/00_init_universe.R` | 96-ticker universe with `asset_class`, `pf_function`, `sub_block` |
| `tech_summary` | `02_data_processed/technical_summary.rds` | Per-ticker `trend_regime`, `momentum_status` |
| `outlier_report` | `02_data_processed/outlier_regime_report.rds` | Per-ticker outlier `label` |

---

### `utils/helpers.R`

Shared constants and renderer functions. Sourced by `global.R`.

#### Theme constants

| Constant | Hex | Used for |
|---|---|---|
| `DARK_BG` | `#111318` | Page / card background |
| `DARK_PANEL` | `#16181d` | Sidebar background |
| `DARK_HDR` | `#0d0f12` | Table header / caption bars |
| `DARK_BORDER` | `#2a2c35` | Grid lines, HR separators |
| `TEXT_MAIN` | `#d1d5db` | Primary text |
| `TEXT_DIM` | `#6b7280` | Secondary / label text |
| `BLUE_TICK` | `#60a5fa` | Ticker symbol highlight |

#### Functions

**`.svg_spark(prices, w=80, h=28)`**
Generates an inline SVG sparkline string from a price vector.
- Uses last 60 observations
- Green (`#22c55e`) if end ≥ start, red (`#ef4444`) otherwise
- Returns empty string `""` if fewer than 3 valid prices

**`.ret_cell(value)`**
Returns an `htmltools::div` badge for a return value.
- Dark green background + light green text for positive
- Dark red background + light red text for negative
- `—` dash for `NA`

#### `tbl_theme`
`reactableTheme` object applying the dark colour palette to all `reactable` tables.

#### `col_defs`
Named list of `colDef` objects for `perf_data`. Covers:

| Column | Display | Notes |
|---|---|---|
| `symbol` | Ticker | Bold, blue |
| `name` | Name | Truncated with ellipsis |
| `spark` | *(sparkline)* | SVG HTML, non-sortable |
| `latest_price` | Last Px | 2 decimal places |
| `ret_1d/5d/1m/ytd` | %1D / %5D / %1M / %YTD | `.ret_cell()` badge renderer |
| `range_pct` | 52W Range | Progress bar + % label |
| `trend_regime` | Regime | Colour-coded: Bullish=green, Bearish=red |
| `pf_function` | PF Function | Colour per role (Anchor=blue, Satellite=orange, …) |
| `sub_block` | Block | Dimmed text |
| `asset_class` | Asset Class | Hidden by default; shown when not grouping |
| `*` hidden | — | `latest_date`, `low_52w`, `high_52w`, `tree_level`, `momentum_status`, `label` |

---

### `modules/mod_perf_table.R`

**`mod_perf_table_ui(id)`**
Returns: caption bar + `reactableOutput`.

**`mod_perf_table_server(id, filtered, relative_mode, bmk, group_by_class)`**

| Argument | Type | Purpose |
|---|---|---|
| `filtered` | `reactive` → tibble | Filtered + sorted `perf_data` |
| `relative_mode` | `reactive` → logical | Subtract benchmark returns from all return cols |
| `bmk` | `reactive` → chr | Benchmark ticker (`"SPY"` or `"URTH"`) |
| `group_by_class` | `reactive` → logical | Group rows by `asset_class` |

When `relative_mode = TRUE`, the module subtracts the benchmark row's returns and relabels columns (e.g. `%YTD vs SPY`).

---

### `modules/mod_treemap.R`

**`mod_treemap_ui(id)`**
Returns: legend caption bar + `plotlyOutput`.

**`mod_treemap_server(id, filtered, treemap_group)`**

| Argument | Type | Purpose |
|---|---|---|
| `filtered` | `reactive` → tibble | Filtered `perf_data` |
| `treemap_group` | `reactive` → chr | Grouping column: `"cluster"` or `"asset_class"` |

**Design:**
- Two-level hierarchy: group node → ticker leaf
- All leaf `values = 1` → equal tile sizes (Finviz-style)
- Colour = YTD return, diverging scale centred at 0 (grey)
- Group tile colour = median YTD of its members
- Emoji dots (🟢 🔴 ⚪) on corner label show 1M direction

**Colour scale (Finviz-style):**

| Position | Colour | Meaning |
|---|---|---|
| 0.00 | `#7f0000` | Large loss |
| 0.30 | `#cc2222` | Moderate loss |
| 0.46 | `#f4aaaa` | Small loss |
| 0.50 | `#555555` | Flat |
| 0.54 | `#a3f0bc` | Small gain |
| 0.70 | `#1a7a2e` | Moderate gain |
| 1.00 | `#00AA44` | Large gain |

---

### `app.R`

Contains UI and server. Does **not** load data — `global.R` handles that.

#### UI components

| Component | Description |
|---|---|
| `page_sidebar()` | Root layout with dark `bslib` theme (bootswatch: darkly + Inter font) |
| `sidebar` (width 210px) | Filters: asset class, PF function, sort, treemap group, relative mode, checkboxes |
| KPI strip | 4 `value_box` tiles: SPY YTD · SPY 1M · Bullish Breadth · Universe count |
| Main card | `tabsetPanel` with Table tab + Treemap tab |

#### Server reactives

**`filtered()`** — central reactive, applies all sidebar filters in sequence:
1. `hide_signal` → removes `pf_function == "Signal"` tickers
2. `asset_class_filter` → single asset class
3. `pf_function_filter` → single PF function, or "Core-Assets" shortcut (Anchor + Core-Growth + Core-Stabilizer)
4. `only_bullish` → keeps `trend_regime == "Bullish"` only
5. `relative_mode` → subtracts benchmark returns if active
6. `sort_by` → `arrange(desc(...))` on chosen column

---

## 6. `perf_data` — Column Reference

| Column | Type | Description |
|---|---|---|
| `symbol` | chr | Ticker |
| `name` | chr | Full ETF name |
| `latest_price` | dbl | Last adjusted close |
| `latest_date` | Date | Most recent data date |
| `ret_1d` | dbl | 1-day arithmetic return |
| `ret_5d` | dbl | 5-day arithmetic return |
| `ret_1m` | dbl | ~22-day arithmetic return |
| `ret_ytd` | dbl | YTD arithmetic return (from Jan 1) |
| `low_52w` | dbl | 52-week low (adjusted) |
| `high_52w` | dbl | 52-week high (adjusted) |
| `range_pct` | dbl | Position in 52W range — 0=at low, 1=at high |
| `spark` | chr | SVG sparkline HTML string (60 days) |
| `asset_class` | chr | Equity / Fixed Income / Commodity / etc. |
| `pf_function` | chr | Anchor / Core-Growth / Satellite / Signal / etc. |
| `sub_block` | chr | Sub-category block |
| `tree_level` | chr | Hierarchy level in universe tree |
| `trend_regime` | chr | Bullish / Bearish / Bullish-Correction / Bearish-Relief |
| `momentum_status` | chr | Momentum signal from `tech_summary` |
| `label` | chr | Outlier label from `outlier_report` |
| `cluster` | chr | Display cluster: Core Assets / SPY Sectors / Rest of Universe |

---

## 7. Sidebar Controls Reference

| Control | Type | Effect |
|---|---|---|
| Asset Class | `selectInput` | Filter `perf_data` by `asset_class` |
| PF Function | `selectInput` | Filter by `pf_function`; "Core-Assets" = Anchor + Core-Growth + Core-Stabilizer |
| Sort By | `selectInput` | Column to sort the table descending |
| Treemap grouping | `selectInput` | Switch treemap between `cluster` and `asset_class` grouping |
| Relative to BMK | `checkboxInput` | Subtract SPY (or URTH) returns from all return columns |
| Benchmark | `selectInput` | Appears when Relative mode is on; choices: SPY, URTH |
| Hide Signal tickers | `checkboxInput` | Removes `pf_function == "Signal"` rows |
| Bullish regime only | `checkboxInput` | Keeps only `trend_regime == "Bullish"` |
| Group by Asset Class | `checkboxInput` | Groups reactable rows by `asset_class` with collapsible headers |

---

## 8. PF Function Colour Palette

| PF Function | Colour | Hex |
|---|---|---|
| Anchor | Blue | `#60a5fa` |
| Core-Growth | Emerald | `#34d399` |
| Core-Stabilizer | Purple | `#a78bfa` |
| Core-Factor | Indigo | `#818cf8` |
| Core-Income | Amber | `#fbbf24` |
| Real-Shield | Red | `#f87171` |
| Satellite | Orange | `#fb923c` |
| Signal | Slate | `#94a3b8` |
| Tactical | Pink | `#e879f9` |

---

## 9. How to Add a New Tab / Module

1. Create `shiny_dashboard/modules/mod_xyz.R` with:
```r
mod_xyz_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # your UI here
  )
}

mod_xyz_server <- function(id, filtered, ...) {
  moduleServer(id, function(input, output, session) {
    # your server logic here
  })
}
```

2. Add to `global.R`:
```r
source(here("shiny_dashboard/modules/mod_xyz.R"))
```

3. Add to `app.R` UI inside `tabsetPanel`:
```r
tabPanel("My Tab", mod_xyz_ui("xyz"))
```

4. Add to `app.R` server:
```r
mod_xyz_server("xyz", filtered, reactive(input$my_param))
```

5. Register in `project_tree.R`:
```r
shiny_mod_xyz = "./shiny_dashboard/modules/mod_xyz.R"
```

---

## 10. How to Add a New Sidebar Filter

1. Add the input control to the `sidebar` block in `app.R`:
```r
checkboxInput("my_filter", "My Filter Label", value = FALSE)
```

2. Add the filter logic to the `filtered()` reactive in `app.R`:
```r
if (input$my_filter) df <- df %>% filter(my_column == "my_value")
```

That's it — `filtered()` flows automatically to all modules.

---

## 11. How to Add a New Data Column to `perf_data`

1. Add the computation to the `summarise()` block in `global.R`
2. Add the join if the data comes from a new source
3. Add a `colDef` entry in `utils/helpers.R` → `col_defs`
4. If the column should appear in the table, set `show = TRUE` (default)

---

## 12. Dependencies

| Package | Version tested | Purpose |
|---|---|---|
| `shiny` | ≥ 1.8 | Core framework |
| `bslib` | ≥ 0.7 | Bootstrap 5 layout + theming |
| `reactable` | ≥ 0.4 | Interactive table |
| `plotly` | ≥ 4.10 | Treemap |
| `tidyverse` | ≥ 2.0 | Data wrangling |
| `htmltools` | ≥ 0.5 | HTML cell renderers |
| `here` | ≥ 1.0 | Root-relative paths |
| `scales` | ≥ 1.3 | Number formatting |
| `lubridate` | ≥ 1.9 | Date arithmetic |

Install all:
```r
install.packages(c("shiny","bslib","reactable","plotly","tidyverse",
                   "htmltools","here","scales","lubridate"))
```

---

## 13. Known Constraints

- **Data is static at launch** — `perf_data` is built once in `global.R` when the app starts. To refresh, restart the app.
- **52W range** uses calendar days (`today - 365`), not 252 trading days.
- **Return periods** are fixed-lag (`nth(adjusted, -2L)` = 1D, `-6L` = 5D, `-22L` = 1M) — these can shift around holidays.
- **Sparkline** is an SVG string computed at data-build time; it does not update reactively.
- **Relative mode** subtracts benchmark returns from the filtered dataset — the benchmark row itself will show all zeros when the benchmark is included in the filtered set.
