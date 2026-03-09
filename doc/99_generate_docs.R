# ==============================================================================
# PROJECT: ETF_SOVEREIGN_ENGINE
# FILE: 99_generate_docs.R
# Purpose: Generate the Project Architecture Reference (Markdown)
# ==============================================================================

doc_content <- "
# 🏛️ Project Architecture: Sovereign ETF Engine

## 1. Project-Wide Data Flow & Naming Map
| Stage | Process | Input File | Output File (Disk) | Naming Logic |
| :--- | :--- | :--- | :--- | :--- |
| **Tier 1: Raw** | `sync_etf_prices()` | API (Yahoo) | `data_raw/raw_p_d.rds` | **Raw Price Daily** |
| **Tier 2: Calc** | `xts_price_to_return()`| `raw_p_d.rds` | **`data_processed/ret_daily.rds`** | **Clean Return Daily** |
| **Tier 3: Alpha** | `abs_to_rel_return()` | `ret_daily.rds`| `data_processed/rel_ret_d.rds` | **Relative Return (Alpha)** |
| **Tier 4: Audit** | `get_z_scores()` | `ret_daily.rds`| *Memory Only (Global)* | **Statistical Disruption** |

## 2. Global Environment Data Structures
| Variable Name | Object Type | Content Description | Precision Level |
| :--- | :--- | :--- | :--- |
| **`xts_r`** | `xts` matrix | Adjusted Daily Returns (Discrete) | 8+ Decimals, Outliers Cleaned |
| **`df_z_scores`** | `tibble` / `df` | Ticker, Z-Score, Vol, and Metadata | Ranked by absolute shock magnitude |
| **`etf_metadata`**| `tibble` / `df` | Ticker, Category, Cluster, Sub-block | Static reference for grouping |
| **`xts_rel`** | `xts` matrix | Asset Return minus SPY Return | Pure Alpha / Relative Strength |

## 3. Sovereign Function Inventory
| Function Name | Input(s) | Output(s) | Mathematical Core |
| :--- | :--- | :--- | :--- |
| **`sync_etf_prices()`** | `ticker_vector` | `.rds` files to `data_raw` | `tidyquant::tq_get` |
| **`xts_price_to_return()`**| `suffix` | `xts` object + `ret_daily.rds` | `PerformanceAnalytics::Return.calculate` |
| **`abs_to_rel_return()`** | `xts_abs`, `bmk` | `xts` object + `rel_ret_d.rds` | Vectorized Subtraction (Alpha) |
| **`get_z_scores()`** | `metadata`, `window`| Ranked `tibble` | `(Return - Mean) / StdDev` |
| **`plot_z_analysis()`** | `df_z_scores` | `ggplot` Object | Horizontal Bar Chart of Shocks |
"

# Write to file
writeLines(doc_content, "PROJECT_ARCHITECTURE.md")
message("✅ PROJECT_ARCHITECTURE.md has been generated in your root folder.")