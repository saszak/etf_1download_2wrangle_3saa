
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

# Think of that .md file as the Constitution of your project. In data engineering, "naming drift" is the number one killer of complex systems (e.g., calling a variable ret in one script and returns_daily in another).
# 
# Here is how you can use this file to keep your "Sovereign Engine" running perfectly:
# 1. The "North Star" for Debugging
# 
# If your Shiny app or a report throws an error like Error: object 'xts_r' not found, you don't have to guess what happened. Open the .md file, look at Section 2, and you’ll see exactly what that object is supposed to be, where it comes from (ret_daily.rds), and which function creates it.
# 2. Standardizing New Scripts
# 
# Whenever you create a new analysis (e.g., a "Correlation Heatmap" or "Factor Attribution"), check the Global Environment Data Structures section.
# 
#     Don't re-download data in the new script.
# 
#     Do use the existing xts_r object.
#     This ensures every single chart in your project is looking at the exact same numbers.
# 
# 3. Onboarding & Future-Proofing
# 
# If you don't touch this code for three months and come back to it, you won't have to spend two hours "re-learning" your own logic. The Data Architecture Map tells you the story of the data flow in 30 seconds.
# 4. GitHub / Documentation
# 
# If you ever move this project to GitHub or share it with a collaborator, this file serves as the Technical README. It tells the world: "This isn't a messy folder of scripts; it's a tiered data refinery."