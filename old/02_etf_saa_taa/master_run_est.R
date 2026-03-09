# ==============================================================================
# PROJECT B: ETF_SAA_TAA - MASTER_RUN.R
# Purpose: Orchestrate 3-Book Sovereign Strategy (00-05)
# Architecture: Book 1 (SAA) + Book 2 (LS TAA) = Book 3 (Total Net)
# ==============================================================================

message("🏗️ Initializing Sovereign SAA-TAA Rebalance...")
start_time <- Sys.time()

# 0. ESTABLISH THE BRIDGE
# Connects to T7 SSD and injects cluster metadata map
source("00_bridge_config.R")

# 1. RUN ALPHA INTELLIGENCE
# Identifies Spring-Loads (<0.3 Range % + Momentum)
source("01_alpha_intelligence.R")

# 2. DEFINE POLICY WEIGHTS
# Constructs Book 1 (60/40) and Book 2 (5% LS Synthetics)
source("02_policy_weights.R")

# 3. CONDUCT DRIFT ANALYSIS
# Audits Book 3 "Total" exposure for sector crowding
source("03_drift_analysis.R")

# 4. GENERATE EXECUTION LIST
# Produces final netted trade list for $1,000,000 AUM
source("04_master_allocator.R")

# 5. GENERATE VISUAL DASHBOARD
# Captures the Book 3 Snapshot as a professional PNG
source("05_visual_dashboard.R")

# --- FINAL AUDIT ---
end_time <- Sys.time()
duration <- round(difftime(end_time, start_time, units = "secs"), 2)

message("\n--------------------------------------------------")
message("✅ SOVEREIGN STRATEGY CYCLE COMPLETE")
message("⏱️ Total Execution Time: ", duration, " seconds")
message("📂 Trade List: /reports/sovereign_trade_list.csv")
message("📸 Snapshot: /reports/portfolio_snapshot.png")
message("--------------------------------------------------")