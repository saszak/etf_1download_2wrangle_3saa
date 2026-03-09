# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts/03_reporting_engine.R
# Purpose: Stage 03 - Visualizing Analytical Sigma & Exhaustion Labels
# ==============================================================================

library(tidyverse)
library(ggplot2)
library(scales)

# --- 1. CONNECT TO INFRASTRUCTURE ---
if(!exists("project_tree")) source("./project_tree.R")
if(!exists("etf_metadata")) source(project_tree$scripts$init)

# Load the refinery report generated in Stage 01
outlier_data <- read_rds(project_tree$products$ref_report)

message("📊 Stage 03: Generating Analytical Sigma Report...")

# --- 2. DATA PREPARATION ---
# Sorting for clean visualization
plot_data <- outlier_data %>%
  arrange(desc(current_sigma)) %>%
  mutate(ticker = factor(ticker, levels = ticker))

# --- 3. THE EXHAUSTION PLOT ---
sigma_plot <- ggplot(plot_data, aes(x = current_sigma, y = ticker, fill = label)) +
  geom_col(show.legend = FALSE) +
  # Use your requested labels: [EXHAUSTION-HIGH], [EXHAUSTION-LOW], [STABLE]
  scale_fill_manual(values = c(
    "[EXHAUSTION-HIGH]" = "#d73027", 
    "[EXHAUSTION-LOW]"  = "#4575b4", 
    "[STABLE]"          = "#bdbdbd"
  )) +
  # Add the text label to the end of the Sigma bar as requested
  geom_text(aes(label = label), 
            hjust = ifelse(plot_data$current_sigma > 0, -0.1, 1.1), 
            size = 3, fontface = "bold") +
  # Theme and Labels
  theme_minimal() +
  labs(title = "ETF Analytical Sigma & Outlier Management",
       subtitle = paste("Status as of:", Sys.Date()),
       x = "Sigma Distance (Z-Score)",
       y = NULL) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_x_continuous(expand = expansion(mult = c(0.2, 0.2)))

# --- 4. PERSISTENCE ---
# Save the report to the /03_reports/ directory defined in the tree
report_path <- file.path(project_tree$dirs$output, "etf_sigma_report.png")
ggsave(report_path, sigma_plot, width = 10, height = 12, dpi = 300)

message("✅ Stage 03 Complete: Report saved to ", report_path)