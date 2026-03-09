# ==============================================================================
# FUNCTION: plot_sovereign_stress_bars
# Purpose: Final Visual Audit of Strategy Resilience
# ==============================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(readr)

# --- 1.0 LOAD DATA ---
base_path <- "/Volumes/T7Red_Work/Rstudio_ssd/etf_1download_2wrangle_3saa"
path_results <- file.path(base_path, "02_etf_saa_taa/data_processed/stress_test_results.rds")

if (!file.exists(path_results)) stop("❌ Data missing. Run 09_stress_test.R first.")
readable_results <- read_rds(path_results)

# --- 2.0 PLOTTING FUNCTION ---
plot_sovereign_stress_bars <- function(results_df) {
  
  # Reshape for ggplot
  plot_data <- results_df %>%
    pivot_longer(cols = c(SAA, TAA), 
                 names_to = "Strategy", 
                 values_to = "Return")
  
  ggplot(plot_data, aes(x = reorder(Scenario, Return), y = Return, fill = Strategy)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    # Dynamic labels: Position based on sign of return
    geom_text(aes(label = percent(Return, accuracy = 0.1)), 
              position = position_dodge(width = 0.8), 
              hjust = ifelse(plot_data$Return >= 0, -0.2, 1.2),
              size = 3.5, fontface = "bold") +
    coord_flip() +
    scale_y_continuous(labels = percent, expand = expansion(mult = c(0.2, 0.2))) +
    scale_fill_manual(values = c("SAA" = "#457B9D", "TAA" = "#E63946")) +
    labs(title = "Sovereign Stress Test: Strategy Resilience",
         subtitle = "Historical Crisis Simulation (SAA vs. TAA)",
         x = NULL, y = "Projected Return (%)",
         caption = "Source: Sovereign Internal Stress Engine") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", size = 14),
          axis.text.y = element_text(face = "bold"))
}

# --- 3.0 EXECUTION & SAVE ---
message("📊 Rendering Stress Audit Chart...")
p_stress <- plot_sovereign_stress_bars(readable_results)

# Export
path_reports <- file.path(base_path, "02_etf_saa_taa/reports")
if(!dir.exists(path_reports)) dir.create(path_reports, recursive = TRUE)
ggsave(file.path(path_reports, "stress_test_audit.png"), p_stress, width = 10, height = 6)

print(p_stress)
message("✅ Plot saved to: ", file.path(path_reports, "stress_test_audit.png"))