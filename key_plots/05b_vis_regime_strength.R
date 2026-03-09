# ==============================================================================
# MODULE 01: 05b_VIS_TICKER_STRENGTH (PRODUCTION)
# ==============================================================================
library(tidyverse)
library(viridis)

# 1. SETUP & DATA LOAD
plot_dir <- file.path(base_path, "key_plots")
sig_data <- read_rds(file.path(base_path, "01_etf_wrangle/data_processed/signal_table.rds"))

# 2. RENDER STRENGTH BAR
p_strength <- sig_data %>%
  mutate(Mom_3M = as.numeric(Mom_3M)) %>%
  slice_max(order_by = Mom_3M, n = 20) %>% 
  mutate(ticker = fct_reorder(ticker, Mom_3M)) %>%
  ggplot(aes(x = ticker, y = 1, fill = Mom_3M)) +
  geom_tile(color = "white", size = 1.8) +
  geom_text(aes(label = ticker), color = "white", fontface = "bold", size = 5.5) +
  scale_fill_viridis_c(option = "magma", name = "Strength") +
  theme_void() +
  theme(legend.position = "bottom", legend.key.width = unit(2.5, "cm"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 24, margin = margin(b=20)),
        plot.background = element_rect(fill = "white", color = NA)) +
  labs(title = "Sovereign Momentum Rank: Top 20 (via Mom_3M)")

ggsave(file.path(plot_dir, "ticker_strength.png"), p_strength, width = 20, height = 5, dpi = 300)