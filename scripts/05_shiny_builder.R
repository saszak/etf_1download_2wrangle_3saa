# ==============================================================================
# PROJECT: ETF_1DOWNLOAD_2WRANGLE_3SAA
# FILE PATH: ./scripts/05_shiny_builder.R
# Purpose: Stage 05 - Sentinel Intelligence UI (Strategic Membership Update)
# ==============================================================================

library(tidyverse)
library(rmarkdown)
library(shiny)
library(here)
library(flexdashboard)

build_and_launch_shiny <- function() {
  
  root_path   <- here()
  report_name <- "sentinel_intelligence.Rmd"
  report_path <- here(project_tree$dirs$output, report_name)
  
  message("🛠️ Building Sentinel Flexdashboard at: ", report_path)
  
  # --- GENERATE THE RMD CONTENT ---
  rmd_content <- c(
    "---",
    "title: 'Sentinel ETF Intelligence'",
    "output: ",
    "  flexdashboard::flex_dashboard:",
    "    orientation: rows",
    "    vertical_layout: scroll", 
    "    theme: ",
    "      version: 4",
    "      bg: '#F8F9FA'", 
    "      fg: '#1A1A1A'",
    "      primary: '#003049'",
    "runtime: shiny",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "library(flexdashboard)",
    "library(tidyverse)",
    "library(PerformanceAnalytics)",
    "library(scales)",
    "library(here)",
    "",
    "# 🛡️ THE BRIDGE: Force-load reporting logic into the Shiny process environment",
    "if(!exists('project_tree')) source(here('project_tree.R'))",
    "if(file.exists(here(project_tree$scripts$advanced_reporting))) {",
    "  source(here(project_tree$scripts$advanced_reporting))",
    "}",
    "```",
    "",
    "Main Dashboard",
    "=====================================",
    "",
    "Sidebar {.sidebar}",
    "-----------------------------------------------------------------------",
    "### System Controls",
    "",
    "```{r}",
    "checkboxInput('outlier_adj', 'Enable Outlier Management', value = TRUE)",
    "helpText('Sentinel Intelligence Suite v4.0')",
    "helpText('Status: Boundary Envelope Active')",
    "```",
    "",
    "Row {data-height=600}",
    "-----------------------------------------------------------------------",
    "### Analytical Sigma: Statistical Shock Analysis (Daily Z-Score)",
    "```{r}",
    "renderPlot({ ",
    "  if(exists('p_sigma')) p_sigma else ggplot() + labs(title='Error: p_sigma not found')",
    "})",
    "```",
    "",
    "Row {data-height=400}",
    "-----------------------------------------------------------------------",
    "### Nominal Performance View: Actual YTD % (Price-Based)",
    "```{r}",
    "renderPlot({ if(exists('p_quad_nominal')) p_quad_nominal else ggplot() })", 
    "```",
    "### Annualized Efficiency Frontier",
    "```{r}",
    "renderPlot({ if(exists('p_eff')) p_eff else ggplot() })",
    "```",
    "",
    "Technicals Command",
    "=====================================",
    "",
    "Sidebar {.sidebar}",
    "-----------------------------------------------------------------------",
    "### Filter Strategy",
    "",
    "```{r}",
    "cats <- sort(unique(standard_metadata$category))",
    "checkboxGroupInput('tech_cats', 'Select Sectors:', choices = cats, selected = cats)",
    "```",
    "",
    "Row {data-height=650}",
    "-----------------------------------------------------------------------",
    "### 📊 Technical Exhaustion: Sigma vs 10MA Stretch",
    "",
    "```{r}",
    "filtered_tech <- reactive({",
    "  unified_mom_df %>% filter(category %in% input$tech_cats)",
    "})",
    "",
    "renderPlot({",
    "  if(exists('plot_technical_exhaustion_quadrant')) {",
    "    plot_technical_exhaustion_quadrant(filtered_tech())",
    "  } else {",
    "    ggplot() + labs(title='Function plot_technical_exhaustion_quadrant not found')",
    "  }",
    "})",
    "```",
    "",
    "Row {data-height=350}",
    "-----------------------------------------------------------------------",
    "### 🛡️ Sentinel Envelope Watch (Outlier Boundary)",
    "",
    "```{r}",
    "renderTable({",
    "  filtered_tech() %>%",
    "    mutate(",
    "      envelope_tag = case_when(",
    "        dist_10 == max(dist_10, na.rm=T) ~ '🚀 UPPER ENVELOPE',",
    "        dist_10 == min(dist_10, na.rm=T) ~ '📉 LOWER ENVELOPE',",
    "        z_score == max(z_score, na.rm=T) ~ '⚡ MAX SHOCK',",
    "        z_score == min(z_score, na.rm=T) ~ '🩸 MIN SHOCK',",
    "        TRUE ~ NA_character_",
    "      )",
    "    ) %>%",
    "    filter(!is.na(envelope_tag) | (dist_10 > 0.05 & z_score > 2.0)) %>%",
    "    mutate(",
    "      exhaustion = if_else(dist_10 > 0.05 & z_score > 2.0, '⚠ EXHAUSTED', '---'),",
    "      `10d dist` = scales::percent(dist_10, accuracy = 0.1)",
    "    ) %>%",
    "    select(ticker, envelope_tag, exhaustion, action_signal, `10d dist`, z_score) %>%",
    "    rename(Ticker = ticker, Boundary = envelope_tag, Status = exhaustion, Action = action_signal, Sigma = z_score)",
    "})",
    "```",
    "",
    "### 🛰️ Sentinel Signal Membership (Strategic Logic)",
    "",
    "```{r}",
    "renderTable({",
    "  filtered_tech() %>% ",
    "    mutate(",
    "      Scenario = case_when(",
    "        z_score < 0 & dist_10 < 0 & trend_regime == 'Bullish' ~ 'The Holy Grail',",
    "        z_score > 0 & dist_10 < 0 & trend_regime == 'Bearish' ~ 'The Trap',",
    "        z_score > 0 & dist_10 > 0 & trend_regime == 'Bullish' ~ 'The Blow-off',",
    "        z_score < 0 & dist_10 < 0 & trend_regime == 'Bearish' ~ 'The Abyss',",
    "        TRUE ~ 'Transition'",
    "      ),",
    "      `The Signal` = case_when(",
    "        Scenario == 'The Holy Grail' ~ 'STRENGTH BUY: Rare dip in monster trend.',",
    "        Scenario == 'The Trap'       ~ 'RELIEF RALLY: Fake bounce. Sell the rip.',",
    "        Scenario == 'The Blow-off'   ~ 'TRIM/HEDGE: Overextended. Take profits.',",
    "        Scenario == 'The Abyss'      ~ 'AVOID: Structural breakdown.',",
    "        TRUE                         ~ 'Wait for confirmation'",
    "      )",
    "    ) %>%",
    "    select(ticker, Scenario, `The Signal`) %>%",
    "    rename(Ticker = ticker)",
    "})",
    "```"
  )
  
  # --- WRITE AND CLEAN ---
  ghost_file <- here(project_tree$dirs$output, paste0("._", report_name))
  if(file.exists(ghost_file)) file.remove(ghost_file)
  
  writeLines(rmd_content, report_path)
  
  # --- LAUNCH ---
  message("🚀 Launching Sentinel Intelligence UI with Membership Logic...")
  rmarkdown::run(report_path, shiny_args = list(launch.browser = TRUE))
}