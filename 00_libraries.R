################################################################################
# 00_libraries.R
# Purpose : Load all project libraries in one call.
#           Source this file at the start of any interactive session.
#
# USAGE
#   source("00_libraries.R")        # from project root
#   source(here("00_libraries.R"))  # from anywhere inside the project
################################################################################

# ── ORDER MATTERS: load MASS before tidyverse to prevent select() masking ────
library(MASS)

# ── Core data manipulation ────────────────────────────────────────────────────
library(tidyverse)      # dplyr, ggplot2, tidyr, purrr, readr, tibble, stringr
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(readr)
library(lubridate)

# ── Financial data & time series ──────────────────────────────────────────────
library(xts)
library(zoo)
library(quantmod)
library(tidyquant)
library(timetk)
library(PerformanceAnalytics)
library(TTR)

# ── Plotting ──────────────────────────────────────────────────────────────────
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(patchwork)
library(cowplot)
library(scales)
library(viridis)
library(treemapify)
library(plotly)
library(fmsb)           # radar / spider charts

# ── Reporting & dashboards ────────────────────────────────────────────────────
library(here)
library(knitr)
library(rmarkdown)
library(reactable)
library(flexdashboard)
library(shiny)
library(bslib)

# ── Modelling & statistics ────────────────────────────────────────────────────
library(glmnet)         # regularised logistic regression
library(pROC)           # ROC / AUC
library(quadprog)       # portfolio optimisation
library(moments)        # skewness / kurtosis

# ── Utilities ─────────────────────────────────────────────────────────────────
library(fs)
library(httr)
library(rvest)
library(tictoc)
library(grid)
library(lattice)
library(chron)

message("00_libraries: all packages loaded.")
