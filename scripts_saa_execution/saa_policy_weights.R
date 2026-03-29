################################################################################
# scripts_saa_execution/saa_policy_weights.R
# Purpose : Construct the 3-book policy weight structure.
#           Ported from etf_saa_taa/02_policy_weights.R.
#
# Input   : 02_data_processed/saa_selection_matrix.rds
# Output  : policy_list  (list, global env)
#           02_data_processed/saa_policy_list.rds
#
# Books
#   BOOK 1 SAA      : 60% SPY  +  40% AGG  (static anchor)
#   BOOK 2 TAA LS   : +5% per Spring-Load (top 3 by alpha_velocity_z)
#                     −5% vs category benchmark (SPY for Equity, AGG for Fixed)
#   BOOK 3 TOTAL    : net sum of Book 1 + Book 2
################################################################################

library(dplyr)
library(here)

if (!exists("selection_matrix")) {
  sel_path <- here::here("02_data_processed/saa_selection_matrix.rds")
  if (!file.exists(sel_path))
    stop("saa_policy: run saa_alpha_intelligence.R first.")
  selection_matrix <- readRDS(sel_path)
}

total_aum    <- 1e6   # $1 M reference portfolio
taa_slot_size <- 0.05  # 5% per LS pair

# ── BOOK 1: Sovereign SAA (60 / 40 anchor) ────────────────────────────────────
saa_book <- tibble(
  ticker = c("SPY", "AGG"),
  weight = c(0.60,  0.40),
  book   = "BOOK_1_SAA"
)

# ── BOOK 2: TAA Long / Short synthetics ───────────────────────────────────────
message("saa_policy: harvesting Spring-Loads for Book 2 ...")

spring_loads <- selection_matrix %>%
  filter(status == "Spring-Load", !ticker %in% c("SPY", "AGG")) %>%
  slice_max(alpha_velocity_z, n = 3)   # top 3 by confirmed momentum

if (nrow(spring_loads) > 0) {

  taa_longs <- spring_loads %>%
    mutate(weight = taa_slot_size, book = "BOOK_2_TAA_LONG") %>%
    select(ticker, weight, book)

  # Short leg: fund from category benchmark
  #   Equity Spring-Load → short SPY
  #   Fixed  Spring-Load → short AGG
  taa_shorts <- spring_loads %>%
    mutate(
      ticker = ifelse(category == "Equity", "SPY", "AGG"),
      weight = -taa_slot_size,
      book   = "BOOK_2_TAA_SHORT"
    ) %>%
    select(ticker, weight, book)

  ls_book <- bind_rows(taa_longs, taa_shorts)

} else {
  message("saa_policy: no Spring-Loads found — Book 2 is flat.")
  ls_book <- tibble(ticker = character(), weight = numeric(), book = character())
}

# ── BOOK 3: Total netted exposure ────────────────────────────────────────────
total_combined <- bind_rows(saa_book, ls_book) %>%
  group_by(ticker) %>%
  summarise(net_weight = sum(weight), .groups = "drop") %>%
  mutate(book = "BOOK_3_TOTAL")

# ── Pack and save ─────────────────────────────────────────────────────────────
policy_list <- list(
  SAA   = saa_book,
  TAA   = ls_book,
  TOTAL = total_combined
)

saveRDS(policy_list, here::here("02_data_processed/saa_policy_list.rds"))
message("saa_policy: SAVED → 02_data_processed/saa_policy_list.rds")

# ── Auto-run guard ────────────────────────────────────────────────────────────
if (!isTRUE(getOption("knitr.in.progress"))) {

  cat("\n── BOOK 1: SAA ──────────────────────────────────────────\n")
  print(policy_list$SAA)

  cat("\n── BOOK 2: TAA LS Synthetics ───────────────────────────\n")
  if (nrow(policy_list$TAA) > 0) print(policy_list$TAA) else cat("  (flat)\n")

  cat("\n── BOOK 3: Total Net Exposure ──────────────────────────\n")
  print(policy_list$TOTAL %>% arrange(desc(net_weight)))

  cat("\n── Net check ───────────────────────────────────────────\n")
  cat("  Sum of net weights:", round(sum(policy_list$TOTAL$net_weight), 4),
      " (should be 1.00)\n")
  cat("  Reference AUM    : $", format(total_aum, big.mark = ","), "\n")
}
