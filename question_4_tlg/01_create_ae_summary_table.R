# question_4_tlg/01_create_ae_summary_table.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(gt)
  library(pharmaverseadam)
  library(stringr)
})

out_dir <- file.path("question_4_tlg", "output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# Pick best available treatment variable from ADSL
get_trt_var <- function(df) {
  candidates <- c("ACTARM", "TRT01A", "TRT01P", "ARM")
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) NA_character_ else hit[1]
}

trt_var <- get_trt_var(adsl)

adsl_trt <- adsl %>%
  mutate(
    TRTGRP = if (!is.na(trt_var)) .data[[trt_var]] else NA_character_,
    TRTGRP = if_else(is.na(TRTGRP) | TRTGRP == "", "Missing", as.character(TRTGRP))
  ) %>%
  distinct(USUBJID, TRTGRP)

# TEAE records
teae <- adae %>%
  filter(TRTEMFL == "Y") %>%
  left_join(adsl_trt, by = "USUBJID") %>%
  mutate(
    AESOC = if_else(is.na(AESOC) | AESOC == "", "Uncoded", AESOC)
  )

# Denominators: unique subjects per TRTGRP (all subjects)
denoms <- adsl_trt %>%
  count(TRTGRP, name = "N_arm")

# Count unique subjects with >=1 TEAE in each AESOC by TRTGRP
soc_counts <- teae %>%
  distinct(USUBJID, TRTGRP, AESOC) %>%
  count(TRTGRP, AESOC, name = "n") %>%
  left_join(denoms, by = "TRTGRP") %>%
  mutate(pct = 100 * n / N_arm)

# Total column across all TRTGRP
N_total <- adsl_trt %>% distinct(USUBJID) %>% nrow()
soc_total <- teae %>%
  distinct(USUBJID, AESOC) %>%
  count(AESOC, name = "n") %>%
  mutate(
    TRTGRP = "Total",
    N_arm = N_total,
    pct = 100 * n / N_arm
  )

soc_all <- bind_rows(soc_counts, soc_total) %>%
  mutate(cell = sprintf("%d (%.1f%%)", n, pct))

# Sort rows by descending Total n
order_soc <- soc_all %>%
  filter(TRTGRP == "Total") %>%
  arrange(desc(n), AESOC) %>%
  pull(AESOC)

soc_all <- soc_all %>%
  mutate(AESOC = factor(AESOC, levels = order_soc))

tbl_df <- soc_all %>%
  select(AESOC, TRTGRP, cell) %>%
  pivot_wider(names_from = TRTGRP, values_from = cell) %>%
  arrange(AESOC) %>%
  mutate(AESOC = as.character(AESOC))

# Any TEAE row
any_teae_arm <- teae %>%
  distinct(USUBJID, TRTGRP) %>%
  count(TRTGRP, name = "n") %>%
  left_join(denoms, by = "TRTGRP") %>%
  mutate(cell = sprintf("%d (%.1f%%)", n, 100 * n / N_arm)) %>%
  select(TRTGRP, cell)

any_teae_total <- teae %>%
  distinct(USUBJID) %>%
  summarise(n = n()) %>%
  mutate(
    TRTGRP = "Total",
    cell = sprintf("%d (%.1f%%)", n, 100 * n / N_total)
  ) %>%
  select(TRTGRP, cell)

any_row <- bind_rows(any_teae_arm, any_teae_total) %>%
  pivot_wider(names_from = TRTGRP, values_from = cell) %>%
  mutate(AESOC = "Any TEAE") %>%
  select(AESOC, everything())

final_df <- bind_rows(any_row, tbl_df)

gt_tbl <- final_df %>%
  gt(rowname_col = "AESOC") %>%
  tab_header(title = "Summary of Treatment-Emergent Adverse Events (TEAEs)") %>%
  tab_source_note(source_note = paste0("Treatment variable used: ", ifelse(is.na(trt_var), "None", trt_var))) %>%
  fmt_missing(everything(), missing_text = "0 (0.0%)")

out_file <- file.path(out_dir, "ae_summary_table.html")
gtsave(gt_tbl, out_file)

message("[DONE] Wrote: ", out_file)
