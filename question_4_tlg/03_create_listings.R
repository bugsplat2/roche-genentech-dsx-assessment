# question_4_tlg/03_create_listings.R

suppressPackageStartupMessages({
  library(dplyr)
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

# TEAEs only
lst <- adae %>%
  filter(TRTEMFL == "Y") %>%
  left_join(adsl_trt, by = "USUBJID") %>%
  mutate(
    # sortable start date from AESTDTC (date part only)
    AESTDT = if_else(
      !is.na(AESTDTC) & str_detect(AESTDTC, "^\\d{4}-\\d{2}-\\d{2}"),
      substr(AESTDTC, 1, 10),
      NA_character_
    ),
    AETERM = if_else(is.na(AETERM) | AETERM == "", "Uncoded", AETERM),
    AESEV  = if_else(is.na(AESEV)  | AESEV  == "", "Missing", AESEV)
  ) %>%
  arrange(USUBJID, AESTDT)

# Relationship column (dataset-dependent)
rel_col <- intersect(names(lst), c("AEREL", "AERELN", "AERELNST", "REL", "AERELC"))
rel_col <- if (length(rel_col) == 0) NA_character_ else rel_col[1]

listing_df <- lst %>%
  transmute(
    USUBJID,
    TRTGRP,
    AETERM,
    AESEV,
    RELATIONSHIP = if (!is.na(rel_col)) .data[[rel_col]] else NA_character_,
    AESTDTC,
    AEENDTC
  )

gt_tbl <- listing_df %>%
  gt() %>%
  tab_header(
    title = "Listing of Treatment-Emergent Adverse Events (TEAEs)",
    subtitle = paste0("Treatment variable used: ", ifelse(is.na(trt_var), "None", trt_var))
  )

out_file <- file.path(out_dir, "ae_listings.html")
gtsave(gt_tbl, out_file)

message("[DONE] Wrote: ", out_file)
