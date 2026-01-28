#!/usr/bin/env Rscript
# 02_create_ds_domain.R
#
# Question 2: Create SDTM DS domain from pharmaverseraw::ds_raw using {sdtm.oak}
#
# Expected variables (per assessment):
# STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT, VISITNUM, VISIT,
# DSDTC, DSSTDTC, DSSTDY

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(pharmaverseraw)
  library(sdtm.oak)
  library(lubridate)
})

out_dir <- file.path("question_2_sdtm", "output")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------
# 1) Load raw input
# ------------------------------------------------------------
raw <- pharmaverseraw::ds_raw

# ------------------------------------------------------------
# 2) Add oak ID vars (required by sdtm.oak mapping helpers)
# ------------------------------------------------------------
raw <- raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

# ------------------------------------------------------------
# 3) Study Controlled Terminology (CT)
# ------------------------------------------------------------
# Using the example CT bundled with sdtm.oak.
study_ct <- read.csv(
  system.file("raw_data/sdtm_ct.csv", package = "sdtm.oak"),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 4) Build DS core
# ------------------------------------------------------------

# DSTERM (reported term)
ds <- assign_no_ct(
  raw_dat = raw,
  raw_var = "IT.DSTERM",
  tgt_var = "DSTERM",
  id_vars = oak_id_vars()
)

# DSDECOD (standardized term) using disposition codelist C66727
ds <- ds %>%
  assign_no_ct(
    raw_dat = raw,
    raw_var = "IT.DSDECOD",
    tgt_var = "DSDECOD",
    id_vars = oak_id_vars()
  )

# Optional free text "Other, specify" (not a required SDTM variable in prompt,
# but kept here for traceability; can be removed if you want.)
ds <- ds %>%
  assign_no_ct(
    raw_dat = raw,
    raw_var = "OTHERSP",
    tgt_var = "DSSP",
    id_vars = oak_id_vars()
  )

# ------------------------------------------------------------
# 5) Dates/times
# ------------------------------------------------------------
# DSDTC: collected disposition date/time from DSDTCOL + DSTMCOL
# In pharmaverseraw, these are character like "01-02-2013" + "13:45" etc.
# We'll treat UNK as unknown token. (If your raw has different unknowns, add them.)
ds <- ds %>%
  assign_datetime(
    raw_dat = raw,
    raw_var = c("DSDTCOL"),
    tgt_var = "DSDTC",
    raw_fmt = c(list(c("d-m-Y", "d-b-Y", "d m Y", "Y-m-d", "m/d/Y"))),
    raw_unk = c("UNK", "NA", ""),
    id_vars = oak_id_vars()
  )

# DSSTDTC: disposition start date (date-only) from IT.DSSTDAT
ds <- ds %>%
  mutate(
    DSSTDTC = {
      x <- raw$`IT.DSSTDAT`
      x <- ifelse(x %in% c("UNK", "NA", ""), NA_character_, x)
      dt <- suppressWarnings(lubridate::parse_date_time(x, orders = c("dmy", "mdy")))
      ifelse(is.na(dt), NA_character_, format(as.Date(dt), "%Y-%m-%d"))
    }
  )

# ------------------------------------------------------------
# 6) VISIT / VISITNUM from INSTANCE
# ------------------------------------------------------------
# INSTANCE values look like "Baseline", "Week 26", etc.
# We'll set VISIT = INSTANCE.
# VISITNUM: parse numbers from "Week X" as X; Baseline -> 0; else NA.
visitnum_from_instance <- function(x) {
  x <- as.character(x)
  out <- rep(NA_real_, length(x))
  out[tolower(x) == "baseline"] <- 0
  
  wk <- grepl("^week\\s*[0-9]+$", tolower(x))
  out[wk] <- as.numeric(gsub("[^0-9]", "", x[wk]))
  
  out
}

# ------------------------------------------------------------
# 7) Add SDTM identifiers + sequence and derive DSSTDY
# ------------------------------------------------------------
# STUDYID = raw$STUDY
# DOMAIN = "DS"
# USUBJID = paste0(STUDYID, "-", PATNUM)
# DSCAT: use FORM label or set constant
ds <- ds %>%
  mutate(
    STUDYID = raw$STUDY,
    DOMAIN  = "DS",
    USUBJID = paste0(raw$STUDY, "-", raw$PATNUM),
    VISIT   = raw$INSTANCE,
    VISITNUM = visitnum_from_instance(raw$INSTANCE),
    DSCAT   = "DISPOSITION EVENT"
  ) %>%
  group_by(USUBJID) %>%
  arrange(USUBJID, DSSTDTC, DSDTC, .by_group = TRUE) %>%
  mutate(
    DSSEQ = row_number()
  ) %>%
  ungroup()

# DSSTDY normally uses RFSTDTC as reference. ds_raw doesn’t include RFSTDTC.
# Assumption (documented): use each subject’s earliest non-missing DSSTDTC as Day 1 reference.
# If DSSTDTC missing but DSDTC exists, use DSDTC’s date portion as a fallback.
# --- Robust date extraction from ISO-ish DTC strings ---
to_date10 <- function(x) {
  # handle character, POSIXct, list cols defensively
  x_chr <- as.character(x)
  # take first 10 chars if looks like YYYY-MM-DD, else NA
  x10 <- ifelse(nchar(x_chr) >= 10, substr(x_chr, 1, 10), NA_character_)
  # parse as Date (YYYY-MM-DD); invalid -> NA
  suppressWarnings(as.Date(x10, format = "%Y-%m-%d"))
}

ds <- ds %>%
  mutate(
    DSSTDTC_DATE = to_date10(DSSTDTC),
    DSDTC_DATE   = to_date10(DSDTC)
  ) %>%
  group_by(USUBJID) %>%
  mutate(
    REFDATE = suppressWarnings(min(DSSTDTC_DATE, na.rm = TRUE)),
    REFDATE = ifelse(is.infinite(REFDATE), NA, REFDATE),
    REFDATE = ifelse(
      is.na(REFDATE),
      suppressWarnings(min(DSDTC_DATE, na.rm = TRUE)),
      REFDATE
    ),
    REFDATE = ifelse(is.infinite(REFDATE), NA, REFDATE),
    EVENTDATE = dplyr::coalesce(DSSTDTC_DATE, DSDTC_DATE),
    DSSTDY = ifelse(!is.na(REFDATE) & !is.na(EVENTDATE),
                    as.integer(EVENTDATE - as.Date(REFDATE)) + 1L,
                    NA_integer_)
  ) %>%
  ungroup() %>%
  select(-DSSTDTC_DATE, -DSDTC_DATE, -REFDATE, -EVENTDATE)

# ------------------------------------------------------------
# 8) Final variable set (as required by prompt)
# ------------------------------------------------------------
ds_out <- ds %>%
  select(
    STUDYID, DOMAIN, USUBJID, DSSEQ,
    DSTERM, DSDECOD, DSCAT,
    VISITNUM, VISIT,
    DSDTC, DSSTDTC, DSSTDY
  )

out_file <- file.path(out_dir, "ds.csv")
write_csv(ds_out, out_file)
message("[DONE] Wrote DS CSV: ", out_file)
