# question_3_adam/create_adsl.R

suppressPackageStartupMessages({
  library(admiral)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(pharmaversesdtm)
})

# ---------------------------
# Helpers (VECTORIZED)
# ---------------------------

get_date_part <- function(dtc) {
  if_else(!is.na(dtc) & str_detect(dtc, "^\\d{4}-\\d{2}-\\d{2}"),
          str_sub(dtc, 1, 10),
          NA_character_)
}

# returns "none", "hh", "hhmm", "hhmmss" for each element
time_granularity <- function(dtc) {
  # default "none"
  out <- rep("none", length(dtc))
  
  has_date <- !is.na(dtc) & str_detect(dtc, "^\\d{4}-\\d{2}-\\d{2}")
  has_T <- has_date & str_detect(dtc, "T")
  
  # extract time part after T (strip timezone if present)
  tpart <- rep(NA_character_, length(dtc))
  tpart[has_T] <- str_split_fixed(dtc[has_T], "T", 2)[, 2]
  tpart[has_T] <- str_replace(tpart[has_T], "([Zz]|[+-]\\d{2}:?\\d{2})$", "")
  
  out[has_T & str_detect(tpart, "^\\d{2}:\\d{2}:\\d{2}$")] <- "hhmmss"
  out[has_T & str_detect(tpart, "^\\d{2}:\\d{2}$")] <- "hhmm"
  out[has_T & str_detect(tpart, "^\\d{2}$")] <- "hh"
  
  out
}

# Impute time to hh:mm:ss per rules (vectorized)
impute_time_to_hms <- function(dtc) {
  datep <- get_date_part(dtc)
  gran <- time_granularity(dtc)
  
  # time part after T, cleaned
  has_T <- !is.na(dtc) & str_detect(dtc, "T") & !is.na(datep)
  tpart <- rep(NA_character_, length(dtc))
  tpart[has_T] <- str_split_fixed(dtc[has_T], "T", 2)[, 2]
  tpart[has_T] <- str_replace(tpart[has_T], "([Zz]|[+-]\\d{2}:?\\d{2})$", "")
  
  hh <- if_else(!is.na(tpart) & nchar(tpart) >= 2, str_sub(tpart, 1, 2), NA_character_)
  mm <- if_else(!is.na(tpart) & nchar(tpart) >= 5, str_sub(tpart, 4, 5), NA_character_)
  ss <- if_else(!is.na(tpart) & nchar(tpart) >= 8, str_sub(tpart, 7, 8), NA_character_)
  
  out <- rep(NA_character_, length(dtc))
  
  # If date part missing => NA
  out[is.na(datep)] <- NA_character_
  
  # No time => 00:00:00
  idx_none <- !is.na(datep) & gran == "none"
  out[idx_none] <- paste0(datep[idx_none], "T00:00:00")
  
  # hh => hh:00:00
  idx_hh <- !is.na(datep) & gran == "hh" & !is.na(hh)
  out[idx_hh] <- paste0(datep[idx_hh], "T", hh[idx_hh], ":00:00")
  
  # hhmm => hh:mm:00
  idx_hhmm <- !is.na(datep) & gran == "hhmm" & !is.na(hh) & !is.na(mm)
  out[idx_hhmm] <- paste0(datep[idx_hhmm], "T", hh[idx_hhmm], ":", mm[idx_hhmm], ":00")
  
  # hhmmss => unchanged (normalized to datep + T + tpart)
  idx_hhmmss <- !is.na(datep) & gran == "hhmmss" & !is.na(tpart)
  out[idx_hhmmss] <- paste0(datep[idx_hhmmss], "T", tpart[idx_hhmmss])
  
  out
}

# Imputation flag per spec nuance (vectorized)
# - time missing OR hh only => "TIME"
# - hh:mm only (seconds missing) => NA
# - full hh:mm:ss => NA
derive_trtsdtm_flag <- function(exstdtc) {
  gran <- time_granularity(exstdtc)
  case_when(
    is.na(get_date_part(exstdtc)) ~ NA_character_, # no usable date -> NA
    gran == "none" ~ "TIME",
    gran == "hh" ~ "TIME",
    gran == "hhmm" ~ NA_character_,
    gran == "hhmmss" ~ NA_character_,
    TRUE ~ NA_character_
  )
}

date_to_yyyymmdd_num <- function(d) {
  if_else(is.na(d), NA_real_, as.numeric(format(d, "%Y%m%d")))
}

parse_ymd_date <- function(x) {
  suppressWarnings(as.Date(x))
}

parse_iso_dtm_utc <- function(x) {
  suppressWarnings(as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
}

is_valid_dose <- function(exdose, extrt) {
  (!is.na(exdose) & exdose > 0) |
    (!is.na(exdose) & exdose == 0 & !is.na(extrt) & str_detect(str_to_upper(extrt), "PLACEBO"))
}

# ---------------------------
# Load source SDTM domains
# ---------------------------

dm <- pharmaversesdtm::dm
vs <- pharmaversesdtm::vs
ex <- pharmaversesdtm::ex
ds <- pharmaversesdtm::ds
ae <- pharmaversesdtm::ae

# ---------------------------
# Start from DM -> ADSL core
# ---------------------------

adsl <- dm %>%
  mutate(
    AGEGR9N = case_when(
      is.na(AGE) ~ NA_real_,
      AGE < 18 ~ 1,
      AGE >= 18 & AGE <= 50 ~ 2,
      AGE > 50 ~ 3,
      TRUE ~ NA_real_
    ),
    AGEGR9 = case_when(
      AGEGR9N == 1 ~ "<18",
      AGEGR9N == 2 ~ "18-50",
      AGEGR9N == 3 ~ ">50",
      TRUE ~ NA_character_
    ),
    ITTFL = if_else(!is.na(ARM) & ARM != "", "Y", "N")
  )

# ---------------------------
# TRTSDTM: earliest valid-dose EXSTDTC with complete date part
# ---------------------------

ex_trt <- ex %>%
  mutate(
    valid_dose = is_valid_dose(EXDOSE, EXTRT),
    ex_date = get_date_part(EXSTDTC)
  ) %>%
  filter(valid_dose, !is.na(ex_date)) %>%
  mutate(
    TRTSDTM_ISO = impute_time_to_hms(EXSTDTC),
    TRTSDTM = parse_iso_dtm_utc(TRTSDTM_ISO),
    TRTSDTMF = derive_trtsdtm_flag(EXSTDTC)
  ) %>%
  filter(!is.na(TRTSDTM)) %>%
  group_by(USUBJID) %>%
  arrange(TRTSDTM, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(USUBJID, TRTSDTM, TRTSDTMF)

adsl <- adsl %>%
  left_join(ex_trt, by = "USUBJID")

# ---------------------------
# ABNSBPFL: abnormal SYSBP in mmHg (>=140 OR <100)
# ---------------------------

abn_sbp <- vs %>%
  filter(VSTESTCD == "SYSBP", VSSTRESU == "mmHg") %>%
  mutate(abn = !is.na(VSSTRESN) & (VSSTRESN >= 140 | VSSTRESN < 100)) %>%
  group_by(USUBJID) %>%
  summarise(ABNSBPFL = if_else(any(abn, na.rm = TRUE), "Y", "N"), .groups = "drop")

adsl <- adsl %>%
  left_join(abn_sbp, by = "USUBJID") %>%
  mutate(ABNSBPFL = coalesce(ABNSBPFL, "N"))

# ---------------------------
# CARPOPFL: any AE with AESOC == "CARDIAC DISORDERS" => "Y" else NA
# ---------------------------

carpop <- ae %>%
  mutate(is_card = !is.na(AESOC) & str_to_upper(AESOC) == "CARDIAC DISORDERS") %>%
  group_by(USUBJID) %>%
  summarise(CARPOPFL = if_else(any(is_card, na.rm = TRUE), "Y", NA_character_), .groups = "drop")

adsl <- adsl %>%
  left_join(carpop, by = "USUBJID")

# ---------------------------
# LSTALVDT candidates + max (numeric YYYYMMDD)
# ---------------------------

last_vs <- vs %>%
  mutate(
    vs_date = get_date_part(VSDTC),
    has_res = !(is.na(VSSTRESN) & (is.na(VSSTRESC) | VSSTRESC == ""))
  ) %>%
  filter(!is.na(vs_date), has_res) %>%
  mutate(vs_dt = parse_ymd_date(vs_date)) %>%
  group_by(USUBJID) %>%
  summarise(VS_LASTDT = max(vs_dt, na.rm = TRUE), .groups = "drop") %>%
  mutate(VS_LASTDT = if_else(is.infinite(VS_LASTDT), as.Date(NA), VS_LASTDT))

last_ae <- ae %>%
  mutate(ae_date = get_date_part(AESTDTC)) %>%
  filter(!is.na(ae_date)) %>%
  mutate(ae_dt = parse_ymd_date(ae_date)) %>%
  group_by(USUBJID) %>%
  summarise(AE_LASTDT = max(ae_dt, na.rm = TRUE), .groups = "drop") %>%
  mutate(AE_LASTDT = if_else(is.infinite(AE_LASTDT), as.Date(NA), AE_LASTDT))

last_ds <- ds %>%
  mutate(ds_date = get_date_part(DSSTDTC)) %>%
  filter(!is.na(ds_date)) %>%
  mutate(ds_dt = parse_ymd_date(ds_date)) %>%
  group_by(USUBJID) %>%
  summarise(DS_LASTDT = max(ds_dt, na.rm = TRUE), .groups = "drop") %>%
  mutate(DS_LASTDT = if_else(is.infinite(DS_LASTDT), as.Date(NA), DS_LASTDT))

last_ex <- ex %>%
  mutate(
    valid_dose = is_valid_dose(EXDOSE, EXTRT),
    ex_date = get_date_part(EXSTDTC)
  ) %>%
  filter(valid_dose, !is.na(ex_date)) %>%
  mutate(ex_dt = parse_ymd_date(ex_date)) %>%
  group_by(USUBJID) %>%
  summarise(EX_LASTDT = max(ex_dt, na.rm = TRUE), .groups = "drop") %>%
  mutate(EX_LASTDT = if_else(is.infinite(EX_LASTDT), as.Date(NA), EX_LASTDT))

adsl <- adsl %>%
  left_join(last_vs, by = "USUBJID") %>%
  left_join(last_ae, by = "USUBJID") %>%
  left_join(last_ds, by = "USUBJID") %>%
  left_join(last_ex, by = "USUBJID") %>%
  rowwise() %>%
  mutate(
    LSTALVDT = {
      cand <- c(VS_LASTDT, AE_LASTDT, DS_LASTDT, EX_LASTDT)
      cand <- cand[!is.na(cand)]
      if (length(cand) == 0) NA_real_ else date_to_yyyymmdd_num(max(cand))
    }
  ) %>%
  ungroup() %>%
  select(-VS_LASTDT, -AE_LASTDT, -DS_LASTDT, -EX_LASTDT)

# Return ADSL
adsl
