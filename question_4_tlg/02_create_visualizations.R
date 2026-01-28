# question_4_tlg/02_create_visualizations.R

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(pharmaverseadam)
  library(stringr)
  library(purrr)
  library(tibble)
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

# TEAEs only + add TRT group
teae <- adae %>%
  filter(TRTEMFL == "Y") %>%
  left_join(adsl_trt, by = "USUBJID") %>%
  mutate(
    AESEV  = if_else(is.na(AESEV)  | AESEV  == "", "Missing", AESEV),
    AETERM = if_else(is.na(AETERM) | AETERM == "", "Uncoded", AETERM)
  )

# ---------------------------
# Plot 1: Severity distribution by treatment (counts)
# ---------------------------
p1_df <- teae %>%
  count(TRTGRP, AESEV, name = "n")

p1 <- ggplot(p1_df, aes(x = TRTGRP, y = n, fill = AESEV)) +
  geom_col(position = "stack") +
  labs(
    title = "TEAE Severity Distribution by Treatment",
    subtitle = paste0("Treatment variable used: ", ifelse(is.na(trt_var), "None", trt_var)),
    x = "Treatment Group",
    y = "Number of TEAE Records",
    fill = "AE Severity"
  ) +
  theme_minimal()

p1_file <- file.path(out_dir, "ae_severity_by_treatment.png")
ggsave(p1_file, p1, width = 10, height = 6, dpi = 300)

# ---------------------------
# Plot 2: Top 10 AEs with 95% CI for incidence (overall)
# Incidence = subjects with >=1 TEAE for term / total subjects
# CI: Wilson 95%
# ---------------------------
N_total <- adsl_trt %>% distinct(USUBJID) %>% nrow()

term_counts <- teae %>%
  distinct(USUBJID, AETERM) %>%
  count(AETERM, name = "n_subj") %>%
  mutate(p = n_subj / N_total) %>%
  arrange(desc(n_subj), AETERM) %>%
  slice_head(n = 10)

wilson_ci <- function(x, n, z = 1.96) {
  p <- x / n
  denom <- 1 + (z^2 / n)
  center <- (p + (z^2 / (2*n))) / denom
  half <- (z * sqrt((p*(1-p)/n) + (z^2/(4*n^2)))) / denom
  c(lower = center - half, upper = center + half)
}

ci_mat <- purrr::map_dfr(term_counts$n_subj, ~{
  ci <- wilson_ci(.x, N_total)
  tibble(ci_lower = ci["lower"], ci_upper = ci["upper"])
})

term_counts <- bind_cols(term_counts, ci_mat) %>%
  mutate(
    pct = 100 * p,
    pct_lo = 100 * ci_lower,
    pct_hi = 100 * ci_upper,
    AETERM = reorder(AETERM, pct)
  )

p2 <- ggplot(term_counts, aes(x = AETERM, y = pct)) +
  geom_point() +
  geom_errorbar(aes(ymin = pct_lo, ymax = pct_hi), width = 0.2) +
  coord_flip() +
  labs(
    title = "Top 10 TEAE Terms (Overall Incidence with 95% CI)",
    x = "AE Term (AETERM)",
    y = "Incidence (%)"
  ) +
  theme_minimal()

p2_file <- file.path(out_dir, "ae_top10_incidence_ci.png")
ggsave(p2_file, p2, width = 10, height = 6, dpi = 300)

message("[DONE] Wrote: ", p1_file)
message("[DONE] Wrote: ", p2_file)
