# Load/Transform data for analysis
#
# This script will load the raw TMA result data and perform
# a number of transformations to make it useful for analysis.
# The result will be a table object that can be stored in the
# PROJECT_HOME/data directory.

all_tma <- readxl::read_xlsx(here::here("data-raw/imports/ME-BRTMA-pdata_0.2.0.xlsx")) |>

  # Relabel the cohorts to align with manuscript
  dplyr::mutate(
    cohort = dplyr::case_when(
      cohort == "H" ~ "HF",
      cohort == "PRBB" ~ "HPR",
      .default = cohort
    ),
    # We want to present these in a fixed order
    cohort = factor(cohort, levels=c("NHW","NHB","HF","HPR"))
  )



#
# We read each stain in independently and transform variables as needed.
# After this, we will merge the results together with brca_tissue.
#


# ER
#
er <- readxl::read_xlsx(here::here("data-raw/imports/ME_BRTMA_path_stain_scores_0.2.0.xlsx"),sheet="er") |>
  dplyr::mutate(
    er_percent_positive = as.numeric(per),
    # Categorization: 0, 1-10, >10
    er_category = dplyr::case_when(
      er_percent_positive > 10 ~ "Positive (>10 PPN)",
      er_percent_positive <= 10 & er_percent_positive > 0 ~ "Low Positive (1-10 PPN)",
      is.na(er_percent_positive) ~ NA_character_,
      .default = "Negative (0 PPN)"
    ),
    er_category = factor(er_category, levels=c("Negative (0 PPN)", "Low Positive (1-10 PPN)", "Positive (>10 PPN)"))
  )


# PR
#
pr <- readxl::read_xlsx(here::here("data-raw/imports/ME_BRTMA_path_stain_scores_0.2.0.xlsx"),sheet="pr") |>
  dplyr::mutate(
    pr_percent_positive = as.numeric(per),
    # Categorization: 0, >0
    pr_category = dplyr::case_when(
      pr_percent_positive > 0 ~ "Positive (>0 PPN)",
      is.na(pr_percent_positive) ~ NA_character_,
      .default = "Negative (0 PPN)"
    ),
    pr_category = factor(pr_category, levels=c("Negative (0 PPN)", "Positive (>0 PPN)"))
  )

# HER2
#
her2 <- readxl::read_xlsx(here::here("data-raw/imports/ME_BRTMA_path_stain_scores_0.2.0.xlsx"),sheet="her2") |>
  dplyr::mutate(
    her2_score = as.numeric(score),
    # Categorization: 0,1+; 2+; 3+
    her2_category = dplyr::case_when(
      her2_score %in% c(0,1) ~ "Negative (0,1+)",
      her2_score == 2 ~ "Equivocal (2+)",
      her2_score == 3 ~ "Positive (3+)"
    ),
    her2_category = factor(
      her2_category,
      levels=c("Negative (0,1+)",
               "Equivocal (2+)",
               "Positive (3+)"
      )
    )
  )
# Ki67
#
ki67 <- readxl::read_xlsx(here::here("data-raw/imports/ME_BRTMA_path_stain_scores_0.2.0.xlsx"),sheet="ki") |>
  dplyr::mutate(
    ki67_percent_positive = as.numeric(per),
    # Categorization: <14, >=14
    ki67_category = ifelse(ki67_percent_positive < 14,"<14 PPN", ">= 14 PPN")
  )


# PAM50
pam50 <- readr::read_tsv(
  here::here("data-raw/imports/mebrtma_rnaseq_metadata_20241121.tsv"),
  show_col_types = FALSE
) |>
  dplyr::select(study_patient_id, dplyr::starts_with("subtype")) |>
  dplyr::rename_with(
    \(.x) {stringr::str_c("pam50_",.x)},
    dplyr::starts_with("subtype")
  ) |>
  dplyr::mutate(
    study_patient_id=as.character(study_patient_id),
    pam50_subtype=dplyr::case_match(
      pam50_subtype,
      "Basal" ~ "Basal-like",
      "Normal" ~ "Normal-like",
      "Her2" ~ "HER2-enriched",
      "LumA" ~ "Luminal A",
      "LumB" ~ "Luminal B"
    )
  ) |>
  # Join back to the tma to get the cohort for the PAM50
  dplyr::left_join(
    dplyr::select(all_tma, study_patient_id, cohort) |> dplyr::distinct(),
    by = "study_patient_id"
  )



# Combine stains together into a single table.
brtma <- all_tma |>
  # We keep only the tumor cores from Breast cancers (there are controls, stroma
  # and normal on the TMA).
  dplyr::filter(diagnosis=="tumor" & !is_control & tissue == "Breast") |>

  dplyr::left_join(
    dplyr::select(er, study_core_id, er_percent_positive, er_category),
    by = "study_core_id"
  ) |>
  dplyr::left_join(
    dplyr::select(pr, study_core_id, pr_percent_positive, pr_category),
    by = "study_core_id"
  ) |>
  dplyr::left_join(
    dplyr::select(her2, study_core_id, her2_score, her2_category),
    by = "study_core_id"
  ) |>
  dplyr::left_join(
    dplyr::select(ki67, study_core_id, ki67_percent_positive, ki67_category),
    by = "study_core_id"
  )


# Clean up objects (avoid using these objects further accidently).
rm(er, pr, her2, ki67)

# Finally, do a little more consolidation (including HR status which is
# complex).
brtma <- brtma |>
  dplyr::mutate(
    `Clinical HER2 IHC` = factor(
      `Clinical HER2 IHC`,
      levels = c("Negative", "Borderline", "Positive")
    ),
    `Clinical ER` = factor(
      `Clinical ER`, levels = c("Negative","Equivocal","Positive")
    ),
    hr_category = dplyr::case_when(
      # Negative is only when both receptors are known and negative.
      er_category == "Negative (0 PPN)" & pr_category=="Negative (0 PPN)" ~ "Negative",
      # The approach used is to treat HR as NA if either observation is NA
      is.na(er_category) | is.na(pr_category) ~ NA,
      # Everything else is positive (either are positive with both observations)
      .default = "Positive"
    ),
    `Clinical HR` = dplyr::case_when(
      `Clinical ER` == "Negative" & `Clinical PR` == "Negative" ~ "Negative",
      is.na(`Clinical ER`) & `Clinical PR` == "Negative" ~ NA,
      `Clinical ER` == "Negative" & is.na(`Clinical PR`) ~ NA,
      is.na(`Clinical ER`) & is.na(`Clinical PR`) ~ NA,
      .default = "Positive"
    ),
    tnbc_category = dplyr::case_when(
      # Here, we do no inference about overall positive/negative, if
      # values are missing they are missing.
      hr_category == "Negative" &
        her2_category %in% c("Negative (0,1+)","Equivocal (2+)") ~ "HR-/HER2-",
      hr_category == "Negative" & her2_category %in% c("Positive (3+)") ~ "HR-/HER2+",
      hr_category == "Positive" & her2_category %in% c("Negative (0,1+)","Equivocal (2+)") ~ "HR+/HER2-",
      hr_category == "Positive" & her2_category %in% c("Positive (3+)") ~ "HR+/HER2+",
      .default = NA
    ),
    tnbc_category = factor(
      tnbc_category,
      levels = c("HR-/HER2-","HR-/HER2+","HR+/HER2-","HR+/HER2+")
    ),
    `Clinical TNBC` = dplyr::case_when(
      # NB: Borderline Clinical HER2/IHC are excluded from TNBC calls and treated as missing
      `Clinical HR` == "Negative" & `Clinical HER2 IHC` %in% c("Negative") ~ "HR-/HER2-",
      `Clinical HR` == "Negative" & `Clinical HER2 IHC` %in% c("Borderline","Positive") ~ "HR-/HER2+",
      `Clinical HR` == "Positive" & `Clinical HER2 IHC` %in% c("Negative") ~ "HR+/HER2-",
      `Clinical HR` == "Positive" & `Clinical HER2 IHC` %in% c("Borderline","Positive") ~ "HR+/HER2+",
      .default = NA
    ),
    `Clinical TNBC` = factor(`Clinical TNBC`, levels=c("HR-/HER2-","HR-/HER2+","HR+/HER2-","HR+/HER2+"))
  )

# These can be used to hand-check that HR is correctly defined.
# table(hr$er_category, hr$pr_category, useNA="always")
# table(hr$`Clinical ER`, hr$`Clinical PR`, useNA="always")

saveRDS(brtma, here::here("data-raw/work/01-brtma.rds"))
saveRDS(pam50, here::here("data-raw/work/01-pam50.rds"))
saveRDS(all_tma, here::here("data-raw/work/01-full_tma.rds"))
