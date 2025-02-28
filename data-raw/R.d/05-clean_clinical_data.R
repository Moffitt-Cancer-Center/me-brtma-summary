 # 05-clean_clinical_data.R

pdata <- readRDS(here::here("data-raw/imports/clinical_data.rds"))

# Clean up pdata for reporting (naming, factors, etc)
pdata <- pdata |>
  dplyr::mutate(
    # Reset to correct cohort labelling
    cohort = ifelse(cohort == "PRBB", "HPR", cohort),
    # And create the same cohort ordering.
    cohort = factor(cohort, levels = c("NHW","NHB","HF","HPR")),
    pathology_t_stage = dplyr::case_when(
      pathology_t_stage == "T2, T3, T4 (Pathology)" ~ "T2, T3, T4",
      pathology_t_stage == "Tis, T0, T1 (Pathology)" ~ "Tis, T0, T1",
      pathology_t_stage == "Missing" ~ NA
    ),
    pathology_t_stage = factor(pathology_t_stage, levels=c("Tis, T0, T1", "T2, T3, T4")),
    grade_differentiation = dplyr::na_if(grade_differentiation, "Missing"),
    grade_differentiation = factor(
      grade_differentiation,
      levels=c(
        "Well Differentiated",
        "Moderately Differentiated",
        "Poorly Differentiated",
        "Undifferentiated"
      )
    ),

    # define outcomes
    os = survival::Surv(months_to_last_contact, vital_status=="Dead"),
    followup = survival::Surv(months_to_last_contact, vital_status=="Alive")
  )



saveRDS(pdata, file=here::here("data-raw/work/05-clinical_data.rds"))

