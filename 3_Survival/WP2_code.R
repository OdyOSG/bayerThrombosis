# WP2: Survival
# ----------------------------------------------------------------
# Survival analysis

# Output folder for Survival
output_surv <- file.path(tempDir,"Survival")
if (!file.exists(output_surv)){
  dir.create(output_surv, recursive = TRUE)}

cdm[[paste0(prefix_db,"outcome_cohorts")]] <- cdm[[paste0(prefix_db,"outcome_cohorts")]] %>%
  dplyr::mutate(cohort_end_date = cohort_start_date) %>%
  dplyr::compute()

attr(cdm[[paste0(prefix_db,"outcome_cohorts")]], "tbl_name") <- paste0(prefix_db,"outcome_cohorts")

surv <- estimateSingleEventSurvival(
  cdm,
  targetCohortTable = paste0(prefix_db,"target_cohorts"),
  outcomeCohortTable = paste0(prefix_db,"outcome_cohorts"),
  outcomeWashout = 0,
  eventGap = 1,
  minimumSurvivalDays = 0
)

write.csv(surv, file = here::here(output_surv, paste0("survival.csv")))
write.csv(attr(surv, "events"), file = here::here(output_surv, paste0("survival_events.csv")))
write.csv(attr(surv, "attrition"), file = here::here(output_surv, paste0("survival_attrition.csv")))

