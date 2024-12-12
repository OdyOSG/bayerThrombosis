# WP1: Incidence and Prevalence

# Output folder for WP1
output_inc <- file.path(tempDir,"Incidence")
if (!file.exists(output_inc)){
  dir.create(output_inc, recursive = TRUE)}

cdm[[paste0(prefix_db,"outcome_cohorts")]] <- cdm[[paste0(prefix_db,"outcome_cohorts")]] %>%
  dplyr::mutate(cohort_end_date = cohort_start_date) %>%
  dplyr::compute()

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# 1a: All outcomes on TC1
# ----------------------------------------------------------------
# ----------------------------------------------------------------

info(logger, '-- Calculating incidence of all outcomes on tia')

# Create denominator cohorts
# No exclusion
cdm <- IncidencePrevalence::generateTargetDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator",
  targetCohortTable = paste0(prefix_db,"target_cohorts"),
  targetCohortId = 5,
  cohortDateRange = c(as.Date("2012-01-01"), as.Date("2024-07-01"))
)

cdm$outcomecut <- cdm[[paste0(prefix_db,"outcome_cohorts")]] %>%
  dplyr::left_join(cdm$denominator %>%
                     dplyr::select("subject_id", 
                                   "target_start" = "cohort_start_date"),
                   by = "subject_id") %>%
  dplyr::filter(!(!is.na(target_start) & cohort_start_date < target_start)) %>%
  dplyr::select(-"target_start") %>% 
  dplyr::compute(name = "outcomecut", temporary = TRUE)


# Exclude people for 1 day after cohort entry
cdm[[c(paste0(prefix_db,"target_cohorts"))]] <- cdm[[c(paste0(prefix_db,"target_cohorts"))]] %>%
  dplyr::mutate(cohort_start_date = cohort_start_date + 1) %>%
  dplyr::compute()

cdm <- IncidencePrevalence::generateTargetDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator1",
  targetCohortTable = paste0(prefix_db,"target_cohorts"),
  targetCohortId = 5,
  cohortDateRange = c(as.Date("2012-01-01"), as.Date("2024-07-01"))
)

cdm$outcomecut1 <- cdm[[paste0(prefix_db,"outcome_cohorts")]] %>%
  dplyr::left_join(cdm$denominator1 %>%
                     dplyr::select("subject_id", 
                                   "target_start" = "cohort_start_date"),
                   by = "subject_id") %>%
  dplyr::filter(!(!is.na(target_start) & cohort_start_date < target_start)) %>%
  select(-"target_start") %>% 
  dplyr::compute(name = "outcomecut1", temporary = TRUE)

# Exclude people for 7 days after cohort entry
cdm[[c(paste0(prefix_db,"target_cohorts"))]] <- cdm[[c(paste0(prefix_db,"target_cohorts"))]] %>%
  dplyr::mutate(cohort_start_date = cohort_start_date + 6) %>%
  dplyr::compute()

cdm <- IncidencePrevalence::generateTargetDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator7",
  targetCohortTable = paste0(prefix_db,"target_cohorts"),
  targetCohortId = 5,
  cohortDateRange = c(as.Date("2012-01-01"), as.Date("2024-07-01"))
)

cdm$outcomecut7 <- cdm[[paste0(prefix_db,"outcome_cohorts")]] %>%
  dplyr::left_join(cdm$denominator7 %>%
                     dplyr::select("subject_id", 
                                   "target_start" = "cohort_start_date"),
                   by = "subject_id") %>%
  dplyr::filter(!(!is.na(target_start) & cohort_start_date < target_start)) %>%
  select(-"target_start") %>% 
  dplyr::compute(name = "outcomecut7", temporary = TRUE)

# Exclude people for 15 days after cohort entry
cdm[[c(paste0(prefix_db,"target_cohorts"))]] <- cdm[[c(paste0(prefix_db,"target_cohorts"))]] %>%
  dplyr::mutate(cohort_start_date = cohort_start_date + 8) %>%
  dplyr::compute()

cdm <- IncidencePrevalence::generateTargetDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator15",
  targetCohortTable = paste0(prefix_db,"target_cohorts"),
  targetCohortId = 5,
  cohortDateRange = c(as.Date("2012-01-01"), as.Date("2024-07-01"))
)

cdm$outcomecut15 <- cdm[[paste0(prefix_db,"outcome_cohorts")]] %>%
  dplyr::left_join(cdm$denominator15 %>%
                     dplyr::select("subject_id", 
                                   "target_start" = "cohort_start_date"),
                   by = "subject_id") %>%
  dplyr::filter(!(!is.na(target_start) & cohort_start_date < target_start)) %>%
  select(-"target_start") %>% 
  dplyr::compute(name = "outcomecut15", temporary = TRUE)

################################################################################
# Start incidence calculations

# First calculation: TAR inf
inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator", 
  outcomeTable = "outcomecut", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc_tarinf.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc_tarinf.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator1", 
  outcomeTable = "outcomecut1", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc_tarinf1.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc_tarinf1.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator7", 
  outcomeTable = "outcomecut7", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc_tarinf7.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc_tarinf7.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator15", 
  outcomeTable = "outcomecut15", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc_tarinf15.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc_tarinf15.csv")))


# TAR 2 years
cdm$denominator <- cdm$denominator %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 730, 
                                 cohort_start_date + 730,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator1 <- cdm$denominator1 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 730, 
                                 cohort_start_date + 730,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator7 <- cdm$denominator7 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 730, 
                                 cohort_start_date + 730,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator15 <- cdm$denominator15 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 730, 
                                 cohort_start_date + 730,
                                 cohort_end_date)) %>%
  dplyr::compute()
  
inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator", 
  outcomeTable = "outcomecut", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc_tar2y.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc_tar2y.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator1", 
  outcomeTable = "outcomecut1", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc_tar2y1.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc_tar2y1.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator7", 
  outcomeTable = "outcomecut7", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc_tar2y7.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc_tar2y7.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator15", 
  outcomeTable = "outcomecut15", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc_tar2y15.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc_tar2y15.csv")))


# TAR 1 year
cdm$denominator <- cdm$denominator %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 365, 
                                 cohort_start_date + 365,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator1 <- cdm$denominator1 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 365, 
                                 cohort_start_date + 365,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator7 <- cdm$denominator7 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 365, 
                                 cohort_start_date + 365,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator15 <- cdm$denominator15 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 365, 
                                 cohort_start_date + 365,
                                 cohort_end_date)) %>%
  dplyr::compute()

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator", 
  outcomeTable = "outcomecut", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc_tar1y.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc_tar1y.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator1", 
  outcomeTable = "outcomecut1", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc_tar1y1.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc_tar1y1.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator7", 
  outcomeTable = "outcomecut7", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc_tar1y7.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc_tar1y7.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator15", 
  outcomeTable = "outcomecut15", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc_tar1y15.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc_tar1y15.csv")))


# TAR6 months
cdm$denominator <- cdm$denominator %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 180, 
                                 cohort_start_date + 180,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator1 <- cdm$denominator1 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 180, 
                                 cohort_start_date + 180,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator7 <- cdm$denominator7 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 180, 
                                 cohort_start_date + 180,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator15 <- cdm$denominator15 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 180, 
                                 cohort_start_date + 180,
                                 cohort_end_date)) %>%
  dplyr::compute()

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator", 
  outcomeTable = "outcomecut", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc_tar6m.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc_tar6m.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator1", 
  outcomeTable = "outcomecut1", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc_tar6m1.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc_tar6m1.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator7", 
  outcomeTable = "outcomecut7", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc_tar6m7.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc_tar6m7.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator15", 
  outcomeTable = "outcomecut15", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc_tar6m15.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc_tar6m15.csv")))


# TAR 3 months
cdm$denominator <- cdm$denominator %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 90, 
                                 cohort_start_date + 90,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator1 <- cdm$denominator1 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 90, 
                                 cohort_start_date + 90,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator7 <- cdm$denominator7 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 90, 
                                 cohort_start_date + 90,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator15 <- cdm$denominator15 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 90, 
                                 cohort_start_date + 90,
                                 cohort_end_date)) %>%
  dplyr::compute()

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator", 
  outcomeTable = "outcomecut", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc_tar3m.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc_tar3m.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator1", 
  outcomeTable = "outcomecut1", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc_tar3m1.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc_tar3m1.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator7", 
  outcomeTable = "outcomecut7", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc_tar3m7.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc_tar3m7.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator15", 
  outcomeTable = "outcomecut15", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc_tar3m15.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc_tar3m15.csv")))


# ----------------------------------------------------------------
# ----------------------------------------------------------------
# 1b: All outcomes on TC2
# ----------------------------------------------------------------
# ----------------------------------------------------------------

info(logger, '-- Calculating incidence of all outcomes on tia_anysaptordapt')

# Take the days back
cdm[[c(paste0(prefix_db,"target_cohorts"))]] <- cdm[[c(paste0(prefix_db,"target_cohorts"))]] %>%
  dplyr::mutate(cohort_start_date = cohort_start_date - 15) %>%
  dplyr::compute()

# Create denominator cohorts
# No exclusion
cdm <- IncidencePrevalence::generateTargetDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator",
  targetCohortTable = paste0(prefix_db,"target_cohorts"),
  targetCohortId = 1,
  cohortDateRange = c(as.Date("2012-01-01"), as.Date("2024-07-01"))
)

cdm$outcomecut <- cdm[[paste0(prefix_db,"outcome_cohorts")]] %>%
  dplyr::left_join(cdm$denominator %>%
                     dplyr::select("subject_id", 
                                   "target_start" = "cohort_start_date"),
                   by = "subject_id") %>%
  dplyr::filter(!(!is.na(target_start) & cohort_start_date < target_start)) %>%
  dplyr::select(-"target_start") %>% 
  dplyr::compute(name = "outcomecut", temporary = TRUE)


# Exclude people for 1 day after cohort entry
cdm[[c(paste0(prefix_db,"target_cohorts"))]] <- cdm[[c(paste0(prefix_db,"target_cohorts"))]] %>%
  dplyr::mutate(cohort_start_date = cohort_start_date + 1) %>%
  dplyr::compute()

cdm <- IncidencePrevalence::generateTargetDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator1",
  targetCohortTable = paste0(prefix_db,"target_cohorts"),
  targetCohortId = 1,
  cohortDateRange = c(as.Date("2012-01-01"), as.Date("2024-07-01"))
)

cdm$outcomecut1 <- cdm[[paste0(prefix_db,"outcome_cohorts")]] %>%
  dplyr::left_join(cdm$denominator1 %>%
                     dplyr::select("subject_id", 
                                   "target_start" = "cohort_start_date"),
                   by = "subject_id") %>%
  dplyr::filter(!(!is.na(target_start) & cohort_start_date < target_start)) %>%
  select(-"target_start") %>% 
  dplyr::compute(name = "outcomecut1", temporary = TRUE)

# Exclude people for 7 days after cohort entry
cdm[[c(paste0(prefix_db,"target_cohorts"))]] <- cdm[[c(paste0(prefix_db,"target_cohorts"))]] %>%
  dplyr::mutate(cohort_start_date = cohort_start_date + 6) %>%
  dplyr::compute()

cdm <- IncidencePrevalence::generateTargetDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator7",
  targetCohortTable = paste0(prefix_db,"target_cohorts"),
  targetCohortId = 1,
  cohortDateRange = c(as.Date("2012-01-01"), as.Date("2024-07-01"))
)

cdm$outcomecut7 <- cdm[[paste0(prefix_db,"outcome_cohorts")]] %>%
  dplyr::left_join(cdm$denominator7 %>%
                     dplyr::select("subject_id", 
                                   "target_start" = "cohort_start_date"),
                   by = "subject_id") %>%
  dplyr::filter(!(!is.na(target_start) & cohort_start_date < target_start)) %>%
  select(-"target_start") %>% 
  dplyr::compute(name = "outcomecut7", temporary = TRUE)

# Exclude people for 15 days after cohort entry
cdm[[c(paste0(prefix_db,"target_cohorts"))]] <- cdm[[c(paste0(prefix_db,"target_cohorts"))]] %>%
  dplyr::mutate(cohort_start_date = cohort_start_date + 8) %>%
  dplyr::compute()

cdm <- IncidencePrevalence::generateTargetDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator15",
  targetCohortTable = paste0(prefix_db,"target_cohorts"),
  targetCohortId = 1,
  cohortDateRange = c(as.Date("2012-01-01"), as.Date("2024-07-01"))
)

cdm$outcomecut15 <- cdm[[paste0(prefix_db,"outcome_cohorts")]] %>%
  dplyr::left_join(cdm$denominator15 %>%
                     dplyr::select("subject_id", 
                                   "target_start" = "cohort_start_date"),
                   by = "subject_id") %>%
  dplyr::filter(!(!is.na(target_start) & cohort_start_date < target_start)) %>%
  select(-"target_start") %>% 
  dplyr::compute(name = "outcomecut15", temporary = TRUE)

################################################################################
# Start incidence calculations

# First calculation: TAR inf
inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator", 
  outcomeTable = "outcomecut", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc1_tarinf.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc1_tarinf.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator1", 
  outcomeTable = "outcomecut1", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc1_tarinf1.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc1_tarinf1.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator7", 
  outcomeTable = "outcomecut7", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc1_tarinf7.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc1_tarinf7.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator15", 
  outcomeTable = "outcomecut15", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc1_tarinf15.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc1_tarinf15.csv")))


# TAR 2 years
cdm$denominator <- cdm$denominator %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 730, 
                                 cohort_start_date + 730,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator1 <- cdm$denominator1 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 730, 
                                 cohort_start_date + 730,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator7 <- cdm$denominator7 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 730, 
                                 cohort_start_date + 730,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator15 <- cdm$denominator15 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 730, 
                                 cohort_start_date + 730,
                                 cohort_end_date)) %>%
  dplyr::compute()

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator", 
  outcomeTable = "outcomecut", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc1_tar2y.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc1_tar2y.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator1", 
  outcomeTable = "outcomecut1", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc1_tar2y1.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc1_tar2y1.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator7", 
  outcomeTable = "outcomecut7", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc1_tar2y7.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc1_tar2y7.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator15", 
  outcomeTable = "outcomecut15", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc1_tar2y15.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc1_tar2y15.csv")))


# TAR 1 year
cdm$denominator <- cdm$denominator %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 365, 
                                 cohort_start_date + 365,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator1 <- cdm$denominator1 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 365, 
                                 cohort_start_date + 365,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator7 <- cdm$denominator7 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 365, 
                                 cohort_start_date + 365,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator15 <- cdm$denominator15 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 365, 
                                 cohort_start_date + 365,
                                 cohort_end_date)) %>%
  dplyr::compute()

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator", 
  outcomeTable = "outcomecut", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc1_tar1y.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc1_tar1y.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator1", 
  outcomeTable = "outcomecut1", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc1_tar1y1.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc1_tar1y1.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator7", 
  outcomeTable = "outcomecut7", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc1_tar1y7.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc1_tar1y7.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator15", 
  outcomeTable = "outcomecut15", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc1_tar1y15.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc1_tar1y15.csv")))


# TAR6 months
cdm$denominator <- cdm$denominator %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 180, 
                                 cohort_start_date + 180,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator1 <- cdm$denominator1 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 180, 
                                 cohort_start_date + 180,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator7 <- cdm$denominator7 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 180, 
                                 cohort_start_date + 180,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator15 <- cdm$denominator15 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 180, 
                                 cohort_start_date + 180,
                                 cohort_end_date)) %>%
  dplyr::compute()

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator", 
  outcomeTable = "outcomecut", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc1_tar6m.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc1_tar6m.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator1", 
  outcomeTable = "outcomecut1", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc1_tar6m1.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc1_tar6m1.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator7", 
  outcomeTable = "outcomecut7", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc1_tar6m7.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc1_tar6m7.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator15", 
  outcomeTable = "outcomecut15", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc1_tar6m15.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc1_tar6m15.csv")))


# TAR 3 months
cdm$denominator <- cdm$denominator %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 90, 
                                 cohort_start_date + 90,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator1 <- cdm$denominator1 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 90, 
                                 cohort_start_date + 90,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator7 <- cdm$denominator7 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 90, 
                                 cohort_start_date + 90,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator15 <- cdm$denominator15 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 90, 
                                 cohort_start_date + 90,
                                 cohort_end_date)) %>%
  dplyr::compute()

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator", 
  outcomeTable = "outcomecut", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc1_tar3m.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc1_tar3m.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator1", 
  outcomeTable = "outcomecut1", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc1_tar3m1.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc1_tar3m1.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator7", 
  outcomeTable = "outcomecut7", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc1_tar3m7.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc1_tar3m7.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator15", 
  outcomeTable = "outcomecut15", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc1_tar3m15.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc1_tar3m15.csv")))



# ----------------------------------------------------------------
# ----------------------------------------------------------------
# 1a: All outcomes on TC3
# ----------------------------------------------------------------
# ----------------------------------------------------------------

info(logger, '-- Calculating incidence of all outcomes on tia_dapt')

# Take the days back
cdm[[c(paste0(prefix_db,"target_cohorts"))]] <- cdm[[c(paste0(prefix_db,"target_cohorts"))]] %>%
  dplyr::mutate(cohort_start_date = cohort_start_date - 15) %>%
  dplyr::compute()

# Create denominator cohorts
# No exclusion
cdm <- IncidencePrevalence::generateTargetDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator",
  targetCohortTable = paste0(prefix_db,"target_cohorts"),
  targetCohortId = 2,
  cohortDateRange = c(as.Date("2012-01-01"), as.Date("2024-07-01"))
)

cdm$outcomecut <- cdm[[paste0(prefix_db,"outcome_cohorts")]] %>%
  dplyr::left_join(cdm$denominator %>%
                     dplyr::select("subject_id", 
                                   "target_start" = "cohort_start_date"),
                   by = "subject_id") %>%
  dplyr::filter(!(!is.na(target_start) & cohort_start_date < target_start)) %>%
  dplyr::select(-"target_start") %>% 
  dplyr::compute(name = "outcomecut", temporary = TRUE)


# Exclude people for 1 day after cohort entry
cdm[[c(paste0(prefix_db,"target_cohorts"))]] <- cdm[[c(paste0(prefix_db,"target_cohorts"))]] %>%
  dplyr::mutate(cohort_start_date = cohort_start_date + 1) %>%
  dplyr::compute()

cdm <- IncidencePrevalence::generateTargetDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator1",
  targetCohortTable = paste0(prefix_db,"target_cohorts"),
  targetCohortId = 2,
  cohortDateRange = c(as.Date("2012-01-01"), as.Date("2024-07-01"))
)

cdm$outcomecut1 <- cdm[[paste0(prefix_db,"outcome_cohorts")]] %>%
  dplyr::left_join(cdm$denominator1 %>%
                     dplyr::select("subject_id", 
                                   "target_start" = "cohort_start_date"),
                   by = "subject_id") %>%
  dplyr::filter(!(!is.na(target_start) & cohort_start_date < target_start)) %>%
  select(-"target_start") %>% 
  dplyr::compute(name = "outcomecut1", temporary = TRUE)

# Exclude people for 7 days after cohort entry
cdm[[c(paste0(prefix_db,"target_cohorts"))]] <- cdm[[c(paste0(prefix_db,"target_cohorts"))]] %>%
  dplyr::mutate(cohort_start_date = cohort_start_date + 6) %>%
  dplyr::compute()

cdm <- IncidencePrevalence::generateTargetDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator7",
  targetCohortTable = paste0(prefix_db,"target_cohorts"),
  targetCohortId = 2,
  cohortDateRange = c(as.Date("2012-01-01"), as.Date("2024-07-01"))
)

cdm$outcomecut7 <- cdm[[paste0(prefix_db,"outcome_cohorts")]] %>%
  dplyr::left_join(cdm$denominator7 %>%
                     dplyr::select("subject_id", 
                                   "target_start" = "cohort_start_date"),
                   by = "subject_id") %>%
  dplyr::filter(!(!is.na(target_start) & cohort_start_date < target_start)) %>%
  select(-"target_start") %>% 
  dplyr::compute(name = "outcomecut7", temporary = TRUE)

# Exclude people for 15 days after cohort entry
cdm[[c(paste0(prefix_db,"target_cohorts"))]] <- cdm[[c(paste0(prefix_db,"target_cohorts"))]] %>%
  dplyr::mutate(cohort_start_date = cohort_start_date + 8) %>%
  dplyr::compute()

cdm <- IncidencePrevalence::generateTargetDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator15",
  targetCohortTable = paste0(prefix_db,"target_cohorts"),
  targetCohortId = 2,
  cohortDateRange = c(as.Date("2012-01-01"), as.Date("2024-07-01"))
)

cdm$outcomecut15 <- cdm[[paste0(prefix_db,"outcome_cohorts")]] %>%
  dplyr::left_join(cdm$denominator15 %>%
                     dplyr::select("subject_id", 
                                   "target_start" = "cohort_start_date"),
                   by = "subject_id") %>%
  dplyr::filter(!(!is.na(target_start) & cohort_start_date < target_start)) %>%
  select(-"target_start") %>% 
  dplyr::compute(name = "outcomecut15", temporary = TRUE)

################################################################################
# Start incidence calculations

# First calculation: TAR inf
inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator", 
  outcomeTable = "outcomecut", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc2_tarinf.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc2_tarinf.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator1", 
  outcomeTable = "outcomecut1", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc2_tarinf1.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc2_tarinf1.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator7", 
  outcomeTable = "outcomecut7", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc2_tarinf7.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc2_tarinf7.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator15", 
  outcomeTable = "outcomecut15", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc2_tarinf15.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc2_tarinf15.csv")))


# TAR 2 years
cdm$denominator <- cdm$denominator %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 730, 
                                 cohort_start_date + 730,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator1 <- cdm$denominator1 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 730, 
                                 cohort_start_date + 730,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator7 <- cdm$denominator7 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 730, 
                                 cohort_start_date + 730,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator15 <- cdm$denominator15 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 730, 
                                 cohort_start_date + 730,
                                 cohort_end_date)) %>%
  dplyr::compute()

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator", 
  outcomeTable = "outcomecut", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc2_tar2y.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc2_tar2y.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator1", 
  outcomeTable = "outcomecut1", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc2_tar2y1.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc2_tar2y1.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator7", 
  outcomeTable = "outcomecut7", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc2_tar2y7.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc2_tar2y7.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator15", 
  outcomeTable = "outcomecut15", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc2_tar2y15.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc2_tar2y15.csv")))


# TAR 1 year
cdm$denominator <- cdm$denominator %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 365, 
                                 cohort_start_date + 365,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator1 <- cdm$denominator1 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 365, 
                                 cohort_start_date + 365,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator7 <- cdm$denominator7 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 365, 
                                 cohort_start_date + 365,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator15 <- cdm$denominator15 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 365, 
                                 cohort_start_date + 365,
                                 cohort_end_date)) %>%
  dplyr::compute()

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator", 
  outcomeTable = "outcomecut", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc2_tar1y.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc2_tar1y.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator1", 
  outcomeTable = "outcomecut1", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc2_tar1y1.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc2_tar1y1.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator7", 
  outcomeTable = "outcomecut7", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc2_tar1y7.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc2_tar1y7.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator15", 
  outcomeTable = "outcomecut15", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc2_tar1y15.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc2_tar1y15.csv")))


# TAR6 months
cdm$denominator <- cdm$denominator %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 180, 
                                 cohort_start_date + 180,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator1 <- cdm$denominator1 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 180, 
                                 cohort_start_date + 180,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator7 <- cdm$denominator7 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 180, 
                                 cohort_start_date + 180,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator15 <- cdm$denominator15 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 180, 
                                 cohort_start_date + 180,
                                 cohort_end_date)) %>%
  dplyr::compute()

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator", 
  outcomeTable = "outcomecut", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc2_tar6m.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc2_tar6m.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator1", 
  outcomeTable = "outcomecut1", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc2_tar6m1.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc2_tar6m1.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator7", 
  outcomeTable = "outcomecut7", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc2_tar6m7.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc2_tar6m7.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator15", 
  outcomeTable = "outcomecut15", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc2_tar6m15.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc2_tar6m15.csv")))


# TAR 3 months
cdm$denominator <- cdm$denominator %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 90, 
                                 cohort_start_date + 90,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator1 <- cdm$denominator1 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 90, 
                                 cohort_start_date + 90,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator7 <- cdm$denominator7 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 90, 
                                 cohort_start_date + 90,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator15 <- cdm$denominator15 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 90, 
                                 cohort_start_date + 90,
                                 cohort_end_date)) %>%
  dplyr::compute()

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator", 
  outcomeTable = "outcomecut", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc2_tar3m.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc2_tar3m.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator1", 
  outcomeTable = "outcomecut1", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc2_tar3m1.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc2_tar3m1.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator7", 
  outcomeTable = "outcomecut7", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc2_tar3m7.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc2_tar3m7.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator15", 
  outcomeTable = "outcomecut15", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc2_tar3m15.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc2_tar3m15.csv")))



# ----------------------------------------------------------------
# ----------------------------------------------------------------
# 1a: All outcomes on TC4
# ----------------------------------------------------------------
# ----------------------------------------------------------------

info(logger, '-- Calculating incidence of all outcomes on tia_noapt')

# Take the days back
cdm[[c(paste0(prefix_db,"target_cohorts"))]] <- cdm[[c(paste0(prefix_db,"target_cohorts"))]] %>%
  dplyr::mutate(cohort_start_date = cohort_start_date - 15) %>%
  dplyr::compute()

# Create denominator cohorts
# No exclusion
cdm <- IncidencePrevalence::generateTargetDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator",
  targetCohortTable = paste0(prefix_db,"target_cohorts"),
  targetCohortId = 3,
  cohortDateRange = c(as.Date("2012-01-01"), as.Date("2024-07-01"))
)

cdm$outcomecut <- cdm[[paste0(prefix_db,"outcome_cohorts")]] %>%
  dplyr::left_join(cdm$denominator %>%
                     dplyr::select("subject_id", 
                                   "target_start" = "cohort_start_date"),
                   by = "subject_id") %>%
  dplyr::filter(!(!is.na(target_start) & cohort_start_date < target_start)) %>%
  dplyr::select(-"target_start") %>% 
  dplyr::compute(name = "outcomecut", temporary = TRUE)


# Exclude people for 1 day after cohort entry
cdm[[c(paste0(prefix_db,"target_cohorts"))]] <- cdm[[c(paste0(prefix_db,"target_cohorts"))]] %>%
  dplyr::mutate(cohort_start_date = cohort_start_date + 1) %>%
  dplyr::compute()

cdm <- IncidencePrevalence::generateTargetDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator1",
  targetCohortTable = paste0(prefix_db,"target_cohorts"),
  targetCohortId = 3,
  cohortDateRange = c(as.Date("2012-01-01"), as.Date("2024-07-01"))
)

cdm$outcomecut1 <- cdm[[paste0(prefix_db,"outcome_cohorts")]] %>%
  dplyr::left_join(cdm$denominator1 %>%
                     dplyr::select("subject_id", 
                                   "target_start" = "cohort_start_date"),
                   by = "subject_id") %>%
  dplyr::filter(!(!is.na(target_start) & cohort_start_date < target_start)) %>%
  select(-"target_start") %>% 
  dplyr::compute(name = "outcomecut1", temporary = TRUE)

# Exclude people for 7 days after cohort entry
cdm[[c(paste0(prefix_db,"target_cohorts"))]] <- cdm[[c(paste0(prefix_db,"target_cohorts"))]] %>%
  dplyr::mutate(cohort_start_date = cohort_start_date + 6) %>%
  dplyr::compute()

cdm <- IncidencePrevalence::generateTargetDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator7",
  targetCohortTable = paste0(prefix_db,"target_cohorts"),
  targetCohortId = 3,
  cohortDateRange = c(as.Date("2012-01-01"), as.Date("2024-07-01"))
)

cdm$outcomecut7 <- cdm[[paste0(prefix_db,"outcome_cohorts")]] %>%
  dplyr::left_join(cdm$denominator7 %>%
                     dplyr::select("subject_id", 
                                   "target_start" = "cohort_start_date"),
                   by = "subject_id") %>%
  dplyr::filter(!(!is.na(target_start) & cohort_start_date < target_start)) %>%
  select(-"target_start") %>% 
  dplyr::compute(name = "outcomecut7", temporary = TRUE)

# Exclude people for 15 days after cohort entry
cdm[[c(paste0(prefix_db,"target_cohorts"))]] <- cdm[[c(paste0(prefix_db,"target_cohorts"))]] %>%
  dplyr::mutate(cohort_start_date = cohort_start_date + 8) %>%
  dplyr::compute()

cdm <- IncidencePrevalence::generateTargetDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator15",
  targetCohortTable = paste0(prefix_db,"target_cohorts"),
  targetCohortId = 3,
  cohortDateRange = c(as.Date("2012-01-01"), as.Date("2024-07-01"))
)

cdm$outcomecut15 <- cdm[[paste0(prefix_db,"outcome_cohorts")]] %>%
  dplyr::left_join(cdm$denominator15 %>%
                     dplyr::select("subject_id", 
                                   "target_start" = "cohort_start_date"),
                   by = "subject_id") %>%
  dplyr::filter(!(!is.na(target_start) & cohort_start_date < target_start)) %>%
  select(-"target_start") %>% 
  dplyr::compute(name = "outcomecut15", temporary = TRUE)

################################################################################
# Start incidence calculations

# First calculation: TAR inf
inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator", 
  outcomeTable = "outcomecut", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc3_tarinf.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc3_tarinf.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator1", 
  outcomeTable = "outcomecut1", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc3_tarinf1.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc3_tarinf1.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator7", 
  outcomeTable = "outcomecut7", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc3_tarinf7.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc3_tarinf7.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator15", 
  outcomeTable = "outcomecut15", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc3_tarinf15.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc3_tarinf15.csv")))


# TAR 2 years
cdm$denominator <- cdm$denominator %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 730, 
                                 cohort_start_date + 730,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator1 <- cdm$denominator1 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 730, 
                                 cohort_start_date + 730,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator7 <- cdm$denominator7 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 730, 
                                 cohort_start_date + 730,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator15 <- cdm$denominator15 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 730, 
                                 cohort_start_date + 730,
                                 cohort_end_date)) %>%
  dplyr::compute()

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator", 
  outcomeTable = "outcomecut", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc3_tar2y.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc3_tar2y.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator1", 
  outcomeTable = "outcomecut1", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc3_tar2y1.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc3_tar2y1.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator7", 
  outcomeTable = "outcomecut7", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc3_tar2y7.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc3_tar2y7.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator15", 
  outcomeTable = "outcomecut15", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc3_tar2y15.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc3_tar2y15.csv")))


# TAR 1 year
cdm$denominator <- cdm$denominator %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 365, 
                                 cohort_start_date + 365,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator1 <- cdm$denominator1 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 365, 
                                 cohort_start_date + 365,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator7 <- cdm$denominator7 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 365, 
                                 cohort_start_date + 365,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator15 <- cdm$denominator15 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 365, 
                                 cohort_start_date + 365,
                                 cohort_end_date)) %>%
  dplyr::compute()

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator", 
  outcomeTable = "outcomecut", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc3_tar1y.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc3_tar1y.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator1", 
  outcomeTable = "outcomecut1", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc3_tar1y1.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc3_tar1y1.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator7", 
  outcomeTable = "outcomecut7", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc3_tar1y7.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc3_tar1y7.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator15", 
  outcomeTable = "outcomecut15", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc3_tar1y15.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc3_tar1y15.csv")))


# TAR6 months
cdm$denominator <- cdm$denominator %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 180, 
                                 cohort_start_date + 180,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator1 <- cdm$denominator1 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 180, 
                                 cohort_start_date + 180,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator7 <- cdm$denominator7 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 180, 
                                 cohort_start_date + 180,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator15 <- cdm$denominator15 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 180, 
                                 cohort_start_date + 180,
                                 cohort_end_date)) %>%
  dplyr::compute()

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator", 
  outcomeTable = "outcomecut", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc3_tar6m.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc3_tar6m.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator1", 
  outcomeTable = "outcomecut1", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc3_tar6m1.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc3_tar6m1.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator7", 
  outcomeTable = "outcomecut7", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc3_tar6m7.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc3_tar6m7.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator15", 
  outcomeTable = "outcomecut15", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc3_tar6m15.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc3_tar6m15.csv")))


# TAR 3 months
cdm$denominator <- cdm$denominator %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 90, 
                                 cohort_start_date + 90,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator1 <- cdm$denominator1 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 90, 
                                 cohort_start_date + 90,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator7 <- cdm$denominator7 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 90, 
                                 cohort_start_date + 90,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator15 <- cdm$denominator15 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 90, 
                                 cohort_start_date + 90,
                                 cohort_end_date)) %>%
  dplyr::compute()

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator", 
  outcomeTable = "outcomecut", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc3_tar3m.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc3_tar3m.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator1", 
  outcomeTable = "outcomecut1", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc3_tar3m1.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc3_tar3m1.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator7", 
  outcomeTable = "outcomecut7", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc3_tar3m7.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc3_tar3m7.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator15", 
  outcomeTable = "outcomecut15", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc3_tar3m15.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc3_tar3m15.csv")))


# ----------------------------------------------------------------
# ----------------------------------------------------------------
# 1a: All outcomes on TC5
# ----------------------------------------------------------------
# ----------------------------------------------------------------

info(logger, '-- Calculating incidence of all outcomes on tia_sapt')

# Take the days back
cdm[[c(paste0(prefix_db,"target_cohorts"))]] <- cdm[[c(paste0(prefix_db,"target_cohorts"))]] %>%
  dplyr::mutate(cohort_start_date = cohort_start_date - 15) %>%
  dplyr::compute()

# Create denominator cohorts
# No exclusion
cdm <- IncidencePrevalence::generateTargetDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator",
  targetCohortTable = paste0(prefix_db,"target_cohorts"),
  targetCohortId = 4,
  cohortDateRange = c(as.Date("2012-01-01"), as.Date("2024-07-01"))
)

cdm$outcomecut <- cdm[[paste0(prefix_db,"outcome_cohorts")]] %>%
  dplyr::left_join(cdm$denominator %>%
                     dplyr::select("subject_id", 
                                   "target_start" = "cohort_start_date"),
                   by = "subject_id") %>%
  dplyr::filter(!(!is.na(target_start) & cohort_start_date < target_start)) %>%
  dplyr::select(-"target_start") %>% 
  dplyr::compute(name = "outcomecut", temporary = TRUE)


# Exclude people for 1 day after cohort entry
cdm[[c(paste0(prefix_db,"target_cohorts"))]] <- cdm[[c(paste0(prefix_db,"target_cohorts"))]] %>%
  dplyr::mutate(cohort_start_date = cohort_start_date + 1) %>%
  dplyr::compute()

cdm <- IncidencePrevalence::generateTargetDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator1",
  targetCohortTable = paste0(prefix_db,"target_cohorts"),
  targetCohortId = 4,
  cohortDateRange = c(as.Date("2012-01-01"), as.Date("2024-07-01"))
)

cdm$outcomecut1 <- cdm[[paste0(prefix_db,"outcome_cohorts")]] %>%
  dplyr::left_join(cdm$denominator1 %>%
                     dplyr::select("subject_id", 
                                   "target_start" = "cohort_start_date"),
                   by = "subject_id") %>%
  dplyr::filter(!(!is.na(target_start) & cohort_start_date < target_start)) %>%
  select(-"target_start") %>% 
  dplyr::compute(name = "outcomecut1", temporary = TRUE)

# Exclude people for 7 days after cohort entry
cdm[[c(paste0(prefix_db,"target_cohorts"))]] <- cdm[[c(paste0(prefix_db,"target_cohorts"))]] %>%
  dplyr::mutate(cohort_start_date = cohort_start_date + 6) %>%
  dplyr::compute()

cdm <- IncidencePrevalence::generateTargetDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator7",
  targetCohortTable = paste0(prefix_db,"target_cohorts"),
  targetCohortId = 4,
  cohortDateRange = c(as.Date("2012-01-01"), as.Date("2024-07-01"))
)

cdm$outcomecut7 <- cdm[[paste0(prefix_db,"outcome_cohorts")]] %>%
  dplyr::left_join(cdm$denominator7 %>%
                     dplyr::select("subject_id", 
                                   "target_start" = "cohort_start_date"),
                   by = "subject_id") %>%
  dplyr::filter(!(!is.na(target_start) & cohort_start_date < target_start)) %>%
  select(-"target_start") %>% 
  dplyr::compute(name = "outcomecut7", temporary = TRUE)

# Exclude people for 15 days after cohort entry
cdm[[c(paste0(prefix_db,"target_cohorts"))]] <- cdm[[c(paste0(prefix_db,"target_cohorts"))]] %>%
  dplyr::mutate(cohort_start_date = cohort_start_date + 8) %>%
  dplyr::compute()

cdm <- IncidencePrevalence::generateTargetDenominatorCohortSet(
  cdm =  cdm,
  name = "denominator15",
  targetCohortTable = paste0(prefix_db,"target_cohorts"),
  targetCohortId = 4,
  cohortDateRange = c(as.Date("2012-01-01"), as.Date("2024-07-01"))
)

cdm$outcomecut15 <- cdm[[paste0(prefix_db,"outcome_cohorts")]] %>%
  dplyr::left_join(cdm$denominator15 %>%
                     dplyr::select("subject_id", 
                                   "target_start" = "cohort_start_date"),
                   by = "subject_id") %>%
  dplyr::filter(!(!is.na(target_start) & cohort_start_date < target_start)) %>%
  select(-"target_start") %>% 
  dplyr::compute(name = "outcomecut15", temporary = TRUE)

################################################################################
# Start incidence calculations

# First calculation: TAR inf
inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator", 
  outcomeTable = "outcomecut", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc4_tarinf.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc4_tarinf.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator1", 
  outcomeTable = "outcomecut1", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc4_tarinf1.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc4_tarinf1.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator7", 
  outcomeTable = "outcomecut7", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc4_tarinf7.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc4_tarinf7.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator15", 
  outcomeTable = "outcomecut15", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc4_tarinf15.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc4_tarinf15.csv")))


# TAR 2 years
cdm$denominator <- cdm$denominator %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 730, 
                                 cohort_start_date + 730,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator1 <- cdm$denominator1 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 730, 
                                 cohort_start_date + 730,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator7 <- cdm$denominator7 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 730, 
                                 cohort_start_date + 730,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator15 <- cdm$denominator15 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 730, 
                                 cohort_start_date + 730,
                                 cohort_end_date)) %>%
  dplyr::compute()

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator", 
  outcomeTable = "outcomecut", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc4_tar2y.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc4_tar2y.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator1", 
  outcomeTable = "outcomecut1", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc4_tar2y1.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc4_tar2y1.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator7", 
  outcomeTable = "outcomecut7", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc4_tar2y7.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc4_tar2y7.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator15", 
  outcomeTable = "outcomecut15", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc4_tar2y15.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc4_tar2y15.csv")))


# TAR 1 year
cdm$denominator <- cdm$denominator %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 365, 
                                 cohort_start_date + 365,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator1 <- cdm$denominator1 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 365, 
                                 cohort_start_date + 365,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator7 <- cdm$denominator7 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 365, 
                                 cohort_start_date + 365,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator15 <- cdm$denominator15 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 365, 
                                 cohort_start_date + 365,
                                 cohort_end_date)) %>%
  dplyr::compute()

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator", 
  outcomeTable = "outcomecut", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc4_tar1y.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc4_tar1y.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator1", 
  outcomeTable = "outcomecut1", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc4_tar1y1.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc4_tar1y1.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator7", 
  outcomeTable = "outcomecut7", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc4_tar1y7.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc4_tar1y7.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator15", 
  outcomeTable = "outcomecut15", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc4_tar1y15.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc4_tar1y15.csv")))


# TAR6 months
cdm$denominator <- cdm$denominator %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 180, 
                                 cohort_start_date + 180,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator1 <- cdm$denominator1 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 180, 
                                 cohort_start_date + 180,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator7 <- cdm$denominator7 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 180, 
                                 cohort_start_date + 180,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator15 <- cdm$denominator15 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 180, 
                                 cohort_start_date + 180,
                                 cohort_end_date)) %>%
  dplyr::compute()

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator", 
  outcomeTable = "outcomecut", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc4_tar6m.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc4_tar6m.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator1", 
  outcomeTable = "outcomecut1", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc4_tar6m1.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc4_tar6m1.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator7", 
  outcomeTable = "outcomecut7", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc4_tar6m7.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc4_tar6m7.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator15", 
  outcomeTable = "outcomecut15", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc4_tar6m15.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc4_tar6m15.csv")))


# TAR 3 months
cdm$denominator <- cdm$denominator %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 90, 
                                 cohort_start_date + 90,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator1 <- cdm$denominator1 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 90, 
                                 cohort_start_date + 90,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator7 <- cdm$denominator7 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 90, 
                                 cohort_start_date + 90,
                                 cohort_end_date)) %>%
  dplyr::compute()

cdm$denominator15 <- cdm$denominator15 %>%
  dplyr::mutate(cohort_end_date = 
                  dplyr::if_else(as.numeric(difftime(cohort_start_date, cohort_end_date)) > 90, 
                                 cohort_start_date + 90,
                                 cohort_end_date)) %>%
  dplyr::compute()

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator", 
  outcomeTable = "outcomecut", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc4_tar3m.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc4_tar3m.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator1", 
  outcomeTable = "outcomecut1", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc4_tar3m1.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc4_tar3m1.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator7", 
  outcomeTable = "outcomecut7", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc4_tar3m7.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc4_tar3m7.csv")))

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm, 
  denominatorTable = "denominator15", 
  outcomeTable = "outcomecut15", 
  interval = "overall",
  completeDatabaseIntervals = FALSE, 
  minCellCount = 5)

write.csv(inc, file = here::here(output_inc, paste0("inc4_tar3m15.csv")))
write.csv(attr(inc, "attrition"), file = here::here(output_inc, paste0("att_inc4_tar3m15.csv")))
