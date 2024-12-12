# Instantiate initial cohorts

info(logger, "- getting initial cohort definitions")

targetCohorts <- CDMConnector::readCohortSet(
    here::here("1_InitialCohorts","targetCohorts")) 

outcomeCohorts <- CDMConnector::readCohortSet(
  here::here("1_InitialCohorts","outcomeCohorts")) 

info(logger, "- getting target cohorts")

cdm <- generateCohortSet_here(cdm, targetCohorts,
                                       name = paste0(prefix_db,"target_cohorts"),
                                       overwrite = TRUE)

cdm <- generateCohortSet_here(cdm, outcomeCohorts,
                                       name = paste0(prefix_db,"outcome_cohorts"),
                                       overwrite = TRUE)

info(logger, "- got initial cohorts")
