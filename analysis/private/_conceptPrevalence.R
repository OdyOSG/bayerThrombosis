# A. File Info -----------------------

# Task: Concept Prevalence


# B. Functions -----------------

## Helper functions -----------------------

# Function that runs function FeatureExtraction::getDbCovariateData without console messages
silentCovariates <- function(con, cdmDatabaseSchema, cohortTable, cohortDatabaseSchema, cohortId, covSettings) {

  # Start timer
  tik <- Sys.time()
  
  #Job log
  cli::cat_bullet("Getting Covariates from database...", bullet = "info", bullet_col = "blue")


  # Get covariate data
  quietCov <- purrr::quietly(FeatureExtraction::getDbCovariateData)

  cov <- quietCov(
    connection = con,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTable = cohortTable,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortId = cohortId,
    covariateSettings = covSettings,
    aggregated = TRUE
  )$result

  # End timer
  tok <- Sys.time()
  tdif <- tok - tik
  tok_format <- paste(scales::label_number(0.01)(as.numeric(tdif)), attr(tdif, "units"))
  
  # Job log
  cli::cat_bullet("Covariates built at: ", crayon::red(tok), bullet = "info", bullet_col = "blue")
  cli::cat_bullet("Covariate build took: ", crayon::red(tok_format), bullet = "info", bullet_col = "blue")

  return(cov)
}


## Domain FE functions -------------------------

# Function that runs function FeatureExtraction::createCovariateSettings for drug eras (exlc. ATC 1st level classes)
getDrugsFE <- function(con,
                       cohortDatabaseSchema,
                       cohortTable,
                       cdmDatabaseSchema,
                       cohortId,
                       type = c("postIndex", "baseline"),
                       timeA,
                       timeB,
                       outputFolder) {

  cli::cat_rule("Build Drug Covariates")

  # Create Drug settings
  covSettings <- FeatureExtraction::createCovariateSettings(
    useDrugGroupEraLongTerm = TRUE,
    excludedCovariateConceptIds = c(21600001, 21600959, 21601237, # Remove ATC 1st class
                                    21601907, 21602359, 21602681,
                                    21602795, 21601386, 21603931,
                                    21604180, 21604847, 21605007,
                                    21603550, 21605212),
    longTermStartDays = timeA,
    endDays = timeB
  )

  # Run FE
  cov <- silentCovariates(con = con,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = cohortTable,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortId = cohortId,
                          covSettings = covSettings)

  # If the cov$covariates object is empty, skip export and continue with the next cohort
  # If TRUE, then it is most likely that the cohort has no counts (Check files strataCounts.csv and cohortManifest.csv)
  if (cov$covariates %>% tally() %>% pull() == 0) {

    cli::cat_bullet(crayon::red("No data returned."), bullet = "info", bullet_col = "blue")
    cli::cat_bullet("Please check files strataCounts.csv and cohortManifest.csv to see if the cohort (id: ",
                    crayon::red(as.character(cohortId)), ") has any counts.", bullet = "info", bullet_col = "blue")
    cli::cat_line()

    return(NA)
  }

  # Format
  drugTbl <- cov$covariates %>%
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) %>%
    dplyr::rename(
      n = sumValue,
      pct = averageValue,
      name = covariateName
    ) %>%
    dplyr::select(cohortDefinitionId, analysisId, conceptId, name, n, pct) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      name = gsub(".*: ", "", name),
      timeWindow = paste0(abs(timeA), "_", abs(timeB)),
      database = executionSettings$databaseName
      )

  # Output file name
  saveName <- paste0("drugs_", type, "_", cohortId, "_", abs(timeA), "_", abs(timeB))

  # Export
  verboseSave(
    object = drugTbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(drugTbl)
}


# Function that runs function FeatureExtraction::createCovariateSettings for condition eras
getConditionsFE <- function(con,
                            cohortDatabaseSchema,
                            cohortTable,
                            cdmDatabaseSchema,
                            cohortId,
                            type = c("postIndex", "baseline"),
                            timeA,
                            timeB,
                            outputFolder) {

  cli::cat_rule("Build Condition Covariates")

  # Create Condition settings
  covSettings <- FeatureExtraction::createCovariateSettings(
    useConditionGroupEraLongTerm = TRUE,
    longTermStartDays = timeA,
    endDays = timeB
  )

  # Run FE
  cov <- silentCovariates(con = con,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = cohortTable,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortId = cohortId,
                          covSettings = covSettings)

  # If the cov$covariates object is empty, skip export and continue with the next cohort
  # If TRUE, then it is most likely that the cohort has no counts (Check files strataCounts.csv and cohortManifest.csv)
  if (cov$covariates %>% tally() %>% pull() == 0) {

    cli::cat_bullet(crayon::red("No data returned."), bullet = "info", bullet_col = "blue")
    cli::cat_bullet("Please check files strataCounts.csv and cohortManifest.csv to see if the cohort (id: ",
                    crayon::red(as.character(cohortId)), ") has any counts.", bullet = "info", bullet_col = "blue")
    cli::cat_line()

    return(NA)
  }

  # Format
  condTbl <- cov$covariates %>%
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) %>%
    dplyr::rename(
      n = sumValue,
      pct = averageValue,
      name = covariateName
    ) %>%
    dplyr::select(cohortDefinitionId, analysisId, conceptId, name, n, pct) %>%
    dplyr::collect() %>%
    dplyr::mutate(name = gsub(".*: ", "", name),
                  timeWindow = paste0(abs(timeA), "_", abs(timeB)),
                  database = executionSettings$databaseName)

  # Output file name
  saveName <- paste0("conditions_", type, "_", cohortId, "_", abs(timeA), "_", abs(timeB))

  # Export
  verboseSave(
    object = condTbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(condTbl)
}


# Function that runs function FeatureExtraction::createCovariateSettings for procedure occurrences
getProceduresFE <- function(con,
                            cohortDatabaseSchema,
                            cohortTable,
                            cdmDatabaseSchema,
                            cohortId,
                            type = c("postIndex", "baseline"),
                            timeA,
                            timeB,
                            outputFolder) {

  cli::cat_rule("Build Procedure Covariates")

  # Create Procedure settings
  covSettings <- FeatureExtraction::createCovariateSettings(
    useProcedureOccurrenceLongTerm = TRUE,
    longTermStartDays = timeA,
    endDays = timeB
  )

  # Run FE
  cov <- silentCovariates(con = con,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = cohortTable,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortId = cohortId,
                          covSettings = covSettings)

  # If the cov$covariates object is empty, skip export and continue with the next cohort
  # If TRUE, then it is most likely that the cohort has no counts (Check files strataCounts.csv and cohortManifest.csv)
  if (cov$covariates %>% tally() %>% pull() == 0) {

    cli::cat_bullet(crayon::red("No data returned."), bullet = "info", bullet_col = "blue")
    cli::cat_bullet("Please check files strataCounts.csv and cohortManifest.csv to see if the cohort (id: ",
                    crayon::red(as.character(cohortId)), ") has any counts.", bullet = "info", bullet_col = "blue")
    cli::cat_line()

    return(NA)
  }

  # Format
  procTbl <- cov$covariates %>%
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) %>%
    dplyr::rename(
      n = sumValue,
      pct = averageValue,
      name = covariateName
    ) %>%
    dplyr::select(cohortDefinitionId, analysisId, conceptId, name, n, pct) %>%
    dplyr::collect() %>%
    dplyr::mutate(name = gsub(".*: ", "", name),
                  timeWindow = paste0(abs(timeA), "_", abs(timeB)),
                  database = executionSettings$databaseName)

  # Output file name
  saveName <- paste0("procedures_", type, "_", cohortId, "_", abs(timeA), "_", abs(timeB))

  # Export
  verboseSave(
    object = procTbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(procTbl)
}


# Function that runs function FeatureExtraction::createCovariateSettings for visit occurrences (Overall visit & by visit concept count)
getVisitsFE <- function(con,
                            cohortDatabaseSchema,
                            cohortTable,
                            cdmDatabaseSchema,
                            cohortId,
                            type = c("postIndex", "baseline"),
                            timeA,
                            timeB,
                            outputFolder) {

  cli::cat_rule("Build Visit Covariates")

  # Create Visit settings
  covSettings <- FeatureExtraction::createCovariateSettings(
    useVisitCountLongTerm = TRUE,
    useVisitConceptCountLongTerm = TRUE,
    longTermStartDays = timeA,
    endDays = timeB
  )

  # Run FE
  cov <- silentCovariates(con = con,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = cohortTable,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortId = cohortId,
                          covSettings = covSettings)

  # If the cov$covariates object is empty, skip export and continue with the next cohort
  # If TRUE, then it is most likely that the cohort has no counts (Check files strataCounts.csv and cohortManifest.csv)
  if (cov$covariates %>% tally() %>% pull() == 0) {

    cli::cat_bullet(crayon::red("No data returned."), bullet = "info", bullet_col = "blue")
    cli::cat_bullet("Please check files strataCounts.csv and cohortManifest.csv to see if the cohort (id: ",
                    crayon::red(as.character(cohortId)), ") has any counts.", bullet = "info", bullet_col = "blue")
    cli::cat_line()

    return(NA)
  }

  # Format
  visitTbl <- cov$covariatesContinuous %>%
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) %>%
    dplyr::rename(
      name = covariateName,
      n = countValue,
      max = maxValue,
      min = minValue,
      mean = averageValue,
      median = medianValue,
      p10 = p10Value,
      p25 = p25Value,
      p75 = p75Value,
      p90 = p90Value,
      sd = standardDeviation
    ) %>%
    dplyr::select(-covariateId) %>%
    dplyr::collect() %>%
    dplyr::mutate(name = gsub(".*: ", "", name),
                  timeWindow = paste0(abs(timeA), "_", abs(timeB)),
                  database = executionSettings$databaseName)


  # Output file name
  saveName <- paste0("visits_", type, "_", cohortId, "_", abs(timeA), "_", abs(timeB))

  # Export
  verboseSave(
    object = visitTbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(visitTbl)
}


# Function that runs function FeatureExtraction::createCovariateSettings for observations
getObservationsFE <- function(con,
                              cohortDatabaseSchema,
                              cohortTable,
                              cdmDatabaseSchema,
                              cohortId,
                              type = c("postIndex", "baseline"),
                              timeA,
                              timeB,
                              outputFolder) {

  cli::cat_rule("Build Observation Covariates")

  # Create Observation settings
  covSettings <- FeatureExtraction::createCovariateSettings(
    useObservationLongTerm = TRUE,
    longTermStartDays = timeA,
    endDays = timeB
  )

  # Run FE
  cov <- silentCovariates(con = con,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = cohortTable,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortId = cohortId,
                          covSettings = covSettings)

  # If the cov$covariates object is empty, skip export and continue with the next cohort
  # If TRUE, then it is most likely that the cohort has no counts (Check files strataCounts.csv and cohortManifest.csv)
  if (cov$covariates %>% tally() %>% pull() == 0) {

    cli::cat_bullet(crayon::red("No data returned."), bullet = "info", bullet_col = "blue")
    cli::cat_bullet("Please check files strataCounts.csv and cohortManifest.csv to see if the cohort (id: ",
                    crayon::red(as.character(cohortId)), ") has any counts.", bullet = "info", bullet_col = "blue")
    cli::cat_line()

    return(NA)
  }

  # Format
  obsTbl <- cov$covariates %>%
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) %>%
    dplyr::rename(
      n = sumValue,
      pct = averageValue,
      name = covariateName
    ) %>%
    dplyr::select(cohortDefinitionId, analysisId, conceptId, name, n, pct) %>%
    dplyr::collect() %>%
    dplyr::mutate(name = gsub(".*: ", "", name),
                  timeWindow = paste0(abs(timeA), "_", abs(timeB)),
                  database = executionSettings$databaseName)


  # Output file name
  saveName <- paste0("observations_", type, "_", cohortId, "_", abs(timeA), "_", abs(timeB))

  # Export
  verboseSave(
    object = obsTbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(obsTbl)
}


# Function that runs function FeatureExtraction::createCovariateSettings for cohorts already created while running `01_buildCohorts`
getCohortFE <- function(con,
                        cohortDatabaseSchema,
                        cohortTable,
                        cdmDatabaseSchema,
                        cohortId,
                        analysisSettings,
                        covId,
                        type = c("postIndex", "baseline"),
                        timeA,
                        timeB,
                        outputFolder) {

  cli::cat_rule("Build Cohort Covariates")

  # Create covariate tibble for FE
  covariateCohorts <- as_tibble(analysisSettings[[1]]$cohorts$covariateCohorts)

  covariateCohorts <- covariateCohorts %>%
    dplyr::filter(id == covId) %>%
    dplyr::rename(
      cohortId = id,
      cohortName = name
    )

  # Create cohort settings
  covSettings <- FeatureExtraction::createCohortBasedCovariateSettings(
    analysisId = 999L,
    covariateCohortDatabaseSchema = cohortDatabaseSchema,
    covariateCohortTable = cohortTable,
    covariateCohorts = covariateCohorts,
    valueType = "binary",
    startDay = timeA,
    endDay = timeB
  )

  # Run FE
  cov <- silentCovariates(
    con = con,
    cdmDatabaseSchema = cdmDatabaseSchema,
    cohortTable = cohortTable,
    cohortDatabaseSchema = cohortDatabaseSchema,
    cohortId = cohortId,
    covSettings = covSettings
    )

  # If the cov$covariates object is empty, skip export and continue with the next cohort
  # If TRUE, then it is most likely that the cohort has no counts (Check files strataCounts.csv and cohortManifest.csv)
  if (cov$covariates %>% tally() %>% pull() == 0) {

    cli::cat_bullet(crayon::red("No data returned."), bullet = "info", bullet_col = "blue")
    cli::cat_bullet("Please check files strataCounts.csv and cohortManifest.csv to see if the cohort (id: ",
                    crayon::red(as.character(cohortId)), ") has any counts.", bullet = "info", bullet_col = "blue")
    cli::cat_line()

    return(NA)
  }

  # Format
  cohortTbl <- cov$covariates %>%
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) %>%
    dplyr::rename(
      n = sumValue,
      pct = averageValue,
      name = covariateName
    ) %>%
    dplyr::collect() %>%
    dplyr::mutate(
      name = gsub(".*: ", "", name),
      timeWindow = paste0(abs(timeA), "_", abs(timeB)),
      database = executionSettings$databaseName,
      cohortDefinitionId = as.integer(cohortDefinitionId),
      n = as.integer(n),
      pct = as.double(pct),
      analysisId = as.integer(analysisId),
      conceptId = as.integer(conceptId),
      covId = as.integer(covId)
                  ) %>%
    dplyr::select(-covariateId)

  # Output file name
  saveName <- paste0("cohort_", type, "_", cohortId, "_", abs(timeA), "_", abs(timeB), "_", covId)

  # Export
  verboseSave(
    object = cohortTbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(cohortTbl)
}


# Function that runs function FeatureExtraction::createCovariateSettings for demographics (race, gender, ethnicity, index year, age group)
getDemographicsFE <- function(con,
                              cohortDatabaseSchema,
                              cohortTable,
                              cdmDatabaseSchema,
                              cohortId,
                              outputFolder) {

  cli::cat_rule("Build Demographic Covariates")

  # Create Demographic settings
  covSettings <- FeatureExtraction::createCovariateSettings(
    useDemographicsGender = TRUE,
    useDemographicsAgeGroup = TRUE,
    useDemographicsRace = TRUE,
    useDemographicsEthnicity = TRUE,
    useDemographicsIndexYear = TRUE
  )

  # Run FE
  cov <- silentCovariates(con = con,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = cohortTable,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortId = cohortId,
                          covSettings = covSettings)

  # If the cov$covariates object is empty, skip export and continue with the next cohort
  # If TRUE, then it is most likely that the cohort has no counts (Check files strataCounts.csv and cohortManifest.csv)
  if (cov$covariates %>% tally() %>% pull() == 0) {

    cli::cat_bullet(crayon::red("No data returned."), bullet = "info", bullet_col = "blue")
    cli::cat_bullet("Please check files strataCounts.csv and cohortManifest.csv to see if the cohort (id: ",
                    crayon::red(as.character(cohortId)), ") has any counts.", bullet = "info", bullet_col = "blue")
    cli::cat_line()

    return(NA)
  }

  # Format
  demoTbl <- cov$covariates %>%
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) %>%
    dplyr::rename(
      n = sumValue,
      pct = averageValue,
      name = covariateName
    ) %>%
    dplyr::collect() %>%
    dplyr::select(-covariateId) %>%
    dplyr::mutate(database = executionSettings$databaseName)

  # Output file name
  saveName <- paste0("demographics_baseline_", cohortId)

  # Export
  verboseSave(
    object = demoTbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(demoTbl)

}


# Function that runs function FeatureExtraction::createCovariateSettings for continuous variables (age, Charlson index, time in cohort)
getContinuousFE <- function(con,
                            cohortDatabaseSchema,
                            cohortTable,
                            cdmDatabaseSchema,
                            cohortId,
                            outputFolder) {

  cli::cat_rule("Build Continuous Covariates")

  # Create Continuous settings
  covSettings <- FeatureExtraction::createCovariateSettings(
    useDemographicsAge = TRUE,
    useCharlsonIndex = TRUE,
    useDcsi = TRUE,
    useChads2 = TRUE,
    useChads2Vasc = TRUE,
    useDemographicsPriorObservationTime = TRUE,
    useDemographicsPostObservationTime = TRUE,
    useDemographicsTimeInCohort = TRUE
  )

  # Run FE
  cov <- silentCovariates(con = con,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = cohortTable,
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortId = cohortId,
                          covSettings = covSettings)

  # If the cov$covariatesContinuous object is empty, skip export and continue with the next cohort
  # If TRUE, then it is most likely that the cohort has no counts (Check files strataCounts.csv and cohortManifest.csv)
  if (purrr::is_null(cov$covariatesContinuous)) {

    cli::cat_bullet(crayon::red("No data returned."), bullet = "info", bullet_col = "blue")
    cli::cat_bullet("Please check files strataCounts.csv and cohortManifest.csv to see if the cohort (id: ",
                    crayon::red(as.character(cohortId)), ") has any counts.", bullet = "info", bullet_col = "blue")
    cli::cat_line()

    return(NA)
  }

  # Format
  ctsTbl <- cov$covariatesContinuous %>%
    dplyr::left_join(cov$covariateRef, by = c("covariateId")) %>%
    dplyr::rename(
      name = covariateName,
      n = countValue,
      max = maxValue,
      min = minValue,
      mean = averageValue,
      median = medianValue,
      p10 = p10Value,
      p25 = p25Value,
      p75 = p75Value,
      p90 = p90Value,
      sd = standardDeviation
    ) %>%
    dplyr::select(-covariateId) %>%
    dplyr::collect() %>%
    dplyr::mutate(name = gsub(".*: ", "", name),
                  database = executionSettings$databaseName)


  # Output file name
  saveName <- paste0("continuous_baseline_", cohortId)

  # Export
  verboseSave(
    object = ctsTbl,
    saveName = saveName,
    saveLocation = outputFolder
  )

  invisible(ctsTbl)
}



## Main function  -----------------

executeConceptCharacterization <- function(con,
                                           type,
                                           runDrugs = FALSE,
                                           runConditions = FALSE,
                                           runVisits = FALSE,
                                           runDemographics = FALSE,
                                           runContinuous = FALSE,
                                           runProcedures = FALSE,
                                           runObservations = FALSE,
                                           runCohorts = FALSE,
                                           executionSettings,
                                           analysisSettings) {
  
  # Instantiate user's role (for Snowflake users only)
  startSnowflakeSession(con = con, executionSettings = executionSettings)
  
  # Start timer
  tik <- Sys.time()

  # Get variables
  cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
  workDatabaseSchema <- executionSettings$workDatabaseSchema
  cohortTable <- executionSettings$cohortTable
  databaseId <- executionSettings$databaseName

  outputFolder <- fs::path(here::here("results"), databaseId, analysisSettings[[1]]$outputFolder) %>%
    fs::dir_create()

  # Get target and covariate cohort ids
  cohortKey <- analysisSettings[[1]]$cohorts$targetCohorts %>% dplyr::arrange(id)
  covariateKey <- analysisSettings[[1]]$cohorts$covariateCohorts %>% dplyr::arrange(id)

  # Get time windows
  timeA <- analysisSettings[[1]]$timeWindows$startDay
  timeB <- analysisSettings[[1]]$timeWindows$endDay
  
  # Job Log: Start characterization
  typeOfAnalysis <- jobTypeOfAnalysis(type = type)
  jobStartCharacterization(typeOfAnalysis = typeOfAnalysis, cohortKey = cohortKey, timeA = timeA, timeB = timeB) 


  if (type == "baseline") {

      ## Demographics ------
      if (runDemographics == TRUE) {

        # Calculate for each cohort
        purrr::pwalk(cohortKey[,2],
                        ~ getDemographicsFE(con = con,
                                            cdmDatabaseSchema = cdmDatabaseSchema,
                                            cohortTable = cohortTable,
                                            cohortDatabaseSchema = workDatabaseSchema,
                                            cohortId = ..1,
                                            outputFolder = outputFolder)
        )

        # Bind and save files
        bindFiles(
          inputPath = outputFolder,
          outputPath = outputFolder,
          filename = "demoBase",
          pattern = "demographics_baseline"
        )

     }

    ## Continuous ------
    if (runContinuous == TRUE) {

      # Calculate for each cohort
      purrr::pwalk(cohortKey[,2],
                      ~ getContinuousFE(con = con,
                                        cdmDatabaseSchema = cdmDatabaseSchema,
                                        cohortTable = cohortTable,
                                        cohortDatabaseSchema = workDatabaseSchema,
                                        cohortId = ..1,
                                        outputFolder = outputFolder)
      )

      # Bind and save files
      bindFiles(
        inputPath = outputFolder,
        outputPath = outputFolder,
        filename = "contBase",
        pattern = "continuous_baseline"
      )

    }
  }


  ## Drugs ------
  if (runDrugs == TRUE) {

    # Create grid data frame for execution
    grid <- createGrid(
      cohortKey = cohortKey,
      timeA = timeA,
      timeB = timeB
    )

    # Calculate for each cohort and time window
    purrr::pwalk(grid,
                    ~ getDrugsFE(con = con,
                                 cdmDatabaseSchema = cdmDatabaseSchema,
                                 cohortTable = cohortTable,
                                 cohortDatabaseSchema = workDatabaseSchema,
                                 type = type,
                                 cohortId = ..1,
                                 timeA = ..3,
                                 timeB = ..4,
                                 outputFolder = outputFolder)
    )

    # Bind and save files
    bindFiles(
      inputPath = outputFolder,
      outputPath = outputFolder,
      filename = paste0("drugs", typeOfAnalysis$shortName),
      pattern = paste0("drugs_", typeOfAnalysis$type)
    )

  }

  ## Conditions ------
  if (runConditions == TRUE) {

    # Create grid data frame for execution
    grid <- createGrid(
      cohortKey = cohortKey,
      timeA = timeA,
      timeB = timeB
    )

    # Calculate for each cohort and time window
    purrr::pwalk(grid,
                    ~ getConditionsFE(con = con,
                                      cdmDatabaseSchema = cdmDatabaseSchema,
                                      cohortTable = cohortTable,
                                      cohortDatabaseSchema = workDatabaseSchema,
                                      type = type,
                                      cohortId = ..1,
                                      timeA = ..3,
                                      timeB = ..4,
                                      outputFolder = outputFolder)
    )

    # Bind and save files
    bindFiles(
      inputPath = outputFolder,
      outputPath = outputFolder,
      filename = paste0("cond", typeOfAnalysis$shortName),
      pattern = paste0("conditions_", typeOfAnalysis$type)
    )

  }

  ## Visits ------
  if (runVisits == TRUE) {

    # Create grid data frame for execution
    grid <- createGrid(
      cohortKey = cohortKey,
      timeA = timeA,
      timeB = timeB
    )

    # Calculate for each cohort and time window
    purrr::pwalk(grid,
                    ~ getVisitsFE(con = con,
                                  cdmDatabaseSchema = cdmDatabaseSchema,
                                  cohortTable = cohortTable,
                                  cohortDatabaseSchema = workDatabaseSchema,
                                  type = type,
                                  cohortId = ..1,
                                  timeA = ..3,
                                  timeB = ..4,
                                  outputFolder = outputFolder)
    )

    # Bind and save files
    bindFiles(
      inputPath = outputFolder,
      outputPath = outputFolder,
      filename = paste0("visit", typeOfAnalysis$shortName),
      pattern = paste0("visits_", typeOfAnalysis$type)
    )

  }

  ## Procedures ------
  if (runProcedures == TRUE) {

    # Calculate for each cohort and time window
    purrr::pwalk(grid,
                    ~ getProceduresFE(con = con,
                                      cdmDatabaseSchema = cdmDatabaseSchema,
                                      cohortTable = cohortTable,
                                      cohortDatabaseSchema = workDatabaseSchema,
                                      type = type,
                                      cohortId = ..1,
                                      timeA = ..3,
                                      timeB = ..4,
                                      outputFolder = outputFolder)
    )

    # Bind and save files
    bindFiles(
      inputPath = outputFolder,
      outputPath = outputFolder,
      filename = paste0("proc", typeOfAnalysis$shortName),
      pattern = paste0("procedures_", typeOfAnalysis$type)
    )

  }

  ## Observations ------
  if (runObservations == TRUE) {

    # Create grid data frame for execution
    grid <- createGrid(
      cohortKey = cohortKey,
      timeA = timeA,
      timeB = timeB
    )

    # Calculate for each cohort and time window
    purrr::pwalk(grid,
                    ~ getObservationsFE(con = con,
                                        cdmDatabaseSchema = cdmDatabaseSchema,
                                        cohortTable = cohortTable,
                                        cohortDatabaseSchema = workDatabaseSchema,
                                        type = type,
                                        cohortId = ..1,
                                        timeA = ..3,
                                        timeB = ..4,
                                        outputFolder = outputFolder)
    )

    # Bind and save files
    bindFiles(
      inputPath = outputFolder,
      outputPath = outputFolder,
      filename = paste0("obs", typeOfAnalysis$shortName),
      pattern = paste0("observations", typeOfAnalysis$type)
    )

  }


  ## Cohorts ------
  if (runCohorts == TRUE) {

    # Create grid df for execution
    cohortGrid <- createGrid2(
      cohortKey = cohortKey,
      covariateKey = covariateKey,
      timeA = timeA,
      timeB = timeB
    )

    # Calculate for each cohort, covariate and time window
    purrr::pwalk(cohortGrid,
                    ~ getCohortFE(con = con,
                                  cdmDatabaseSchema = cdmDatabaseSchema,
                                  cohortTable = cohortTable,
                                  cohortDatabaseSchema = workDatabaseSchema,
                                  analysisSettings = analysisSettings,
                                  type = type,
                                  covId = ..4,
                                  cohortId = ..2,
                                  timeA = ..5,
                                  timeB = ..6,
                                  outputFolder = outputFolder)
    )

    # Bind and save files
    bindFiles(
      inputPath = outputFolder,
      outputPath = outputFolder,
      filename = paste0("cohort", typeOfAnalysis$shortName, abs(timeA)),
      pattern = paste0("cohort_", typeOfAnalysis$type)
    )

  }

  
  # End timer
  tok <- Sys.time()
  tdif <- tok - tik  # Elapsed time
  
  # Job Log: Finished execution
  jobFinishedExecution(tok = tok, tdif = tdif)

  invisible(tok)
}
