# A. File Info -----------------------

# Task: Incidence
# Description: The purpose of the _incidence.R script is to provide functions for the incidence analysis portion of the study


# B. Functions ------------------------

# defineIncidenceAnalysis <- function(cohortId,
#                                     cohortName,
#                                     denomCohorts,
#                                     irSettings,
#                                     windowYear) {
# 
#   targets <- purrr::pmap(
#     denomCohorts,
#     ~CohortIncidence::createCohortRef(
#       id = ..2,
#       name = ..1
#     )
#   )
# 
#   o1 <- CohortIncidence::createOutcomeDef(
#     id = cohortId,
#     name = cohortName,
#     cohortId = cohortId,
#     cleanWindow = irSettings$cleanWindow
#   )
# 
#   timeMap <- tibble::tibble(
#     id = seq_along(irSettings$startOffset),
#     startDays = irSettings$startOffset,
#     endDays = irSettings$endOffset
#   )
# 
#   tars <- purrr::pmap(
#     timeMap,
#     ~CohortIncidence::createTimeAtRiskDef(
#       id = ..1,
#       startWith = irSettings$startWith,
#       startOffset = ..2,
#       endWith = irSettings$endsWith,
#       endOffset = ..3
#     )
#   )
# 
#   # Create all permutations of denominator pop and tar
#   analysisMap <- tidyr::expand_grid(
#     't' = purrr::map_int(targets, ~.x$id),  # this needs to be the cohort Id of the denom cohort to be used
#     'tar' = seq_along(tars)
#   )
# 
#   analysisList <- purrr::pmap(
#     analysisMap,
#     ~CohortIncidence::createIncidenceAnalysis(
#       targets = ..1,
#       outcomes = c(o1$id),
#       tars = ..2
#     )
#   )
# 
# 
#   #strataSettings <- CohortIncidence::createStrataSettings(byYear = TRUE, byAge = TRUE, ageBreaks = c(0,30,45,56))
#   strataSettings <- CohortIncidence::createStrataSettings(byYear = TRUE, byAge = FALSE)
# 
#   studyWindowStart <- paste(windowYear, "01", "01", sep = "-")
#   studyWindowEnd   <- paste(windowYear, "12", "31", sep = "-")
#   studyWindow      <- CohortIncidence::createDateRange(studyWindowStart, studyWindowEnd)
# 
#   irDesign <- CohortIncidence::createIncidenceDesign(
#     targetDefs = targets,
#     outcomeDefs = list(o1),
#     tars = tars,
#     analysisList = analysisList,
#     strataSettings = strataSettings,
#     studyWindow = studyWindow
#   )
# 
#   return(irDesign)
# }
# 
# 
# generateIncidenceAnalysis <- function(con,
#                                       executionSettings,
#                                       cohortId,
#                                       cohortName,
#                                       denomCohorts,
#                                       irSettings,
#                                       refId,
#                                       windowYear) {
# 
#   ## get schema vars
#   cdmDatabaseSchema <- executionSettings$cdmDatabaseSchema
#   workDatabaseSchema <- executionSettings$workDatabaseSchema
#   cohortTable <- paste(executionSettings$workDatabaseSchema, executionSettings$cohortTable, sep = ".")
#   databaseId <- executionSettings$databaseName
# 
#   outputFolder <- fs::path(here::here("results"), databaseId, analysisSettings[[1]]$outputFolder) %>%
#     fs::dir_create()
# 
#   timeMap <- tibble::tibble(
#     id = seq_along(irSettings$startOffset),
#     startDays = irSettings$startOffset,
#     endDays = irSettings$endOffset
#   )
# 
#   irDesign <- defineIncidenceAnalysis(cohortId = cohortId,
#                                       cohortName = cohortName,
#                                       denomCohorts = denomCohorts,
#                                       irSettings = irSettings,
#                                       windowYear = windowYear)
# 
# 
#   buildOptions <- CohortIncidence::buildOptions(
#     cohortTable = cohortTable,
#     cdmDatabaseSchema = cdmDatabaseSchema,
#     sourceName = databaseId,
#     resultsDatabaseSchema = workDatabaseSchema,
#     vocabularySchema = cdmDatabaseSchema,
#     useTempTables = FALSE,
#     refId = refId)
# 
#   ## IR SQL code
#   analysisSql <- CohortIncidence::buildQuery(incidenceDesign = as.character(irDesign$asJSON()),
#                                              buildOptions = buildOptions)
# 
#   analysisSql <- SqlRender::translate(analysisSql, targetDialect = "snowflake")
#   #cat(analysisSql)
# 
#   cli::cat_line()
#   cli::cat_bullet("Executing Incidence Analysis Id: ", crayon::green(refId),
#                   bullet = "checkbox_on", bullet_col = "green")
#   cli::cat_bullet("Denominator Population", bullet = "pointer", bullet_col = "yellow")
#   cli::cat_line("   ", crayon::yellow(cli::symbol$star), " Cohort Id: ", crayon::green(cohortId), "\n",
#                 "   ", crayon::yellow(cli::symbol$star)," Cohort Name: ", crayon::green(cohortName))
# 
#   cli::cat_bullet("Outcome Population", bullet = "pointer", bullet_col = "yellow")
#   print_outcomeIds <- paste(denomCohorts$id, collapse = ", ")
#   cli::cat_line("   ", crayon::yellow(cli::symbol$star), " Outcomes Id: ", crayon::green(print_outcomeIds))
#   cli::cat_bullet("Time at Risk", bullet = "pointer", bullet_col = "yellow")
#   print_tar <- paste(timeMap$startDays, timeMap$endDays, sep = "-")
#   cli::cat_line("   ", crayon::yellow(cli::symbol$star), " tar: ", crayon::green(print_tar))
# 
# 
#   executeResults <- CohortIncidence::executeAnalysis(
#     connection = con,
#     incidenceDesign = irDesign,
#     buildOptions = buildOptions)
# 
# 
#   verboseSave(
#     object = executeResults,
#     saveName = paste("incidence_analysis_ref", refId, windowYear, sep = "_"),
#     saveLocation = outputFolder
#   )
# 
#   invisible(executeResults)
# }


executeIncidenceAnalysis <- function(cdm,
                                     executionSettings,
                                     analysisSettings) {

  ## Load cohort names and incidence analysis settings
  numeratorCohort <- analysisSettings$incidenceAnalysis$cohorts$numeratorCohorts
  denomCohort <- analysisSettings$incidenceAnalysis$cohorts$denominatorCohorts
  irSettings <- analysisSettings$incidenceAnalysis$incidenceSettings

  outputFolder <- fs::path(here::here("results"), executionSettings$databaseName, analysisSettings[["incidenceAnalysis"]][["outputFolder"]]) %>%
    fs::dir_create()

  ## Start timer
  tik <- Sys.time()

  ## Job Log
  cli::cat_boxx("Building Incidence Analysis")
  cli::cat_line()


  ## Build numerator cohort(s)
  cohortSet1 <- readCohortSet(path = here::here("cohortsToCreate", "03_outcome"))
  cdm <- generateCohortSet(cdm, cohortSet1, name = "numerator") 

  ## Build denominator cohort(s)
  cohortSet2 <- readCohortSet(path = here::here("cohortsToCreate", "01_target"))
  cdm <- generateCohortSet(cdm, cohortSet2, name = "denominator") 

  ## Stratify denominator cohort by sex
  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm,
    name = tolower(denomCohort$name),   # Name of table with denominator data
    targetCohortTable = "denominator",  # Name of table with stratified denominator cohorts e.g. age groups
    sex = irSettings$sex,
    requirementInteractions = TRUE
  )

  ## Calculate incidence
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = tolower(denomCohort$name),  # Should be the same variable as the 'name' argument in 'generateTargetDenominatorCohortSet'
    outcomeTable = "numerator",
    interval = irSettings$interval,
    completeDatabaseIntervals = irSettings$completeDatabaseIntervals,
    includeOverallStrata = TRUE,        # Default value
    repeatedEvents = FALSE              # Default value
  )

  ## Save results
  verboseSave(
    object = inc,
    saveName = paste0("incidence"),
    saveLocation = outputFolder
  )

  ## End timer
  tok <- Sys.time()
  tdif <- tok - tik
  
  ## Job Log
  jobFinishedExecution(tok = tok, tdif = tdif) 
  

  invisible(denomCohort)
}


## Connect to the server with the CDMConnector package (Snowflake, PostgreSQL, Redshift)
cdmFromConAllDbs <- function(executionSettings) {

  ## Set variables
  dbName <- strsplit(executionSettings$cdmDatabaseSchema, split = ".", fixed = TRUE)[[1]][1]
  schemaName <- strsplit(executionSettings$cdmDatabaseSchema, split = ".", fixed = TRUE)[[1]][2]
  writeDbName <- strsplit(executionSettings$workDatabaseSchema, split = ".", fixed = TRUE)[[1]][1]
  writeSchemaName <- strsplit(executionSettings$workDatabaseSchema, split = ".", fixed = TRUE)[[1]][2]
  host <- strsplit(executionSettings$connectionString, split = "/", fixed = TRUE)[[1]][1]

  ## Remove double quotes from string variables
  dbName <- str_remove_all(dbName, "\"")
  writeDbName <- str_remove_all(writeDbName, "\"")


  ## Snowflake
  if (executionSettings$dbms == "snowflake") {

    ## Connect to server
    con <- DBI::dbConnect(
      odbc::odbc(),
      dsn = executionSettings$dbms,
      database = dbName,
      schema = schemaName,
      uid = executionSettings$user,
      role = executionSettings$role,
      pwd = executionSettings$password
    )

    ## Set the DATE_INPUT_FORMAT session parameter
    DBI::dbExecute(con, "ALTER SESSION SET DATE_INPUT_FORMAT = 'YYYY-MM-DD'")
    DBI::dbExecute(con, "ALTER SESSION SET JDBC_QUERY_RESULT_FORMAT='JSON'")

    ## Connect to database
    cdm <- cdm_from_con(
      con = con,
      cdm_schema = c(catalog = dbName, schema = schemaName),
      write_schema = c(catalog = writeDbName, schema = writeSchemaName),
      cdm_name = dbName,
      .soft_validation = TRUE
    )

  }


  ## PostgreSql
  if (executionSettings$dbms == "postgresql") {

    ## Connect to server
    con <- DBI::dbConnect(
      drv = RPostgres::Postgres(),
      host = executionSettings$server,
      port = executionSettings$port,
      dbname = dbName,
      user = executionSettings$user,
      password = executionSettings$password
    )

    ## Connect to database
    cdm <- cdm_from_con(
      con = con,
      cdm_schema = schemaName,
      write_schema = writeSchemaName
    )

  }

  ## Redshift
  if (executionSettings$dbms == "redshift") {

    ## Connect to server
    con <- DBI::dbConnect(
      drv = RPostgres::Redshift(),
      host = executionSettings$server,
      port = executionSettings$port,
      dbname = dbName,
      user = executionSettings$user,
      password = executionSettings$password
    )

    ## Connect to database
    cdm <- cdm_from_con(
      con = con,
      cdm_schema = schemaName,
      write_schema = writeSchemaName
    )

  }

  conCdm <- list(cdm = cdm,
                 con = con)

  return(conCdm)
}
