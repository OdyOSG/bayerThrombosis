# A. File Info -----------------------

# Study:
# Task: Cohort Diagnostics


# B. Dependencies ----------------------

## Load libraries and scripts
library(tidyverse, quietly = TRUE)
library(DatabaseConnector)
source(here::here('analysis/private/_buildCohorts.R'))
source(here::here('analysis/private/_executeStudy.R'))
source(here::here('analysis/private/_utilities.R'))


# C. Connection ----------------------

## Set connection block
# <<<
configBlock <- "[block]"
# >>>

## Provide connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = Sys.getenv("dbms"),
  user = Sys.getenv("username"),
  password = Sys.getenv("password"),
  connectionString = Sys.getenv("connectionString")
)

## Connect to database
con <- DatabaseConnector::connect(connectionDetails)


# D. Variables -----------------------

## Administrative Variables
executionSettings <- list("projectName"         = "bayerThrombosis",
                          "databaseName"        = configBlock,
                          "cohortTable"         = paste0("thrombosis", "_", configBlock),
                          "cdmDatabaseSchema"   = Sys.getenv(configBlock),
                          "vocabDatabaseSchema" = Sys.getenv(configBlock),
                          "workDatabaseSchema"  = Sys.getenv("workDatabaseSchema"),
                          "dbms"                = Sys.getenv("dbms"),
                          "role"                = Sys.getenv("role"))

## Create output folder
outputFolder <- here::here("results") %>%
  fs::path(executionSettings$databaseName, "02_cohortDiagnostics") %>%
  fs::dir_create()

## Add study variables or load from settings
diagCohorts <- getCohortManifest() %>%
  dplyr::filter(type %in% c("target", "outcome"))


# E. Script --------------------

## Run cohort diagnostics

runCohortDiagnostics(executionSettings = executionSettings,
                     con = con,
                     cohortManifest = diagCohorts,
                     outputFolder = outputFolder)


# F. Disconnect ------------------------

DatabaseConnector::disconnect(con)
