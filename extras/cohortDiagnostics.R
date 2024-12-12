# A. File info -------------

# Task: Review CohortDiagnostics


# B. Dependencies ----------------------

## Load libraries and scripts
library(tidyverse, quietly = TRUE)
library(CohortDiagnostics)


# C. Script --------------------

###
#####  Need to add function here to copy/paste the CohortDiagnostics zip files to folder 'cohortDiagnostics/zipFiles'
###


## Create path to cohort diagnostics results and add to .gitignore file
cohortDiagnosticsZipFiles <- fs::path_abs("cohortDiagnostics/zipFiles") %>% fs::dir_create()
usethis::use_git_ignore("cohortDiagnostics")

## Create a scratch folder 
scratchDiagnosticsFolder <- fs::path_abs("cohortDiagnostics/scratchDiagnostics") %>% fs::dir_create()

## Path to sqlite database
sqlLiteDbPath <- fs::path(scratchDiagnosticsFolder, glue::glue("finerod"), ext = "sqlite")

## Create merged results file i.e. sqlite database file
CohortDiagnostics::createMergedResultsFile(dataFolder = cohortDiagnosticsZipFiles, 
                                           sqliteDbPath = sqlLiteDbPath,
                                           overwrite = TRUE)

## Launch diagnostics
CohortDiagnostics::launchDiagnosticsExplorer(sqliteDbPath = sqlLiteDbPath,
                                             publishDir = file.path(here::here("cohortDiagnostics/cohortDiagnostics_051224")),
                                             overwritePublishDir = TRUE,
                                             makePublishable = TRUE
                                             )


## Create DiagnosticsExplorer R shiny package (Optional)
# CohortDiagnostics::createDiagnosticsExplorerZip(outputZipfile = here::here("cohortDiagnostics/cohortDiagnostics_finerod.zip"),
#                                                 sqliteDbPath = sqlLiteDbPath,
#                                                 overwrite = TRUE)

