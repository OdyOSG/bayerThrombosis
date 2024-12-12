# Create zip file
zipName <- paste0(db.name,"_Results")
tempDir <- zipName
tempDirCreated <- FALSE
if (!dir.exists(tempDir)) {
  dir.create(tempDir)
  tempDirCreated <- TRUE
}

start <- Sys.time()

# Start log
log_file <- paste0(tempDir, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# Read internal functions needed
source(here("internal_functions.R"))
devtools::load_all(here("CohortSurvival-main"))

# Read and instantiate cohorts
if (readInitialCohorts){
  info(logger, 'INSTANTIATING INITIAL COHORTS')
cdm <- cdm_from_con(
  con = db,
  cdm_schema = c(catalog = db.name, schema = cdm_database_schema),  
  write_schema = c(catalog = results_database_prefix, schema = results_database_schema),
  cdm_name = db.name,
  .soft_validation = TRUE
)
  source(here("1_InitialCohorts", "InstantiateStudyCohorts.R"), local = TRUE)
  info(logger, 'GOT INITIAL COHORTS')
} else {
  info(logger, 'INITIAL COHORTS ALREADY INSTANTIATED')
cdm <- cdm_from_con(
  con = db,
  cdm_schema = c(catalog = db.name, schema = cdm_database_schema),  
  write_schema = c(catalog = results_database_prefix, schema = results_database_schema),  
  cohort_tables = c(paste0(prefix_db,"target_cohorts"), paste0(prefix_db,"outcome_cohorts")),
  cdm_name = db.name,
  .soft_validation = TRUE
)
  info(logger, 'INITIAL COHORTS READ')
}

# Get the cdm snapshot
snapshot <- CDMConnector::snapshot(cdm)
write.csv(snapshot, 
          file = here::here(
            tempDir, 
            paste0(attr(cdm, "cdm_name"),"_snapshot.csv")))
info(logger, 'CDM SNAPSHOT RETRIEVED')

# Run: Survival analyses
if(doSurvival) {
  source(here("3_Survival","WP2_code.R"), local = TRUE)
  info(logger, 'GOT SURVIVAL RESULTS')
}

# Run: Incidence analyses
if(doIncidencePrevalence) {
  source(here("2_IncidencePrevalence","WP1_code.R"), local = TRUE)
  info(logger, 'GOT INCIDENCE OF PRIMARY ENDPOINT')
}

info(logger, 'SAVED RESULTS IN THE OUTPUT FOLDER')

zip::zip(zipfile = paste0(zipName, ".zip"),
         files = list.files(tempDir, full.names = TRUE))

print("Done!")
print("If all has worked, there should now be a zip file with your results in the output folder to share")
print("Thank you for running the study!")
Sys.time() - start
readLines(log_file)

if (tempDirCreated) {
  unlink(tempDir, recursive = TRUE)
}
