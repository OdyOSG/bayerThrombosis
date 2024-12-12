# This is the only code the user should interact with
# Runs the Bayer Studyathon on Asundexian

# Required packages: 
library("RPostgres")
library("tools")
library("DBI")
library("dplyr")
library("dbplyr")
library("CirceR")
library("CDMConnector")
library("here")
library("log4r")
library("zip")
library("ggplot2")
library("IncidencePrevalence")
library("PatientProfiles")

# install the following package like this, with remotes 
# library("remotes")
# remotes::install_github("OHDSI/CirceR")

# Name of the output folder to save the results. Change to "output" or any other
# desired path
output.folder <- here::here()

results_database_prefix <- "OHDSI_SCRATCH" 
results_database_schema <- "SCRATCH_[your CWID]" 

# Study start date, should not change this
study_start_date <- as.Date("2012-01-01")

# Decide which parts of the study you want to run 
readInitialCohorts <- TRUE
doIncidencePrevalence <- TRUE
doSurvival <- TRUE

################################################################################
# First db: OPTUM
cdm_database_schema <- "CDM_2022Q4" 
db.name <- "OPTUM_CLAIMS_OMOP"

# Connect to database
# please see examples to connect here:
# https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html
db <- dbConnect(
  odbc::odbc(),
  dsn = "snowflake",
  database = db.name,
  schema = "CDM",
  uid = "LOC_OHDSI_[your CWID]",
  role = "[your CWID]",
  pwd = keyring::key_get(keyring = "[your CWID]", service = "snowflake", username = "LOC_OHDSI_[your CWID]")
)

# Set the DATE_INPUT_FORMAT session parameter
DBI::dbExecute(db, "ALTER SESSION SET DATE_INPUT_FORMAT = 'YYYY-MM-DD'")

prefix_db <- "optum_bs_"
# Run the study
source(here("RunStudy.R"), local = TRUE)

# Run successful :)

################################################################################
# Second db: CPRD GOLD
#db.name <- "CPRD_EHR_GOLD_OMOP"
#cdm_database_schema <- "CDM_2022H2" 

# Connect to database
# please see examples to connect here:
# https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html
#db <- dbConnect(
#  odbc::odbc(),
#  dsn = "snowflake",
#  database = db.name,
#  schema = "CDM",
#  uid = "LOC_OHDSI_[your CWID]",
#  role = "[your CWID]",
#  pwd = keyring::key_get(keyring = "[your CWID]", service = "snowflake", username = "LOC_OHDSI_[your CWID]")
#)

# Set the DATE_INPUT_FORMAT session parameter
#DBI::dbExecute(db, "ALTER SESSION SET DATE_INPUT_FORMAT = 'YYYY-MM-DD'")

# Run the study
#source(here("RunStudy.R"), local = TRUE)

# ERROR! No people in target cohorts

################################################################################
# Third db: CPRD AURUM
#db.name <- "CPRD_EHR_AURUM_OMOP"
#cdm_database_schema <- "CDM" 

# Connect to database
# please see examples to connect here:
# https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html
#db <- dbConnect(
#  odbc::odbc(),
#  dsn = "snowflake",
#  database = db.name,
#  schema = "CDM",
#  uid = "LOC_OHDSI_[your CWID]",
#  role = "[your CWID]",
#  pwd = keyring::key_get(keyring = "[your CWID]", service = "snowflake", username = "LOC_OHDSI_[your CWID]")
#)

# Set the DATE_INPUT_FORMAT session parameter
#DBI::dbExecute(db, "ALTER SESSION SET DATE_INPUT_FORMAT = 'YYYY-MM-DD'")

# Run the study
#source(here("RunStudy.R"), local = TRUE)

# ERROR! No people in target cohorts

################################################################################
# Fifth db: MDV HF
db.name <- "MDV_CLAIMS_HF_OMOP"
cdm_database_schema <- "CDM_202308V2" 

# Connect to database
# please see examples to connect here:
# https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html
db <- dbConnect(
  odbc::odbc(),
  dsn = "snowflake",
  database = db.name,
  schema = "CDM",
  uid = "LOC_OHDSI_[your CWID]",
  role = "[your CWID]",
  pwd = keyring::key_get(keyring = "[your CWID]", service = "snowflake", username = "LOC_OHDSI_[your CWID]")
)

# Set the DATE_INPUT_FORMAT session parameter
DBI::dbExecute(db, "ALTER SESSION SET DATE_INPUT_FORMAT = 'YYYY-MM-DD'")
prefix_db <- "mdvhf_bs_"

# Run the study
source(here("RunStudy.R"), local = TRUE)
 
# Run successful :)

################################################################################
# Sixth db: MDV DM
db.name <- "MDV_CLAIMS_DM_OMOP"
cdm_database_schema <- "CDM_202308V3" 

# Connect to database
# please see examples to connect here:
# https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html
db <- dbConnect(
  odbc::odbc(),
  dsn = "snowflake",
  database = db.name,
  schema = "CDM",
  uid = "LOC_OHDSI_[your CWID]",
  role = "[your CWID]",
  pwd = keyring::key_get(keyring = "[your CWID]", service = "snowflake", username = "LOC_OHDSI_[your CWID]")
)

# Set the DATE_INPUT_FORMAT session parameter
DBI::dbExecute(db, "ALTER SESSION SET DATE_INPUT_FORMAT = 'YYYY-MM-DD'")
prefix_db <- "mdvdm_bs_"

# Run the study
source(here("RunStudy.R"), local = TRUE)

# Run successful :)

################################################################################
# Seventh db: RWDCO
db.name <- "RWDCO_CLAIMS_EHR_CKD_OMOP"
cdm_database_schema <- "CDM_202311V3" 

# Connect to database
# please see examples to connect here:
# https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html
db <- dbConnect(
  odbc::odbc(),
  dsn = "snowflake",
  database = db.name,
  schema = "CDM",
  uid = "LOC_OHDSI_[your CWID]",
  role = "[your CWID]",
  pwd = keyring::key_get(keyring = "[your CWID]", service = "snowflake", username = "LOC_OHDSI_[your CWID]")
)

# Set the DATE_INPUT_FORMAT session parameter
DBI::dbExecute(db, "ALTER SESSION SET DATE_INPUT_FORMAT = 'YYYY-MM-DD'")
prefix_db <- "rwdckd_bs_"

# Run the study
source(here("RunStudy.R"), local = TRUE)

# Run successful :)

################################################################################
# Eigth db: RWDCO
db.name <- "RWDCO_CLAIMS_EHR_HF_OMOP"
cdm_database_schema <- "CDM_202311V2" 

# Connect to database
# please see examples to connect here:
# https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html
db <- dbConnect(
  odbc::odbc(),
  dsn = "snowflake",
  database = db.name,
  schema = "CDM",
  uid = "LOC_OHDSI_[your CWID]",
  role = "[your CWID]",
  pwd = keyring::key_get(keyring = "[your CWID]", service = "snowflake", username = "LOC_OHDSI_[your CWID]")
)

# Set the DATE_INPUT_FORMAT session parameter
DBI::dbExecute(db, "ALTER SESSION SET DATE_INPUT_FORMAT = 'YYYY-MM-DD'")
prefix_db <- "rwdhf_bs_"

# Run the study
source(here("RunStudy.R"), local = TRUE)

# Run successful :)

################################################################################
# Fourth db: MARKETSCAN
#cdm_database_schema <- "CDM_2022Q3" 
#run_db("MKTSCAN_CLAIMS_OMOP")
# ERROR! Overlapping OPs

db.name <- "MKTSCAN_CLAIMS_OMOP"
cdm_database_schema <- "CDM_2022Q3" 

# Connect to database
# please see examples to connect here:
# https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html
db <- dbConnect(
  odbc::odbc(),
  dsn = "snowflake",
  database = db.name,
  schema = "CDM",
  uid = "LOC_OHDSI_[your CWID]",
  role = "[your CWID]",
  pwd = keyring::key_get(keyring = "[your CWID]", service = "snowflake", username = "LOC_OHDSI_[your CWID]")
)

# Set the DATE_INPUT_FORMAT session parameter
DBI::dbExecute(db, "ALTER SESSION SET DATE_INPUT_FORMAT = 'YYYY-MM-DD'")
prefix_db <- "mktscan_bs_"

# Run the study
source(here("RunStudy.R"), local = TRUE)

# After this is run you should have a zip file in your output folders to share