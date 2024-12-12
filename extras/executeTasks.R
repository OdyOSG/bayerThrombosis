## Build Cohorts
source(here::here("analysis/studyTasks/01_buildCohorts_mkt.R"))
source(here::here("analysis/studyTasks/01_buildCohorts.R"))           ## Optum Claims
source(here::here("analysis/studyTasks/01_buildCohorts_optumEhr.R"))

source(here::here("analysis/studyTasks/01_buildCohorts_mdv.R"))
source(here::here("analysis/studyTasks/01_buildCohorts_rwd.R"))


## Run CohortDiagnostics

source(here::here("analysis/studyTasks/02_cohortDiagnostics_mkt.R"))
source(here::here("analysis/studyTasks/02_cohortDiagnostics.R"))           ## Optum Claims
source(here::here("analysis/studyTasks/02_cohortDiagnostics_optumEhr.R"))
source(here::here("analysis/studyTasks/02_cohortDiagnostics_mdv.R"))
source(here::here("analysis/studyTasks/02_cohortDiagnostics_rwd.R"))

