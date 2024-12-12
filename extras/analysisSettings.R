# A. File Info -----------------------

# Study:
# Task: Analysis Settings


# B. Dependencies ----------------------

## Load libraries and scripts
library(tidyverse, quietly = TRUE)
library(yaml)
source(here::here("analysis/private/_utilities.R"))


# C. Script --------------------

## Create output folder of settings files
fs::dir_create("analysis/settings")

## Select all cohorts
cohortManifest <- getCohortManifest()


## 2. Characterization --------------------
### 2.1 Baseline Characteristics [-365,0] --------------------

## Select target cohorts
targetCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("target", "strata")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

covariateCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("covariate")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

yaml2 <- list(
  'baselineCharacteristics' = list(
    'cohorts' = list(
      'targetCohorts' = targetCohorts,
      'covariateCohorts' = covariateCohorts
    ),
    'timeWindows' = tibble::tibble(
        startDay = c(-365L),
        endDay = c(0)
      ),
    'outputFolder' = fs::path("03_baselineCharacteristics")
  )
)

# Create yaml file
yaml::write_yaml(yaml2, file = here::here("analysis/settings/baseline365.yml"), column.major = FALSE)


### 2.2 Baseline Characteristics [-180,0] --------------------

## Select target cohorts
targetCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("target", "strata")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

covariateCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("covariate2")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

yaml3 <- list(
  'baselineCharacteristics' = list(
    'cohorts' = list(
      'targetCohorts' = targetCohorts,
      'covariateCohorts' = covariateCohorts
    ),
    'timeWindows' = tibble::tibble(
      startDay = c(-180L),
      endDay = c(0)
    ),
    'outputFolder' = fs::path("03_baselineCharacteristics")
  )
)

# Create yaml file
yaml::write_yaml(yaml3, file = here::here("analysis/settings/baseline180.yml"), column.major = FALSE)


## 3. Incidence Analysis --------------------

## Select target cohorts
numeratorCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("outcome")) %>%
  #dplyr::filter(name %in% c("ami", "hyperkalemia")) %>%
  #dplyr::filter(name %in% c("ami")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)

## Select denominator cohorts
denominatorCohorts <- cohortManifest %>%
  dplyr::filter(type %in% c("target")) %>%
  dplyr::mutate(id = as.integer(id)) %>%
  dplyr::select(name, id)


yaml4 <- list(
  'incidenceAnalysis' = list(
    'cohorts' = list(
      'numeratorCohorts' = numeratorCohorts,
      'denominatorCohorts' = denominatorCohorts
    ),
    'incidenceSettings' = list(
      'sex' = "Both",                                              # "Male", "Female", "Both"
      #'ageGroups' = list(c(0,130), c(0,29), c(30,44), c(45,55)),  # The first age group (0-130) is for overall age group analysis
      'interval' = c("years", "overall"),
      'completeDatabaseIntervals' = FALSE,
      'cellCount' = 5L
    ),
    'outputFolder' = fs::path("04_incidenceAnalysis")
  )
)

# Create yaml file
yaml::write_yaml(yaml4, file = here::here("analysis/settings/incidenceAnalysis.yml"), column.major = FALSE)



# ## 3. Post-Index Characteristics --------------------
# 
# covariateCohorts <- cohortManifest %>%
#   dplyr::filter(type %in% c("outcomeSurgeries", "outcomeDrugs")) %>%
#   dplyr::mutate(id = as.integer(id)) %>%
#   dplyr::select(name, id)
# 
# yaml3 <- list(
#   'postIndexCharacteristics' = list(
#     'cohorts' = list(
#       'targetCohorts' = allCohorts,
#       'covariateCohorts' = covariateCohorts
#     ),
#     'timeWindows' = tibble::tibble(
#       startDay = c(1, 0, 1, 0),
#       endDay = c(9999, 9999, 183, 183)
#       ),
#     'outputFolder' = fs::path("05_postIndexCharacteristics")
#   )
# )
# 
# # Create yaml file
# write_yaml(yaml3, file = here::here("analysis/settings/postIndex.yml"), column.major = FALSE)
# 
# 
# ## 4.1 Time To Event (Whole cohort) -------------------
# 
# eventCohorts <- cohortManifest %>%
#   dplyr::filter(type %in% c("outcomeSurgeries", "outcomeDrugs")) %>%
#   dplyr::mutate(id = as.integer(id)) %>%
#   dplyr::select(name, id)
# 
# yaml4 <- list(
#   'tte' = list(
#     'cohorts' = list(
#       'targetCohorts' = allCohorts,
#       'eventCohorts' = eventCohorts
#     ),
#     'outputFolder' = list(
#       fs::path("06_tte")
#     )
#   )
# )
# 
# # Create yaml file
# write_yaml(yaml4, file = here::here("analysis/settings/tte.yml"), column.major = FALSE)
# 
# 
# ## 4.2 Time To Event (Only Surgery patients) -------------------
# 
# eventCohorts <- cohortManifest %>%
#   dplyr::filter(type %in% c("outcomeSurgeries")) %>%
#   dplyr::mutate(id = as.integer(id)) %>%
#   dplyr::select(name, id)
# 
# yaml5 <- list(
#   'tte' = list(
#     'cohorts' = list(
#       'targetCohorts' = allCohorts,
#       'eventCohorts' = eventCohorts
#     ),
#     'outputFolder' = list(
#       fs::path("07_tte2")
#     )
#   )
# )
# 
# # Create yaml file
# write_yaml(yaml5, file = here::here("analysis/settings/tte2.yml"), column.major = FALSE)
#
#
# ## 1. Stratas --------------------
# 
# yaml1 <- list(
#   'strata' = list(
#     'cohorts' = list(
#       'targetCohorts' = targetCohorts,
#       'strataCohorts' = targetCohorts
#     ),
#     'demographics' = tibble::tibble(
#       strataId = 1L:14L,
#       strataName = c("below_65",
#                      "65_and_above",
#                      "male",
#                      "female",
#                      "black",
#                      "white",
#                      "asian",
#                      "unknown_race",
#                      "hispanic",
#                      "notHispanic",
#                      "unknown_ethnicity",
#                      "cci_0",
#                      "cci_1",
#                      "cci_2")
#     ),
#     'outputFolder' = fs::path("03_buildStrata")
#   )
# )
# 
# # Create yaml file
# write_yaml(yaml1, file = here::here("analysis/settings/strata.yml"), column.major = FALSE)
# 
# 
# ### All cohorts (target and its stratas) ------------------
# 
# demoStrata <- yaml1$strata$demographics
# 
# allCohorts <- expand_grid(targetCohorts, demoStrata) %>%
#   dplyr::mutate(
#     id = id * 1000 + strataId,
#     name = paste(name, strataName)
#   ) %>%
#   dplyr::select(id, name) %>%
#   rbind(targetCohorts)
