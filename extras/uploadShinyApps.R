# R code to upload all files and sub-directories in an rstudio file directory (specified by path)
# to the specified bucket (shiny) and sub-directory (specified by prefix) on the shiny server.

library(aws.s3)

# CHANGE THESE TWO VARIABLES BEFORE RUNNING THIS R CODE
rstudio_directory_path_to_upload <- here::here("cohortDiagnostics/cohortDiagnostics_051224")
shiny_subdirectory_prefix <- "FINEROD/CohortDiagnostics_051224/"   # This will be the sub-directory created on the shiny server

httr::set_config(httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L))

Sys.setenv("AWS_ACCESS_KEY_ID" = "AKCAC4SF7DEE3OHDSI")
Sys.setenv("AWS_SECRET_ACCESS_KEY" = "wJaArXUtnFEMICK7MDENGCbPxRfiCYOHDSI")


aws.s3::s3sync(bucket = "shiny",
               prefix = shiny_subdirectory_prefix,
               path = rstudio_directory_path_to_upload,
               direction = "upload",
               region = "",
               base_url = "minio.ohdsi.internal:9000",
               use_https = FALSE,
               verbose = FALSE,
               multipart = TRUE)
