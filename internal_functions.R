generateCohortSet_here <- function(cdm,
                              cohortSet,
                              name,
                              computeAttrition = TRUE,
                              overwrite = TRUE) {
  rlang::check_installed("CirceR")
  rlang::check_installed("SqlRender")
  
  if (!is.data.frame(cohortSet)) {
    if (!is.list(cohortSet)) {
      rlang::abort("cohortSet must be a dataframe or a named list of Capr cohort definitions")
    }
    
    checkmate::assertList(cohortSet,
                          types = "Cohort",
                          min.len = 1,
                          names = "strict",
                          any.missing = FALSE)
    
    cohortSet <- dplyr::tibble(
      cohort_definition_id = seq_along(cohortSet),
      cohort_name = names(cohortSet),
      cohort = purrr::map(cohortSet, ~jsonlite::fromJSON(generics::compile(.), simplifyVector = FALSE)),
      json = purrr::map_chr(cohortSet, generics::compile)
    )
    class(cohortSet) <- c("CohortSet", class(cohortSet))
  }
  
  checkmate::assertDataFrame(cohortSet, min.rows = 1, col.names = "named")
  
  cli::cli_alert_info("Generating {nrow(cohortSet)} cohort{?s}")
  withr::local_options(list("cli.progress_show_after" = 0, "cli.progress_clear" = FALSE))
  
  
  checkmate::assertClass(cdm, "cdm_reference")
  con <- cdmCon(cdm)
  checkmate::assertTRUE(DBI::dbIsValid(con))
  checkmate::assert_character(name, len = 1, min.chars = 1, any.missing = FALSE, pattern = "[a-zA-Z0-9_]+")
  checkmate::assertLogical(computeAttrition, len = 1)
  checkmate::assertLogical(overwrite, len = 1)
  
  write_schema <- cdmWriteSchema(cdm)
  checkmate::assert_character(write_schema,
                              min.chars = 1,
                              min.len = 1,
                              max.len = 3,
                              null.ok = FALSE)
  
  if ("prefix" %in% names(write_schema)) {
    prefix <- unname(write_schema["prefix"])
  } else {
    prefix <- ""
  }
  
  # Handle OHDSI cohort sets
  if ("cohortId" %in% names(cohortSet) && !("cohort_definition_id" %in% names(cohortSet))) {
    cohortSet$cohort_definition_id <- cohortSet$cohortId
  }
  
  if ("cohortName" %in% names(cohortSet) && !("cohort_name" %in% names(cohortSet))) {
    cohortSet$cohort_name <- cohortSet$cohortName
  }
  
  if (!("cohort" %in% names(cohortSet)) && ("json" %in% names(cohortSet))) {
    cohortColumn <- list()
    for (i in seq_len(nrow(cohortSet))) {
      x <- cohortSet$json[i]
      if (!validUTF8(x)) { x <- stringi::stri_enc_toutf8(x, validate = TRUE) }
      if (!validUTF8(x)) { rlang::abort("Failed to convert json UTF-8 encoding") }
      cohortColumn[[i]] <- jsonlite::fromJSON(x, simplifyVector = FALSE)
    }
    cohortSet$cohort <- cohortColumn
  }
  
  checkmate::assertTRUE(all(c("cohort_definition_id", "cohort_name", "json") %in% colnames(cohortSet)))
  
  # check name -----
  checkmate::assertCharacter(name, len = 1, min.chars = 1, pattern = "[a-z_]+")
  
  if (paste0(prefix, name) != tolower(paste0(prefix, name))) {
    cli::cli_abort("Cohort table name {.code {paste0(prefix, name)}} must be lowercase!")
  }
  
  # Make sure tables do not already exist
  existingTables <- listTables(con, write_schema)
  
  for (x in paste0(name, c("", "_count", "_set", "_attrition"))) {
    if (x %in% existingTables) {
      if (overwrite) {
        DBI::dbRemoveTable(con, CDMConnector:::.inSchema(write_schema, x, dbms = dbms(con)))
      } else {
        cli::cli_abort("The cohort table {paste0(prefix, name)} already exists.\nSpecify overwrite = TRUE to overwrite it.")
      }
    }
  }
  
  # Create the OHDSI-SQL for each cohort ----
  
  cohortSet$sql <- character(nrow(cohortSet))
  
  for (i in seq_len(nrow(cohortSet))) {
    cohortJson <- cohortSet$json[[i]]
    cohortExpression <- CirceR::cohortExpressionFromJson(expressionJson = cohortJson)
    cohortSql <- CirceR::buildCohortQuery(expression = cohortExpression,
                                          options = CirceR::createGenerateOptions(
                                            generateStats = computeAttrition))
    cohortSet$sql[i] <- SqlRender::render(cohortSql, warnOnMissingParameters = FALSE)
  }
  
  CDMConnector:::createCohortTables(con, write_schema, name, computeAttrition)
  
  # Run the OHDSI-SQL ----
  cdm_schema <- attr(cdm, "cdm_schema")
  checkmate::assertCharacter(cdm_schema, max.len = 3, min.len = 1, min.chars = 1)
  
  if ("prefix" %in% names(cdm_schema)) {
    cdm_schema_sql <- glue::glue_sql_collapse(DBI::dbQuoteIdentifier(con, cdm_schema[-which(names(cdm_schema) == "prefix")]), sep = ".")
  } else {
    cdm_schema_sql <- glue::glue_sql_collapse(DBI::dbQuoteIdentifier(con, cdm_schema), sep = ".")
  }
  
  if ("prefix" %in% names(write_schema)) {
    write_schema_sql <- paste(DBI::dbQuoteIdentifier(con, write_schema[-which(names(write_schema) == "prefix")]), collapse = ".")
  } else {
    write_schema_sql <- paste(DBI::dbQuoteIdentifier(con, write_schema), collapse = ".")
  }
  
  # dropTempTableIfExists <- function(con, table) {
  #   # used for dropping temp emulation tables
  #   suppressMessages(
  #     DBI::dbExecute(
  #       con,
  #       SqlRender::translate(
  #         glue::glue("IF OBJECT_ID('#{table}', 'U') IS NOT NULL DROP TABLE #{table};"),
  #         targetDialect = dbms(con))
  #       )
  #   )
  # }
  
  generate <- function(i) {
    pct <- ""
    cli::cli_progress_step("Generating cohort ({i}/{nrow(cohortSet)}{pct}) - {cohortSet$cohort_name[i]}", spinner = interactive())
    
    sql <- cohortSet$sql[i] %>%
      SqlRender::render(
        cdm_database_schema = cdm_schema_sql,
        vocabulary_database_schema = cdm_schema_sql,
        target_database_schema = write_schema_sql,
        results_database_schema.cohort_inclusion        = paste0(write_schema_sql, ".", DBI::dbQuoteIdentifier(con, paste0(prefix, name, "_inclusion"))),
        results_database_schema.cohort_inclusion_result = paste0(write_schema_sql, ".", DBI::dbQuoteIdentifier(con, paste0(prefix, name, "_inclusion_result"))),
        results_database_schema.cohort_summary_stats    = paste0(write_schema_sql, ".", DBI::dbQuoteIdentifier(con, paste0(prefix, name, "_summary_stats"))),
        results_database_schema.cohort_censor_stats     = paste0(write_schema_sql, ".", DBI::dbQuoteIdentifier(con, paste0(prefix, name, "_censor_stats"))),
        results_database_schema.cohort_inclusion        = paste0(write_schema_sql, ".", DBI::dbQuoteIdentifier(con, paste0(prefix, name, "_inclusion"))),
        target_cohort_table = DBI::dbQuoteIdentifier(con, paste0(prefix, name)),
        target_cohort_id = cohortSet$cohort_definition_id[i],
        warnOnMissingParameters = FALSE
      )
    
    if (dbms(con) == "snowflake") {
      # we don't want to use temp emulation on snowflake. We want to use actual temp tables.
      sql <- stringr::str_replace_all(sql, "CREATE TABLE #", "CREATE TEMPORARY TABLE ") %>%
        stringr::str_replace_all("create table #", "create temporary table ") %>%
        stringr::str_replace_all("#", "")
      
      # temp tables created by circe that can be left dangling.
      tempTablesToDrop <- c(
        "Codesets",
        "qualified_events",
        "cohort_rows",
        "Inclusion",
        "strategy_ends",
        "inclusion_events",
        "included_events",
        "final_cohort",
        "inclusion_rules",
        "BEST_EVENTS",
        paste0("Inclusion_", 0:9))
      
      for (j in seq_along(tempTablesToDrop)) {
        DBI::dbExecute(con, paste("drop table if exists", tempTablesToDrop[j]))
      }
      
      namesToQuote <- c("cohort_definition_id",
                        "subject_id",
                        "cohort_start_date",
                        "cohort_end_date",
                        "mode_id",
                        "inclusion_rule_mask",
                        "person_count",
                        "rule_sequence",
                        "gain_count",
                        "person_total",
                        "base_count", "final_count")
      
      for (n in namesToQuote) {
        sql <- stringr::str_replace_all(sql, n, DBI::dbQuoteIdentifier(con, n))
      }
    }
    
    # total hack workaround for circe - temp23019_chrt0_inclusion"_stats
    quoteSymbol <- substr(as.character(DBI::dbQuoteIdentifier(con, "a")), 1, 1)
    sql <- stringr::str_replace_all(sql,
                                    paste0("_inclusion", quoteSymbol, "_stats"),
                                    paste0("_inclusion_stats", quoteSymbol))
    
    # if parameters exist in the sql (starting with @), stop.
    stopifnot(length(unique(stringr::str_extract_all(sql, "@\\w+"))[[1]]) == 0)
    
    # remove comments from SQL which are causing an issue on spark
    # --([^\n])*?\n => match strings starting with -- followed by anything except a newline
    sql <- stringr::str_replace_all(sql, "--([^\n])*?\n", "\n")
    
    if (dbms(con) != "spark") {
      sql <- SqlRender::translate(sql,
                                  targetDialect = CDMConnector::dbms(con),
                                  tempEmulationSchema = "SQL ERROR")
      
      if (stringr::str_detect(sql, "SQL ERROR")) {
        cli::cli_abort("sqlRenderTempEmulationSchema being used for cohort generation!
        Please open a github issue at {.url https://github.com/darwin-eu/CDMConnector/issues} with your cohort definition.")
      }
    } else {
      # we need temp emulation on spark as there are no temp tables
      
      if ("schema" %in% names(write_schema)) {
        s <- unname(write_schema["schema"])
      } else if (length(write_schema) == 1) {
        s <- unname(write_schema)
      } else {
        s <- unname(write_schema[2])
      }
      
      sql <- SqlRender::translate(sql,
                                  targetDialect = CDMConnector::dbms(con),
                                  tempEmulationSchema = s)
    }
    
    if (dbms(con) == "duckdb") {
      # hotfix for duckdb sql translation https://github.com/OHDSI/SqlRender/issues/340
      sql <- gsub("'-1 \\* (\\d+) day'", "'-\\1 day'", sql)
    }
    
    if (dbms(con) == "spark") {
      # issue with date add translation on spark
      sql <- stringr::str_replace_all(sql, "date_add", "dateadd")
      sql <- stringr::str_replace_all(sql, "DATE_ADD", "DATEADD")
    }
    
    sql <- stringr::str_replace_all(sql, "\\s+", " ")
    
    sql <- stringr::str_split(sql, ";")[[1]] %>%
      stringr::str_trim() %>%
      stringr::str_c(";") %>% # remove empty statements
      stringr::str_subset("^;$", negate = TRUE)
    
    # drop temp tables if they already exist
    drop_statements <- stringr::str_subset(sql, "DROP TABLE") %>%
      stringr::str_replace("DROP TABLE", "DROP TABLE IF EXISTS") %>%
      purrr::map_chr(~SqlRender::translate(., dbms(con)))
    
    for (k in seq_along(drop_statements)) {
      DBI::dbExecute(con, drop_statements[k], immediate = TRUE)
    }
    
    for (k in seq_along(sql)) {
      # cli::cat_rule(glue::glue("sql {k} with {nchar(sql[k])} characters."))
      # cli::cat_line(sql[k])
      DBI::dbExecute(con, sql[k], immediate = TRUE)
      
      if (interactive()) {
        pct <- ifelse(k == length(sql), "", glue::glue(" ~ {floor(100*k/length(sql))}%"))
        cli::cli_progress_update()
      }
    }
  }
  
  # this loop makes cli updates look correct
  for (i in seq_len(nrow(cohortSet))) {
    generate(i)
  }
  
  cohort_ref <- dplyr::tbl(con, CDMConnector:::.inSchema(write_schema, name, dbms = dbms(con)))
  
  # Create attrition attribute ----
  if (computeAttrition) {
    cohort_attrition_ref <- CDMConnector:::computeAttritionTable(
      cdm = cdm,
      cohortStem = name,
      cohortSet = cohortSet,
      overwrite = overwrite
    ) |>
      dplyr::collect()
  } else {
    cohort_attrition_ref <- NULL
  }
  
  # Create cohort_set attribute -----
  # if (paste0(name, "_set") %in% existingTables) {
  #   DBI::dbRemoveTable(con, inSchema(write_schema, paste0(name, "_set"), dbms = dbms(con)))
  # }
  
  cdm[[name]] <- cohort_ref |>
    omopgenerics::newCdmTable(src = attr(cdm, "cdm_source"), name = name)
  
  # browser()
  # Create the object. Let the constructor handle getting the counts.----
  cdm[[name]] <- omopgenerics::newCohortTable(
    table = cdm[[name]],
    cohortSetRef = cohortSet[,c("cohort_definition_id", "cohort_name")],
    cohortAttritionRef = cohort_attrition_ref,
    .softValidation = TRUE)
  
  cli::cli_progress_done()
  
  return(cdm)
}