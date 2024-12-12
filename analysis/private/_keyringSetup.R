## Functions for keyringSetup.R script --------------


checkConfig <- function () {
  
  check <- config_check()
  
  if (check) {
    
    pp <- usethis::proj_path() %>% fs::path("config.yml")
    
    cli::cat_bullet("config.yml exists in ", crayon::green(pp), bullet = "info", bullet_col = "blue")
    
    openConfig <- usethis::ui_yeah("Would you like to edit config.yml?")
    
    if (openConfig) {
      rstudioapi::navigateToFile(pp)
    }
    
  }
  else {
    
    txt <- glue::glue("`initConfig()` ")
    cli::cat_bullet("config.yml does not exist. To create it run function:\n\n  ", crayon::red(txt), "\n", 
                    bullet = "warning", bullet_col = "yellow")
    
    #cli::cat_line("To create config.yml edit and run function:\n\n  ", crayon::red(txt), "\n")
    cli::cat_line()
    
  }
  
  invisible(check)
}


initConfig <- function(block = "BlockName", database = "DatabaseName", 
                        withKeyring = FALSE, projectPath = here::here(), open = TRUE) {
  
  studyMeta <- retrieveStudySettings(projectPath = projectPath)$study
  
  data <- rlang::list2(Title = studyMeta$title, ID = studyMeta$id, 
                       Cohort = snakecase::to_snake_case(paste(studyMeta$id, 
                                                               block, sep = "_")), Block = block, Database = database)
  
  if (withKeyring) {
    template_file <- "config_keyring.yml"
  }
  
  else {
    template_file <- "config_raw.yml"
  }
  
  usethis::use_template(template = template_file, save_as = fs::path("config.yml"), 
                        data = data, open = open, package = "Ulysses")
  
  usethis::use_git_ignore(ignores = "config.yml")
  
  invisible(data)
}


makeConfig <- function() {
  
  projName <- basename(projectPath)
  
  data <- rlang::list2(Project = projName, 
                       Cohort = paste(projName, database, sep = "_"), 
                       Block = block, 
                       Database = database)
  
  usethis::use_template(template = "config.yml", data = data, open = open, package = "Ulysses")
  
  usethis::use_git_ignore(ignores = "config.yml")
  
  invisible(data)
}


setStudyKeyring <- function(keyringName, keyringPassword) {
  
  check <- checkKeyring(keyringName = keyringName, keyringPassword = keyringPassword)
  
  if (check) {
    ask1 <- usethis::ui_yeah("This keyring already exists. Do you want to drop the keyring and its contents?")
    
    if (ask1) {
      dropKeyring(keyringName = keyringName, keyringPassword = keyringPassword)
      setKeyring(keyringName = keyringName, keyringPassword = keyringPassword)
    }
    
    else {
      cli::cat_bullet("Keeping keyring ", crayon::cyan(keyringName), ". ")
    }
  }
  
  else {
    setKeyring(keyringName = keyringName, keyringPassword = keyringPassword)
  }
  
  invisible(keyringName)
}


setMultipleCredentials <- function (creds, db, keyringName, keyringPassword, forceCheck = TRUE) {
  
  if (keyring::keyring_is_locked(keyring = keyringName)) {
    keyring::keyring_unlock(keyring = keyringName, password = keyringPassword)
  }
  
  cli::cat_bullet("Input your credentials in the dialog box", bullet = "warning", bullet_col = "yellow")
  
  purrr::walk(creds, ~set_cred(cred = .x, db = db, keyringName = keyringName))
  
  if (forceCheck) {
    purrr::walk(creds, ~checkDatabaseCredential(cred = .x, db = db, 
                                                keyringName = keyringName, verbose = FALSE))
  }
  
  invisible(creds)
}


defaultCredentials <- function ()  {
  
  creds <- c("dbms", "user", "password", "connectionString", 
             "cdmDatabaseSchema", "vocabDatabaseSchema", "workDatabaseSchema")
  
  return(creds)
}


set_cred <- function(cred, db, keyringName) {
  
  key_name <- paste(db, cred, sep = "_")
  
  prompt_txt <- glue::glue("Set {key_name}: ")
  
  keyring::key_set(service = key_name, keyring = keyringName, prompt = prompt_txt)
  
  invisible(key_name)
}


setCredential <- function (cred, db, keyringName, keyringPassword, forceCheck = TRUE) {
  
  if (keyring::keyring_is_locked(keyring = keyringName)) {
    keyring::keyring_unlock(keyring = keyringName, password = keyringPassword)
  }
  
  cli::cat_bullet("Input your credentials in the dialog box", bullet = "warning", bullet_col = "yellow")
  
  set_cred(cred = cred, db = db, keyringName = keyringName)
  
  if (forceCheck) {
    checkDatabaseCredential(cred = cred, db = db, keyringName = keyringName)
  }
  
  invisible(cred)
}


config_check <- function () {
  
  pp <- usethis::proj_path()
  
  fs::path(pp, "config.yml") %>% fs::file_exists() %>% unname()
  
}


retrieveStudySettings <- function(projectPath) {
  
  ymlPath <- fs::path(projectPath, "_study.yml")
  
  studyYml <- yaml::read_yaml(ymlPath)
  
  return(studyYml)
}


checkKeyring <- function(keyringName, keyringPassword) {
  
  allKeyrings <- keyring::keyring_list()
  
  keyringName %in% allKeyrings$keyring
}


dropKeyring <- function(keyringName, keyringPassword) {
  
  if (keyring::keyring_is_locked(keyring = keyringName)) {
    keyring::keyring_unlock(keyring = keyringName, password = keyringPassword)
  }
  
  cli::cat_bullet("Delete existing keyring: ", keyringName, bullet = "warning", bullet_col = "yellow")
  
  keys <- keyring::key_list(keyring = keyringName)
  
  if (nrow(keys) > 0) {
    for (i in 1:nrow(keys)) {
      keyring::key_delete(keys$service[i], keyring = keyringName)
    }
  }
  
  keyring::keyring_delete(keyring = keyringName)
  
  invisible(keyringName)
}


setKeyring <- function(keyringName, keyringPassword) {
  
  cli::cat_bullet("Creating a study keyring for: ", crayon::cyan(keyringName),bullet = "info", bullet_col = "blue")
  
  keyring::keyring_create(keyring = keyringName, password = keyringPassword)
  
  invisible(keyringName)
}


# checkDatabaseCredential <- function(cred, keyringName, verbose = TRUE) {
#   
#   key_name <- cred
#   
#   if (verbose) {
#     cli::cat_bullet("Check that credential ", crayon::green(key_name), " is correct.", bullet = "warning", bullet_col = "yellow")
#   }
#   
#   blurCreds(item = key_name, keyringName = keyringName)
#   
#   invisible(key_name)
# }


checkDatabaseCredential <- function(cred, db, keyringName, verbose = TRUE) {
  
  key_name <- paste(db, cred, sep = "_")
  
  if (verbose) {
    cli::cat_bullet("Check that credential ", crayon::green(key_name), " is correct.",
                    bullet = "warning", bullet_col = "yellow")
  }
  
  # Print blurred credentials in console
  blurCreds(item = key_name, keyringName = keyringName)
  
  invisible(key_name)
}


blurCreds <- function(item,  keyringName) {
  
  cred <- keyring::key_get(service = item, keyring = keyringName)
  
  txt <- glue::glue(item, ": ", crayon::blurred(cred))
  
  cli::cat_bullet(txt, bullet = "info", bullet_col = "blue")
  
  invisible(item)
}
