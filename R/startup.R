rProfileTxt <- "
  .First <- function(){
  library(crayon)
  cat(crayon::white$bold(\"\\nWelcome to the ANPC environment \"), date(), \"\\n\")
  if (!dir.exists(Sys.getenv()['ONEDRIVE'])) {
    cat(crayon::red(\"ERROR: OneDrive folder not found in .Renviron file.\\n\"))
    cat(\"You need it configured to get access to datasets folder.\\n\")
    cat(\"Run fusion::startup().\\n\")
  } else {
    cat(crayon::green(\"OneDrive found: \"), Sys.getenv()['ONEDRIVE'], \"\\n\")

    if (!dir.exists(Sys.getenv()['DATASETS'])) {
     cat(crayon::red(\"ERROR: The datasets folder was not found in your OneDrive Folder.\\n\"))
     cat(\"Please check that you are in sync.\\n\")
     cat(\"Or run fusion::startup().\\n\")
    } else {
      cat(crayon::green(\"Datasets found: \"), Sys.getenv()['DATASETS'], \"\\n\")
    }

  }
}"

configurePath <- function() {
  sink(file = rEnvironPath, append = TRUE)
  cat(paste0("ONEDRIVE=", ONEDRIVE, "\n"))
  cat(paste0("DATASETS=", DATASETS, "\n"))
  sink()
}

setPath <- function(folder, environ = rEnvironPath) {
  if(file.exists(rEnvironPath)) {
    cat(paste0("The path to your home folder is: ", Sys.getenv("HOME"), "\n"))
    cat(paste0("The path to your .Renviron file is: ", environ, "\n"))
    question <- paste0("What is the path to your ", folder, " folder/file?\n")
    PATH <- readline(prompt = question)
    sink(file = environ, append = TRUE)
    cat(paste0(toupper(folder), "=", PATH, "\n"))
    sink()
    cat(crayon::green("Path updated.\n"))
  } else {
    cat(crayon::red(".Renviron file not found, please provide a valid path.\n"))
    cat(crayon::red$bold("Nothing done.\n"))
  }
}

modifyPath <- function(folder, oldPath) {
  cat(paste0("The current path is: ", oldPath, "\n"))
  choice <- menu(c(crayon::green("Yes"),
                   crayon::red("no")),
                 title = "Would you like to modify it?\n")
  switch(choice,
         setPath(folder),
         cat("Nothing done.\n"))
}

updateProfile <- function() {
  sink(file = rProfilePath, append = TRUE)
  cat("\n", rProfileTxt, "\n")
  sink()
  cat(crayon::green(".Rprofile was successfully updated.\n"))
  cat("Please restart your session.\n")
}

parseFileFor = function(path, pattern) {
  con <- file(path, "r")
  fi <- c()
  while ( TRUE ) {
    line <- readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    fi <- c(fi, grepl(pattern, line))
  }
  close(con)
  ifelse(sum(fi) > 0, return(TRUE), return(FALSE))
}
#parseFileFor(rProfilePath, pattern = "Welcome to the ANPC environment")

#' startup script to define shared folders for ANPC users
#'
#' @return void
#' @export
startup <- function() {
  homePath <- Sys.getenv("HOME")
  rProfilePath <- file.path(homePath, ".Rprofile")
  rEnvironPath <- file.path(homePath, ".Renviron")
  rHomePath <- file.path(Sys.getenv("R_HOME"))
  os <- Sys.info()['sysname']
  machine <- Sys.info()['machine']

  #we check for ONEDRIVE and DATASETS in Renviron

  if (!is.na(Sys.getenv()['ONEDRIVE'][[1]]) &
      !is.na(Sys.getenv()['DATASETS'][[1]])) {
    if (dir.exists(Sys.getenv()['ONEDRIVE'][[1]]) &
        dir.exists(Sys.getenv()['DATASETS'][[1]])) {
      cat(crayon::green("Renviron is configured, nothing to do.\n"))
      cat(crayon::green("Folders exists, nothing to do.\n"))
    } else {
      cat(crayon::red("Folders not found where expected
                    from your .Renviron file.\n"))
      cat("Please check that the paths are corrects using:\n")
      cat(crayon::white$italic("Sys.getenv()['ONEDRIVE']\n"))
      cat(crayon::white$italic("Sys.getenv()['DATASETS']\n"))
      cat("If necessary, correct the paths in:\n", blue(rEnvironPath), "\n")
      modifyPath("OneDrive", Sys.getenv()['ONEDRIVE'])
      modifyPath("datasets", Sys.getenv()['DATASETS'])
    }
  } else {
    cat(crayon::red(".Renviron is NOT configured.\n"))
    if(file.exists(rEnvironPath)) {
      modifyPath("OneDrive", Sys.getenv()['ONEDRIVE'])
      modifyPath("datasets", Sys.getenv()['DATASETS'])
    } else {
      cat(crayon::red$bold(".Renviron file was not found, aborting.\n"))
      cat(crayon::red$bold("Try to enter it manually.\n"))
      question <- paste0("What is the path to your .Renviron file?\n")
      rEnvironPath <- readline(prompt = question)
      modifyPath("OneDrive", Sys.getenv()['ONEDRIVE'])
      modifyPath("datasets", Sys.getenv()['DATASETS'])
    }
  }

  if(file.exists(rProfilePath)) {
    existence <- parseFileFor(rProfilePath, pattern = "Welcome to the ANPC environment")
    if (!existence) {
      choice <- menu(c(crayon::green("Yes"),
                       crayon::red("no")),
                     title = "Would you allwow me to update your .Rprofile file?\n")
      switch(choice,
             updateProfile(),
             cat("Nothing done.\n"))
    }
  } else {
    updateProfile()
  }
}





