#' extract a parameter from a bruker file (procs or acqus)
#'
#' @param path - the path to the expName folder
#' @param paramName - the name of the parameter to read
#' @return the parameter
#'
#' @export
readParam <- function(path, paramName){
  buf <- file(path, open = "r")
  txt <- readLines(buf, n = -1, warn = FALSE)
  if (length(txt) == 0) {
    cat(crayon::red("fusion::readParam file", paramName, "is empty\n"))
    return(NULL)
  }
  close(buf)
  parameter <- strsplit(txt[grep(paste0(paramName, "="), txt)], "=")[[1]][2]
  if (grepl("<", parameter) == TRUE) {
    return(gsub(" ", "", (gsub("<", "", (gsub(">", "", parameter))))))
  } else {
    return(as.numeric(parameter))
  }
}
