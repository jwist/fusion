#' extract a parameter from a bruker folder
#'
#' @param filePath - the path to the expName folder
#' @param paramName - the name of the parameter to read
#' @return the parameter
#'
#' @export

getParam <- function(filePath, paramName){
  buf <- file(filePath, open = "r")
  txt <- readLines(buf, n = -1, warn = FALSE)
  close(buf)
  parameter <- strsplit(txt[grep(paramName, txt)], "=")[[1]][2]
  if (grepl("<", parameter) == TRUE) {
    return(gsub(" ", "", (gsub("<", "", (gsub(">", "", parameter))))))
  } else {
    return(as.numeric(parameter))
  }
}

