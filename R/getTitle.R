#' extract title from a bruker folder
#'
#' @param filePath - the path to the expName folder
#' @return the title
#'
#' @export

getTitle <- function(filePath){
  if (file.exists(filePath)) {
    buf <- file(filePath, open = "r")
    txt <- readLines(buf, n = -1, warn = FALSE)
    close(buf)
    return(txt)
  } else {
    return("no title")
  }
}

