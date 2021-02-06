#' print information for dataElement objects
#'
#' @param x dataElement
#' @param ... other parameters
#' @return the information about dataElement
#'
#' @export
#' @importFrom methods hasArg

print.dataElement <- function(x, ...){
  txt <- "blabla"
  if (hasArg("translate")) {
      cat("bla")
  }
  return(txt)
}

