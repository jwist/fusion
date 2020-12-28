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
  if (hasArg(translate)) {
    if (list(...)$translate == TRUE){
      cat("bla")
    }
  }
  return(txt)
}

