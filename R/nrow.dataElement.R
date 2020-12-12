
#' S3 method nrow for dataElement objects (S4)
#'
#' A nrow method for \emph{dataElement} objects
#'
#' @param da a dataElement
#' @param ... parameters of the nrow generic function
#' @return the number of rows
#'
#' @export

nrow.dataElement <- function(da, ...){
  if (da@type == 'ANN') {
    n <- nrow(da@obsDescr)
  } else {
    n <- nrow(da)
  }
  return(n)
}
