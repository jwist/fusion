#' cbind dataElements
#'
#' @param ... a list of dataElements
#' @return the information about dataElement
#'
#' @export rbind.dataElement
#' @export
#' @importFrom methods hasArg

cbind.dataElement <- function(...){
  newData <- list()
  newObs <- list()
  type <- method <- "init"
  counter <- 1
  for (el in list(...)) {
    if (is(el)[1] != "dataElement"){
      warning(paste("fusion: ", el, " is not a dataElement and is ignored"))
    } else {
      if (el@type != type & type != "init") {
        stop("fusion: all elements must be of same type")
      } else  {

        if (el@type != "ANN") {
          if (el@method != method & method != "init") {
            stop("fusion: all elements must be of same method")
          } else {
            newData[[counter]] <- el@.Data
            newObs[[counter]] <- el@obsDescr
          }
        }
        method = el@method
      }
      type = el@type
    }
    counter <- counter + 1
  }

  obsDescr <- list()
  newDescr <- do.call("c", newObs)

  if (el@type != "ANN") {
    .Data <- do.call("cbind", newData)
    newElement <- new("dataElement",
                      .Data = .Data,
                      obsDescr = newDescr,
                      varName = el@varName,
                      type = type,
                      method = method)
  } else {
    newElement <- new("dataElement",
                      obsDescr = newDescr,
                      type = type,)
  }

  return(newElement)
}

