#' print information for dataElement objects
#'
#' @param ... a sequence of dataElements
#' @return the information about dataElement
#'
#' @export
#' @importFrom methods hasArg

rbind.dataElement <- function(...){
  newData <- list()
  newObs <- list()
  type <- method <- "init"
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
            newData <- c(newData, el@.Data)
          }
          method = el@method
        }
        if (identical())
          if (el@type != "T-MS") {
            for (obs in el@obsDescr) {
              newObs <- c(newObs, el@obsDescr[[1]])
            }
          } else {
            newObs <- c(newObs, el@obsDescr[[1]])
          }
      }
      type = el@type
    }
  }
  obsDescr <- do.call("rbind", newObs)
  if (el@type != "ANN") {
    .Data <- do.call("rbind", newData)
    newElement <- new("dataElement",
                      .Data = .Data,
                      obsDescr = list(obsDescr),
                      varName = varName,
                      type = type,
                      method = method)
  } else {
    newElement <- new("dataElement",
                      obsDescr = obsDescr,
                      type = type,)
  }

  return(newElement)
}

