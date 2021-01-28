#' print information for dataElement objects
#'
#' @return the information about dataElement
#'
#' @export rbind.dataElement
#' @export
#' @importFrom methods hasArg

rbind.dataElement <- function(...){
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
  for (obs in 1:length(el@obsDescr)) {
    obsDescr[[obs]] <- do.call("rbind", do.call("rbind", newObs)[,obs])
  }

  if (el@type != "ANN") {
    .Data <- do.call("rbind", newData)
    newElement <- new("dataElement",
                      .Data = .Data,
                      obsDescr = obsDescr,
                      varName = el@varName,
                      type = type,
                      method = method)
  } else {
    newElement <- new("dataElement",
                      obsDescr = list(obsDescr),
                      type = type,)
  }

  return(newElement)
}

