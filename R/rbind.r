#' rbind dataElements
#'
#' @param ... a list of dataElements
#' @return the information about dataElement
#'
#' @export rbind.dataElement
#' @export
#' @importFrom methods hasArg
#' @importFrom crayon %+%

rbind.dataElement <- function(...){
  newData <- list()
  newObs <- list()
  type <- method <- "init"
  counter <- 1
  for (el in list(...)) {
    if (is(el)[1] != "dataElement"){
      stop(crayon::red("fusion::rbind >> Some are not dataElements"))
    } else {
      if (el@type != type & type != "init") {
        stop(crayon::red("fusion::rbind >> All elements must be of same type"))
      } else  {

        if (el@type != "ANN") {
          if (el@method != method & method != "init") {
            cat(crayon::red("fusion::rbind >> All elements must be of same method"))
            cat(crayon::red("fusion::rbind >> CHECK THAT THOSE DATA ARE COMPATIBLE"))
            newData[[counter]] <- el@.Data
          } else {
            newData[[counter]] <- el@.Data
          }
        }
        newObs[[counter]] <- el@obsDescr
        method = el@method
      }
      type = el@type
    }
    counter <- counter + 1
  }

  obsDescr <- list()
  for (obs in 1:length(el@obsDescr)) {
    newDescr <- do.call("rbind", do.call("rbind", newObs)[,obs])
    newDescr$sampleID <- makeUnique(newDescr$sampleID, sep = "#")
    obsDescr[[obs]] <- newDescr
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
                      obsDescr = obsDescr,
                      type = type,)
  }

  return(newElement)
}

