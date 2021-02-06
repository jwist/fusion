#' cbind dataElements
#'
#' @param ... a list of dataElements
#' @return the information about dataElement
#'
#' @export rbind.dataElement
#' @export
#' @importFrom methods hasArg

cbind.dataElement <- function(...){
  if (!all(sapply(list(...), function(x) is(x)[1] == "dataElement"))) {
    stop(crayon::red("fusion::rbind >>
Some are not dataElements"))
  }

  if (length(unique(sapply(list(...), function(x) x@type))) > 1) {
    stop(crayon::red("fusion::rbind >>
All elements must be of same type"))
  }
  type <- list(...)[[1]]@type

  if (type != "ANN") {
    if (length(unique(sapply(list(...), function(x) x@method))) > 1) {
      stop(crayon::red("fusion::rbind >>
All elements must be of same method"))
    }
  }
  method <- list(...)[[1]]@method

  IDs <- getID(list(...)[[1]])
  if (!all(sapply(list(...),
                  function(x) identical(getID(x), IDs)))) {
    stop(crayon::red("fusion::rbind >>
All must have identical IDs"))
  }

  newData <- list()
  newObs <- list()
  counter <- 1

  for (el in list(...)) {
    if (el@type != "ANN") {

      newData[[counter]] <- el@.Data

    }
    newObs[[counter]] <- el@obsDescr
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

