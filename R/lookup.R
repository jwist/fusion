#' function to lookup annotations for each samples
#'
#' @param da a dataElement
#' @param ann a data.frame with annotations
#' @param using the name of the column to lookup
#' @return a dataElement with matched samples
#'
#' @export
#' @importFrom methods getDataPart is new setDataPart
#'
lookup <- function(da, ann, using = c("sourceID", "sampleID")) {
  check(da, using = "sampleID")
  fi <- match(getID(da, using[1]),
              ann[using[2]])
  sampleAnnotations <-new("dataElement",
                          obsDescr = ann[fi,])
  return(sampleAnnotations)
}
