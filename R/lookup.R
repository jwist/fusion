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
  uid <- unlist(da@obsDescr[[1]][using[1]])
  fi <- match(uid,
              unlist(ann[using[2]]))
  if(sum(is.na(fi)) > 0) {
    txt = paste(uid[which(is.na(fi))])
    warning(paste("fusion: NA found, check:", txt, "\n"))
  }
  newAnn <- cbind(sampleID = da@obsDescr[[1]]$sampleID, ann[fi,])
  sampleAnnotations <-new("dataElement",
                          obsDescr = list(newAnn),
                          type = "ANN")
  return(sampleAnnotations)
}
