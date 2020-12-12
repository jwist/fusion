#' make a vector of character unique
#'
#' matching different dataElements is only possible if all possess
#' a vector with unique IDs, sampleID in its experimentalParameters.
#' If this vector is not unique, due to the presence of quality
#' controls or reference samples, then this function makes it unique
#' by numbering all duplicates
#'
#' @param sampleID - a vector of characters
#' @return a vector of unique characters
#' @import methods
#' @export

makeUnique <- function(sampleID){
  if (is(sampleID, "character")) {
    dup <- duplicated(sampleID)
    i = 2
    while (sum(dup) > 0) {
      newName <- unlist(lapply(sampleID[dup], function(x) strsplit(x, "_")[[1]][1]))
      sampleID[dup] <- paste0(newName, "_", i)
      dup <- duplicated(sampleID)
      i <- i + 1
    }
    return(sampleID)
  } else {
    stop("fusion::makeUnique - only applies to vector of type character")
  }
}
