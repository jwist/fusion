#' function to make ID unique
#'
#' @param da sampleID
#' @param sep the separator
#' @return unique ID
#'
#' @example
#' a <- c("a", "a", "b", "a", "b", "c")
#'
#' @export
makeUnique <- function(sampleID, sep = "#"){
  dup <- duplicated(sampleID) | duplicated(sampleID, fromLast = TRUE)
  i = 1
  while (sum(dup) > 0) {
    newName <- unlist(lapply(sampleID[dup], function(x) strsplit(x, sep)[[1]][1]))
    sampleID[dup] <- paste0(newName, sep , i)
    dup <- duplicated(sampleID)
    i <- i + 1
  }
  return(sampleID)
}
