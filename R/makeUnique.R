#' function to make ID unique
#'
#' @param sampleID sampleID
#' @param sep the separator
#' @param fromFirst if true all replicate including the first will be marked
#' @param reverse if true start marking from last
#' @param first a number to start numbering from
#' @return unique ID
#'
#' @examples
#' a <- c("a", "a", "b", "a", "b", "c")
#' makeUnique(a)
#'
#' @export
makeUnique <- function(sampleID,
                       sep = "#",
                       fromFirst = FALSE,
                       reverse = FALSE,
                       first = 1){
  if (fromFirst) {
    dup <-  duplicated(sampleID) |
      duplicated(sampleID, fromLast = TRUE)
  } else {
    dup <- duplicated(sampleID)
  }
  if (reverse) {
    sampleID <- rev(sampleID)
  }
  i <- first
  while (sum(dup) > 0) {
    newName <- unlist(lapply(sampleID[dup],
                             function(x) strsplit(x, sep)[[1]][1]))
    sampleID[dup] <- paste0(newName, sep , i)
    dup <- duplicated(sampleID)
    i <- i + 1
  }
  if (reverse) {
    sampleID <- rev(sampleID)
  }
  return(sampleID)
}
