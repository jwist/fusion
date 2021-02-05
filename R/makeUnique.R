#' function to make ID unique
#'
#' @param da sampleID
#' @param sep the separator
#' @return unique ID
#'
#' @export
makeUnique <- function(sampleID, sep = "#"){
  dup <- duplicated(sampleID)
  i = 1
  while (sum(dup) > 0) {
    newName <- unlist(lapply(sampleID[dup], function(x) strsplit(x, "_")[[1]][1]))[1]
    print(newName)
    adam <- which(sampleID == newName)[1]
    if (length(adam) > 0) {
      sampleID[adam] <- paste0(newName, "_", 0)
    }
    sampleID[dup] <- paste0(newName, "_", i)
    dup <- duplicated(sampleID)
    i <- i + 1
  }
  return(sampleID)
}
