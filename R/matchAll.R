#' function to match all dataElements
#'
#' @param list list of dataElements
#' @return all dataElements matched by sampleID
#'
#' @export
matchAll <- function(list) {
  firstMatch <- matchPairwise(list[[1]], list[[2]])
  res <- lapply(list[-c(1, 2)],
                function(x) matchPairwise(firstMatch[[1]], x)[[2]])
  res <- c(firstMatch, res)
  return(res)

}
