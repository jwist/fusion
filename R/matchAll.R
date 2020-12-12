#' function to match all dataElements
#'
#' @param list list of dataElements
#' @return all dataElements matched by sampleID
#'
#' @export
matchAll <- function(list) {
  res <- lapply(list[-1],
                function(x) matchPairwise(list[[1]], x)[[2]])
  firstEl <- matchPairwise(list[[1]], res[[1]])[1]
  res <- c(firstEl, res)
  return(res)

}
