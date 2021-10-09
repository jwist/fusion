#' convert values to significance
#' @param val - a vector of values
#' @return a vector with significance text
#' @export
numToSignificance <- function(val) {
  newCol <- lapply(val, function(i) {
    if (!is.na(i)){
      i <- abs(as.numeric(i))
      if (i < 0.05 & i >= 0.01) {"*"
      } else if (i < 0.01 & i >= 0.001) {
        "**"
      } else if (i < 0.001) {
        "***"
      } else {
        "NS"
      }
    } else {
      NA
    }
  })
  return(unlist(newCol))
}
