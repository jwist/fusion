#' function to match two dataElement
#'
#' @param daA first dataElement
#' @param daB second dataElement
#' @return two dataElement with matched samples
#'
#' @export
matchPairwise <- function(daA, daB) {
  if (class(daA) == class(daB) & class(daA) == "dataElement") {
    fi <- match(getID(daA), getID(daB))

    # check for diffs (NA)
    fn <- is.na(fi)
    daA <- filterWith(daA, !fn)

    # sort and order
    idx <- sort(fi, index.return = TRUE)$ix
    daA <- orderWith(daA, idx)
    return(daA)
  } else {
    stop("fusion: both objects to match must be of class dataElement")
  }
}
