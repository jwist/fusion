#' function to match one dataElement against a second one
#'
#' @param daA first dataElement
#' @param daB second dataElement
#' @return dataElement with matched samples
#'
#' @export
matchTo <- function(daA, daB, using = "sampleID") {
  check(daA, using)
  check(daB, using)
  if (is(daA, "dataElement") == is(daB, "dataElement")) {

    fi <- match(getID(daA, using), getID(daB, using))

    # check for diffs (NA)
    fn <- is.na(fi)
    if (sum(fn) > 0) {
      daA <- filterWith(daA, !fn)
    }

    ldiff <-  sum(fn)
    if (ldiff != 0) {
      warning(paste("fusion::matchWith -",
                    ldiff ,
                   "sample(s) dropped"))
    }

    # sort and order
    idx <- sort(fi, index.return = TRUE)$ix
    daA <- orderWith(daA, idx)

    return(daA)

  } else {
    stop("fusion: both objects to match
         must be of class dataElement")
  }
}
