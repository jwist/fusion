#' function to match all dataElement
#'
#' @param list list of dataElement
#' @return all dataElement matched by sampleID
#'
#' @export
matchAll <- function(list) {

  chkClass <- lapply(list, function(x) is(x, "dataElement"))
  if (sum(unlist(chkClass)) == length(list)) {
    fi <- match(getID(daA), getID(daB))

    # check for dimensions
    ldiff <- abs(nrow(daB) - length(fi))

    # check for diffs (NA)
    fn <- is.na(fi)
    if (sum(fn) > 0) {
      daA <- filterWith(daA, !fn)
      ldiff <- ldiff + sum(fn)
    }

    if (ldiff != 0) {
      warning(paste("fusion::matchPairwise -",
                    ldiff ,
                    "sample(s) dropped"))
    }

    # sort and order
    idx <- sort(fi, index.return = TRUE)$ix
    daA <- orderWith(daA, idx)
    return(daA)
  } else {
    stop("fusion: all objects to match must be of class dataElement")
  }
}
