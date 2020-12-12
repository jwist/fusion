#' function to match two dataElement pairwise
#'
#' @param daA first dataElement
#' @param daB second dataElement
#' @return two dataElement with matched samples
#'
#' @export
matchPairwise <- function(daA, daB) {

    A <- matchTo(daA, daB)
    B <- matchTo(daB, A)

    return(list(A, B))
}
