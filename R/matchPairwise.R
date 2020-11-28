#' function to match two dataElement pairwise
#'
#' @param daA first dataElement
#' @param daB second dataElement
#' @return two dataElement with matched samples
#'
#' @export
matchPairwise <- function(daA, daB) {

    A <- matchWith(daA, daB)
    B <- matchWith(daB, A)

    return(list(A, B))
}
