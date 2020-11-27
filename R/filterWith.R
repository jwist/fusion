#' S4 method to filter a dataElement
#'
#' @param daA first dataElement
#' @param fi the filter
#' @return the reordered dataElement
#'
#' @export
setGeneric("filterWith", function(da, fi) {
  standardGeneric("filterWith")
})

#' S4 method to filter a dataElement
#'
#' @param daA first dataElement
#' @param fi the filter
#' @return the reordered dataElement
#'
#' @export
setMethod("filterWith",
          c(da = "dataElement", fi = "logical"),
          function(da, fi) {
            if (length(fi) == nrow(da)) {
              da <- setDataPart(da, getDataPart(da)[fi,, drop = FALSE])
              da@experimentalParameter <- da@experimentalParameter[fi,, drop = FALSE]
              return(da)
            } else {
              stop("dimension mismatch")
            }
          }
)
