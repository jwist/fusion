#' S4 method to reorder a dataElement
#'
#' a dataElement is ordered using the index provided as second
#' argument. This is to ensure that both the .Data and the
#' experimentalParameters slots are rotated accordingly
#'
#' @param da first dataElement
#' @param idx the index
#' @return the reordered dataElement
#'
#' @export
setGeneric("orderWith", function(da, idx) {
  standardGeneric("orderWith")
})

#' S4 method to reorder a dataElement
#'
#' @param da first dataElement
#' @param idx the index
#' @return the reordered dataElement
#'
#' @export
setMethod("orderWith",
          c(da = "dataElement", idx = "numeric"),
          function(da, idx) {
            if (length(idx) == nrow(da@obsDescr)) {
              if (da@type != "ANN") {
                da <- setDataPart(da, getDataPart(da)[idx,, drop = FALSE])
              }
              da@obsDescr <- da@obsDescr[idx,, drop = FALSE]
              return(da)
            } else {
              stop("dimension mismatch")
            }
          }
)
