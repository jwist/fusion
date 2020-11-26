#' S4 method to reorder a dataElement
#'
#' @param daA first dataElement
#' @param idx the index
#' @return the reordered dataElement
#'
#' @export
setGeneric("orderWith", function(da, idx) {
  standardGeneric("orderWith")
})

#' S4 method to reorder a dataElement
#'
#' @param daA first dataElement
#' @param idx the index
#' @return the reordered dataElement
#'
#' @export
setMethod("orderWith",
          c(da = "dataElement", idx = "numeric"),
          function(da, idx) {
            if (length(idx) == nrow(da)) {
              setDataPart(da, getDataPart(da)[idx,, drop = FALSE])
              da@experimentalParameter <- da@experimentalParameter[idx,, drop = FALSE]
              return(da)
            }
          }
)
