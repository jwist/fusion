#' S4 method to reorder a dataElement
#'
#' @param daA first dataElement
#' @param idx the index
#' @return the reordered dataElement
#'
#' @export
setGeneric("order", function(da, idx) {
  standardGeneric("order")
})

#' S4 method to reorder a dataElement
#'
#' @param daA first dataElement
#' @param idx the index
#' @return the reordered dataElement
#'
#' @export
setMethod("order",
          c(da = "dataElement", idx = "numeric"),
          function(da, idx) {
            if (length(idx) == nrow(da)) {
              setDataPart(da, getDataPart(da)[idx,])
              da@experimentalParameter <- da@experimentalParameter[idx,]
              return(da)
            }
          }
)
