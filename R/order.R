#' S4 method to reorder a dataElement
#'
#' @param daA first dataElement
#' @param idx the index
#' @return the reordered dataElement
#' @example
#'
#' @export
setGeneric("order", function(da) {
  standardGeneric("order")
})

#' S4 method to reorder a dataElement
#'
#' @param daA first dataElement
#' @param idx the index
#' @return the reordered dataElement
#' @example
#'
#' @export
setMethod("order",
          c(da = "dataElement"),
          function(da) {
            if (length(idx) == nrow(da)) {
              setDataPart(da) <- getDataPart(da)[idx,]
              da@experimentalParameter <- da@experimentalParameter[idx,]
            }
            return(da)
          }
)
