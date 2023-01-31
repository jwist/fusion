#' S4 method to select from a dataElement
#'
#' @param da first dataElement
#' @param idx the filter
#' @return the selected row of dataElement
#'
#' @export
setGeneric("selectWith", function(da, idx) {
  standardGeneric("selectWith")
})

#' S4 method to filter a dataElement
#'
#' @param da first dataElement
#' @param idx the index for selection
#' @return the reordered dataElement
#'
#' @export
setMethod("selectWith",
          c(da = "dataElement", idx = "numeric"),
          function(da, idx) {

            if (da@type != "ANN") {
              da <- setDataPart(da, getDataPart(da)[idx,, drop = FALSE])
            }
            for (i in 1:length(da@obsDescr)) {
              da@obsDescr[[i]] <- da@obsDescr[[i]][idx,, drop = FALSE]
            }
            return(da)

          }
)
