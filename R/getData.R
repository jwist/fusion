#' S4 method get data from dataElement
#'
#' @param da dataElement
#' @param using either sample, qc, standard, or blank or a combination of
#' @return a dataElement with selected types
#'
#' @export
#'
setGeneric("getData", function(da, type) {
  standardGeneric("getData")
})

#' S4 method get data from dataElement
#'
#' @param da dataElement
#' @param using either sample, qc, standard, or blank or a combination of
#' @return a dataElement with selected types
#'
#' @export getData
#' @export
setMethod("getData",
          c(da = "dataElement", type = c()),
          function(da, type = c("sample")) {
            fi <- getType(da) == type
            newDa <- filterWith(da, fi)
            return(newDa)
          }
)

