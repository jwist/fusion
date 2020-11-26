#' An S4 class for dataElement
#'
#' @slot .Data the data matrix
#' @slot variableName a vector containing the name of each variable
#' @slot experimentalParameter a data.frame containing experimental
#' conditions and a field called sampleID that MUST be unique.
#' @slot type type can be NMR, MSU, MST, ANN
#' @slot name the name of this dataElement
#' @return a dataElement
#' @examples
#'
#' x = matrix(sample(100), 10, 10)
#' a = new("dataElement", x, variableName = rep(1, 10), type = "MS")
#'
#' @export

setClass("dataElement",
         slots = list(.Data = "matrix",
                      variableName = "numeric",
                      experimentalParameters = "data.frame",
                      type = "character"),
         contains = list("matrix")
)

setGeneric("check", function(da) {
  standardGeneric("check")
})

setMethod("check",
          c(da = "dataElement"),
          function(da) {
            if (length(da@variableName) != ncol(da@.Data)) {
              stop("the length of variableName vector does't
              match the number of columns of the data matrix")
            }
            if (!"sampleID" %in% names(da@experimentalParameter)) {
              stop("experimentalParameter MUST contain a field 'sampleID'")
            }
            if (length(da@experimentalParameter$sampleID) != nrow(da@.Data)) {
              stop("the length of sampleID vector does't
              match the number of rows of the data matrix")
            }
            if (sum(duplicated(da@experimentalParameter$sampleID)) > 0) {
              stop("the sampleID are not unique, run
                   'duplicated(sampleID)' to find dups")
            }
           return(da)
          }
)
