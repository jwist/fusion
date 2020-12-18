#' An S4 class for dataElement
#'
#' @slot .Data the data matrix
#' @slot varName a vector containing the name of each variable
#' @slot obsDescr a data.frame containing experimental
#' conditions and a field called sampleID that MUST be unique.
#' @slot type type can be NMR, MS-U, MS-T, ANN
#' @return a dataElement
#' @examples
#'
#' x = matrix(sample(100), 10, 10)
#' a = new("dataElement", x, varName = as.character(rep(1, 10)), type = "MS")
#'
#' @export
setClass("dataElement",
         slots = list(.Data = "matrix",
                      varName = "character",
                      obsDescr = "data.frame",
                      type = "character"),
         prototype = NULL,
         contains = list("matrix")
)
