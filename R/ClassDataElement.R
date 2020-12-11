#' An S4 class for dataElement
#'
#' @slot .Data the data matrix
#' @slot varName a vector containing the name of each variable
#' @slot obsDescr a data.frame containing experimental
#' conditions and a field called sampleID that MUST be unique.
#' @slot type type can be NMR, MSU, MST, ANN
#' @slot name the name of this dataElement
#' @return a dataElement
#' @examples
#'
#' x = matrix(sample(100), 10, 10)
#' a = new("dataElement", x, varName = rep(1, 10), type = "MS")
#'
#' @export
setClass("dataElement",
         slots = list(.Data = "matrix",
                      varName = "numeric",
                      obsDescr = "data.frame",
                      type = "character"),
         contains = list("matrix")
)
