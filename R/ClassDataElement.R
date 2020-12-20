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
         representation = representation(.Data = "matrix",
                      varName = "character",
                      obsDescr = "data.frame",
                      type = "character"),
         prototype(varName = NA_character_,
                   obsDescr = data.frame(),
                   type = NA_character_),
         validity = function(object) {
           if (sum(duplicated(object@obsDescr$sampleID)) > 0) {
             stop("the sampleID are not unique, run
                   'duplicated(sampleID)' to find dups or
                   make.unique(x, '_') to solve the issue")
           }
           if (is.na(object@type)) {
             stop("a type must be given")
           }
           if (!object@type %in% c("NMR", "MS", "MS-T", "ANN", "IVDR", "LIPO")) {
             stop("unsupported type")
           }
           TRUE
         },
         contains = list("matrix")
)
