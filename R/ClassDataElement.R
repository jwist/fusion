#' An S4 class for dataElement
#'
#' @slot .Data the data matrix
#' @slot varName a vector containing the name of each variable
#' @slot obsDescr a data.frame containing experimental
#' conditions and a field called sampleID that MUST be unique.
#' @slot method a name for the method used to acquire the data
#' @slot type type can be NMR, MS-U, MS-T, ANN
#' @return a dataElement
#' @examples
#'
#' x = matrix(sample(100), 10, 10)
#' a = new("dataElement", x, varName = as.character(rep(1, 10)), type = "MS", method = "tryptophan")
#'
#' @export
setClass("dataElement",
         representation = representation(.Data = "matrix",
                      varName = "character",
                      obsDescr = "list",
                      type = "character",
                      method = "character"),
         prototype(varName = NA_character_,
                   obsDescr = data.frame(),
                   method = NA_character_,
                   type = NA_character_),
         validity = function(object) {
           if (is.na(object@type)) {
             stop("a type must be given")
           }
           if (!object@type %in% c("NMR", "MS", "T-MS", "ANN", "IVDR", "LIPO")) {
             stop("unsupported type")
           }
           if (object@type == "NMR" & is.na(object@method)) {
             stop("fusion: NMR dataElement must contain a method Use meltdown()
                  to check for valid types")
           }
           if (object@type == "MS" & is.na(object@method)) {
             stop("fusion: MS dataElement must contain a method Use meltdown()
                  to check for valid types")
           }
           ann <- object@obsDescr[[1]]
           if ("sampleID" %in% names(ann)) {
             if (sum(duplicated(ann$sampleID)) > 0) {
               stop("the sampleID are not unique, run
                   'duplicated(sampleID)' to find dups or
                   make.unique(x, '_') to solve the issue")
             }
           } else {
             stop("the metadata must contain a column: sampleID")
           }
           if (object@type != "ANN") {
             if ("sampleType" %in% names(ann)) {
               cat(crayon::green(names(table(ann$sampleType))) %+%
                     crayon::green(": ") %+%
                     crayon::green(table(ann$sampleType)), fill = 2)
             } else {
               stop("the metadata must contain a column: sampleType")
             }
           }
           TRUE
         },
         contains = list("matrix")
)
