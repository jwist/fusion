#' An S4 class for dataElement
#'
#' @slot .Data the data matrix
#' @slot varName a vector containing the name of each variable
#' @slot obsDescr a data.frame containing experimental
#' conditions and a field called sampleID that MUST be unique.
#' @slot method a name for the method used to acquire the data
#' @slot type type can be NMR, MS-U, MS-T, ANN
#' @return a dataElement
#' @export
#' @importFrom crayon %+%
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
             stop(crayon::red("fusion:ClassDataElement >>
a type must be given"))
           }
           if (!object@type %in% c("NMR", "MS", "T-MS", "ANN", "IVDR", "LIPO")) {
             stop(crayon::red("fusion:ClassDataElement >>
unsupported type"))
           }
           if (object@type == "NMR" & is.na(object@method)) {
             stop(crayon::red("fusion:ClassDataElement >>
fusion: NMR dataElement must contain a method Use meltdown()
to check for valid types"))
           }
           if (object@type == "MS" & is.na(object@method)) {
             stop(crayon::red("fusion:ClassDataElement >>
fusion: MS dataElement must contain a method Use meltdown()
to check for valid types"))
           }
           if (object@type == "ANN" & any(!is.na(object@method),
               !is.na(object@varName), !is.na(object@.Data))) {
             stop(crayon::red("fusion:ClassDataElement >>
fusion: ANN dataElement must not contain a method, a varName or .Data"))
           }
           if (!is.list(object@obsDescr)){
             stop(crayon::red("fusion:ClassDataElement >>
fusion: obsDescr must be of type list"))
           }
           meta <- object@obsDescr[[1]]
           if ("sampleID" %in% names(meta)) {
             if (object@type != "ANN") {
               fi <- getType(object) == "sample"
               ids <- getID(object)[fi]
             } else {
               ids <- getID(object)
             }
             if (sum(duplicated(ids)) > 0) {
               stop(crayon::red("fusion:ClassDataElement >>
the sampleID (fpr samples) are not unique"))
             }
           } else {
             stop(crayon::red("fusion:ClassDataElement >>
the metadata must contain a column: sampleID"))
           }
           if (object@type != "ANN") {
             if ("sampleType" %in% names(meta)) {
               cat(crayon::green(names(table(meta$sampleType))) %+%
                     crayon::green(": ") %+%
                     crayon::green(table(meta$sampleType)), fill = 2)
             } else {
               stop(crayon::red("fusion:ClassDataElement >>
the metadata must contain a column: sampleType"))
             }
           }
           return(check(object))
         },
         contains = list("matrix")
)
