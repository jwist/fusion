#' S4 method to check dataElement
#'#'
#' @param da dataElement object to be checked
#' @return void
#'
#' check("dataElement")
#'
#' @export
setGeneric("check", function(da) {
  standardGeneric("check")
})

#' SS4 method to check dataElement
#'
#' @param da dataElement object to be checked
#' @return void
#'
#' check("dataElement")
#'
#' @export
setMethod("check",
          c(da = "dataElement"),
          function(da) {
            if (length(da@varName) != ncol(da@.Data)) {
              stop("the length of variableName vector does't
              match the number of columns of the data matrix")
            }
            if (!"sampleID" %in% names(da@obsDescr)) {
              stop("experimentalParameter MUST contain a field 'sampleID'")
            }
            if (length(da@obsDescr$sampleID) != nrow(da@.Data)) {
              stop("the length of sampleID vector does't
              match the number of rows of the data matrix")
            }
            if (sum(duplicated(da@obsDescr$sampleID)) > 0) {
              stop("the sampleID are not unique, run
                   'duplicated(sampleID)' to find dups")
            }
            if (!da@type %in% c("NMR", "MS", "ANN")) {
              stop("un supported type")
            }
            return(da)
          }
)
