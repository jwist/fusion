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
            if (!"sampleID" %in% names(da@obsDescr)) {
              stop("obsDescr MUST contain a field 'sampleID'")
            }
            if (sum(duplicated(da@obsDescr$sampleID)) > 0) {
              stop("the sampleID are not unique, run
                   'duplicated(sampleID)' to find dups or
                   make.unique(x, '_') to solve the issue")
            }
            if (!da@type %in% c("NMR", "MS", "ANN")) {
              stop("unsupported type")
            }
            if (da@type == "ANN") {
              if (is.null(da@.Data)) {
                stop("dataElement of type ANN cannot contain data")
              }
            } else {
              if (length(da@varName) != ncol(da@.Data)) {
                stop("the length of varName vector does't
              match the number of columns of the data matrix")
              }
              if (length(da@obsDescr$sampleID) != nrow(da@.Data)) {
                stop("the length of sampleID vector does't
              match the number of rows of the data matrix")
              }
            }
            return(TRUE)
          }
)
