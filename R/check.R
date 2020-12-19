#' S4 method to check dataElement
#'#'
#' @param da dataElement object to be checked
#' @param using the name of the column used as UID
#' @return void
#'
#' check("dataElement")
#'
#' @export
setGeneric("check", function(da, using = "sampleID") {
  standardGeneric("check")
})

#' SS4 method to check dataElement
#'
#' @param da dataElement object to be checked
#' @param using the name of the column used as UID
#' @return void
#'
#' check("dataElement")
#'
#' @export
setMethod("check",
          c(da = "dataElement"),
          function(da, using = "sampleID") {
            if (da@type == "ANN") {
              if (is.null(da@.Data)) {
                stop("dataElement of type ANN cannot contain data")
              }
              if (!using %in% names(da@obsDescr)) {
                stop("obsDescr MUST contain a field with unique ID")
              }
            } else {
              if (!"sampleID" %in% names(da@obsDescr)) {
                stop("obsDescr MUST contain a field 'sampleID'")
              }
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
