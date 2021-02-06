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
              if (!using %in% names(da@obsDescr[[1]])) {
                stop(crayon::red("fusion::check >>
obsDescr MUST contain a field with unique ID"))
              }
            } else {
              if (!"sampleID" %in% names(da@obsDescr[[1]])) {
                stop(crayon::red("fusion::check >>
obsDescr MUST contain a field 'sampleID'"))
              }
              if (length(da@varName) != ncol(da@.Data)) {
                stop(crayon::red("fusion::check >>
the length of varName vector does't
match the number of columns of the data matrix"))
              }
              if (length(da@obsDescr[[1]]$sampleID) != nrow(da@.Data)) {
                stop(crayon::red("fusion::check >>
the length of sampleID vector does't
match the number of rows of the data matrix"))
              }
            }
            return(TRUE)
          }
)
