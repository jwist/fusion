#' S4 method get sampleType from dataElement
#'
#' @param da dataElement
#' @param using the name of the column used as sampleType
#' @return the sampleType
#'
#' @export
#'
setGeneric("getType", function(da, using = "sampleType") {
  standardGeneric("getType")
})

#' S4 method get sampleType from dataElement
#'
#' @param da dataElement
#' @param using the name of the column used as sampleType
#' @return the sampleType
#'
#' @export
#'
setMethod("getType",
          c(da = "dataElement"),
          function(da, using = "sampleType") {
            sampleType <- unlist(unname(da@obsDescr[[1]][using]))
            cat(crayon::green(names(table(sampleType))) %+%
                  crayon::green(": ") %+%
                  crayon::green(table(sampleType)), fill = 2)
            return(sampleType)
          }
)
