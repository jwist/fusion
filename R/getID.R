#' S4 method get sampleID from dataElement
#'
#' @param da dataElement
#' @return the sampleID
#'
#' @export
#'
setGeneric("getID", function(da, using = "sampleID") {
  standardGeneric("getID")
})

#' S4 method get sampleID from dataElement
#'
#' @param da dataElement
#' @return the sampleID
#'
#' @export
#'
setMethod("getID",
          c(da = "dataElement"),
          function(da, using = "sampleID") {
            sampleID <- unlist(unname(da@obsDescr[using]))
            return(sampleID)
          }
)
