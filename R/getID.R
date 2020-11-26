#' S4 method get sampleID from dataElement
#'
#' @param da dataElement
#' @return the sampleID
#' @example
#'
#' @export
setGeneric("getID", function(da) {
  standardGeneric("getID")
})

#' S4 method get sampleID from dataElement
#'
#' @param da dataElement
#' @return the sampleID
#' @example
#'
#' @export
setMethod("getID",
          c(da = "dataElement"),
          function(da) {
            sampleID <- da@experimentalParameter$sampleID
            return(sampleID)
          }
)
