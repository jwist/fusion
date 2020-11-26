#' S4 method get sampleID from dataElement
#'
#' @slot da dataElement
#' @return the sampleID
#'
#' @export
#'
setGeneric("getID", function(da) {
  standardGeneric("getID")
})

#' S4 method get sampleID from dataElement
#'
#' @slot da dataElement
#' @return the sampleID
#'
#' @export
#'
setMethod("getID",
          c(da = "dataElement"),
          function(da) {
            sampleID <- da@experimentalParameter$sampleID
            return(sampleID)
          }
)
