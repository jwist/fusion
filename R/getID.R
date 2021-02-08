#' S4 method get sampleID from dataElement
#'
#' @param da dataElement
#' @param using the name of the column used as UID
#' @param re.rm if TRUE replicate markers are removed
#' @return the sampleID
#'
#' @export
#'
setGeneric("getID", function(da, using = "sampleID", re.rm = FALSE) {
  standardGeneric("getID")
})

#' S4 method get sampleID from dataElement
#'
#' @param da dataElement
#' @param using the name of the column used as UID
#' @param re.rm if TRUE replicate markers are removed
#' @return the sampleID
#'
#' @export getID
#' @export
setMethod("getID",
          c(da = "dataElement"),
          function(da, using = "sampleID", re.rm = FALSE) {
            sampleID <- unlist(unname(da@obsDescr[[1]][using]))
            if (re.rm) {
              sampleID <- gsub("#.", "", sampleID)
            }
            return(sampleID)
          }
)

