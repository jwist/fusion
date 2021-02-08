#' S4 method get sampleID from dataElement
#'
#' @param da dataElement
#' @param using the name of the column used as UID
#' @param re.rm if TRUE replicate markers are removed
#' @param type type of sample to get the ids (either all or a valid type)
#' @return the sampleID
#'
#' @export
#'
setGeneric("getID", function(da,
                             using = "sampleID",
                             re.rm = FALSE,
                             type = "all") {
  standardGeneric("getID")
})

#' S4 method get sampleID from dataElement
#'
#' @param da dataElement
#' @param using the name of the column used as UID
#' @param re.rm if TRUE replicate markers are removed
#' @param type type of sample to get the ids (either all or a valid type
#' @return the sampleID
#'
#' @export getID
#' @export
setMethod("getID",
          c(da = "dataElement"),
          function(da,
                   using = "sampleID",
                   re.rm = FALSE,
                   type = "all") {
            ID <- da@obsDescr[[1]][using]
            sampleID <- unlist(unname(ID))
            if (type != "all") {
              fi <- da@obsDescr[[1]]$sampleType == type
              sampleID <- sampleID[fi]
            }
            if (re.rm) {
              sampleID <- gsub("#.", "", sampleID)
            }
            return(sampleID)
          }
)

