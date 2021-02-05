#' S4 method get data from dataElement
#'
#' @param da dataElement
#' @param using either sample, qc, standard, or blank or a combination of
#' @param replicates either takeLast, notAtAll or keepAll
#' @return a dataElement with selected types
#'
#' @export
#'
setGeneric("getData", function(da, type = c("sample"), replicates = "takeLast") {
  standardGeneric("getData")
})

#' S4 method get data from dataElement
#'
#' @param da dataElement
#' @param using either sample, qc, standard, or blank or a combination thereof
#' @return a dataElement with selected types
#'
#' @export getData
#' @export
setMethod("getData",
          c(da = "dataElement",
            type = c(),
            replicates = "character"),
          function(da,
                   type = c("sample"),
                   replicates = "takeLast") {
            fi <- getType(da) == type
            newDa <- filterWith(da, fi)
            if (replicates == "takeLast") {
              idx <- which(grepl("#", getID(newDa)))
              ori <- unique(gsub("#.", "", getID(newDa)[idx]))
              fi <- getID(newDa) %in% ori
              newDa <- filterWith(newDa, !fi)
            } else if (replicates == "notAtAll"){
              fi <- grepl("#", getID(newDa))
              newDa <- filterWith(da, !fi)
            }
            return(newDa)
          }
)

