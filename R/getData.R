#' S4 method get data from dataElement
#'
#' @param da dataElement
#' @param using either sample, ltr, pqc, qc, standard, or blank or a combination of
#' @param replicates either last, rm or all
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
setMethod("getData",
          c(da = "dataElement",
            type = c(),
            replicates = "character"),
          function(da,
                   type = c("sample"),
                   replicates = "last") {
            if (missing(type)) {
              type <- "sample"
            } else {
              if (!type %in% c("sample", "pqc", "ltr", "qc", "standar", "blank")) {
                stop("fusion: incorrect value for sample type")
              }
            }
            if (missing(replicates)) {
              replicates <- "all"
            } else {
              if (!replicates %in% c("last", "rm", "all")) {
                stop("fusion: incorrect value for replicates")
              }
            }
            fi <- getType(da) == type
            da <- filterWith(da, fi)
            if (replicates == "last") {
              idx <- which(grepl("#", getID(da)))
              ori <- unique(gsub("#.", "", getID(da)[idx]))
              fi <- getID(da) %in% ori
              da <- filterWith(da, !fi)
              newID <- gsub("#.", "", getID(da))
              da <- setID(da, newID)
            } else if (replicates == "rm"){
              fi <- grepl("#", getID(da))
              da <- filterWith(da, !fi)
            }
            return(da)
          }
)

