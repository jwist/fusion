#' S4 method set sampleID from dataElement
#'
#' @param da dataElement
#' @param ID a vector of IDs
#' @param using the name of the column used as UID
#' @return void
#'
#' @export
#'
setGeneric("setID", function(da, ID, using = "sampleID") {
  standardGeneric("setID")
})

#' S4 method set sampleID from dataElement
#'
#' @param da dataElement
#' @param ID a vector of IDs
#' @param using the name of the column used as UID
#' @return void
#'
#' @export
#'
setMethod("setID",
          c(da = "dataElement"),
          function(da, ID, using = "sampleID") {
            for (i in 1:length(da@obsDescr)) {
              da@obsDescr[[i]][using] <- ID
            }
            return(da)
          }
)
