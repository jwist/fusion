#' get time stamp to the millisecond to use as uuid generator when no uuid is
#' available via rolodex database.
#' @param n number of ids
#' @return a number that is hopefully unique
#' @importFrom stats runif
#'
stamp <- function(n) {
  stps <- sapply(1:n, function(x) {
    stp <- paste0(as.character(round(as.numeric(Sys.time())*1000, 0)),
                  round(runif(1, min = 1001, max = 9999), 0))
    Sys.sleep(0.001)
    return(stp)
  })

  return(stps)
}
