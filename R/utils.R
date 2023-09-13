
#' a function to unlist but replacing empty values by NA to keep the length
#' @param l - a list
#' @param unname - boolean switch to unname response (default = true)
#' @returns a vector
delist <- function(l, unname = TRUE) {
  l[sapply(l, function(x) length(x)==0L)] <- NA
  l <- unlist(l)
  if (unname) {
    l <- unname(l)
  }
  return(l)
}

#' a function to get duplicated pairs
#' @param vector - a vector
#' @returns a logical vector
#' @importFrom data.table data.table
dduplicated <- function(vector) {
  fi <- duplicated(vector) | duplicated(vector, fromLast = TRUE)
  return(fi)
}

#' a function to show pairs of duplicated items
#' @param aTable - a table
#' @param colWithDuplicates = the column with duplicates
#' @param colToSelect - the column to display
#' @returns a logical vector
#' @importFrom data.table data.table
showDuplicated <- function(aTable, colWithDuplicates, colToSelect) {
  dt <- data.table(aTable)
  sortedTable <- dt(order())
}
