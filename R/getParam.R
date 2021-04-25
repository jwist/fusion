#' get a parameter from procs or acqus dataframe
#'
#' @param df - the name of the dataframe
#' @param paramName - the name of the parameter to get
#' @return the parameter
#'
#' @export
getParam <- function(df, paramName){
  idx <- df$name == paramName
  if (grepl("^[-+]?[0-9]*[\\.]?[0-9]*", df$value[idx]) == TRUE) {
    return(as.numeric(df$value[idx]))
  } else {
    return(df$value[idx])
  }
}


