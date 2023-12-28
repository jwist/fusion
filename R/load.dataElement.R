#' A nrow method for \emph{dataElement} objects
#'
#' @param filepath a path to the file to load
#' @param as name of the variable where to store the content
#' @param ... parameters of the nrow generic function
#' @return the content of the dataElement
#' @export loadAs
#'
loadAs <- function(filepath){
  var <- local(get(load(filepath)))
  return(var)
  cat(crayon::yellow("your has been loaded into:", as))
}

