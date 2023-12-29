#' A nrow method for \emph{dataElement} objects
#'
#' @param file a path to the file to load
#' @return the content of the dataElement
#' @export
#'
#'
load.daE <- function(file) {
  extension <- tools::file_ext(file)
  if (extension == "daE") {
    as <- local(get(load(file)))
    return(as)
  } else {
    cat(crayon::yellow("This file is not a daE, use load() instead."))
  }
}
