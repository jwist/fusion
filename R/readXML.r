#' extract content from a xml object
#'
#' @param xml - an xml object
#' @param attr - boolean object
#' @return assign value to content
#'
#' @export
#' @importFrom xml2 xml_children xml_path xml_text xml_name xml_attrs
readXML <- function(xml, attr = FALSE) {
  children <- xml_children(xml)
  length <- length(children)
  content <- list()
  if (length > 0) {
    for (i in 1:length) {
      readXML(children[[i]], attr = attr)
    }
  } else {
    if (attr == TRUE) {
      new <- c(content, list(c(path = xml_path(xml),
                               name = xml_name(xml),
                               value = xml_text(xml),
                               attribute = xml_attrs(xml))))
    } else {
      new <- c(content, list(c(path = xml_path(xml),
                               name = xml_name(xml),
                               value = xml_text(xml))))
    }
    # assign("content", new, envir = .GlobalEnv)
    return(new)
  }
}


