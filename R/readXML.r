#' extract content from a xml object
#'
#' @param xml - an xml object
#' @return assign value to content
#'
#' @export
readXML <- function(xml, attr = FALSE) {
  children <- xml_children(xml)
  length <- length(children)
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
    assign("content", new, envir = .GlobalEnv)
  }
}


