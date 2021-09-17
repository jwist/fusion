#' extract lipoprotein  quantification information from a bruker xml
#'
#' @param path - the path to the expName folder
#' @return the values
#'
#' @export
#' @importFrom xml2 read_xml xml_attr xml_find_all
getLipoprotein <- function(path){
  path <- file.path(path, "pdata", "1", "lipo_results.xml")
  if (file.exists(path)) {
    xml <- read_xml(path, options = "NOBLANKS")
    comment <- xml_attr(xml_find_all(xml, ".//PARAMETER"), "comment")
    id <- xml_attr(xml_find_all(xml, ".//PARAMETER"), "name")
    type <- xml_attr(xml_find_all(xml, ".//PARAMETER"), "type")
    value <- xml_attr(xml_find_all(xml, ".//VALUE"), "value")
    unit <- xml_attr(xml_find_all(xml, ".//VALUE"), "unit")
    refMax <- xml_attr(xml_find_all(xml, ".//REFERENCE"), "vmax")
    refMin <- xml_attr(xml_find_all(xml, ".//REFERENCE"), "vmin")
    refUnit <- xml_attr(xml_find_all(xml, ".//REFERENCE"), "unit")

    fraction <- sapply(comment, function(x) strsplit(x, ",")[[1]][1])
    name <- sapply(comment, function(x) strsplit(x, ",")[[1]][2])
    abbr <- sapply(comment, function(x) strsplit(x, ",")[[1]][3])

    res <- data.frame(fraction,
                      name,
                      abbr,
                      id,
                      type,
                      value = as.numeric(value),
                      unit,
                      refMax = as.numeric(refMax),
                      refMin = as.numeric(refMin),
                      refUnit)
    fi <- duplicated(res$id)
    return(res[!fi,])
  } else {
    cat(crayon::yellow("fusion::getLipoprotein >>", path, "not found\n"))
  }
}

