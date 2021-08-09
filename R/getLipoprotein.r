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
    name <- xml_attr(xml_find_all(xml, ".//PARAMETER"), "name")
    value <- xml_attr(xml_find_all(xml, ".//VALUE"), "value")
    unit <- xml_attr(xml_find_all(xml, ".//VALUE"), "unit")
    refMax <- xml_attr(xml_find_all(xml, ".//REFERENCE"), "vmax")
    refMin <- xml_attr(xml_find_all(xml, ".//REFERENCE"), "vmin")
    refUnit <- xml_attr(xml_find_all(xml, ".//REFERENCE"), "unit")

    res <- data.frame(name, value, unit, refMax, refMin, refUnit)
    fi <- duplicated(res)
    return(res[!fi,])
  } else {
    cat(crayon::yellow("fusion::getLipoprotein >>", path, "not found\n"))
  }
}

