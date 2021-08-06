#' extract small molecules  quantification information from a bruker xml
#'
#' @param path - the path to the expName folder
#' @return the values
#'
#' @export

getIvdr <- function(path, reportName){
  path <- file.path(path, "pdata", "1", reportName)
  if (file.exists(path)) {
    xml <- read_xml(path, options = "NOBLANKS")
    name <- xml_attr(xml_find_all(xml, ".//PARAMETER"), "name")
    value <- xml_attr(xml_find_all(xml, ".//VALUE"), "conc")
    unit <- xml_attr(xml_find_all(xml, ".//VALUE"), "concUnit")
    error <- xml_attr(xml_find_all(xml, ".//RELDATA"), "errConc")
    refMax <- xml_attr(xml_find_all(xml, ".//REFERENCE"), "vmax")
    refMin <- xml_attr(xml_find_all(xml, ".//REFERENCE"), "vmin")
    refUnit <- xml_attr(xml_find_all(xml, ".//REFERENCE"), "unit")
    return(data.frame(name, value, unit, refMax, refMin, refUnit))
  } else {
    cat(crayon::yellow("fusion::getIvdr >>", path, "not found\n"))
  }
}

