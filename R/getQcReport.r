#' extract lipoprotein  quantification information from a bruker xml
#'
#' @param path - the path to the expName folder
#' @param reportName - the file name of the report
#' (plamsa_qc_report.xml / urine_qc_report.xml)
#' @return the values
#'
#' @export
#' @importFrom xml2 read_xml xml_attr xml_find_all
getQcReport <- function(path, reportName){
  path <- file.path(path, "pdata", "1", reportName)
  if (file.exists(path)) {
    xml <- read_xml(path, options = "NOBLANKS")
    sampleName <- xml_attr(xml_find_all(xml, ".//INFO"), "name")
    sampleValue <- xml_attr(xml_find_all(xml, ".//INFO"), "value")

    name <- xml_attr(xml_find_all(xml, ".//PARAMETER"), "name")
    comment <- xml_attr(xml_find_all(xml, ".//PARAMETER"), "comment")
    type <- xml_attr(xml_find_all(xml, ".//PARAMETER"), "type")
    value <- xml_attr(xml_find_all(xml, ".//VALUE"), "value")

    res <- c(sampleValue, rbind(type, comment, value))
    names(res) <- c(sampleName,
                    rbind(paste(name, "type"),
                    paste(name, "comment"),
                    paste(name, "value")))
    return(res)
  } else {
    cat(crayon::yellow("fusion::getQcReport >>", path, "not found\n"))
  }
}

