#' extract xml quantification information from a bruker xml
#'
#' @param path - the path to the expName folder
#' @return the title
#'
#' @export
#' @importFrom xml2 read_xml
getEretic <- function(path){
  path <- file.path(path, "QuantFactorSample.xml")
  if (file.exists(path)) {
    xml <- read_xml(path, options = "NOBLANKS")
    ereticPosition <- as.numeric(xml_text(xml_find_all(xml, ".//Artificial_Eretic_Position")))
    ereticLineWidth <- as.numeric(xml_text(xml_find_all(xml, ".//Artificial_Eretic_Line_Width")))
    ereticConcentration <- as.numeric(xml_text(xml_find_all(xml, ".//Artificial_Eretic_Concentration")))
    tubeID <- xml_attr(xml_find_all(xml, ".//Eretic_Sample_Tube"), "ID")
    Tmin <- as.numeric(xml_text(xml_find_all(xml, ".//Temperature_min")))
    Tmax <- as.numeric(xml_text(xml_find_all(xml, ".//Temperature_max")))
    ereticCalibration <- as.numeric(xml_text(xml_find_all(xml, ".//Eretic_Calibration//Eretic_Factor")))
    ereticFactor <- as.numeric(xml_text(xml_find_all(xml, ".//Application_Parameter//Eretic_Factor")))
    df <- data.frame(ereticPosition,
                     ereticLineWidth,
                     ereticConcentration,
                     tubeID,
                     Tmin,
                     Tmax,
                     ereticCalibration,
                     ereticFactor)
    return(df)
  } else {
    cat(crayon::yellow("fusion::getEretic >>", path, "not found\n"))
  }
}

