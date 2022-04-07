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
    CalEreticPosition <- as.numeric(xml_text(xml_find_all(xml, ".//Artificial_Eretic_Position")))
    CalEreticLineWidth <- as.numeric(xml_text(xml_find_all(xml, ".//Artificial_Eretic_Line_Width")))
    CalEreticConcentration <- as.numeric(xml_text(xml_find_all(xml, ".//Artificial_Eretic_Concentration")))
    CalTubeID <- xml_attr(xml_find_all(xml, ".//Eretic_Sample_Tube"), "ID")
    CalTmin <- as.numeric(xml_text(xml_find_all(xml, ".//Temperature_min")))
    CalTmax <- as.numeric(xml_text(xml_find_all(xml, ".//Temperature_max")))
    CalP1 <- as.numeric(xml_text(xml_find_all(xml, ".//Eretic_Calibration//P1")))
    CalEreticCalibration <- as.numeric(xml_text(xml_find_all(xml, ".//Eretic_Calibration//Eretic_Factor")))
    ereticFactor <- as.numeric(xml_text(xml_find_all(xml, ".//Application_Parameter//Eretic_Factor")))
    P1 <- as.numeric(xml_text(xml_find_all(xml, ".//Application_Parameter//P1")))
    temperature <- as.numeric(xml_text(xml_find_all(xml, ".//Application_Parameter//Temperature")))
    df <- data.frame(CalEreticPosition,
                     CalEreticLineWidth,
                     CalEreticConcentration,
                     CalTubeID,
                     CalTmin,
                     CalTmax,
                     CalP1,
                     CalEreticCalibration,
                     ereticFactor,
                     temperature,
                     P1)
    return(df)
  } else {
    cat(crayon::yellow("fusion::getEretic >>", path, "not found\n"))
  }
}

