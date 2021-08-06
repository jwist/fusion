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
    conc_v <- xml_attr(xml_find_all(xml, ".//VALUE"), "conc")
    concUnit_v <- xml_attr(xml_find_all(xml, ".//VALUE"), "concUnit")
    lod_v <- xml_attr(xml_find_all(xml, ".//VALUE"), "lod")
    lodUnit_v <- xml_attr(xml_find_all(xml, ".//VALUE"), "lodUnit")
    loq_v <- xml_attr(xml_find_all(xml, ".//VALUE"), "loq")
    loqUnit_v <- xml_attr(xml_find_all(xml, ".//VALUE"), "loqUnit")
    conc_vr <- xml_attr(xml_find_all(xml, ".//VALUERELATIVE"), "conc")
    concUnit_vr <- xml_attr(xml_find_all(xml, ".//VALUERELATIVE"), "concUnit")
    lod_vr <- xml_attr(xml_find_all(xml, ".//VALUERELATIVE"), "lod")
    lodUnit_vr <- xml_attr(xml_find_all(xml, ".//VALUERELATIVE"), "lodUnit")
    loq_vr <- xml_attr(xml_find_all(xml, ".//VALUE"), "loq")
    loqUnit_vr <- xml_attr(xml_find_all(xml, ".//VALUE"), "loqUnit")
    sigCorrUnit <- xml_attr(xml_find_all(xml, ".//RELDATA"), "sigCorrUnit")
    sigCorr <- xml_attr(xml_find_all(xml, ".//RELDATA"), "sigCorr")
    rawConcUnit <- xml_attr(xml_find_all(xml, ".//RELDATA"), "rawConcUnit")
    rawConc <- xml_attr(xml_find_all(xml, ".//RELDATA"), "rawConc")
    errConc <- xml_attr(xml_find_all(xml, ".//RELDATA"), "errConc")
    errConcUnit <- xml_attr(xml_find_all(xml, ".//RELDATA"), "errConcUnit")
    refMax <- xml_attr(xml_find_all(xml, ".//REFERENCE"), "vmax")
    refMin <- xml_attr(xml_find_all(xml, ".//REFERENCE"), "vmin")
    refUnit <- xml_attr(xml_find_all(xml, ".//REFERENCE"), "unit")
    return(data.frame(name,
                      conc_v,
                      concUnit_v,
                      lod_v,
                      lodUnit_v,
                      loq_v,
                      loqUnit_v,
                      conc_vr,
                      concUnit_vr,
                      lod_vr,
                      lodUnit_vr,
                      loq_vr,
                      loqUnit_vr,
                      sigCorrUnit,
                      sigCorr,
                      rawConcUnit,
                      rawConc,
                      errConc,
                      errConcUnit,
                      refMax, refMin, refUnit))
  } else {
    cat(crayon::yellow("fusion::getIvdr >>", path, "not found\n"))
  }
}

