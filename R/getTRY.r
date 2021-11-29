#' extract targeted MS data from task export xml file
#'
#' @param path - the path to the expName folder
#' @return a dataElement object
#'
#' @export
#' @importFrom xml2 read_xml xml_attr xml_find_all xml_attrs
getTRY <- function(path){
  xml <- read_xml(path)
  # retrieving sample information from SAMPLELISTDATA
  sample <- xml_children(xml_find_all(xml, "//GROUPDATA/GROUP/SAMPLELISTDATA"))
  sample <- data.frame(do.call("rbind", lapply(xml_attrs(sample), function(x) unlist(x))))
  sampleInfo <- sample %>% dplyr::select(c(id, name,
                                           createdate,
                                           createtime,
                                           type,
                                           desc,
                                           stdconc,
                                           vial,
                                           inletmethodname,
                                           msmethodname,
                                           tunemethodname,
                                           instrument))

  # retrieving data for each samples from SAMPLELISTDATA
  compound <- xml_attrs(xml_find_all(xml, "//SAMPLELISTDATA/SAMPLE/COMPOUND"))
  COM <- data.frame(do.call("rbind", lapply(compound, function(x) unlist(x))))

  peak <- xml_attrs(xml_find_all(xml, "//SAMPLELISTDATA/SAMPLE/COMPOUND/PEAK"))
  PEAK <- data.frame(do.call("rbind", lapply(peak, function(x) unlist(x))))

  ispeak <- xml_attrs(xml_find_all(xml, "//SAMPLELISTDATA/SAMPLE/COMPOUND/PEAK/ISPEAK"))
  ISPEAK <- data.frame(do.call("rbind", lapply(ispeak, function(x) unlist(x))))

  COMPOUND <- cbind(COM, PEAK, ISPEAK)

  # performing a few data integrity checks
  ## nrow(COMPOUND) must be a ntuple of the number of compounds
  if (!nrow(COMPOUND) / nrow(sampleInfo) == nrow(COMPOUND) %/% nrow(sampleInfo)) {
    cat(crayon::red("fusion::getTRY >> dimension problems"))
  } else {
    N <- nrow(COMPOUND) / nrow(sampleInfo)
  }

  ## all samples are expected to have the same measurements
  if (!sum(unname(table(COMPOUND$sampleid)) == N) == nrow(sampleInfo)) {
    cat(crayon::red("fusion::getTRY >> dimension problems with compounds"))
  }

  # matching sample IDs from SAMPLELISTDATA
  idx <- match(COMPOUND$sampleid, sampleInfo$id)
  if (!identical(order(unique(idx)), unique(idx))) {
    cat(crayon::red("fusion::getTRY >> sampleInfo not in the same order as data"))
  }

  # creating proper columns for dataElement
  sampleID <- sampleInfo$name[idx]

  sampleType <- factor(sampleInfo$type[idx],
                       levels = c("Analyte", "Blank", "ltr", "QC", "Standard"),
                       labels = c("analyte", "blank", "ltr", "qc", "standard"))
  # creating LTR type
  idx <- grep("LTR|PLASMA", sampleID)
  sampleType[idx] <- "ltr"

  # adding required fields (columns)
  COMPOUND$sampleID <- sampleID
  COMPOUND$sampleType <- sampleType


  .Data <- dcast(COMPOUND, sampleid ~ name, value.var = "analconc")[, -1]
  obsDescr <- split(COMPOUND, 1:35)
  varName <- colnames(.Data)

  # creating dataElement
  da <- new("dataElement",
            .Data = .Data,
            obsDescr = obsDescr,
            varName = unlist(varName),
            type = "T-MS",
            method = "tryptophane")
  return(da)
}

#
# xml_name(xml)
# xml_attrs(xml)
# xml_name(xml_children(xml))
# xml_name(xml_children(xml_children(xml)[[3]]))
# xml_name(xml_children(xml_children(xml_children(xml)[[3]])))
# xml_name(xml_children(xml_children(xml_children(xml)[[3]]))[[1]])
# xml_name(xml_children(xml_children(xml_children(xml_children(xml)[[3]]))[[2]]))
# xml_name(xml_children(xml_children(xml_children(xml_children(xml)[[3]]))[[3]]))
#
# xml_name(xml_children(xml_children(xml_children(xml_children(xml_children(xml)[[3]]))[[2]]))[[1]])
# xml_name(xml_children(xml_children(xml_children(xml_children(xml_children(xml_children(xml)[[3]]))[[2]]))[[1]]))
# xml_name(xml_children(xml_children(xml_children(xml_children(xml_children(xml_children(xml_children(xml)[[3]]))[[2]]))[[1]])[[1]]))
#
# xml_name(xml_children(xml_children(xml_children(xml_children(xml_children(xml)[[3]]))[[3]])))
#



# response <- xml_attrs(xml_find_all(xml, "//CALIBRATIONDATA/COMPOUND/RESPONSE"))
# RESPONSE <- data.frame(do.call("rbind", lapply(response, function(x) unlist(x))))
# curve <- xml_attrs(xml_find_all(xml, "//CALIBRATIONDATA/COMPOUND/CURVE"))
# CURVE <- data.frame(do.call("rbind", lapply(curve, function(x) unlist(x))))
# calcurve <- xml_attrs(xml_find_all(xml, "//CALIBRATIONDATA/COMPOUND/CURVE/CALIBRATIONCURVE"))
# CALCURVE <- data.frame(do.call("rbind", lapply(calcurve, function(x) unlist(x))))
# correlation <- xml_attrs(xml_find_all(xml, "//CALIBRATIONDATA/COMPOUND/CURVE/CORRELATION"))
# CORRELATION <- data.frame(do.call("rbind", lapply(correlation, function(x) unlist(x))))
# determination <- xml_attrs(xml_find_all(xml, "//CALIBRATIONDATA/COMPOUND/CURVE/DETERMINATION"))
# DETERMINATION <- data.frame(do.call("rbind", lapply(determination, function(x) unlist(x))))
# responsefactor <- xml_attrs(xml_find_all(xml, "//CALIBRATIONDATA/COMPOUND/CURVE/RESPONSEFACTOR"))
# RESPONSEFACTOR <- data.frame(do.call("rbind", lapply(responsefactor, function(x) unlist(x))))
#


