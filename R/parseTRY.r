#' extract targeted MS data from task export xml file
#'
#' @param path - the path to the expName folder
#' @param options list of options
#' \itemize{
#'    \item codePosition - position of the code in the file name
#'    \item columnList - list of columns to be selected
#' }
#' @return a dataElement
#'
#' @export
#' @importFrom xml2 read_xml xml_attr xml_find_all xml_attrs
#' @importFrom dplyr %>%
parseTRY <- function(path, options = list()){
  id <- name <- createdate <- createtime <- type <- desc <- stdconc <- NULL
  vial <- inletmethodname <- msmethodname <- tunemethodname <- instrument <- NULL
  # get sampleID position in title
  if ("codePosition" %in% names(options)) {
    codePosition <- options$codePosition
  } else {
    codePosition <- 8
  }
  # get list of metabolites if not default
  if ("columnsList" %in% names(options)) {
    columnsList <- options$columnsList
  } else {
    columnsList <- c("tryptophan",
                     "3-hydroxykynurenine",
                     "3-hydroxyanthranilic acid",
                     "kynurenic acid",
                     "nicotinamide riboside",
                     "quinolinic acid",
                     "nicotinic acid",
                     "indole-3-acetic acid",
                     "picolinic acid",
                     "xanthurenic acid",
                     "kynurenine",
                     "citrulline",
                     "dopamine",
                     "5-hydroxyindole acetic acid",
                     "neopterin",
                     "beta-nicotinamide mononucleotide",
                     "serotonin",
                     "nicotinamide adenine dinucleotide",
                     "SIL tryptophan D5",
                     "SIL 3-hydroxykynurenine 13C215N",
                     "SIL 3-hydroxyanthranilic acid D3",
                     "SIL kynurenic acid D5",
                     "SIL nicotinamide riboside D3",
                     "SIL quinolinic acid D3",
                     "SIL nicotinic acid D4",
                     "SIL picolinic acid D3",
                     "SIL xanthurenic acid D4",
                     "SIL kynurenine D4",
                     "SIL indole-3-acetic acid D4",
                     "SIL citrulline D7",
                     "SIL dopamine D4",
                     "SIL 5-hydroxyindole acetic acid D5",
                     "SIL neopterin 13C5",
                     "SIL beta-nicotinamide mononucleotide D3",
                     "Melatonin")
  }

  xml <- read_xml(path)
  # retrieving sample information from SAMPLELISTDATA
  sample <- xml_children(xml_find_all(xml, "//GROUPDATA/GROUP/SAMPLELISTDATA"))
  sample <- data.frame(do.call("rbind", lapply(xml_attrs(sample), function(x) unlist(x))))
  sampleInfo <- sample %>% dplyr::select(c(id,
                                           name,
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

  # retrieving compound names
  compoundNames <- unique(COMPOUND$name)

  # performing a few data integrity checks
  # checking for missing metabolites
  missing <- setdiff(columnsList, compoundNames)
  if (length(missing) > 0) {
    msg <- paste("fusion::getTryXML >> Missing metabolites:", missing, "\n")
    message(crayon::red(msg))
  }
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
  code <- sampleInfo$name[idx]
  sampleID <- gsub(
    " ",
    "",
    makeUnique(
      sapply(
        strsplit(code, "_"), "[", codePosition),
      fromFirst = TRUE))

  sourceID <- gsub(
    " ",
    "",
    makeUnique(
      sapply(
        strsplit(code, "_"), "[", codePosition + 1),
      fromFirst = TRUE))

  sampleType <- factor(sampleInfo$type[idx],
                       levels = c("Analyte", "Blank", "ltr", "QC", "Standard"),
                       labels = c("analyte", "blank", "ltr", "qc", "standard"))
  # creating LTR type
  idx <- grep("LTR", sampleID)
  sampleType[idx] <- "ltr"

  # adding required fields (columns)
  COMPOUND$sampleID <- sampleID
  COMPOUND$sourceID <- sourceID
  COMPOUND$sampleType <- sampleType
  COMPOUND$expname <- basename(path)

  # casting the data
  # (as.numeric is mandatory otherwise dcast will order id in alphabetical order)
  .Data <- dcast(COMPOUND, as.numeric(sampleid) ~ as.numeric(id), value.var = "analconc")

  # checking that data are matched with sampleInfo
  if(!identical(as.character(.Data[,1]), sampleInfo$id)){
    cat(crayon::red("fusion::getTRY >> sampleInfo not in the same order as .Data"))
  } else {
    .Data <- .Data[,-1]
  }

  obsDescr <- split(COMPOUND, 1:N)
  #checking that description matches data columns
  if(!identical(unname(sapply(obsDescr, function(x) unique(x$id))), colnames(.Data))) {
    cat(crayon::red("fusion::getTRY >> order of data and obsDescr does not match\n"))
  } else {
    varName <- unname(sapply(obsDescr, function(x) unique(x$name)))
  }

  # making data numeric
  .Data <- sapply(.Data, function(x) as.numeric(x))
  colnames(.Data) <- varName
  rownames(.Data) <- sampleInfo$name

  # dividing by molecular weight
  mw <- tMsTestsets$mw
  idx <- match(varName, mw$analyte)
  for (i in 1:length(varName)) {
    if(varName[i] %in% mw$analyte) {
      .Data[,i] <- .Data[,i] / mw$mw[which(mw$analyte == varName[i])]
    } else {
      cat(crayon::yellow("fusion::getTRY >> ", varName[i], "molecular weight not found\n"))
    }
  }

  # creating dataElement
  da <- new("dataElement",
            .Data = .Data,
            obsDescr = obsDescr,
            varName = unlist(varName),
            type = "T-MS",
            method = "tryptophan")
  return(da)
}

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



