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

# addValues(lip, options = list(fold = TRUE, scale = FALSE, dotColor = "red"))
# makeLipoReport(lip, options = list(fold = FALSE, dotColor = "blue", dotPch = 21))
# addValues(lip, options = list(dotColor = "red", dotPch = 22))

#' get names and description of lipoproteins IVDr parameters
#' @return a data.frame with information
#' @export
getLipoTable <- function() {

  lipo$range <- paste0(lipo$refMin, " - ", lipo$refMax,
                       " (", lipo$refUnit, ")")
  names(lipo) <-c("Fraction",
                  "Compound",
                  "Abbreviation",
                  "ID",
                  "Type",
                  "Value",
                  "Unit",
                  "Max Value (ref.)",
                  "Min Value (ref.)",
                  "Reference Unit",
                  "Reference Range [Unit]")
  # correcting typo in xml
  lipo$Compound[9] <- "Apo-B100 / Apo-A1"
  rownames(lipo) <- c(1:nrow(lipo))

  # cleaning column text
  lipo$Compound <- gsub(" ", "", lipo$Compound)
  lipo$Abbreviation <- gsub(" ", "", lipo$Abbreviation)

  # creating label for publication
  tag <- paste0(lipo$Compound, ", ", lipo$Abbreviation)
  tag[1] <- "Triglycerides, total"
  tag[2] <- "Cholesterol, total"
  tag[3] <- "Cholesterol, LDL"
  tag[4] <- "Cholesterol, HDL"
  tag[5] <- "Apo-A1, total"
  tag[6] <- "Apo-A2, total"
  tag[7] <- "Apo-B100, total"

  tag[8] <- "LDL-Chol/HDL-Chol"
  tag[9] <- "Apo-B100/Apo-A1"
  tag[10] <- "Apo-B100, particle number"
  tag[11] <- "VLDL, particle number"
  tag[12] <- "IDL, particle number"
  tag[13] <- "LDL, particle number"
  tag[14] <- "LD1, particle number"
  tag[15] <- "LD2, particle number"
  tag[16] <- "LD3, particle number"
  tag[17] <- "LD4, particle number"
  tag[18] <- "LD5, particle number"
  tag[19] <- "LD6, particle number"

  lipo$tag <- tag

  return(lipo[,c(1,2,4,11, 12)])
}

