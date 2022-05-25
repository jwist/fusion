
parseTryXML <- function(path){
  if (file.exists(path)) {
    xml <- read_xml(path, options = "NOBLANKS")
    # reading sample counts
    nSamples <- as.numeric(
      xml_attr(
        xml_find_all(xml, ".//SAMPLELISTDATA"), "count"))
    msg <- paste("Number of samples read:", nSamples, "(all types)")
    message(crayon::yellow(msg))
    message(crayon::yellow(msg))

    # reading in list of samples
    sampleList  <- data.frame(
      do.call("rbind",
              xml_attrs(
                xml_find_all(xml, ".//SAMPLE"))))
    message(crayon::yellow(
      paste(names(table(sampleList$type)),
            table(sampleList$type),
            "\n")), appendLF = FALSE)

    # reading in list of compounds and filtering
    compounds <- xml_find_all(xml, "//SAMPLELISTDATA//SAMPLE//COMPOUND")
    metList <- data.frame(
      do.call("rbind",
              xml_attrs(
                compounds)))
    metList %>%
      group_by(name) %>%
      filter(row_number() == 1) %>%
      select(id, name) -> metList

    nMet <- nrow(metList)
    msg <- paste("Number of metabolites read:", nMet, "(all types)")
    message(crayon::yellow(msg))

    # retrieving the data (per compounds)
    res <- list()
    for (c in 1:nMet) {
      pathToCompound <- paste0("//SAMPLELISTDATA//SAMPLE//COMPOUND[", c, "]")
      cmpd <- data.frame(do.call("rbind",
                                 xml_attrs(xml_find_all(xml, pathToCompound))))
      cmpd$sampleID <- sampleList$name[as.numeric(cmpd$sampleid)]
      cmpd$sampleType <- sampleList$type[as.numeric(cmpd$sampleid)]
      cmpd$sampleType[cmpd$sampleType == "Analyte"] <- "sample"
      cmpd$sampleType[cmpd$sampleType == "Standard"] <- "standard"
      cmpd$sampleType[grep("LTR", cmpd$sampleID)] <- "ltr"

      peak <- data.frame(
        do.call("rbind",
                xml_attrs(
                  xml_find_all(
                    xml, paste0(pathToCompound, "//PEAK")))))

      method <- data.frame(
        do.call("rbind",
                xml_attrs(
                  xml_find_all
                  (xml, paste0(pathToCompound, "//METHOD")))))

      res[[c]] <- cbind(cmpd, peak, method)
    }

    # selecting info to create dataset and merging
    new <- res[[1]][,c("sampleid", "analconc")]
    colnames(new) <- c("sampleid", res[[1]]$name[1])
    for (i in 2:length(res)) {
      int <- res[[i]][,c("sampleid", "analconc")]
      colnames(int) <- c("sampleid", res[[i]]$name[1])
      new <- merge(new, int, by = "sampleid", sort = FALSE)
    }

    # create dataElement
    da <- new("dataElement",
             .Data = new,
             obsDescr = res,
             varName = unlist(colnames(new)),
             type = "T-MS",
             method = "tryptophan")
  }
}

# path <- "tests/testthat/2021-09-21_LGW_COVID_HARVARD_RE-EDIT_P43.xml"
# da <- parseTryXML(path)
# xml_attr(xml_find_all(xml, "//SAMPLELISTDATA//SAMPLE//COMPOUND[2]"), "stdconc")

