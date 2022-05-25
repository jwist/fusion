#JUST A SLOWER VERSION OF GETTRY
# parseTryXML <- function(path, options = list()){
#   # get sampleID position in title
#   if ("codePosition" %in% names(options)) {
#     codePosition <- options$codePosition
#   } else {
#     codePosition <- 8
#   }
#   # get list of metabolites if not default
#   if ("columnsList" %in% names(options)) {
#     columnsList <- options$columnsList
#   } else {
#     columnsList <- c("tryptophan",
#                      "3-hydroxykynurenine",
#                      "3-hydroxyanthranilic acid",
#                      "kynurenic acid",
#                      "nicotinamide riboside",
#                      "quinolinic acid",
#                      "nicotinic acid",
#                      "indole-3-acetic acid",
#                      "picolinic acid",
#                      "xanthurenic acid",
#                      "kynurenine",
#                      "citrulline",
#                      "dopamine",
#                      "5-hydroxyindole acetic acid",
#                      "neopterin",
#                      "beta-nicotinamide mononucleotide",
#                      "serotonin",
#                      "nicotinamide adenine dinucleotide",
#                      "SIL tryptophan D5",
#                      "SIL 3-hydroxykynurenine 13C215N",
#                      "SIL 3-hydroxyanthranilic acid D3",
#                      "SIL kynurenic acid D5",
#                      "SIL nicotinamide riboside D3",
#                      "SIL quinolinic acid D3",
#                      "SIL nicotinic acid D4",
#                      "SIL picolinic acid D3",
#                      "SIL xanthurenic acid D4",
#                      "SIL kynurenine D4",
#                      "SIL indole-3-acetic acid D4",
#                      "SIL citrulline D7",
#                      "SIL dopamine D4",
#                      "SIL 5-hydroxyindole acetic acid D5",
#                      "SIL neopterin 13C5",
#                      "SIL beta-nicotinamide mononucleotide D3",
#                      "Melatonin")
#   }
#
#   # if file exists then read data
#   if (file.exists(path)) {
#     xml <- read_xml(path, options = "NOBLANKS")
#     # reading sample counts
#     nSamples <- as.numeric(
#       xml_attr(
#         xml_find_all(xml, ".//SAMPLELISTDATA"), "count"))
#     msg <- paste("fusion::getTryXML >> Number of samples read:", nSamples, "(all types)")
#     message(crayon::yellow(msg))
#
#     # reading in list of samples
#     sampleList  <- data.frame(
#       do.call("rbind",
#               xml_attrs(
#                 xml_find_all(xml, ".//SAMPLE"))))
#     message(crayon::yellow(
#       paste(names(table(sampleList$type)),
#             table(sampleList$type),
#             "\n")), appendLF = FALSE)
#
#     # reading in list of compounds and filtering
#     compounds <- xml_find_all(xml, "//SAMPLELISTDATA//SAMPLE//COMPOUND")
#     metList <- data.frame(
#       do.call("rbind",
#               xml_attrs(
#                 compounds)))
#     metList %>%
#       group_by(name) %>%
#       filter(row_number() == 1) %>%
#       select(id, name) -> metList
#
#     # checking for missing metabolites
#     missing <- setdiff(columnsList, metList$name)
#     if (length(missing) > 0) {
#       msg <- paste("fusion::getTryXML >> Missing metabolites:", missing, "\n")
#       message(crayon::red(msg))
#     }
#
#     nMet <- nrow(metList)
#     msg <- paste("fusion::getTryXML >> Number of metabolites read:", nMet, "(all types)")
#     message(crayon::yellow(msg))
#
#     # retrieving the data (per compounds)
#     res <- list()
#     for (c in 1:nMet) {
#       pathToCompound <- paste0("//SAMPLELISTDATA//SAMPLE//COMPOUND[", c, "]")
#       cmpd <- data.frame(do.call("rbind",
#                                  xml_attrs(xml_find_all(xml, pathToCompound))))
#       # creating unique sampleID
#       code <- sampleList$name[as.numeric(cmpd$sampleid)]
#       cmpd$sampleID <- gsub(
#         " ",
#         "",
#         makeUnique(
#           sapply(
#             strsplit(code, "_"), "[", codePosition),
#           fromFirst = TRUE))
#
#       cmpd$sourceID <- gsub(
#         " ",
#         "",
#         makeUnique(
#         sapply(
#           strsplit(code, "_"), "[", codePosition + 1),
#         fromFirst = TRUE))
#
#       # creating standard sample types
#       cmpd$sampleType <- sampleList$type[as.numeric(cmpd$sampleid)]
#       cmpd$sampleType[cmpd$sampleType == "Analyte"] <- "sample"
#       cmpd$sampleType[cmpd$sampleType == "Standard"] <- "standard"
#       cmpd$sampleType[grep("LTR", cmpd$sampleID)] <- "ltr"
#
#       # removing sourceID for calibrants
#       cmpd$sourceID[cmpd$sampleType != "sample"] <- NA
#
#       peak <- data.frame(
#         do.call("rbind",
#                 xml_attrs(
#                   xml_find_all(
#                     xml, paste0(pathToCompound, "//PEAK")))))
#
#       method <- data.frame(
#         do.call("rbind",
#                 xml_attrs(
#                   xml_find_all
#                   (xml, paste0(pathToCompound, "//METHOD")))))
#
#       res[[c]] <- cbind(cmpd, peak, method)
#     }
#
#     # selecting info to create dataset and merging
#     new <- res[[1]][,c("sampleid", "analconc")]
#     colnames(new) <- c("sampleid", res[[1]]$name[1])
#     for (i in 2:length(res)) {
#       int <- res[[i]][,c("sampleid", "analconc")]
#       colnames(int) <- c("sampleid", res[[i]]$name[1])
#       new <- merge(new, int, by = "sampleid", sort = FALSE)
#     }
#     rownames(new) <- sampleList$name[as.numeric(new$sampleid)]
#     new <- new[,-1]
#
#     # dividing by molecular weight
#     varName <- unlist(colnames(new))
#     mw <- tMsTestsets$mw
#     idx <- match(varName, mw$analyte)
#     for (i in 1:length(varName)) {
#       if(varName[i] %in% mw$analyte) {
#         new[,i] <- as.numeric(new[,i]) / mw$mw[which(mw$analyte == varName[i])]
#       } else {
#         message(
#           crayon::yellow(
#             "fusion::getTryXML >> ", varName[i], "molecular weight not found"))
#       }
#     }
#
#     # create dataElement
#     da <- new("dataElement",
#               .Data = new,
#               obsDescr = res,
#               varName = varName,
#               type = "T-MS",
#               method = "tryptophan")
#   }
# }
#
# # path <- "tests/testthat/2021-09-21_LGW_COVID_HARVARD_RE-EDIT_P43.xml"
# # da <- parseTryXML(path)
# # xml_attr(xml_find_all(xml, "//SAMPLELISTDATA//SAMPLE//COMPOUND[2]"), "stdconc")
#
