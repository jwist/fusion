#' importation function for targeted MS assays
#'
#' @param file - the file path to be imported
#' @param method - the method used to acquire the assay
#' @param options list of options
#' \itemize{
#'    \item codePosition - position of the code in the file name
#'    \item columnList - list of columns to be selected
#' }
#' @return a dataElement
#'
#' @export
#' @importFrom utils data read.table
#' @importFrom reshape2 dcast
#' @importFrom crayon red blue yellow white green bold
#' @importFrom utils read.delim2
parseTargetedMS <- function(file, method, options = list()) {
  cat(paste("fusion: using import method for",
            method, "\n"))

  if (method == "tryptophan") {
    da <- parseTRY(file, options)
  }
  if (method == "aminoAcids") {
    da <- parseMS_AA(file, options)
  }

  return(da)
}

parseMS_AA <- function(file, options) {
  rawData <- read.delim2(file = file,
                         fileEncoding = "latin1",
                         header = TRUE,
                         check.names = FALSE)

  cat(paste("fusion:", nrow(rawData),
            "line(s) read\n"))

  fi <- !is.na(rawData$AnalyteName)
  rawData <- rawData[fi,]
  cat(paste("fusion:",
            sum(!fi),
            "empty line(s) removed\n"))

  # checking columns
  if ("columnsList" %in% names(options)) {
    columnsList <- options$`columnsList`
  } else {
    columnsList <- c("AnalyteName",
                     "AnalysisName",
                     "SampleType",
                     "expected m/z",
                     "m/z",
                     "Retention Time[min]",
                     "Expected Retention Time[min]",
                     "mSigma",
                     "Area",
                     "Height",
                     "Visited",
                     "Quantity",
                     "Unit of Quantity",
                     "Expected Quantity",
                     "Residuals[%]",
                     "R2",
                     "Accuracy/Recovery[%]",
                     "Internal Standard(ISTD)")
  }
  missingCol <- setdiff(columnsList, names(rawData))
  if (length(missingCol) > 0) {
    cat(crayon::red("fusion: column ") %+%
          crayon::red$bold(missingCol) %+%
          crayon::red(" is missing from file."), fill = TRUE)
  } else {
    cat("fusion: no missing columns")
  }

  if ("columnsList" %in% names(options)) {
    idx <- match(options$columnsList, names(rawData))
    rawData <- rawData[,idx]
  }

  compoundList <- unique(rawData$AnalyteName)
  numberOfCompounds <- length(compoundList)
  cat(paste("fusion:",
            numberOfCompounds,
            "compound(s) found\n"))

  cat(paste("fusion:",
            length(unique(rawData$AnalysisName)),
            "sample(s) found\n"))

  # removing unnecessary columns
  fi <- names(rawData) %in% columnsList
  rawData <- rawData[, fi]

  # removing unnecessary compounds
  cleaningList <- c("4--hydroxyproline",
                    "5-Oxoproline",
                    "Aminoadipic acid",
                    "Ethanolamine",
                    "Tryptophan",
                    "Carnosine",
                    "Cystathionine")

  fi <- is.na(match(rawData$AnalyteName, cleaningList))
  rawData <- rawData[fi,]

  compoundList <- unique(rawData$AnalyteName)
  numberOfCompounds <- length(compoundList)
  cat(paste("fusion:",
            numberOfCompounds,
            "compound(s) retained\n"))

  cat(crayon::blue("fusion: ") %+%
        crayon::blue$bold(cleaningList) %+%
        crayon::blue(" ignored for that method."), fill = TRUE)

  # names(rawData) <- cleanNames(names(rawData))

  # looking for duplicated lines
  idx <- which(duplicated(rawData[,c(1,2)]) |
                 duplicated(rawData[,c(1,2)], fromLast = TRUE))
  if (length(idx) > 0) {
    cat(crayon::red("fusion: " %+%
                      crayon::red(idx) %+%
                      crayon::red(" duplicated line(s) found")),
        fill = TRUE)
  }
  idx <- which(duplicated(rawData[,c(1,2)]))

  if (length(idx) > 0) {
    rawData <- rawData[-idx,]
    cat(crayon::white("fusion: " %+%
                      crayon::white(idx) %+%
                      crayon::white(" duplicated line(s) removed")),
        fill = TRUE)
  }

  # recasting to get data
  idx <- which(tolower(names(rawData)) == "quantity")
  newData <- tryCatch(dcast(rawData[,c(1,2,idx)],
                   AnalysisName ~ AnalyteName,
                   value.var = "Quantity"),
                   message=function(m) stop("fusion: duplicated line(s) found in rawData"))

  sID <- newData[,1] #sapply(strsplit(newData[,1], "_"), "[", options$codePosition)
  newData <- newData[,-1]

  # adding sampleID
  code <- sapply(strsplit(rawData$AnalysisName, "_"), "[", options$codePosition)
  rawData <- cbind("sampleID" = code, rawData)

  # cleaning sampleType
  sampleType <- as.factor(rawData$SampleType)
  cat(bold(blue(levels(sampleType)) %+% blue(" sample type was found")), fill = TRUE)
  if ("sampleTypes" %in% names(options)) {
    cat(red(options$sampleType), " using these from option\n")
    levels(sampleType) <- options$sampleTypes
  } else {
    levels(sampleType) <- c("BLANK", "CALIBRANT", "QUALITYCONTROL", "SAMPLE")
  }
  rawData$SampleType <- as.character(sampleType)

  fi <- which(colnames(rawData) == "SampleType")
  colnames(rawData)[fi] <- "sampleType"

  # cleaning LTR
  fi <- grepl("PLA|URI|SER|LTR", rawData$sampleID)
  rawData$sampleType[fi] <- "ltr"

  # concatenating metadata with the same order as the data
  # we use analysisName that is unique and contain plate information
  # for multiplate imports.
  rowList <- data.frame("AnalysisName" = sID)
  obsDescr <- list()
  for (i in seq_along(compoundList)) {
    descr <- rawData[rawData$AnalyteName == compoundList[i],]
    obsDescr[[i]]  <- merge(rowList,
                       descr,
                       all = TRUE)

    code <- sapply(strsplit(obsDescr[[i]]$AnalysisName, "_"), "[", options$codePosition)
    obsDescr[[i]]$sampleID <- code

  }
  dimDescr <- dim(obsDescr[[1]])

  if (sum(sapply(obsDescr, function(x) dim(x) != dimDescr)) != 0){
    cat(crayon::red("fusion: metadata dimension mismatch"))
  }

  # check for empty sampleType in obsDescr
  tmp <- obsDescr[[1]]$sampleType
  for (i in 2:length(obsDescr)) {
    fi <- which(is.na(tmp))
    if (length(fi > 0)) {
      tmp[fi] <- obsDescr[[i]]$sampleType[fi]
    }
  }

  fi <- is.na(tmp)
  newData <- newData[!fi,]

  for (i in 1:length(obsDescr)) {
    obsDescr[[i]] <- obsDescr[[i]][!fi,]
    obsDescr[[i]]$sampleType <- tmp[!fi]
  }

  da <- new("dataElement",
            .Data = newData,
            obsDescr = obsDescr,
            varName = unlist(colnames(newData)),
            type = "T-MS",
            method = "aminoAcids")

  cat(crayon::blue$bold("fusion: ") %+%
        crayon::blue$bold(ncol(da)) %+%
        crayon::blue$bold(" / ") %+%
        crayon::blue$bold(numberOfCompounds) %+%
        crayon::blue$bold(" metabolite imported for that method.\n"))

  return(da)
}

# preParseTr <- function(file) {
#   res <- readLines(file)
#   li <- sapply(res, function(x) length(strsplit(x, "\t")[[1]]))
#   le <- unique(li)
#
#   ne <- lapply(res, function(x) {
#     ch <- strsplit(x, "\t")[[1]]
#     if (length(ch) == max(le)) {
#       return(ch)
#     } else if (length(ch) > 0){
#       if (grepl("Compound", ch)[1]) {
#         return(c(ch, rep("NA", max(le) - length(ch))))
#       }
#       if (length(ch) == max(le)) {
#         return(c(ch, rep("NA", max(le) - length(ch))))
#       }
#     }
#   })
#
#   new <- do.call("rbind", ne)
#   new <- new[-1,]
#
#   return(new)
# }

# parseMS_Tr <- function(file, options) {
#   mw <- tMsTestsets$mw
#
#   rawData <- preParseTr(file)
#
#   cat(paste("fusion: using import method for tryptophan\n"))
#   cat(paste("fusion:", nrow(rawData),
#             "line(s) read\n"))
#   oldHeaders <- rawData[2,] # capturing column order
#   oldHeaders[1] <- "rowIndex"
#
#   fi <- rawData[,1] == "" # excluding title rows
#   rawData <- rawData[!fi,]
#   cat(paste("fusion:",
#             sum(fi),
#             "empty line(s) removed\n"))
#
#   spliter <- which((grepl("Compound", rawData[,1])))
#   numberOfCompounds <- length(spliter)
#   cat(paste("fusion:",
#             numberOfCompounds,
#             "compound(s) found\n"))
#
#   dataChkLength <- c(spliter[2:length(spliter)],
#                      nrow(rawData) + 1) - spliter[1:length(spliter)]
#
#   if (!length(unique(dataChkLength)) == 1) {
#     stop("fusion: data chunks have different size, check your data")
#   } else {
#     cat(paste("fusion:",
#               dataChkLength[1],
#               "is data chunk size\n"))
#   }
#
#   dataLength <- sum(dataChkLength)
#   cat(paste("fusion:",
#             dataLength,
#             "line(s) of data found\n"))
#
#   newData <- list()
#   headersOrder <- c("Name",
#              "Sample Text",
#              "Type",
#              "%Dev",
#              "Primary Flags",
#              "Conc.",
#              "Std. Conc",
#              "RT",
#              "Area",
#              "IS Area",
#              "Response")
#
#   for (i in 1:numberOfCompounds){
#
#     rge <- spliter[i]:(spliter[i] + dataChkLength[i] - 1)
#     cpndName <- rawData[spliter[i], 1]
#     dataChk <- rawData[rge,]
#
#     cpndName <- strsplit(dataChk[1,1], ":  ")[[1]][2] # reading title
#     dataChk <- dataChk[-1,] # removing title
#
#     idx <- match(headersOrder, oldHeaders)
#     dataChk <- dataChk[,idx]
#     colnames(dataChk) <- headersOrder
#
#     newData[[i]] <- list(cpndName = cpndName, #reading data chunk
#                          dataChk = data.frame(dataChk))
#   }
#
#   newDataLength <- sum(unlist(lapply(newData, function(x) dim(x$dataChk)[1])))
#
#   if (dataLength != newDataLength + length(spliter)) {
#     stop(paste("fusion: the expected length of the data is:",
#                dataLength,
#                "/ received:",
#                newDataLength + length(spliter)))
#   } else {
#     cat(paste("fusion:", numberOfCompounds, "compounds imported\n"))
#   }
#
#   # flipping the matrix
#   dataMatrix <- list()
#   obsDescr <- list()
#   varName <- list()
#   sampleNames <- newData[[1]][[2]]$Name
#
#   extractCode <- function(path) {
#     l <- strsplit(path, "_")
#     len <- length(l[[1]])
#     code <- l[[1]][options$codePosition]
#     return(code)
#   }
#   code <- do.call("rbind",
#                   lapply(sampleNames,
#                          function(x) extractCode(x)
#                   )
#   )
#   uid <- makeUnique(code, "#")
#
#   cleaningList <- c("dopamine",
#                     "citrulline")
#   for (chk in newData) {
#     cpndName <- chk[[1]]
#     if (tolower(cpndName) %in% tolower(cleaningList)) {
#       cat(crayon::blue("fusion: ") %+%
#             crayon::blue$bold(cpndName) %+%
#             crayon::blue(" ignored for that method."), fill = TRUE)
#     } else {
#     dataCol <- data.frame(as.numeric(unlist(chk[[2]]$`Conc.`)))
#     names(dataCol) <- cpndName
#     if (identical(chk[[2]]$Name, sampleNames)){
#       if (cpndName %in% mw$analyte) {
#         idx <- which(mw$analyte == cpndName)
#         dataMatrix <- c(dataMatrix, dataCol / mw$mw[idx])
#       } else {
#         dataMatrix <- c(dataMatrix, dataCol)
#         warning(paste("fusion:", cpndName, "molecular weight not found,
#                     concentration is not exported correctly\n"))
#       }
#       varName <- c(varName, cpndName)
#     } else {
#       stop ("fusion: row order alterated, matrix cannot be flipped")
#     }
#     fi <- which(names(chk[[2]]) == "Conc.")
#     descr <- data.frame(chk[[2]][, -fi], check.names = FALSE)
#
#     # adding sampleID
#     descr <- cbind(sampleID = uid, descr)
#
#     # renaming sampleType
#     fi <- which(colnames(descr) == "Type")
#     colnames(descr)[fi] <- "sampleType"
#
#     # set LTR to type ltr
#     fi <- grepl("PLA", descr$sampleID) |
#       grepl("LTR", descr$sampleID) |
#       grepl("URI", descr$sampleID)
#     descr$sampleType[fi] <- "ltr"
#
#     descr$sampleType <- tolower(descr$sampleType)
#     descr$sampleType[descr$sampleType == "analyte"] <- "sample"
#
#     obsDescr <- c(obsDescr, list(descr))
#     }
#   }
#
#   .Data <- do.call("cbind", dataMatrix)
#   if (nrow(.Data) == dataChkLength[1] - 1
#       & ncol(.Data) == numberOfCompounds) {
#     cat(paste("fusion: matrix flipped\n"))
#   }
#
#   da <- new("dataElement",
#             .Data = .Data,
#             obsDescr = obsDescr,
#             varName = unlist(varName),
#             type = "T-MS",
#             method = "tryptophan")
#   return(da)
# }
