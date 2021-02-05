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
parseTargetedMS <- function(file, method, options) {
  cat(paste("fusion: using import method for",
            method, "\n"))

  if (method == "tryptophan") {
    da <- parseMS_Tr(file, options)
  }
  if (method == "aminoAcids") {
    da <- parseMS_AA(file, options)

    if (ncol(da) == 60) {
      cat(crayon::green$bold("60/60 metabolite imported for that method.\n"))
    } else {
      cat(crayon::red$bold(ncol(da)) %+%
            crayon::red$bold("/60 metabolite imported for that method.\n"))
    }
    #p <- getType(da, using = "sampleType")
  }

  return(da)
}

parseMS_AA <- function(file, options) {
  rawData <- read.csv2(file,
                       sep = ",",
                       header = TRUE,
                       dec = ".",
                       check.names = FALSE)

  cat(paste("fusion:", nrow(rawData),
            "line(s) read\n"))

  fi <- !is.na(rawData$`Analyte Name`)
  rawData <- rawData[fi,]
  cat(paste("fusion:",
            sum(!fi),
            "empty line(s) removed\n"))

  # checking columns
  columnsList <- c("Analyte Name",
                   "Data Set",
                   "SampleType",
                   "m/z expected",
                   "_m/z [ppm]",
                   "RT [min]",
                   "mSigma",
                   "Area of PI",
                   "A/H",
                   "Review State",
                   "Quantity [units]",
                   "Quantity exp [units]",
                   "Accuracy [%]",
                   "Area [IS]",
                   "Recovery [%]")
  missingCol <- setdiff(columnsList, names(rawData))
  if (length(missingCol) > 0) {
    cat(crayon::red("fusion: column ") %+%
          crayon::red$bold(missingCol) %+%
          crayon::red("is missing from file."), fill = TRUE)
  }

  remainingCol <- setdiff(names(rawData), columnsList)
  if (length(remainingCol) > 0) {
    cat(crayon::blue("fusion: column ") %+%
          crayon::blue$bold(remainingCol) %+%
          crayon::blue(" is ignored for that method."), fill = TRUE)
  }

  if ("columnsList" %in% names(options)) {
    idx <- match(options$columnsList, names(rawData))
    rawData <- rawData[,idx]
  }

  # cleaning duplicated lines
  idx <- which(duplicated(rawData[,1:2]))
  if (length(idx) > 0) {
    d <- c()
    toRemove <- c()
    toCheck <- c()
    for (i in idx) {
      c <- which(rawData[,2] == rawData[i,2] & rawData[,1] == rawData[i,1])
      if (!identical(c, d)) {
        idxx <- which(rawData$`Quantity [units]`[c] != "n.c.")
        if (length(idxx) == 1) {
          if (length(c) > 1) {
            toRemove <- c(toRemove, c[-idxx])
          }
        } else if (length(idxx) == 0) {
          toRemove <- c(toRemove, c[-1])
        } else {
          toCheck <- c(toCheck, c[-1])
        }
      }
      d <- c
    }

    cat(crayon::red("fusion: duplicated line ") %+%
          crayon::red$bold(toRemove) %+%
          crayon::red(" was ignored."), fill = TRUE)
    #rawData <- rawData[-toRemove, ]

    cat(crayon::red$bold("fusion: cannot remove duplicated line. Please do it manually!\n"))
    cat(crayon::red("please check line ") %+%
          crayon::red$bold(toCheck), fill = TRUE)
    #rawData <- rawData[-toCheck, ]
    rawData <- rawData[-c(toCheck, toRemove), ]
  }

  compoundList <- unique(rawData$`Analyte Name`)
  numberOfCompounds <- length(compoundList)
  cat(paste("fusion:",
            numberOfCompounds,
            "compound(s) found\n"))

  dataChkLength <- table(factor(rawData$`Analyte Name`))
  if (!length(unique(dataChkLength)) == 1) {
    stop("fusion: data chunks have different size, check your data")
  } else {
    cat(paste("fusion:",
              dataChkLength[1],
              "is data chunk size\n"))
  }

  dataLength <- nrow(rawData)
  cat(paste("fusion:",
            dataLength,
            "line(s) of data found\n"))
  spliter <- c(1, cumsum(dataChkLength) + 1)[1:numberOfCompounds]

  newData <- list()
  for (i in 1:numberOfCompounds){

    rge <- spliter[i]:(spliter[i] + dataChkLength[i] - 1)
    cpndName <- rawData[spliter[i], 1]
    dataChk <- rawData[rge,]

    newData[[i]] <- list(cpndName = cpndName, #reading data chunk
                         dataChk = dataChk)
  }

  newDataLength <- sum(unlist(lapply(newData, function(x) dim(x$dataChk)[1])))
  if (dataLength != newDataLength) {
    stop(paste("fusion: the expected length of the data is:",
               dataLength,
               "/ received:",
               newDataLength))
  } else {
    cat(paste("fusion:", numberOfCompounds, "compounds imported\n"))
  }

  # flipping the matrix
  dataMatrix <- list()
  obsDescr <- list()
  varName <- list()
  sampleNames <- newData[[1]][[2]]$`Data Set`

  extractCode <- function(path) {
    l <- strsplit(path, "_")
    len <- length(l[[1]])
    code <- l[[1]][options$codePosition]
    return(code)
  }
  code <- do.call("rbind",
                  lapply(sampleNames,
                         function(x) extractCode(x)
                  )
  )
  uid <- makeUnique(code, "#")

  #4-hydroyproline, 5-oxoproline, Aminoadipic acid, Ethanolamine and Tryptophan
  cleaningList <- c("4--hydroxyproline",
                    "5-Oxoproline",
                    "Aminoadipic acid",
                    "Ethanolamine",
                    "Tryptophan",
                    "Carnosine")
  for (chk in newData) {
    cpndName <- chk[[1]]
    # removing unwanted variables
    if (tolower(cpndName) %in% tolower(cleaningList)) {
      cat(crayon::blue("fusion: ") %+%
            crayon::blue$bold(cpndName) %+%
            crayon::blue(" ignored for that method."), fill = TRUE)
    } else {
      # multiplication according to sample preparation
      dataCol <- suppressWarnings(as.numeric(chk[[2]]$`Quantity [units]`) * 2)
      names(dataCol) <- cpndName
      if (identical(chk[[2]]$`Data Set`, sampleNames)){
        dataMatrix <- c(dataMatrix, data.frame(cpndName = dataCol))
        varName <- c(varName, cpndName)
      } else {
        stop ("fusion: row order alterated, matrix cannot be flipped")
      }
      fi <- which(names(chk[[2]]) == "Quantity [units]")
      descr <- data.frame(chk[[2]][, -fi], check.names = FALSE)

      # adding sampleID
      descr <- cbind(sampleID = uid, descr)

      # renaming sampleType
      fi <- which(colnames(descr) == "SampleType")
      colnames(descr)[fi] <- "sampleType"

      # set LTR to type ltr
      fi <- grepl("PLA", descr$sampleID)
      descr$sampleType[fi] <- "ltr"

      descr$sampleType <- factor(descr$sampleType)
      levels(descr$sampleType) <- c("blank", "standard", "ltr", "qc", "sample")
      obsDescr <- c(obsDescr, list(descr))
    }
  }
  .Data <- do.call("cbind", dataMatrix)
  if (nrow(.Data) == dataChkLength[1]
      & ncol(.Data) == numberOfCompounds) {
    cat(paste("fusion: matrix flipped\n"))
  }

  da <- new("dataElement",
            .Data = .Data,
            obsDescr = obsDescr,
            varName = unlist(varName),
            type = "T-MS",
            method = "aminoAcids")
  return(da)
}

parseMS_Tr <- function(file, options) {
  data(mv)
  rawData <- read.table(file,
                        fill = TRUE,
                        sep = "\t",
                        dec = ".")

  cat(paste("fusion: using import method for",
            method, "\n"))
  cat(paste("fusion:", nrow(rawData),
            "line(s) read\n"))

  rawData <- rawData[-c(1,2),] # removing first title rows

  fi <- rawData[,1] == "" # excluding empty rows
  rawData <- rawData[!fi,]
  cat(paste("fusion:",
            sum(fi),
            "empty line(s) removed\n"))

  spliter <- which((grepl("Compound", rawData[,1])))
  numberOfCompounds <- length(spliter)
  cat(paste("fusion:",
            numberOfCompounds,
            "compound(s) found\n"))

  dataChkLength <- c(spliter[2:length(spliter)],
                     nrow(rawData) + 1) - spliter[1:length(spliter)]

  if (!length(unique(dataChkLength)) == 1) {
    stop("fusion: data chunks have different size, check your data")
  } else {
    cat(paste("fusion:",
              dataChkLength[1],
              "is data chunk size\n"))
  }

  dataLength <- sum(dataChkLength)
  cat(paste("fusion:",
            dataLength,
            "line(s) of data found\n"))

  newData <- list()
  for (i in 1:numberOfCompounds){

    rge <- spliter[i]:(spliter[i] + dataChkLength[i] - 1)
    cpndName <- rawData[spliter[i], 1]
    dataChk <- rawData[rge,]

    cpndName <- strsplit(dataChk[1,1], ":  ")[[1]][2] # reading title
    dataChk <- dataChk[-1,] # removing title
    names(dataChk) <- c("idx",
                        "index",
                        "Name",
                        "Sample Text",
                        "Type",
                        "%Dev",
                        "Primary Flags",
                        "Conc.",
                        "Std. Conc",
                        "RT",
                        "Area",
                        "IS Area",
                        "Response")
    newData[[i]] <- list(cpndName = cpndName, #reading data chunk
                         dataChk = dataChk[,2:13])
  }

  newDataLength <- sum(unlist(lapply(newData, function(x) dim(x$dataChk)[1])))

  if (dataLength != newDataLength + length(spliter)) {
    stop(paste("fusion: the expected length of the data is:",
               dataLength,
               "/ received:",
               newDataLength + length(spliter)))
  } else {
    cat(paste("fusion:", numberOfCompounds, "compounds imported\n"))
  }

  # flipping the matrix
  dataMatrix <- list()
  obsDescr <- list()
  sampleNames <- newData[[1]][[2]]$Name

  extractCode <- function(path) {
    l <- strsplit(path, "_")
    len <- length(l[[1]])
    code <- l[[1]][options$codePosition]
    return(code)
  }
  code <- do.call("rbind",
                  lapply(sampleNames,
                         function(x) extractCode(x)
                  )
  )
  uid <- makeUnique(code, "#")

  for (chk in newData) {
    cpndName <- chk[[1]]
    dataCol <- data.frame(chk[[2]]$`Conc.`)
    names(dataCol) <- cpndName
    if (identical(chk[[2]]$Name, sampleNames)){
      if (cpndName %in% mv$analyte) {
        idx <- which(mv$analyte == cpndName)
        dataMatrix <- c(dataMatrix, dataCol / mv$mw[idx])
      } else {
        dataMatrix <- c(dataMatrix, dataCol)
        warning(paste("fusion:", cpndName, "molecular weight not found,
                    concentration is not exported correctly\n"))
      }
    } else {
      stop ("fusion: row order alterated, matrix cannot be flipped")
    }
    fi <- which(names(chk[[2]]) == "Conc.")
    descr <- data.frame(chk[[2]][, -fi], check.names = FALSE)

    # adding sampleID
    descr <- cbind(sampleID = uid, descr)

    # renaming sampleType
    fi <- which(colnames(descr) == "Type")
    colnames(descr)[fi] <- "sampleType"

    # set LTR to type ltr
    fi <- grepl("PLA", descr$sampleID)
    descr$sampleType[fi] <- "ltr"

    descr$sampleType <- factor(descr$sampleType)
    levels(descr$sampleType) <- c("sample", "blanck", "ltr", "qc", "standard")
    obsDescr <- c(obsDescr, list(descr))
  }

  .Data <- do.call("cbind", dataMatrix)
  if (nrow(.Data) == dataChkLength[1] - 1
      & ncol(.Data) == numberOfCompounds) {
    cat(paste("fusion: matrix flipped\n"))
  }

  varName <- unlist(lapply(newData, function(x) x[[1]]))
  da <- new("dataElement",
            .Data = .Data,
            obsDescr = obsDescr,
            varName = varName,
            type = "T-MS",
            method = "tryptophane")
  return(da)
}
