#' importation function for targeted MS assays
#'
#' @param file - the file path to be imported
#' @param method - the method used to acquire the assay
#' @param codePosition - the position of the code in the file name
#' @return a dataElement
#'
#' @export
parseTargetedMS <- function(file, method, codePosition) {
  if (method == "tryptophan") {
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
      code <- l[[1]][codePosition]
      return(code)
    }
    code <- do.call("rbind",
                    lapply(sampleNames,
                           function(x) extractCode(x)
                    )
    )
    uid <- make.unique(code, "_")

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
      descr <- data.frame(chk[[2]][, -fi])
      descr <- cbind(sampleID = uid, descr)
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
  }
  ## Amino acids
  if (method == "aminoAcids") {
    rawData <- readxl::read_xlsx(file,
                                 sheet = "Sheet0")
    cat(paste("fusion: using import method for",
              method, "\n"))
    cat(paste("fusion:", nrow(rawData),
              "line(s) read\n"))

    fi <- !is.na(rawData$`Analyte Name`)
    rawData <- rawData[fi,]
    cat(paste("fusion:",
              sum(fi),
              "empty line(s) removed\n"))


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
    sampleNames <- newData[[1]][[2]]$`Data Set`

    extractCode <- function(path) {
      l <- strsplit(path, "_")
      len <- length(l[[1]])
      code <- l[[1]][codePosition]
      return(code)
    }
    code <- do.call("rbind",
                    lapply(sampleNames,
                           function(x) extractCode(x)
                    )
    )
    uid <- make.unique(code, "_")

    #4-hydroyproline, 5-oxoproline, Aminoadipic acid, Ethanolamine and Tryptophan
    for (chk in newData) {
      cpndName <- chk[[1]]
      # multiplication according to sample preparation
      dataCol <- suppressWarnings(as.numeric(chk[[2]]$`Quantity [units]`) * 2)
      names(dataCol) <- cpndName
      if (identical(chk[[2]]$`Data Set`, sampleNames)){
        dataMatrix <- c(dataMatrix, data.frame(dataCol))
      } else {
        stop ("fusion: row order alterated, matrix cannot be flipped")
      }
      fi <- which(names(chk[[2]]) == "Quantity [units]")
      descr <- data.frame(chk[[2]][, -fi])
      descr <- cbind(sampleID = uid, descr)
      obsDescr <- c(obsDescr, list(descr))
    }

    .Data <- do.call("cbind", dataMatrix)
    if (nrow(.Data) == dataChkLength[1]
        & ncol(.Data) == numberOfCompounds) {
      cat(paste("fusion: matrix flipped\n"))
    }

    varName <- unname(unlist(lapply(newData, function(x) x[[1]])))
    da <- new("dataElement",
              .Data = .Data,
              obsDescr = obsDescr,
              varName = varName,
              type = "T-MS",
              method = "aminoAcids")
  }
  return(da)
}
