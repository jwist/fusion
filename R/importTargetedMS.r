importTargetedMS <- function(file, method) {
  if (method == "tryptophan") {
    rawData <- read.table(file,
                          fill = TRUE,
                          sep = "\t",
                          dec = ".")

    cat(paste("fusion: using import method for",
              method, "\n"))
    cat(paste("fusion:", nrow(rawData),
              "line(s) read\n"))

    rawData <- rawData[-c(1,2),] #removing first title rows

    fi <- rawData[,1] == "" # excluding empty rows
    rawData <- rawData[!fi,]
    cat(paste("fusion:",
              sum(fi),
              "empty line(s) removed"))

    spliter <- which((grepl("Compound", rawData[,1])))
    numberOfCompounds <- length(spliter)
    cat(paste("fusion:",
              numberOfCompounds,
              "compound(s) found"))

    dataChkLength <- c(spliter[2:length(spliter)],
                       nrow(rawData) + 1) - spliter[1:length(spliter)]

    if (!length(unique(dataChkLength)) == 1) {
      stop("fusion: data chunks have different size, check your data")
    } else {
      cat(paste("fusion:",
                dataChkLength[1],
                "is data chunk size"))
    }

    dataLength <- sum(dataChkLength)
    cat(paste("fusion:",
              dataLength,
              "line(s) of data found"))

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
      cat(paste("fusion:", numberOfCompounds, "imported"))
    }

    # flipping the matrix
    dataMatrix <- list()
    obsDescr <- list()
    sampleNames <- newData[[1]][[2]]$Name

    for (chk in newData) {
      dataCol <- data.frame(chk[[2]]$`Conc.`)
      names(dataCol) <- chk[[1]]
      if (identical(chk[[2]]$Name, sampleNames)){
        dataMatrix <- c(dataMatrix, dataCol)
      } else {
        stop ("fusion: row order alterated, matrix cannot be flipped")
      }
      fi <- which(names(chk[[2]]) == "Conc.")
      obsDescr <- c(obsDescr, list(data.frame(chk[[2]][, -fi])))
    }

    .Data <- do.call("cbind", dataMatrix)
    if (nrow(.Data) == dataChkLength[1] - 1
        & ncol(.Data) == numberOfCompounds) {
      cat(paste("fusion: matrix flipped"))
    }

    varName <- unlist(lapply(newData, function(x) x[[1]]))
    da <- new("dataElement",
              .Data = .Data,
              obsDescr = obsDescr,
              varName = varName,
              type = "MS",
              method = "tryptophane")
  }
  return(da)
}
