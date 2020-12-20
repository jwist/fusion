#' get information from a list of runs
#'
#' @param listOfExpName - a list of experiment folders
#' @return a data frame with information
#'
#' @export

getInfoFromExpNameList <- function(listOfExpName) {
  expNo <- 0
  param <- list()
  for (j in 1:length(expNameList)) {

    folderPath <- file.path(dataPath, expNameList[j])
    folderList <- sort(as.numeric(dir(folderPath)))
    fi <- c(which(folderList == 99999), which(folderList == 98888))
    folderList <- folderList[-fi]

    p <- list()
    for (i in seq_along(folderList)) {
      path <- folderPath
      p[[i]] <- data.frame(path = dataPath,
                           expname = expNameList[j],
                           expno = folderList[i],
                           pulprog = getParam(file.path(path, folderList[i], "acqus"), "\\#\\#\\$PULPROG"),
                           exp = getParam(file.path(path, folderList[i], "acqus"), "\\#\\#\\$EXP"),
                           code = getParam(file.path(path, folderList[i], "acqus"), "\\#\\#\\$USERA2"),
                           title = getTitle(file.path(path, folderList[i], "pdata", "1", "title"))[1]
      )
    }
    param[[j]] <- do.call("rbind", p)
    expNo <- expNo + length(folderList)
    print(paste(j, "/", length(expNameList),
                "found:",  length(folderList),
                "expno in", expNameList[j],
                "cum:", expNo))
  }
  all <- do.call("rbind", param)
  return(all)
}



