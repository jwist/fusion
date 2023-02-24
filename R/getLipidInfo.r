#' get information about lipids from names
#' @param - an array with lipids names
#' @return - a table with names, classes and chain lengths
#' @export

getLipidsInfo <- function(lipid) {
  lmc <- strsplit(lipid, "\\(")
  lmc <- rapply(lmc, function(x) c(x[1], gsub("\\)", "", x[2])), how = "replace")
  r <- list()
  for (i in 1:length(lmc)) {
    struc <- strsplit(lmc[[i]][2], "_")[[1]]
    if (length(struc) == 1 | lmc[[i]][1] == "TAG") {
      totalCarbon <- gsub("[a-zA-Z\\-]+",
                          "",
                          strsplit(struc, ":")[[1]][1])
      if (lmc[[i]][1] == "TAG") {
        sideChain <- gsub("[a-zA-Z\\-]+",
                          "",
                          strsplit(lmc[[i]][2], "_")[[1]][2])
      } else {
        unsat <- gsub("[a-zA-Z\\-]+",
                      "",
                      strsplit(struc, ":")[[1]][2])
        sideChain <- paste0(totalCarbon, ":", unsat)
      }
    } else {
      t <- strsplit(struc, ":")
      sc <- unlist(lapply(t, function(x)
        as.numeric(
          gsub("[a-zA-Z\\-]+",
               "",
               x[1])
        )))
      unsatSc <- unlist(lapply(t, function(x)
        as.numeric(
          gsub("[a-zA-Z\\-]+",
               "",
               x[2])
        )))
      totalCarbon <- sum(sc)
      sideChain <- paste0(sc, ":", unsatSc)
      unsat <- sum(unsatSc)
    }
    r[[i]] <- c(lmc[[i]][1], totalCarbon, unsat, sideChain)
  }
  l <- max(unlist(lapply(r, function(x) length(x))))
  r <- lapply(r, function(x) c(x, rep(NA, l-length(x))))
  lipidClass <- data.frame(do.call("rbind", r))
  colnames(lipidClass) <- c("class", "nC", "r", "sc1", "sc2")
  return(lipidClass)
}
