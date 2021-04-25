
readFolder <- function(path) {
  expList <- dir(path)
  cat(crayon::blue("fusion::readFolder >> ", length(expList), "expnames found\n"))
  noesy <- list()
  cpmg <- list()
  for (exp in expList) {
    expPath <- file.path(path, exp)
    newExp <- readExperiment(file.path(expPath, "10"))
    if (!is.null(newExp)) {
      if ("ivdr" %in% names(newExp) & "lipoproteins" %in% names(newExp)) {
        noesy <- c(noesy, list(newExp))
        newExp <- readExperiment(file.path(expPath, "11"))
        if (!is.null(newExp)) {
          cpmg <- c(cpmg, list(newExp))
        } 
      } else {
        cat(crayon::yellow("fusion::readFolder >>", expPath, "ivdr or lipoproteins not found\n"))
      }
    }
  }
  cat(crayon::blue("fusion::readFolder >>", length(noesy), "noesy read\n"))
  cat(crayon::blue("fusion::readFolder >>", length(cpmg), "cpmg read\n"))
  return(list(noesy = noesy, cpmg = cpmg))
}

getValue <- function(list, name) {
  which(d$noesy[[1]]$procs$name == "WDW")
}

X <- t(sapply(d$noesy, function(x) x$spec$y))
eretic <- sapply(d$noesy, function(x) getEreticFactor(x$eretic))
fac <- sapply(eretic, function(x) x[[1]])
ref <- sapply(eretic, function(x) x[[2]])
ppm <- d$noesy[[1]]$spec$x

plot(X[1,c(85000:87000)], type = "l")
matplot(ppm[c(20300:20800)], t(X[,c(20300:20800)]), type = "l")
matplot(ppm[c(20300:20800)], t(X[,c(20300:20800)] / fac), type = "l")


plot(X[1,c(85000:87000)], type = "l")
matplot(ppm[c(20300:20800)], t(X[,c(20300:20800)]), type = "l")
matplot(ppm[c(85000:87000)], t(X[,c(85000:87000)] / fac), type = "l")

maximum <- apply(X[,c(20300:20800)] / fac, 1, max)
plot(maximum)
# pathToData <- "/home/rstudio/data/imports/data2/BIOGUNE"
# d <- readFolder(pathToData)